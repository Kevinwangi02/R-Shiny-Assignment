library(shiny)
library(rhandsontable)
library(ggplot2)
library(DT)
library(dplyr)
library(tidyr)
library(reshape2)
library(plotly)
library(rsconnect)


# UI
ui <- fluidPage(
  navbarPage(
    title = "R Shiny Claims Prediction",
    tabPanel(
      "Claims Prediction",
      sidebarLayout(
        sidebarPanel(
          sliderInput("start_year", "Start Year:", min = 1900, max = 2040, value = 2017, step = 1),
          sliderInput("loss_years", "Number of Loss Years:", min = 1, max = 20, value = 3, step = 1),
          sliderInput("dev_years", "Number of Development Years:", min = 2, max = 10, value = 4, step = 1),
          actionButton("generate_table", "Generate Table"),
          br(),
          selectInput(
            "tail_factor_mode",
            "Tail Factor Mode:",
            choices = c("Predefined Factors", "Custom Input")
          ),
          conditionalPanel(
            condition = "input.tail_factor_mode == 'Predefined Factors'",
            selectInput("tail_factor", "Tail Factor:", choices = seq(1, 2, by = 0.1), selected = 1.1)
          ),
          conditionalPanel(
            condition = "input.tail_factor_mode == 'Custom Input'",
            numericInput("custom_tail_factor", "Custom Tail Factor:", value = 1.1, min = 1, max = 5, step = 0.1)
          ),
          fileInput("upload_file", "Upload Incremental Claims (CSV):", accept = c(".csv")),
          actionButton("calculate", "Calculate Ultimate Claims"),
          br(),
          helpText("Note: Enter incremental claims in the table below or upload a CSV file. Leave empty cells where predictions are needed."),
          downloadButton("download_results", "Download Results")
        ),
        mainPanel(
          tabsetPanel(
            tabPanel(
              "Claims Input Table",
              rHandsontableOutput("claims_input_table")
            ),
            tabPanel(
              "Cumulative Paid Claims Table",
              DTOutput("cumulative_table")
            ),
            tabPanel(
              "Development Factors",
              DTOutput("dev_factors_table")
            ),
            tabPanel(
              "Cumulative Paid Claims Plot",
              plotlyOutput("claims_plot")
            )
          )
        )
      )
    )
  )
)

# Server
server <- function(input, output, session) {
  # Reactive values to store data
  data_storage <- reactiveValues(
    incremental_data = NULL,
    cumulative_data = NULL,
    dev_factors = NULL
  )
  
  # Generate Initial Claims Table
  observeEvent(input$generate_table, {
    num_loss_years <- input$loss_years
    num_dev_years <- input$dev_years
    start_year <- input$start_year
    dev_years <- paste0("Dev_Year_", 1:num_dev_years)
    
    # Generate a data frame with initial structure
    initial_data <- data.frame(
      Loss_Year = seq(start_year, start_year + num_loss_years - 1),
      matrix(NA, nrow = num_loss_years, ncol = num_dev_years, dimnames = list(NULL, dev_years))
    )
    data_storage$incremental_data <- initial_data
  })
  
  # Handle File Upload
  observeEvent(input$upload_file, {
    req(input$upload_file)
    uploaded_data <- read.csv(input$upload_file$datapath)
    data_storage$incremental_data <- uploaded_data
  })
  
  # Render Editable Claims Input Table
  output$claims_input_table <- renderRHandsontable({
    if (!is.null(data_storage$incremental_data)) {
      rhandsontable(data_storage$incremental_data, rowHeaders = FALSE)
    }
  })
  
  # Update Incremental Data
  observeEvent(input$claims_input_table, {
    data_storage$incremental_data <- hot_to_r(input$claims_input_table)
  })
  
  # Calculate Cumulative Claims, Development Factors, and Predict Missing Values
  observeEvent(input$calculate, {
    incremental_data <- data_storage$incremental_data
    if (is.null(incremental_data) || ncol(incremental_data) < 2) {
      showNotification("Error: Please provide valid incremental claims data.", type = "error")
      return()
    }
    
    # Step 1: Calculate cumulative claims
    cumulative_data <- incremental_data
    for (j in 2:ncol(cumulative_data)) {
      for (i in 1:nrow(cumulative_data)) {
        if (!is.na(incremental_data[i, j])) {
          cumulative_data[i, j] <- sum(incremental_data[i, 2:j], na.rm = TRUE)
        }
      }
    }
    
    # Step 2: Calculate development factors using consistent rows
    dev_factors <- numeric(ncol(cumulative_data) - 2)
    for (j in 2:(ncol(cumulative_data) - 1)) {
      valid_rows <- which(!is.na(cumulative_data[, j]) & !is.na(cumulative_data[, j + 1]))
      if (length(valid_rows) > 0) {
        sum_curr_year <- sum(cumulative_data[valid_rows, j + 1], na.rm = TRUE)
        sum_prev_year <- sum(cumulative_data[valid_rows, j], na.rm = TRUE)
        dev_factors[j - 1] <- ifelse(sum_prev_year > 0, sum_curr_year / sum_prev_year, NA)
      } else {
        dev_factors[j - 1] <- NA
      }
    }
    
    # Step 3: Predict missing cumulative claims using development factors
    for (j in 3:ncol(cumulative_data)) {
      for (i in 1:nrow(cumulative_data)) {
        if (is.na(cumulative_data[i, j]) && !is.na(cumulative_data[i, j - 1]) && !is.na(dev_factors[j - 2])) {
          cumulative_data[i, j] <- cumulative_data[i, j - 1] * dev_factors[j - 2]
        }
      }
    }
    
    # Step 4: Apply tail factor for the last development year
    tail_factor <- if (input$tail_factor_mode == "Predefined Factors") {
      as.numeric(input$tail_factor)
    } else {
      input$custom_tail_factor
    }
    
    for (i in 1:nrow(cumulative_data)) {
      if (!is.na(cumulative_data[i, ncol(cumulative_data) - 1])) {
        cumulative_data[i, ncol(cumulative_data)] <- cumulative_data[i, ncol(cumulative_data) - 1] * tail_factor
      }
    }
    
    # Store results
    data_storage$cumulative_data <- cumulative_data
    data_storage$dev_factors <- dev_factors
  })
  
  # Render Cumulative Claims Table
  output$cumulative_table <- renderDT({
    if (!is.null(data_storage$cumulative_data)) {
      datatable(data_storage$cumulative_data, options = list(pageLength = 5))
    }
  })
  
  # Render Development Factors Table
  output$dev_factors_table <- renderDT({
    if (!is.null(data_storage$dev_factors)) {
      df <- data.frame(
        Transition_Years = paste0(1:(length(data_storage$dev_factors)), "-", 2:(length(data_storage$dev_factors) + 1)),
        Development_Factor = round(data_storage$dev_factors, 9)
      )
      datatable(df, options = list(pageLength = 5))
    }
  })
  
  # Render Interactive Cumulative Claims Plot
  output$claims_plot <- renderPlotly({
    if (!is.null(data_storage$cumulative_data)) {
      data <- data_storage$cumulative_data
      melted_data <- melt(data, id.vars = "Loss_Year", variable.name = "Development_Year", value.name = "Claims")
      melted_data$Development_Year <- as.numeric(gsub("Dev_Year_", "", melted_data$Development_Year))
      
      plot_ly(melted_data, x = ~Development_Year, y = ~Claims, color = ~as.factor(Loss_Year), type = 'scatter', mode = 'lines+markers') %>%
        layout(
          title = "Cumulative Paid Claims",
          xaxis = list(title = "Development Year"),
          yaxis = list(title = "Cumulative Paid Claims ($)", tickformat = ","),
          legend = list(title = list(text = "Loss Year"))
        )
    }
  })
  
  # Allow Results Download
  output$download_results <- downloadHandler(
    filename = function() {
      paste("cumulative_claims_results", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_storage$cumulative_data, file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui, server)