
library(shiny)
library(fpp3)
library(readr)

# Load data as a tsibble
wine_data <- read_csv(
  "AustralianWines.csv",
  na = "*",
  col_types = cols(Rose = col_number()),
  show_col_types = FALSE
) |>
  fill(Rose, .direction = "down") |>
  mutate(
    Month = paste0(
      substr(Month, 1, 4),
      "19",
      substr(Month, 5, 6)
    ),
    Month = yearmonth(Month, format = "%b-%Y")
  ) |>
  pivot_longer(
    cols = -Month,
    names_to = "Varietal",
    values_to = "Sales"
  ) |>
  as_tsibble(
    key = Varietal,
    index = Month
  )

# Define UI
ui <- fluidPage(
  
  titlePanel("Australian Wine Sales"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput(
        inputId = "varietal",
        label = "Select a wine varietal:",
        choices = sort(unique(wine_data$Varietal)),
        selected = sort(unique(wine_data$Varietal))[1]
      ),
      dateRangeInput(
        inputId = "date_range",
        label = "Select date range:",
        start = as.Date(min(wine_data$Month)),
        end = as.Date(max(wine_data$Month)),
        min = as.Date(min(wine_data$Month)),
        max = as.Date(max(wine_data$Month))
      ),
      numericInput(
        inputId = "horizon",
        label = "Forecast horizon (months):",
        value = 12,
        min = 1,
        max = 60,
        step = 1
      ),
      sliderInput(
        inputId = "train_pct",
        label = "Training data percentage:",
        min = 60,
        max = 90,
        value = 80,
        step = 5,
        post = "%"
      ),
      selectInput(
        inputId = "selected_model",
        label = "Select forecasting model:",
        choices = c("TSLM", "ETS", "ARIMA"),
        selected = "ARIMA"
      ),
      selectInput(
        inputId = "transformation",
        label = "Select Transformation:",
        choices = c(
          "None" = "none",
          "Log" = "log",
          "Square Root" = "sqrt",
          "Box-Cox" = "boxcox"
        ),
        selected = "none"
      )
    ),
    
    mainPanel(
      plotOutput("stlPlot", height = "450px"),
      h3("Model Specifications"),
      tableOutput("model_specs"),
      h3("Model Accuracy"),
      tableOutput("accuracy_table"),
      h3("Comparative Model Forecasts"),
      plotOutput("comparison_plot", height = "550px")
      )
    )
  )

# Define server
server <- function(input, output) {
  
  # Filter data based on selected varietal
  selected_wine <- reactive({
    wine_data |>
      filter(
        Varietal == input$varietal,
        Month >= yearmonth(input$date_range[1]),
        Month <= yearmonth(input$date_range[2])
      )
  })
  
  # Generate STL decomposition plot
  output$stlPlot <- renderPlot({
    
    selected_wine() |>
      model(
        STL(Sales ~ season(window = "periodic"))
      ) |>
      components() |>
      autoplot() +
      labs(
        title = paste(input$varietal, "Wine Sales: STL Decomposition"),
        x = "Month"
      )
    
  })

  
  # Split data into training and validation
  split_data <- reactive({
    
    data <- selected_wine()
    
    train_size <- floor(nrow(data) * input$train_pct / 100)
    
    list(
      train = slice(data, 1:train_size),
      validation = slice(data, (train_size + 1):nrow(data))
    )
  })
  
  # Fit models using the selected transformation
  fit_models <- function(data) {
    
    # Apply no transformation
    if (input$transformation == "none") {
      
      data |>
        model(
          TSLM = TSLM(Sales ~ trend() + season()),
          ETS = ETS(Sales),
          ARIMA = ARIMA(Sales)
        )
      
      # Apply logarithmic transformation
    } else if (input$transformation == "log") {
      
      data |>
        model(
          TSLM = TSLM(log(Sales) ~ trend() + season()),
          ETS = ETS(log(Sales)),
          ARIMA = ARIMA(log(Sales))
        )
      
      # Apply square root transformation
    } else if (input$transformation == "sqrt") {
      
      data |>
        model(
          TSLM = TSLM(sqrt(Sales) ~ trend() + season()),
          ETS = ETS(sqrt(Sales)),
          ARIMA = ARIMA(sqrt(Sales))
        )
      
      # Apply Box-Cox transformation
    } else if (input$transformation == "boxcox") {
      
      lambda <- data |>
        features(Sales, guerrero) |>
        pull(lambda_guerrero)
      
      data |>
        model(
          TSLM = TSLM(box_cox(Sales, lambda) ~ trend() + season()),
          ETS = ETS(box_cox(Sales, lambda)),
          ARIMA = ARIMA(box_cox(Sales, lambda))
        )
    }
  } 
  
  # Fit models after transformation
  fitted_models <- reactive({
    
    train_data <- split_data()$train
    
    fit_models(train_data)
    
  })
  
  # Refit models using all selected historical data
  future_models <- reactive({
    
    fit_models(selected_wine())
    
  })
  
  # Forecast over validation period
  validation_forecasts <- reactive({
    
    fitted_models() |>
      forecast(h = nrow(split_data()$validation))
  })
  
  # Forecast into the future
  future_forecasts <- reactive({
    
    future_models() |>
      select(all_of(input$selected_model)) |>
      forecast(h = input$horizon)
    
  })
  
  model_parameters <- reactive({
    fitted_models() |>
      tidy()
  })
  
  model_accuracy <- reactive({
    
    # Get the selected model
    selected_fit <- fitted_models() |>
      select(all_of(input$selected_model))
    
    # Calculate training accuracy
    train_accuracy <- accuracy(selected_fit) |>
      mutate(Dataset = "Training")
    
    # Generate validation forecasts using selected model only
    selected_validation_fc <- selected_fit |>
      forecast(h = nrow(split_data()$validation))
    
    # Calculate testing accuracy
    test_accuracy <- accuracy(
      selected_validation_fc,
      split_data()$validation
    ) |>
      mutate(Dataset = "Testing")
    
    # Combine training and testing accuracy
    bind_rows(train_accuracy, test_accuracy) |>
      select(Dataset, RMSE, MAE, MAPE)
    
  })
  
  # Accuracy table output
  output$accuracy_table <- renderTable({
    model_accuracy()
  }, digits = 3)
  
  # Model Specifications
  model_specs <- reactive({
    
    models <- fitted_models()
    
    # Get the selected model specification
    specification <- model_sum(
      models[[input$selected_model]][[1]]
    )
    
    # Display Box-Cox lambda when applicable
    if (input$transformation == "boxcox") {
      
      lambda <- split_data()$train |>
        features(Sales, guerrero) |>
        pull(lambda_guerrero)
      
      tibble(
        Model = input$selected_model,
        Transformation = "Box-Cox",
        Lambda = round(lambda, 3),
        Specification = specification
      )
      
    } else {
      
      tibble(
        Model = input$selected_model,
        Transformation = switch(
          input$transformation,
          none = "None",
          log = "Log",
          sqrt = "Square Root"
        ),
        Specification = specification
      )
    }
  })
  
  output$model_specs <- renderTable({
    model_specs()
  })
  
  # forecast plot of selected model with historical data
  output$forecast_plot <- renderPlot({
    
    historical_data <- selected_wine() |>
      select(Month, Sales) |>
      as_tsibble(index = Month)
    
    forecast_data <- future_forecasts()
    
    autoplot(forecast_data, historical_data) +
      labs(
        title = paste(input$varietal, "Wine Sales Forecast"),
        subtitle = paste("Model:", input$selected_model),
        x = "Month",
        y = "Wine Sales"
      ) +
      theme_minimal()
    
  })
  
  # plot all model forecasts
  comparative_forecasts <- reactive({
    
    fitted_models() |>
      forecast(h = input$horizon)
    
  })
  
  output$comparison_plot <- renderPlot({
    
    forecast_data <- comparative_forecasts()
    
    historical_data <- split_data()$train
    
    autoplot(
      forecast_data,
      level = c(80, 95)
    ) +
      facet_wrap(~.model, nrow = 1) +
      labs(
        title = paste(input$varietal, "Comparative Forecasts"),
        x = "Month",
        y = "Wine Sales"
      ) +
      theme_minimal()
    
  })
}

# Run application
shinyApp(ui = ui, server = server)
