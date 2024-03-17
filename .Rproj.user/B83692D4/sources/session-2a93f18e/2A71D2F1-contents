library(shiny)
library(ggplot2)
library(plotly)
library(shinyjs)
library(eyelinkReader)
library(data.table)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Interactive Gaze Data Plotting"),
  
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Choose EyeLink Data File"),
      uiOutput("trial_selector"),
      uiOutput("eye_selector"),
      selectInput("axis_selector", "Select Axis:", choices = c("X", "Y"), selected = "X"),
      sliderInput("plus_ten_slider", "+10 Degrees:", min = 0, max = 1920, value = 500),
      sliderInput("minus_ten_slider", "-10 Degrees:", min = 0, max = 1920, value = 500),
      hidden(sliderInput("analysis_start_slider", "Beginning of analysis segment:", min = 0, max = 1920, value = 500)),
      hidden(sliderInput("analysis_end_slider", "End of analysis segment:", min = 0, max = 1920, value = 500)),
      hidden(actionButton("analysis_button", "Analyse section")),
      actionButton("rescale_button", "Rescale Data"),
      downloadButton("downloadPlot", "Download Plot")
    ),
    mainPanel(
      plotlyOutput("plot")
    )
  )
)


  
server <- function(input, output, session) {
    
    gaze_data <- reactive({
      req(input$file)
      data <- read_edf(input$file$datapath, import_samples = TRUE)
      
    })
    rescale_lines_visible <- reactiveVal(TRUE)
    transformed_data <- reactiveVal(NULL)
    stored_slope <- reactiveVal()
    stored_intercept <-reactiveVal()
    
  # Choose which trial
  output$trial_selector <- renderUI({
    req(gaze_data())
    selectInput("trial", "Select Trial:", choices = unique(gaze_data()$samples$trial))
  })
  # Choose which eye (or both)
  output$eye_selector <- renderUI({
    req(gaze_data())
    if(all(is.na(gaze_data()$samples$gxL))){
      choices<- c("Right Eye" = "R")
    }
    else{
      choices <- c("Left Eye" = "L", "Right Eye" = "R", "Both Eyes" ="B")
    }
    
    selectInput("eye", "Select Eye Data:", choices = choices)
  })
  
  # Plotting data based on values from inputs
  plot_data <- reactive({
    req(input$trial, gaze_data())
    # Update data based on trial selected
    trial_data <- gaze_data()$samples[gaze_data()$samples$trial == input$trial,]
    
    # Update data based on eye selected
    if(input$eye == "L" || input$eye == "B") {
      trial_data <- trial_data[, c("time_rel", "gxL", "gyL")]
      colnames(trial_data) <- c("T", "X", "Y")
    
    } else {
      trial_data <- trial_data[, c("time_rel","gxR","gyR")]
      colnames(trial_data) <- c("T", "X", "Y")
    }
    
    if(input$eye == "B") {
      # Use combined average
      average_values <- (trial_data$X + trial_data$Y) / 2
      trial_data$X <- average_values
      trial_data$Y <- average_values
    }
    
    
    trial_data
  })
  
  
  output$plot <- renderPlotly({
    req(plot_data())
    # If the transformed reactiveVal has been updated then use rescaled data
    #data_to_plot <- if(is.null(transformed_data())) plot_data() else transformed_data()
    data_to_plot <- plot_data()
    
    if(!is.null(stored_slope()) && !is.null(stored_intercept())){
      if(input$axis_selector == "X"){
        data_to_plot$X <- data_to_plot$X * stored_slope() + stored_intercept()
      } else{
        data_to_plot$Y <- data_to_plot$Y * stored_slope() + stored_intercept()
      }
    }
    #selected_axis <- ifelse(input$axis_selector == X, plot_data()$X, plot_data()$Y)
    y_col <- if(input$axis_selector == "X") "X" else "Y"
    
    gg <- ggplot(data_to_plot, aes(x = T,y=.data[[y_col]])) +
      geom_line() + geom_hline(yintercept= 0, linetype = "dashed") +
      labs(title = "Gaze Data Plotting", x = "Time", y = input$axis_selector) +
      theme_minimal()
    if(rescale_lines_visible()){
      gg <- gg + geom_hline(yintercept = input$plus_ten_slider, color = "red") +
        geom_hline(yintercept = input$minus_ten_slider, color = "blue")
    } else{
      gg <- gg + geom_vline(xintercept = input$analysis_start_slider, color = "red") +
        geom_vline(xintercept = input$analysis_end_slider, color = "blue")
    }
    #str(head(data_to_plot))
    ggplotly(gg)
  })
  
  # Rescaling Data
  observeEvent(input$rescale_button, {
    current_data <- plot_data()
    
    degs <- c(-10, 10)
    valsfromsliders <- c(input$minus_ten_slider, input$plus_ten_slider)
    fit <- lm(degs ~ valsfromsliders)
    slope <- coef(fit)[2]
    intercept <- coef(fit)[1]
    
    # Apply the transformation
    if(input$axis_selector == "X") {
    # Update reactiveVal with transformed data
    transformed <- transform(current_data, X = slope * X + intercept)
    transformed_data(transformed)
      print("Here")
    } else {
    transformed <- transform(current_data, Y = slope * Y + intercept)
    transformed_data(transformed)
      print("There")
    }
    # Store slope and intercept
    stored_slope(slope)
    stored_intercept(intercept)
    # Remove rescaling lines and hide sliders
    rescale_lines_visible(FALSE)
    shinyjs::hide("minus_ten_slider")
    shinyjs::hide("plus_ten_slider")
    shinyjs::disable("rescale_button")
    #Update analysis slider default values before showing
    max_time_val <- reactive({
      req(plot_data())
      max(plot_data()$T, na.rm = TRUE)    })
    
    updateSliderInput(session, "analysis_start_slider", min = 0, max = max_time_val(), value = max_time_val()/2)
    updateSliderInput(session, "analysis_end_slider", min = 0, max = max_time_val(), value = max_time_val()/4)
    
    # Show lines to define area of analysis
    show("analysis_start_slider")
    show("analysis_end_slider")
  })
  observeEvent(input$analysis_button,{
    # Get nystagmus metrics
    event_data <- gaze_data()$samples[gaze_data()$samples$trial == input$trial,]
    
  })
  
  # Downloading the plot
  output$downloadPlot <- downloadHandler(
    filename = function() { paste("gaze_data_plot", Sys.Date(), ".png", sep = "") },
    content = function(file) {
      g <- ggplotly(plot_data())
      ggsave(file, plot = g)
    }
  )
}

shinyApp(ui = ui, server = server)