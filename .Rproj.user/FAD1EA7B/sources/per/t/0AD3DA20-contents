library(shiny)
library(ggplot2)
library(plotly)
library(shinyjs)
library(eyelinkReader)
library(data.table)

gaze <- read_edf(file.choose(), import_samples = TRUE)

# Sample data (two datasets for demonstration)
trial1 <- gaze$samples[gaze$samples$trial == 1,]
str(tail(trial1))
gdata <- data.frame(
  T = trial1$time_rel,
  X = trial1$gxR,
  Y = trial1$gyR
)

rdata <- data.frame(
  T = trial1$time_rel,
  X = trial1$pxR,
  Y = trial1$pyR
)

ui <- fluidPage(
  useShinyjs(),
  titlePanel("Interactive Data Plotting"),
  
  mainPanel(
    selectInput("dataset_selector", "Select Gaze or Raw:",
                choices = c("Gaze", "Raw"), selected = "Gaze"),
    selectInput("column_selector", "Select X or Y:",
                choices = c("X", "Y"), selected = "X"),
    sliderInput("plus_ten_slider", "Input +10%:", min = 0, max = 1500, value = 500),
    sliderInput("minus_ten_slider", "Input -10%:", min = 0, max = 1500, value = 500),
    actionButton("rescale_button", "Rescale Data"),
    plotlyOutput("plot")
  )
)

server <- function(input, output, session) {
  plot_data <- reactiveVal(gdata)
  
  # output$plot <- renderPlotly({
  #   selected_data <- switch(input$dataset_selector,
  #                           "Gaze" = plot_data(),
  #                           "Raw" = rdata,
  #                           default = plot_data()
  #   )
  observe({
    selected_data <- input$dataset_selector
    updated_value<- switch(
      selected_data,
      "Gaze" = gdata,
      "Raw" = rdata,
      default = gdata)
    plot_data(updated_value)
    
  })
  output$plot<- renderPlotly({
    gg <- ggplot(data = plot_data(), aes(T, X)) +
      geom_line() + geom_line(y = 0, linetype = "dashed") +
      geom_hline(yintercept = input$plus_ten_slider, color = "red") +
      labs(x = "X-axis", y = "Y-axis") + 
      geom_line() + geom_line(y = 0, linetype = "dashed") +
      geom_hline(yintercept = input$minus_ten_slider, color = "blue") +
      labs(x = "X-axis", y = "Y-axis") +
      theme_minimal() +
      labs(title = "Gaze Data Plotting")
    
    p <- ggplotly(gg)
    p
  })
  
  observeEvent(input$rescale_button, {
    current_data <- plot_data()
    degs <- c(-10, 10)
    valsfromsliders <- c(input$plus_ten_slider, input$minus_ten_slider)
    coeffs <- lm(degs ~ valsfromsliders)
    current_data$X <- current_data$X * coeffs$coefficients[2] + coeffs$coefficients[1]
    plot_data(current_data)
    updateSliderInput(session, "plus_ten_slider", value = 10, min = 0, max = 90)
    updateSliderInput(session, "minus_ten_slider", value = -10, min = -90, max = 0)
    shinyjs::disable("rescale_button")
  })
}




shinyApp(ui = ui, server = server)

