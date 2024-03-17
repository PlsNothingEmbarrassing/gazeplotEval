# Load necessary libraries
library(shiny)
library(ggplot2)
library(plotly)
library(eyelinkReader)
library(data.table)

gaze <- read_edf(file.choose(), import_samples = TRUE)

# Sample data (two datasets for demonstration)
trial1 <- gaze$samples[gaze$samples$trial ==1,]


# Sample data (three datasets for demonstration)
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

data <- gdata

# # Sample data
# data <- data.frame(
#   X = 1:10,
#   Y = seq(1, 10, by = 1)
# )

# Define the UI
ui <- fluidPage(
  titlePanel("Line Plot with Rescale Button"),
  
  # Main panel for the plot and rescale button
  mainPanel(
    plotlyOutput("plot"),
    sliderInput("line_positions", "Adjust Line Positions:",
                min = -100, max = 2000, value = c(400, 700)),
    actionButton("rescale_button", "Rescale Data")
  )
)

# Define the server
server <- function(input, output) {
  # Initialize a reactive value to store the data
  plot_data <- reactiveVal(data)
  
  # Render the plot based on the reactive data
  output$plot <- renderPlotly({
    gg <- ggplot(data = plot_data(), aes(T, X)) +
      geom_line() +
      geom_hline(yintercept = input$line_positions[1], linetype = "dashed", color = "red") +
      geom_hline(yintercept = input$line_positions[2], linetype = "dashed", color = "blue") +
      #labs(title = "Line Plot with Rescale Button") 
      ylim(min(plot_data()[2]-5),max(plot_data()[2]+5))
    
    p <- ggplotly(gg)
    p
  })
  
  # Observer to rescale the data when the button is pressed
  observeEvent(input$rescale_button, {
    current_data <- plot_data()
    degs <<- c(-10,10)
    valsfromsliders <<- c(input$line_positions[1],input$line_positions[2]) 
    coeffs <<- lm(degs~valsfromsliders) # use linear model 
    current_data$X <- current_data$X *coeffs$coefficients[2]+coeffs$coefficients[1] #Apply rescale
    #input$line_positions[1] <- 3
    plot_data(current_data)
  })
}

# Create the Shiny app
shinyApp(ui = ui, server = server)
