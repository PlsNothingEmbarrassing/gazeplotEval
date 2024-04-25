library(shiny)
library(ggplot2)
library(plotly)
library(shinyjs)
library(eyelinkReader)
library(data.table)
library(dplyr)
library(gridExtra)

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
      downloadButton("downloadPlot", "Download Plot"),
      actionButton("reset_button", "Reset")
    ),
    mainPanel(
      plotlyOutput("plot"),
      hidden(textOutput("metrics"))
    )
  )
)


  
server <- function(input, output, session) {
    
    
   load_gaze_data <-function(){
     req(input$file)
     isolate({
     validate(
       need(tolower(tools::file_ext(input$file$name))=="edf", "Please upload valid .edf EyeLink file")
     )})
     data <- read_edf(input$file$datapath, import_samples = TRUE)
     return(data)
   }
    gaze_data <- reactive({
      load_gaze_data()
    })
    prev_file_path <- reactiveVal(NULL)
    rescale_lines_visible <- reactiveVal(TRUE)
    transformed_data <- reactiveVal(NULL)
    stored_slope <- reactiveVal()
    stored_intercept <-reactiveVal()
    sampling_freq <-reactiveVal(1000) # default val
    frequency_result <- reactiveVal(NULL)
    avg_amplitude <- reactiveVal(NULL)
    text_output <- reactiveVal(NULL)
    plot_download<- reactive(NULL)
    # must  be non null for reactive plot
    saccade_times <- reactiveVal(data.frame(sttime_rel=numeric(), entime_rel=numeric()))
  
    
    
  # Choose which trial
  output$trial_selector <- renderUI({
    req(gaze_data())
    selectInput("trial", "Select Trial:", choices = unique(gaze_data()$samples$trial))
  })
  # Reset button
  observeEvent(input$reset_button, {
    # Reload the application
    session$reload()
  })
  # If file changes then reset otherwise unexpected behaviour

    observe({
      current_file_path <- input$file$datapath
      
      if(!is.null(prev_file_path()) && current_file_path != prev_file_path()){
        session$reload()
      }
      prev_file_path(current_file_path)
    })
  
  
  
  # Choose which eye (or both)
  output$eye_selector <- renderUI({
    req(gaze_data())
    if(all(is.na(gaze_data()$samples$gxL))){
      choices<- c("Right Eye" = "RIGHT")
    }
    else{
      choices <- c("Left Eye" = "LEFT", "Right Eye" = "RIGHT", "Both Eyes" ="B")
    }
    
    selectInput("eye", "Select Eye Data:", choices = choices)
  })
  
  # Plotting data based on values from inputs
  plot_data <- reactive({
    req(input$trial, gaze_data())
    # Update data based on trial selected
    trial_data <- gaze_data()$samples[gaze_data()$samples$trial == input$trial,]
    # infer sampling frequency
    #time_diffs <- diff(trial_data$time_rel)
    #mode_time_diff <- which.max(table(time_diffs))
    #sampling_freq(ifelse(mode_time_diff == 1,1/1,1/2))
    
    # Update data based on eye selected
    if(input$eye == "LEFT") {
      trial_data <- trial_data[, c("time_rel", "gxL", "gyL")]
      colnames(trial_data) <- c("T", "X", "Y")
      
    
    } else if (input$eye =="RIGHT"){
      trial_data <- trial_data[, c("time_rel","gxR","gyR")]
      colnames(trial_data) <- c("T", "X", "Y")

      
    }
    
    else{
      # Use combined average
      trial_data <- trial_data[, c("time_rel", "gxL", "gyL", "gxR", "gyR")]
      colnames(trial_data) <- c("T", "X_left", "Y_left", "X_right", "Y_right")
      
      # Calculate average for X and Y
      trial_data$X <- rowMeans(trial_data[, c("X_left", "X_right")], na.rm = TRUE)
      trial_data$Y <- rowMeans(trial_data[, c("Y_left", "Y_right")], na.rm = TRUE)
      # Remove individual columns for left and right eye
      trial_data <- trial_data[, !(names(trial_data) %in% c("X_left", "Y_left", "X_right", "Y_right"))]

    }
    
    
    trial_data
  })
  
  
  output$plot <- renderPlotly({
    req(plot_data(), saccade_times())
    # If the transformed reactiveVal has been updated then use rescaled data
    #data_to_plot <- if(is.null(transformed_data())) plot_data() else transformed_data()
    
    data_to_plot <- plot_data()
    saccades <- saccade_times()
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
    
    
    ggplotly(gg)
    
  })
  
  # Rescaling Data
  observeEvent(input$rescale_button, {
    current_data <- plot_data()
    
    degs <- c(-10, 10)
    if(input$minus_ten_slider == input$plus_ten_slider){
      showModal(modalDialog(
        title = "Error",
        "Rescaling values cannot be the same."
      ))
      # reset slider
      updateSliderInput(session, "minus_ten_slider", value = input$plus_ten_slider - 1)
      # Abort rest of analysis
      return(NULL)
    }
    valsfromsliders <- c(input$minus_ten_slider, input$plus_ten_slider)
    fit <- lm(degs ~ valsfromsliders)
    slope <- coef(fit)[2]
    intercept <- coef(fit)[1]
    
    # Apply the transformation
    transformed <- transform(current_data,
                             X = slope * X + intercept,
                             Y = slope * Y + intercept)
    transformed_data(transformed)
    
    # Store slope and intercept
    stored_slope(slope)
    stored_intercept(intercept)
    # Remove rescaling lines and hide sliders
    rescale_lines_visible(FALSE)
    shinyjs::hide("minus_ten_slider")
    shinyjs::hide("plus_ten_slider")
    shinyjs::disable("rescale_button")
    shinyjs::hide("rescale_button")
    #Update analysis slider default values before showing
    max_time_val <- reactive({
      req(plot_data())
      max(plot_data()$T, na.rm = TRUE)    })
    
    updateSliderInput(session, "analysis_start_slider", min = 0, max = max_time_val(), value = max_time_val()/4)
    updateSliderInput(session, "analysis_end_slider", min = 0, max = max_time_val(), value = max_time_val()/2)
    
    # Show lines to define area of analysis
    show("analysis_start_slider")
    show("analysis_end_slider")
    show("analysis_button")
  })
  
  observeEvent(input$analysis_button,{
    # Get nystagmus metrics
    # Check to make sure that end > start time
    if(input$analysis_start_slider > input$analysis_end_slider){
      showModal(modalDialog(
        title = "Error",
        "Analysis start value cannot be higher than the end analysis value."
      ))
      # reset slider
      updateSliderInput(session, "analysis_start_slider", value = input$analysis_end_slider)
      # Abort rest of analysis
      return(NULL)
    }
    # Get saccadic event data from selected trial
    saccade_data <- gaze_data()$event[gaze_data()$event$type %in% c("ENDSACC") & gaze_data()$event$trial == input$trial,]
    if(any(input$eye == 'LEFT' || input$eye == 'RIGHT')){
      # if left or right eye then get data for each eye
      # Get saccade event data for eye choice
      saccade_eye_data <- saccade_data[saccade_data$eye %in% input$eye,]
      
      # Filter saccade data based on the analysis time interval
      saccade_filtered <- saccade_eye_data[
        saccade_eye_data$sttime_rel >= input$analysis_start_slider &
          saccade_eye_data$entime_rel <= input$analysis_end_slider,
        c('sttime_rel', 'entime_rel')
      ]
      
      # Update reactiveVal with filtered saccade timings
      
    }
    else{
      # if both then get all saccades
      
      # Get saccade event data for eye choice
      saccade_eye_data <- saccade_data[saccade_data$eye %in% c('LEFT', 'RIGHT'),]
      
      # Filter saccade data based on the analysis time interval
      saccade_filtered <- saccade_eye_data[
        saccade_eye_data$sttime_rel >= input$analysis_start_slider &
          saccade_eye_data$entime_rel <= input$analysis_end_slider,
        c('sttime_rel', 'entime_rel')
      ]
      
      
    }
    # Check to ensure there are saccades within the analysis region
    if(nrow(saccade_filtered) == 0){
      showModal(modalDialog(
        title = "No Saccades",
        "No saccades detected in this region"
      ))
      return(NULL)
    }
    saccade_times(saccade_filtered)
    output$metrics <- renderText({
      isolate({
      req(saccade_extremes(), frequency_result(), avg_amplitude())
      sacc_data <- saccade_extremes()
      rounded_avg <- round(avg_amplitude(), 3)
      rounded_freq <- round(frequency_result(), 3)
      frequency_result(rounded_freq)
      avg_amplitude(rounded_avg)
      text <- paste("Frequency: ", frequency_result(), "Hz", " Amplitude: ", avg_amplitude(), " degrees")
      text_output(text)
      return(text_output())
    })
    })
    shinyjs::hide("trial_selector")
    show("metrics")
  })
  
  saccade_extremes <- reactive({
    req(saccade_times(), transformed_data(), input$axis_selector, sampling_freq)
    
    saccades <- saccade_times()
    data <- transformed_data()
    
    results <- lapply(1:nrow(saccades), function(i) {
      current_saccade <- saccades[i,]
      
      interval_data <- data %>%
        filter(T >= current_saccade$sttime_rel, T <= current_saccade$entime_rel)
      
      if (input$axis_selector == 'X') {
        if (nrow(interval_data) > 0) {
          list(
            saccade_index = i,
            max_x = max(interval_data$X, na.rm = TRUE),
            min_x = min(interval_data$X, na.rm = TRUE)
          )
        } else {
          list(
            saccade_index = i,
            max_x = NA,
            min_x = NA
          )
        }
      } else {
        if (nrow(interval_data) > 0) {
          list(
            saccade_index = i,
            max_y = max(interval_data$Y, na.rm = TRUE),
            min_y = min(interval_data$Y, na.rm = TRUE)
          )
        } else {
          list(
            saccade_index = i,
            max_y = NA,
            min_y = NA
          )
        }
      }
    })
    
    # Combine into a data frame
    extremes_df <- do.call(rbind, lapply(results, as.data.frame))
    
    # Calculate Amplitude
    if(input$axis_selector == 'X'){
      print(extremes_df$max_x)
      diff_val <- with(extremes_df, abs(max_x - min_x))
      print("X Diff:")
      print(diff_val)
      
      amplitude <- abs(diff_val)
      print("X Amplitude:")
      print(amplitude)
      
      average_amp <- mean(amplitude)
      print("X Average Amplitude:")
      print(average_amp)
    }
    else{
      
      diff_val <- with(extremes_df, abs(max_y - min_y))
      
      
      amplitude <- abs(diff_val)

      
      average_amp <- mean(amplitude)

      
    }
    
    # Get analysis times, convert to seconds, then calculate frequency
    analysis_time <- input$analysis_end_slider - input$analysis_start_slider
    analysis_time_secs <- analysis_time/sampling_freq()
    
    num_sacc <- nrow(extremes_df)
    if(input$eye == "B"){
      print(num_sacc)
      both_num_sacc<- num_sacc / 2
      frequency_result(both_num_sacc/analysis_time_secs)
    }
    else{
      print(num_sacc)
      frequency_result(num_sacc/analysis_time_secs)
    }

    
    avg_amplitude(average_amp)
    
    
    return(extremes_df)
  })
  # Downloading the plot and metrics
  output$downloadPlot <- downloadHandler(
    filename = function() { paste("gaze_data_plot_and_metrics", Sys.Date(), ".html", sep = "") },
    content = function(file) {
      
      # Generate plotly plot
      p <- output$plot
      
      # Combine plot and metrics
      combined_html <- paste(htmlwidgets::saveWidget(p, file = NULL), "<br>", text_output(), sep = "\n")
      
      # Save as HTML
      writeLines(combined_html, file)
    }
  )

}




shinyApp(ui = ui, server = server)