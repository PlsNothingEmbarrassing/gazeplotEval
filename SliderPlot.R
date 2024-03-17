library(dplyr)
library(plotly)

data <- data.frame(y = sample(0:50, 100, replace = TRUE),
                   x = round(runif(100),2))

plot_ly(data, x = ~x, y = ~y, type = 'scatter') %>%
  layout(shapes = list(
    #horizontal line
    list(type = "line", x0 = 0.0, x1 = 1, 
         y0 = 40, y1 = 40, xref = "paper"),
    #horizontal line
    list(type = "line", x0 = 0, x1 = 1, 
         y0 = 40, y1 = 40, xref = "paper"))) %>%
  # allow to edit plot by dragging lines
  config(edits = list(shapePosition = TRUE))
