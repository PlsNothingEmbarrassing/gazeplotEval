app <- ShinyDriver$new("../../")
app$snapshotInit("test_plot_download.R")

app$uploadFile(file = "../binokn1.edf") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(plus_ten_slider = 1147)
app$setInputs(minus_ten_slider = 694)
app$setInputs(rescale_button = "click")
app$setInputs(analysis_button = "click")
app$snapshot()
