app <- ShinyDriver$new("../../")
app$snapshotInit("test_analysis_section")

app$uploadFile(file = "../binokn1.edf") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(plus_ten_slider = 1267)
app$setInputs(minus_ten_slider = 814)
app$setInputs(rescale_button = "click")
app$setInputs(analysis_end_slider = 6592)
app$setInputs(analysis_button = "click")
app$snapshot()
