app <- ShinyDriver$new("../../")
app$snapshotInit("test_analysis_bars")

app$uploadFile(file = "../binokn1.edf") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(plus_ten_slider = 1082)
app$setInputs(minus_ten_slider = 786)
app$setInputs(rescale_button = "click")
app$setInputs(analysis_end_slider = 6266)
app$setInputs(analysis_start_slider = 692)
app$setInputs(analysis_start_slider = 3011)
app$snapshot()
