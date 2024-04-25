app <- ShinyDriver$new("../../")
app$snapshotInit("test_no_sacc_analysis")

app$uploadFile(file = "binokn1.edf") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(plus_ten_slider = 1101)
app$setInputs(minus_ten_slider = 786)
app$setInputs(rescale_button = "click")
app$setInputs(analysis_start_slider = 3459)
app$setInputs(analysis_start_slider = 1221)
app$setInputs(analysis_end_slider = 1831)
app$setInputs(analysis_end_slider = 1221)
app$setInputs(analysis_button = "click")
app$snapshot()
