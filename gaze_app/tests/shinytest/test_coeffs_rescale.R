app <- ShinyDriver$new("../../")
app$snapshotInit("test_coeffs_rescale")

app$uploadFile(file = "../binokn1.edf") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(trial = "6")
app$setInputs(plus_ten_slider = 1239)
app$setInputs(minus_ten_slider = 675)
app$setInputs(rescale_button = "click")
app$setInputs(trial = "3")
app$snapshot()
