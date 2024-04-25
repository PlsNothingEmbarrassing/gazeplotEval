app <- ShinyDriver$new("../../")
app$snapshotInit("test_equal_rescale_vals.R")

app$uploadFile(file = "../binokn1.edf") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(rescale_button = "click")
app$snapshot()
