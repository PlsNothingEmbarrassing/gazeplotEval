app <- ShinyDriver$new("../../")
app$snapshotInit("test_basic_rescale")

app$uploadFile(file = "../binokn1.edf") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$setInputs(plus_ten_slider = 1165)
app$setInputs(minus_ten_slider = 934)
app$setInputs(minus_ten_slider = 721)
app$setInputs(plus_ten_slider = 1276)
app$setInputs(rescale_button = "click")
app$snapshot()
