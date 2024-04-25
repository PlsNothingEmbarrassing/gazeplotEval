app <- ShinyDriver$new("../../")
app$snapshotInit("file_upload_test")

app$uploadFile(file = "../binokn1.edf") # <-- This should be the path to the file, relative to the app's tests/shinytest directory
app$snapshot()

