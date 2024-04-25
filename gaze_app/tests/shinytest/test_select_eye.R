app <- ShinyDriver$new("../../")
app$snapshotInit("test_select_eye")

app$uploadFile(file = "../binokn1.edf")
app$setInputs(eye = "RIGHT")
app$snapshot()
