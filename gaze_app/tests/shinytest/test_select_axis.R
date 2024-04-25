app <- ShinyDriver$new("../../")
app$snapshotInit("test_select_axis")

app$uploadFile(file = "../binokn1.edf")
app$setInputs(axis_selector = "Y")
app$snapshot()
