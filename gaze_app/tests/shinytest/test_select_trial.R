app <- ShinyDriver$new("../../")
app$snapshotInit("test_select_trial")

app$uploadFile(file = "../binokn1.edf")
app$setInputs(trial = "4")
app$snapshot()
