app <- ShinyDriver$new("../../")
app$snapshotInit("test_angle_sliders")

app$uploadFile(file = "../binokn1.edf")
app$setInputs(plus_ten_slider = 1286)
app$setInputs(minus_ten_slider = 740)
app$snapshot()
