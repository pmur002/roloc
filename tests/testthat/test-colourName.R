
test_that("Simple colours convert", {
    expect_equal(colourName("red"), "red")
    expect_equal(colourName("#FF0000"), "red")
    expect_equal(colourName("#FF0101"), "red")
    expect_equal(colourName("#00FF00"), "green")
    expect_equal(colourName("#0000FF"), "blue")
    expect_equal(colourName("#FFFFFF"), "white")
})

test_that("R colour names round trip", {
    ## Colour names in colour list should be a no-op
    expect_equal(colourName(colours()), colours())
})
