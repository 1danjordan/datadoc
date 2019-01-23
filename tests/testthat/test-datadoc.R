context("Generating a datadoc")

test_that("datadoc aborts when data does not evaluate as a expression", {

  expect_error(datadoc(abc()), "abc()")
  expect_error(datadoc(letters), "`data` is a character not a data frame")

})
