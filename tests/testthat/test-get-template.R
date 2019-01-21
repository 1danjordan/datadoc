context("Retreiving templates")

test_that("get_template retreives templates correctly", {

  template <- get_template("categorical")

  expect_true(fs::file_exists(template))

  expect_error(get_template("abcd"), "No template was found")
})
