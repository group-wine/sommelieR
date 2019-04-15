context("linear.mod")
test_that("linear.mod throws an error when the input data does not contain the response vector", {

  data(red_train)
  expect_error(linear.mod(dat = red_train),
               "The input data should include the response.")

})

