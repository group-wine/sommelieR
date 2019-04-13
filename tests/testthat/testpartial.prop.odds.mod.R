context("partial.prop.odds.mod")
test_that("partial.prop.odds.mod throws an error when model is not specified", {

  data(red_train)
  expect_error(partial.prop.odds.mod(y ="quality", in.data = red_train),
               "At least one of prop.odds.formula or non.prop.odds.formula must be specified.")

})

