context("Accurate parameter estimation")
library(VGAM)
library(sommelieR)
library(dplyr)

data("red_train")

test_that("Our estimates of beta are the same as the R standard",{

  #Get coefficients of fit from vglm and get them in the same format as those from
  #fit_multinomial_regression
  standard_fit <- vglm(formula = quality ~ 1 + alcohol, data = red_train,
                       family = multinomial(refLevel = "8")) %>%
    coef() %>%
    matrix(ncol = 2) %>%
    t() %>%
    matrix(nrow = 1)

  #Our fit
  our_fit <- sommelieR::fit_multinomial_regression(data = red_train,
                                        quality ~ 1 + alcohol,
                                        ref_level = "8")[[1]] %>%
    matrix(nrow = 1)

  #Compute Euclidian Distance
  distance <- ((c(standard_fit) - c(our_fit))^2) %>%
    sum() %>% sqrt()

  expect_equal(distance, 0, tolerance = 0.1)

})
