#' Reformats dataframe to multinomial regression form
#'
#' Given a dataframe and a formula object, will return the necessary dependent
#' variable matrix and the model matrix needed to run the optimization for
#' multinomial regression for a given reference level.
#'
#' @param data a dataframe
#' @param formula a formula object describing the regression to be run
#' @param ref_level a string which specifies the reference level of the factor dependent variable
#' @param intercept logical, whether to include an intercept or not in the regression (default is TRUE)
#'
#' @return a list containing:
#' \itemize{
#' \item An (n x (k-1)) response matrix of 0's and 1's
#' \item An (n x p) model matrix where n is the number of observations, p is the number of coefficients in the regression formula, and k is the number of possible levels of the dependent variable y.
#' }
#'
#' @import dplyr
#'
#' @export
data_to_mulitnomial <- function(data, formula, ref_level = "8",intercept = T){

  vars <- all.vars(formula)
  response_variable_name <- vars[1]
  independent_variable_names <- vars[-1]

  ###Set up y
  #Get the dependent variable
  y <- data %>% dplyr::select(!! response_variable_name) %>% as.matrix()

  #Determine the column in the matrix to which each value of y corresponds
  unique_y <- unique(y)
  drop_ref_y <- unique_y[unique_y != ref_level]
  positions_y <- drop_ref_y %>% rank()
  ncols <- drop_ref_y %>% length() #because we need one less than number of categories.
  nrows <- y %>% length()

  #Set up empty y matrix
  multi_y <- matrix(rep(0, ncols*nrows), nrow = nrows, ncol = ncols)
  colnames(multi_y) <- drop_ref_y %>% factor() %>% levels()
  for(i in 1:nrows){
    if(y[i] != ref_level){
      #Set the j value to be the position associated with the given y_i
      j <- positions_y[which(drop_ref_y == y[i])]
      multi_y[i,j] <- 1
    }
  }

  ###Set up X
  X <- model.matrix(formula, data = data)

  return(list(multi_y, X))
}

#' Computes the log likelihood for multinomial regression
#'
#' Given a matrix of responses, a design matrix, and a vector of beta coefficients
#' (must be a vector to be compatible with optim), it will return the value of the
#' log likelihood for multinomial regression.
#'
#'
#' @param y an (n x (k-1)) matrix for the dependent variable as created by the data_to_multinomial function, where k is the number of categories for y.
#' @param X an (n x p) matrix where p is the number of predictors
#' @param b_vector a (p*(k-1)) vector
#'
#' @return the value of the mulitnomial regression log-likelihood
#'
multinomial_log_likelihood <- function(y, X, b_vector){

  #Make b_vector into a matrix that is easier for calculations.
  beta <- matrix(b_vector, ncol = ncol(y))

  #Basic dimension checks to make sure that all of the preprocessing functions
  #did their job.
  if(nrow(y)  != nrow(X)){
    stop("Make sure that X and y have the same number of rows")
  }

  if(ncol(y) != ncol(beta)){
    stop("Make sure y and beta have the same number of columns")
  }

  if(ncol(X) != nrow(beta)){
    stop("Make sure that X and beta have conformable columns and rows")
  }

  N <- nrow(y)
  l <- c(rep(0, N))

  for(i in 1:N){
    #We consider two cases, one where the individual had the response which was the
    #reference level, and where the individual had any other response.
    if(sum(y[i,]) == 0){
      l[i] <- -log(1 + sum(exp(X[i,] %*% beta)))
    }
    else{
      index_of_the_one = which(y[i,] == 1)
      t1 <- (X[i,] %*% beta)[index_of_the_one]
      t2 <- log(1 + sum(exp(X[i,] %*% beta)))
      l[i] <- (X[i,] %*% beta)[index_of_the_one] - log(1 + sum(exp(X[i,] %*% beta)))
    }
  }
  l_n <- sum(l)
  return(l_n)
}

#' Fits multinomial regression through `optimx`
#'
#' Given a dataframe and a formula object, `fit_multinomial_regression` will run a
#' user-specified algorithm to fit multinomial linear regression for a user specified
#' reference level.
#'
#' I should probably add the details of the assumptions of the model here.
#'
#' @param data a dataframe on which one would like to apply multinomial regression
#' @param formula a formula obect - in the style of `lm` which fits mulitnomial regression
#' @param ref_level a character, the reference level of the multinomial model
#' @param intercept default is TRUE, whether or not to include an intercept in the model
#' @param method default is BFGS, argument is passed to optimx
#' @param trace default is 0, passed to optimx
#' @param tol default is 10e-5, passed to optimx
#' @param itters default is 200, passed to optimx
#'
#' @return a list containing:
#' \itemize{
#' \item the fitted estimates of the beta coefficients in multinomial regression
#' \item The formula used to fit the model. Used to facilitate prediction.
#' }
#' @importFrom optimx optimx
#'
#' @export
fit_multinomial_regression <- function(data, formula, ref_level,
                                       intercept = T,
                                       method = "BFGS",
                                       trace = 0,
                                       tol = 10e-5,
                                       itters = 200
                                      ){
  #1. data, which is a dataframe

  #2. A formula object to be parsed to establish the model matrix and response matrix.
  #It is passed to the data_to_mulitnomial function to create these. It is also used
  #later as a check to make sure that the estimated beta parameters are similar enough
  #to those output by vglm in the VGAM package.

  #3. A reference level for creation of the response matrix

  l <- data_to_mulitnomial(data, formula, ref_level)

  y <- l[[1]]
  X <- l[[2]]

  b_vector <- matrix(rnorm(runif(ncol(y)*ncol(X),-2,2)),
                     byrow = T, nrow = ncol(X)*ncol(y), ncol = 1)

  fit <- optimx(
    par = c(b_vector),
    fn = function(x, y, X){multinomial_log_likelihood(y = y,
                                                      X = X,
                                                      b_vector = x)},
    y = y,
    X = X,
    hessian = F,
    method =  method,
    control = list(
      trace = trace,
      abstol = tol,
      maxit = itters,
      maximize = T
    )
  )
  #Get fitted betas in matrix form
  beta_hat <- matrix(as.numeric(fit[1:(ncol(y) * ncol(X))]), nrow = ncol(X))

  l <- list(beta_hat, formula = formula, cat_names = colnames(y),
            ref_level = ref_level)
  return(l)
}

#' A function to do prediction with multinomial regression models
#'
#' Given a the output of fit_multinomial_regression and new data,
#' predict_multinomial predicts values for new data on the scale
#' of the response.
#'
#' @param multi_fit the output of fit_multinomial_regression
#' @param newdata a dataframe for which you want predictions
#'
#' @return a vector of predicted values
#'
#' @export
predict_multinomial <- function(multi_fit, newdata){

  X <- model.matrix(multi_fit$formula, newdata)
  beta_hat <- multi_fit[[1]]
  # Fitted values
  fitted_vals <- exp ( X %*% beta_hat) / (1 + rowSums(exp(X %*% beta_hat)))
  fitted_vals <- cbind(fitted_vals, 1 - rowSums(fitted_vals))
  colnames(fitted_vals) <- c(multi_fit$cat_names, multi_fit$ref_level)

  prediction <- colnames(fitted_vals)[apply(fitted_vals, 1,
                                           function(x){which(x == max(x))})]
  return(prediction)
}


