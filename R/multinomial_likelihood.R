data_to_mulitnomial <- function(data, formula, intercept = T){
  #Given a dataframe and a formula object, will return the necessary dependent
  #variable matrix and the model matrix needed to run the optimization for
  #multinomial regression.

  vars <- all.vars(formula)
  response_variable_name <- vars[1]
  independent_variable_names <- vars[-1]

  ###Set up y
  #Get the dependent variable
  y <- data %>% select(response_variable_name) %>% as.matrix()
  #Determine the column in the matrix to which each value of y corresponds
  positions_y <- rank(unique(y))
  unique_y <- unique(y)
  ncols <- y %>% unique() %>% length()
  nrows <- y %>% length()

  #Set up empty y matrix
  multi_y <- matrix(rep(0, ncols*nrows), nrow = nrows, ncol = ncols)
  for(i in 1:nrows){
    #Set the j value to be the position associated with the given y_i
    j <- positions_y[which(unique_y == y[i])]
    multi_y[i,j] <- 1
  }

  ###Set up X
  X <- model.matrix(formula, data = data)

  return(list(multi_y, X))
}

multinomial_log_likelihood <- function(y, X, betas){
  #This function expects:

  #y should be an (n x (k-1)) matrix for the dependent variable as created by the
  #data_to_multinomial function, where k is the number of categories for y.

  #X should be an (n x p) matrix, where p is the number of predictors

  #beta should be a (p x (k - 1)) matrix.

  n <- nrow(y)
  l <- c(rep(0, n))
  for(i in 1:n){
    l[i] <- (X[i,] %*% beta)[which(y[2,] == 1)] - log(1 + sum(exp(X[i,] %*% beta)))
  }
  l_n <- sum(l)
  return(l_n)
}





















