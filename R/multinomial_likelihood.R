data_to_mulitnomial <- function(data, formula, ref_level = "8",intercept = T){
  #Given a dataframe and a formula object, will return the necessary dependent
  #variable matrix and the model matrix needed to run the optimization for
  #multinomial regression for a given reference level.

  vars <- all.vars(formula)
  response_variable_name <- vars[1]
  independent_variable_names <- vars[-1]

  ###Set up y
  #Get the dependent variable
  y <- data %>% select(!! response_variable_name) %>% as.matrix()

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

multinomial_log_likelihood <- function(y, X, b_vector){
  #This function expects:

  #y should be an (n x (k-1)) matrix for the dependent variable as created by the
  #data_to_multinomial function, where k is the number of categories for y.

  #X should be an (n x p) matrix, where p is the number of predictors

  #b_vector should be a (p*(k-1) x 1) vector because the optim function expects the
  #parameters on which we optimize to be in a vector. However, the function converts
  #this vector to a (p x (k-1)) matrix initially for ease of programming.
  beta <- matrix(b_vector, ncol = ncol(y))
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

mll_gradient <- function(y, X, b_vector){

  #This function expects:

  #y should be an (n x (k-1)) matrix for the dependent variable as created by the
  #data_to_multinomial function, where k is the number of categories for y.

  #X should be an (n x p) matrix, where p is the number of predictors

  #b_vector should be a (p*(k-1) x 1) vector because the optim function expects the
  #parameters on which we optimize to be in a vector. However, the function converts
  #this vector to a (p x (k-1)) vector initially for ease of programming.

  beta <- matrix(b_vector, ncol = ncol(y))
  if(nrow(y)  != nrow(X)){
    stop("Make sure that X and y have the same number of rows")
  }

  if(ncol(y) != ncol(beta)){
    stop("Make sure y and beta have the same number of columns")
  }

  if(ncol(X) != nrow(beta)){
    stop("Make sure that X and beta have conformable columns and rows")
  }

  #Will return a (p*(k -1) x 1) vector.

  n <- nrow(y)
  p <- nrow(beta)
  k_minus_one <- ncol(beta)
  g <- matrix(rep(0, p*k_minus_one), ncol = k_minus_one)
  print(c(p, k_minus_one))
  #Itterate down the row in each column of the beta matrix. Each row corresponds to
  #the parameter associated with each covariate in the model.
  for(i in 1:p){
    #For each column in the new beta matrix corresponding to each level of the dependent
    #variable.
    for(j in 1:k_minus_one){
      element <- rep(0, n)
      #Iterate over all subjects since they all contribute to each value of the gradient.
      for (k in 1:n){
        element[k] <- y[k,j]*X[k,][i] + (exp(X[k,] %*% beta[,j])*X[k,][i])/(1 + sum(exp(X[j,] %*% beta)))
      }
      g[i,j] <- sum(element)
    }
  }
  #Convert gradient back to (p*(k - 1) x 1) vector and return it.
  return(matrix(g, ncol = 1))
}



















