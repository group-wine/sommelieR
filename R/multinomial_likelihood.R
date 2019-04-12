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

multinomial_log_likelihood <- function(data, formula, ref_level = "8",intercept = T,
                                       b_vector){
  #This function expects:
  #1. data, which is a dataframe

  #2. A formula object to be parsed to establish the model matrix and response matrix.
  #It is passed to the data_to_mulitnomial function to create these. It is also used
  #later as a check to make sure that the estimated beta parameters are similar enough
  #to those output by vglm in the VGAM package.

  #3. A reference level for creation of the response matrix

  #4. b_vector, which should be a (p*(k-1) x 1) vector because the optim function
  #expects the parameters on which we optimize to be in a vector.
  #However, the function converts this vector to a (p x (k-1)) matrix initially
  #for ease of programming.

  l <- data_to_mulitnomial(data, formula, ref_level)

  #y should be an (n x (k-1)) matrix for the dependent variable as created by the
  #data_to_multinomial function, where k is the number of categories for y.
  y <- l[[1]]

  #X should be an (n x p) matrix, where p is the number of predictors
  X <- l[[2]]

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





