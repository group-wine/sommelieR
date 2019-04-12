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
