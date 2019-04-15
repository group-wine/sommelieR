#' A function to run linear regression
#'
#' This function runs linear regression model treating the response as continuous
#'
#' @param dat n*(p+1) data frame with the covariates and the response vector
#' @param intercept logical. whether to include the intercept or not. default is TRUE
#'
#' @return A list of
#' \item{betahat}{A vector of beta estimates from the linear regression model.}
#' \item{yhat}{A vector of estimated response using the beta estimates from the model.}
#'
#' @examples
#'
#' data(red_train)
#' rbeta = c('volatile.acidity', 'total.sulfur.dioxide','pH','alcohol','sulphates')
#' dat = red_train[,which(names(red_train)%in% c(rbeta,'quality'))]
#' linfit = linear.mod(dat, intercept=TRUE)
#' beta = linfit$betahat
#'
#' @export
linear.mod = function(dat,intercept=TRUE) {
  # dat is the data
  # dat$quality is the response
  # result : betahat (beta)
  # result : estimated y (yhat)

  if (! "quality" %in% colnames(dat)) stop('The input data should include the response.')

  if (intercept==TRUE) {
    X = as.matrix(cbind(intercept=1,dat[,colnames(dat)!='quality']),nr=nrow(dat),nc=ncol(dat))
  } else {
    X = as.matrix(dat[,colnames(dat)!='quality'],nr=nrow(dat),nc=ncol(dat)-1)
  }
  Y = as.matrix(dat$quality,nc=1)

  betahat = solve(crossprod(X),crossprod(X,Y))
  yhat = X %*% betahat
  return(list(betahat=betahat, yhat=yhat))
}

