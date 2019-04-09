#function to maximize the partial proportional odds model log likelihood
max.partial.prop.odds.ll <- function(y, y.levels, in.data, int.vector, method = "BFGS", itnmax = NULL, x.prop.odds = NULL,
                                     x.non.prop.odds = NULL, beta.prop.odds = NULL, beta.non.prop.odds = NULL){

  ##################################################################################################
  #arguments;
  #   y: an ordinal outcome vector
  #   y.levels: a vector of the unique, ordinal levels of y
  #   in.data: the input data object
  #   int.vector: a vector of intercept estimates (one for each level of the outcome variable, except the top level).
  #         These need to be in the order of the levels of the outcome variable (from low to high).
  #   x.prop.odds: a design martrix (no intercept) for the variables assumed to have proportional odds
  #   method: the optimization method to be used (by package optimx)
  #   x.non.prop.odds: a design martrix (no intercept) for the variables assumed to not have proportional odds
  #   beta.prop.odds: a vector of beta values for each predictor assumed to have proportional odds
  #   beta.non.prop.odds: a matrix of beta values for each predictor assumed to not have proportional odds
  #                       across the levels of the ordinal outcome
  ###################################################################################################


  #concatenate all parameters to be estimated into a single vector
  in.params <- int.vector
  int.index <- 1:length(int.vector)
  if( ! is.null(beta.prop.odds)){

    prop.odds.index <- (length(in.params) + 1):(length(in.params) + length(beta.prop.odds))
    in.params <- c(in.params, beta.prop.odds)


  } else {

    prop.odds.index <- NULL

  }
  if (! is.null(beta.non.prop.odds)){

    beta.non.prop.odds.vector <- as.vector(beta.non.prop.odds)
    non.prop.odds.index <- (length(in.params) + 1):(length(in.params) + length(beta.non.prop.odds.vector))
    non.prop.odds.rows <- nrow(beta.non.prop.odds)
    in.params <- c(in.params, beta.non.prop.odds.vector)


  } else {

    non.prop.odds.index <- NULL
    non.prop.odds.rows <- NULL

  }

  #feed into optimix and maximize the likelihood
  optimx(in.params, function(x, int.index, prop.odds.index, non.prop.odds.index, non.prop.odds.rows,
                             y, y.levels, in.data, x.prop.odds,
                             x.non.prop.odds){

    int.vector <- x[int.index]
    if (!is.null(prop.odds.index)){

      beta.prop.odds <- x[prop.odds.index]

    } else {

      beta.prop.odds <- NULL

    }

    if (!is.null(non.prop.odds.index)){

      beta.non.prop.odds <- matrix(x[non.prop.odds.index], nrow = non.prop.odds.rows, byrow = F)

    } else {

      beta.non.prop.odds <- NULL

    }
    partial.prop.odds.ll(y, y.levels, in.data, int.vector, x.prop.odds, x.non.prop.odds,
                         beta.prop.odds, beta.non.prop.odds)
  },
  int.index = int.index, prop.odds.index = prop.odds.index,
  non.prop.odds.index = non.prop.odds.index, non.prop.odds.rows = non.prop.odds.rows,
  y = y, y.levels = y.levels, in.data = in.data, x.prop.odds = x.prop.odds,
  x.non.prop.odds = x.non.prop.odds,
  method = method,
  control= list(maximize = TRUE),
  itnmax = itnmax)

}
