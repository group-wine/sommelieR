#function to yield joint partial proportional odds model log likelihood
partial.prop.odds.ll <- function(y, y.levels, in.data, int.vector, x.prop.odds = NULL, x.non.prop.odds = NULL,
                                 beta.prop.odds = NULL, beta.non.prop.odds = NULL){

  ##################################################################################################
  #arguments;
  #   y: an ordinal outcome vector
  #   y.levels: a vector of the unique, ordinal levels of y
  #   in.data: the input data object
  #   int.vector: a vector of intercept estimates (one for each level of the outcome variable, except the top level).
  #         These need to be in the order of the levels of the outcome variable (from low to high).
  #   x.prop.odds: a design martrix (no intercept) for the variables assumed to have proportional odds
  #   x.non.prop.odds: a design martrix (no intercept) for the variables assumed to not have proportional odds
  #   beta.prop.odds: a vector of beta values for each predictor assumed to have proportional odds
  #   beta.non.prop.odds: a matrix of beta values for each predictor assumed to not have proportional odds
  #                       across the levels of the ordinal outcome
  ###################################################################################################

  #generate matrix of ordinal outcomes for each subject
  y.mat <- generate.multinomial(y, y.levels)

  #to get log likelihood, need to subtract j-1 level prob
  #so also get the intercepts for j-1 level
  #if the observed category is the lowest level of y, set to -99 as a placeholder
  y.minus1 <- ifelse(y - 1 >= min(y.levels), y-1, -99)
  y.minus1.levels <- sort(unique(y.minus1))
  y.minus1.mat <- generate.multinomial(y.minus1, y.minus1.levels)

  #generate intercepts for each subject
  #note that I'm adding 0 to int.vector as a place holder for the un-needed top level intercept
  alpha.j <- y.mat %*% c(int.vector, 0)

  #also for the j-1 category
  #again adding a zero as a placeholder for the non-existent j-1th category for the the lowest observed category
  int.minus1.vector <- c(0, int.vector)
  alpha.j.minus1 <- y.minus1.mat %*% int.minus1.vector

  if (!is.null(x.prop.odds)){

    #get xb for the proportional odds predictors, if included in model
    #this will produce a column of xb's, where each entry is one subject's xb value
    xb.prop.odds <- x.prop.odds %*% beta.prop.odds

  } else {

    xb.prop.odds <- 0

  }

  if (!is.null(x.non.prop.odds)){

    #get xb for the non-proportional odds, if included in model
    #note that I'm dropping the column for the top and bottom levels since these will be
    #fixed later (i.e., the probabilities will be set to 1 or zero)

    xb.non.prop.odds <- t(x.non.prop.odds %*% beta.non.prop.odds)
    y.xb.non.prop.odds <- diag(y.mat[ , 1:(ncol(y.mat) - 1)] %*% xb.non.prop.odds)
    y.minus1.xb.non.prop.odds <- diag(y.minus1.mat[ , 2:(ncol(y.minus1.mat))] %*% xb.non.prop.odds)

  } else {

    y.xb.non.prop.odds <- 0
    y.minus1.xb.non.prop.odds <- 0

  }

  #now put together the pieces of the log likelihood

  #first for the probability <= jth category
  alphaj.xb <- alpha.j + xb.prop.odds + y.xb.non.prop.odds
  plogis.alphaj.xb <- exp(alphaj.xb)/(1 + exp(alphaj.xb))

  #set equal to 1 when we have the highest observed category
  plogis.alphaj.xb[y == max(y.levels)] <- 1

  #second for probability <= j-1th category
  alphaj.minus1.xb <- alpha.j.minus1 + xb.prop.odds + y.minus1.xb.non.prop.odds
  plogis.alphaj.minus1.xb <- exp(alphaj.minus1.xb)/(1 + exp(alphaj.minus1.xb))

  #set equal to zero when the observed category is already the lowest
  plogis.alphaj.minus1.xb[y == min(y.levels)] <- 0

  #get probability for each y
  y.prob <- plogis.alphaj.xb - plogis.alphaj.minus1.xb

  #may return negative probabilities, set to very small number in these cases
  #this a limitation of these models, but not allowing negative or
  #zero probabilities allows iterative maximization to continue, hopefully
  #to non-negative probabilities
  #this will require post-convergence checks
  y.prob <- ifelse(y.prob <= 0, 10^-20, y.prob)

  #return the log-likelihood
  sum(log(y.prob))

}




