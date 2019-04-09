#function to run partial proportional odds model
partial.prop.odds.mod <- function(y, in.data, prop.odds.formula = NULL, non.prop.odds.formula = NULL,
                                  method = "BFGS", itnmax = NULL, seed = c(14, 15)){

  #################################################################################3
  #Arguments
  #
  #   y: a character specifying the name of the y variable to be modeled. This variable should
  #     be of type integer or numeric.
  #   in.data: the input data object
  #   prop.odds.formula: an optional formula specifying the predictor variables assumed to have
  #                     proportional odds across levels of y. At least one of prop.odds.formula
  #                     and non.prop.odds.formula must be specified.
  #   non.prop.odds.formula: an optional forumula specifying the predictor variables assumed to have
  #                         non-proportional odds across levels of y. At least one of prop.odds.formula
  #                         and non.prop.odds.formula must be specified.
  #   method: The method used to maximize the log-likelihood (in package optimx). Defaults to "BFGS"
  #   seed: the seed used to generate the starting values for the iterative maximization procedure
  #
  #################################################################################

  #get outcome vector
  y <- in.data[ , y]

  #get levels of y
  y.levels <- sort(unique(y))
  n.ylevels <- length(y.levels)

  #get design matrix for proportional odds predictors
  if (!is.null(prop.odds.formula)){

    x.prop.odds <- model.matrix(prop.odds.formula, in.data)

    #get rid of intercept, we will specify these separately
    x.prop.odds <- x.prop.odds[ , ! colnames(x.prop.odds) == "(Intercept)"]

    #also need starting values
    #just using random uniform draws
    n.prop.betas <- ncol(x.prop.odds)
    set.seed(seed[1])
    beta.prop.odds <- runif(n.prop.betas)

  } else{

    x.prop.odds <- NULL
    beta.prop.odds <- NULL

  }

  #get design matrix for non-proportional odds predictors
  if (!is.null(non.prop.odds.formula)){

    x.non.prop.odds <- model.matrix(non.prop.odds.formula, in.data)

    #get rid of intercept, we will specify these separately
    x.non.prop.odds <- x.non.prop.odds[ , ! colnames(x.non.prop.odds) == "(Intercept)"]

    #also need starting values
    #just using random uniform draws
    n.non.prop.preds <- ncol(x.non.prop.odds)
    non.prop.betas <- n.non.prop.preds*(n.ylevels - 1)
    set.seed(seed[2])
    beta.non.prop.odds <- matrix(runif(non.prop.betas), byrow = F, nrow = n.non.prop.preds)

  } else{

    x.non.prop.odds <- NULL
    beta.non.prop.odds <- NULL

  }

  #starting value for intercepts
  #make intercepts start proportional to the level of y
  int.vector <- plogis(y.levels[1:(n.ylevels - 1)])

  #maxmize parameter estimates
  optim.result <- max.partial.prop.odds.ll(y = y, y.levels = y.levels, in.data = in.data, int.vector = int.vector, method = method,
                                   x.prop.odds = x.prop.odds, x.non.prop.odds = x.non.prop.odds, beta.prop.odds = beta.prop.odds,
                                   beta.non.prop.odds = beta.non.prop.odds, itnmax = itnmax)

  #pick out the appropriate peices of the output
  intercepts <- unlist(optim.result[1:length(int.vector)])
  ll <- optim.result$value
  conv.code <- optim.result$convcode
  if(conv.code != 0){

    warning("log-likelihood maximization did not converge")

  }

  #start putting together results
  results.list <- list(log.lik = ll, conv.code = conv.code, intercepts = intercepts)

  #beta's for proportional odds predictors
  if (! is.null(beta.prop.odds)){

    beta.hat.prop.odds <- unlist(optim.result[(length(int.vector) + 1): (length(int.vector) + length(beta.prop.odds))])
    results.list$beta.hat.prop.odds <- beta.hat.prop.odds

    #also store result for calculating probabilites of different categories
    xb.prop.odds <- x.prop.odds %*% beta.hat.prop.odds

  }

  #betas for non-proportional odds predictors
  if (! is.null(beta.non.prop.odds)){

    beta.hat.non.prop.odds <- unlist(optim.result[(length(int.vector) + length(beta.prop.odds)+1):
                                                    (length(int.vector) + length(beta.prop.odds) + length(beta.non.prop.odds))])
    beta.hat.non.prop.odds.mat <- matrix(beta.hat.non.prop.odds, nrow = n.non.prop.preds, byrow = F)
    results.list$beta.hat.non.prop.odds <- beta.hat.non.prop.odds.mat

    #again store result for calculating estimated probabilities of each category
    xb.non.prop.odds <- x.non.prop.odds %*% beta.hat.non.prop.odds.mat

  }

  #estimated probabilities for each category of the outcome
  top.minus1.level <- length(y.levels) - 1
  top.level <- length(y.levels)

  if (! is.null(beta.prop.odds) & ! is.null(beta.non.prop.odds)){

    top.level.prob <- 1 - plogis(intercepts[top.minus1.level] +  xb.prop.odds + xb.non.prop.odds[ , top.minus1.level])
    bottom.level.prob <- plogis(intercepts[1] + xb.prop.odds + xb.non.prop.odds[ , 1])
    middle.levels <- sapply(2:top.minus1.level, function(mid.level){

      plogis(intercepts[mid.level] + xb.prop.odds + xb.non.prop.odds[ , mid.level]) -
        plogis(intercepts[mid.level - 1] + xb.prop.odds + xb.non.prop.odds[ , mid.level - 1])

    })


  } else if (! is.null(beta.prop.odds)){

    top.level.prob <- 1 - plogis(intercepts[top.minus1.level] +  xb.prop.odds)
    bottom.level.prob <- plogis(intercepts[1] + xb.prop.odds)
    middle.levels <- sapply(2:top.minus1.level, function(mid.level){

      plogis(intercepts[mid.level] + xb.prop.odds) - plogis(intercepts[mid.level - 1] + xb.prop.odds)

    })

  } else if (! is.null(beta.non.prop.odds)){

    top.level.prob <- 1 - plogis(intercepts[top.minus1.level] + xb.non.prop.odds[ , top.minus1.level])
    bottom.level.prob <- plogis(intercepts[1] + xb.non.prop.odds[ , 1])
    middle.levels <- sapply(2:top.minus1.level, function(mid.level){

      plogis(intercepts[mid.level] + xb.non.prop.odds[ , mid.level]) -
        plogis(intercepts[mid.level - 1] + xb.non.prop.odds[ , mid.level - 1])

    })

  }

  probs <- cbind(bottom.level.prob, middle.levels, top.level.prob)
  colnames(probs) <- y.levels

  #check for negative probabilities
  if (any(probs <= 0)){

    stop("Model did not converge and has estimated negative or zero probabilities")

  }
  results.list$est.probs <- probs
  return(results.list)

}
