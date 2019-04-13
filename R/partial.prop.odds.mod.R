#' Run partial proportional odds models for ordinal outcomes
#'
#' This function runs partial proportional odds models for ordinal outcomes.
#'
#' @param y.name A character vector specifying the name of the variable to be modeled.
#' @param in.data The input data object of type data frame or matrix.
#' @param prop.odds.formula An optional formula specifying the predictor variables assumed to have proportional odds across levels of y. At least one of prop.odds.formula and non.prop.odds.formula must be specified.
#' @param beta.prop.odds.start A vector of starting values for proportional odds betas. This should only be specified in conjunction with prop.odds.formula.
#' @param non.prop.odds.formula An optional formula specifying the predictor variables assumed not to have proportional odds across levels of y. At least one of prop.odds.formula and non.prop.odds.formula must be specified.
#' @param beta.non.prop.odds.start A matrix of starting values for non proportional odds betas. This should only be specified in conjunction with non.prop.odds.formula. Columns correspond to the j-1 bottom levels of the outcome variable y, rows correspond to variables.
#' @param method A character specifying the optimization method to be used by package optimx in maximizing the log likelihood. Defaults to BFGS.
#' @param int.vec.scale A tuning parameter used to adjust the starting values for the intercepts. Defaults to 5.
#' @param itnmax An optional scalar specifying the iteration limit used in maximizing the log likelihood. Defaults to the default optimx value for the given method.
#' @param seed A vector of length 2 specifying the seed used to generate starting values for model coefficients, if not user specified. Defaults to c(14, 15).
#'
#' @return A list of class partial.prop.odds
#' \item{y.name}{A character vector specifying the model outcome.}
#' \item{y.levels}{The ordered levels of the model outcome.}
#' \item{prop.odds.formula}{The formula used for the proportional odds betas.}
#' \item{non.prop.odds.formula}{The formula used for the non-proportional odds betas.}
#' \item{log.lik}{The log-likelihood of the fitted model.}
#' \item{conv.code}{The convergence code from optimx.}
#' \item{intercepts}{The fitted model intercepts}
#' \item{beta.hat.prop.odds}{A vector of the estimated proportional odds coefficients, if specified.}
#' \item{beta.hat.non.prop.odds}{A matrix of the estimated non-proportional odds coefficients, where the j-1 columns correspond to the j-1 bottom levels of y, and the rows are betas.}
#' \item{est.probs}{The fitted probabilities of each level of y for each subject. Rows are subjects, columns are levels of y.}
#'
#' @examples
#'
#' data(red_train)
#' starts <- coef(lm(quality ~ alcohol+ pH + volatile.acidity + sulphates + total.sulfur.dioxide, data = red_train))
#' test <- partial.prop.odds.mod(y ="quality", in.data = red_train,
#' prop.odds.formula = ~ alcohol + pH+ volatile.acidity + sulphates,
#' beta.prop.odds.start = starts[2:5],
#' non.prop.odds.formula = ~total.sulfur.dioxide,
#' beta.non.prop.odds.start = matrix(rep(starts[6], 5), nrow = 1),
#' method = "BFGS",
#' seed = c(14, 15), itnmax = 1000)
#'
#' @useDynLib sommelieR
#' @export
partial.prop.odds.mod <- function(y.name, in.data, prop.odds.formula = NULL, beta.prop.odds.start = NULL,
                                  non.prop.odds.formula = NULL, beta.non.prop.odds.start = NULL,
                                  method = "BFGS", int.vec.scale = 5, itnmax = NULL, seed = c(14, 15)){

  #################################################################################3
  #Arguments
  #
  #   y.name: a character specifying the name of the y variable to be modeled.
  #   in.data: the input data object
  #   prop.odds.formula: an optional formula specifying the predictor variables assumed to have
  #                     proportional odds across levels of y. At least one of prop.odds.formula
  #                     and non.prop.odds.formula must be specified.
  #   beta.prop.odds.start: an optional vector of starting values for the non-proportional odds betas
  #   non.prop.odds.formula: an optional forumula specifying the predictor variables assumed to have
  #                         non-proportional odds across levels of y. At least one of prop.odds.formula
  #                         and non.prop.odds.formula must be specified.
  #   beta.non.prop.odds.start: an optional matrix of starting values for non-proportional odds betas
  #   method: The method used to maximize the log-likelihood (in package optimx). Defaults to "BFGS"
  #   int.vec.scale: tuning parameter used to adjust the starting values for the intercepts. Defaults to 5.
  #   seed: the seed used to generate the starting values for the iterative maximization procedure
  #
  #################################################################################

  #make sure we have at least one of prop.odds.formula and non.prop.odds.formula specified
  if (is.null(prop.odds.formula) & is.null(non.prop.odds.formula)){

    stop("At least one of prop.odds.formula or non.prop.odds.formula must be specified.")

  }

  #get outcome vector
  y <- in.data[ , y.name]

  #get levels of y
  y.levels <- sort(unique(y))
  n.ylevels <- length(y.levels)

  #get design matrix for proportional odds predictors
  if (!is.null(prop.odds.formula)){

    x.prop.odds <- model.matrix(prop.odds.formula, in.data)

    #get rid of intercept, we will specify these separately
    x.prop.odds <- x.prop.odds[ , ! colnames(x.prop.odds) == "(Intercept)", drop = F]

    #also need starting values if not speciified
    if (! is.null(beta.prop.odds.start)){

      beta.prop.odds <- beta.prop.odds.start

    } else {

      #just using random uniform draws
      n.prop.betas <- ncol(x.prop.odds)
      set.seed(seed[1])
      beta.prop.odds <- runif(n.prop.betas)


    }

  } else{

    x.prop.odds <- NULL
    beta.prop.odds <- NULL

  }

  #get design matrix for non-proportional odds predictors
  if (!is.null(non.prop.odds.formula)){

    x.non.prop.odds <- model.matrix(non.prop.odds.formula, in.data)

    #get rid of intercept, we will specify these separately
    x.non.prop.odds <- x.non.prop.odds[ , ! colnames(x.non.prop.odds) == "(Intercept)", drop = F]
    n.non.prop.preds <- ncol(x.non.prop.odds)

    #also need starting values if not specified
    if (! is.null(beta.non.prop.odds.start)){

      beta.non.prop.odds <- beta.non.prop.odds.start

    } else {

      #just using random uniform draws
      non.prop.betas <- n.non.prop.preds*(n.ylevels - 1)
      set.seed(seed[2])
      beta.non.prop.odds <- matrix(runif(non.prop.betas), byrow = F, nrow = n.non.prop.preds)

    }

  } else{

    x.non.prop.odds <- NULL
    beta.non.prop.odds <- NULL

  }

  #starting value for intercepts
  #make intercepts start proportional to the level of y
  cat.probs <- as.vector(by(in.data, in.data$quality, function(x) nrow(x))) / nrow(red)
  int.vector <- log(int.vec.scale*cumsum(cat.probs[1:(n.ylevels - 1)]))


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
  results.list <- list(y.name = y.name, y.levels = y.levels, prop.odds.formula = prop.odds.formula,
                       non.prop.odds.formula = non.prop.odds.formula,
                       log.lik = ll, conv.code = conv.code, intercepts = intercepts)

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
  class(results.list) <- "partial.prop.odds"
  return(results.list)

}
