#' A function to predict probabilities for each level of an ordinal outcome for testing data using partial proportional odds model fit on training data.
#'
#' This function takes a testing dataset and the output object from a partial proportional odds model fit on a training dataset, and predicts probabilities of each level of the outcome for each individual in the test data.
#'
#' @param object The output object from partial.prop.odds.mod run on a training dataset.
#' @param newdata A test dataset to be used for predictions. The outcome for test data must have the exact same levels as the training dataset
#' @param ... Other parameters
#'
#' @return A matrix of predicted probabilities for each level of the ordinal outcome for each subject, where the subjects are rows and the levels of the outcome are columns.
#'
#' @examples
#'
#' data(red_test)
#' data(red_train)
#' starts <- coef(lm(quality ~ alcohol+ pH + volatile.acidity, data = red_train))
#' training.result <- partial.prop.odds.mod(y ="quality", in.data = red_train,
#' prop.odds.formula = ~ alcohol + pH,
#' beta.prop.odds.start = starts[2:3],
#' non.prop.odds.formula = ~ volatile.acidity,
#' beta.non.prop.odds.start = matrix(rep(starts[4], 5), nrow = 1),
#' method = "BFGS",
#' seed = c(14, 15), itnmax = 1000)
#' predictions <- predict(training.result, red_test)
#'
#' @importFrom stats model.matrix
#' @importFrom stats plogis
#' @importFrom stats rnorm
#' @importFrom stats runif
#' @export
predict.partial.prop.odds <- function(object, newdata, ...){

  ######################################################################
  #Arguments:
  #
  #   object: the output object from partial.prop.odds.mod run
  #             on a training data set
  #   newdata: a test dataset to be used for predictions
  #               Note that the outcome for test data must have
  #               the exact same levels as the training dataset
  #
  # Output:
  #
  #   A data frame, where rows are subjects and columns are the ordered levels
  #   of the outcome variable, of probabilities of each outcome level for each
  #   subject.
  ########################################################################

  #confirm test data has the same outcome levels as the training data

  y.new <- newdata[ , object$y.name]
  y.new.levels <- sort(unique(y.new))
  if (! all(y.new.levels == object$y.levels)){

    stop("The outcome in the test data does not have the same levels as the training data.")

  }

  #get design matrix and begin estimates for proportional odds predictors
  if (!is.null(object$prop.odds.formula)){

    x.prop.odds <- model.matrix(object$prop.odds.formula, newdata)

    #get rid of intercept, we will specify these separately
    x.prop.odds <- x.prop.odds[ , ! colnames(x.prop.odds) == "(Intercept)", drop = F]

    #store for predictions
    xb.prop.odds <- x.prop.odds %*% object$beta.hat.prop.odds

  }

  if (! is.null(object$non.prop.odds.formula)){

    x.non.prop.odds <- model.matrix(object$non.prop.odds.formula, newdata)

    #get rid of intercept, we will specify these separately
    x.non.prop.odds <- x.non.prop.odds[ , ! colnames(x.non.prop.odds) == "(Intercept)", drop = F]
    n.non.prop.preds <- ncol(x.non.prop.odds)

    #store for predicting probabilities
    xb.non.prop.odds <- x.non.prop.odds %*% object$beta.hat.non.prop.odds

  }

  #predict probabilities

  #estimated probabilities for each category of the outcome
  top.minus1.level <- length(y.new.levels) - 1
  top.level <- length(y.new.levels)

  if (! is.null(object$beta.hat.prop.odds) & ! is.null(object$beta.hat.non.prop.odds)){

    top.level.prob <- 1 - plogis(object$intercepts[top.minus1.level] + xb.prop.odds +
                                   xb.non.prop.odds[ , top.minus1.level])
    bottom.level.prob <- plogis(object$intercepts[1] + xb.prop.odds + xb.non.prop.odds[ , 1])
    middle.levels <- sapply(2:top.minus1.level, function(mid.level){

      plogis(object$intercepts[mid.level] + xb.prop.odds + xb.non.prop.odds[ , mid.level]) -
        plogis(object$intercepts[mid.level - 1] + xb.prop.odds + xb.non.prop.odds[ , mid.level - 1])

    })


  } else if (! is.null(object$beta.hat.prop.odds)){

    top.level.prob <- 1 - plogis(object$intercepts[top.minus1.level] +  xb.prop.odds)
    bottom.level.prob <- plogis(object$intercepts[1] + xb.prop.odds)
    middle.levels <- sapply(2:top.minus1.level, function(mid.level){

      plogis(object$intercepts[mid.level] + xb.prop.odds) -
        plogis(object$intercepts[mid.level - 1] + xb.prop.odds)

    })

  } else if (! is.null(object$beta.hat.non.prop.odds)){

    top.level.prob <- 1 - plogis(object$intercepts[top.minus1.level] + xb.non.prop.odds[ , top.minus1.level])
    bottom.level.prob <- plogis(object$intercepts[1] + xb.non.prop.odds[ , 1])
    middle.levels <- sapply(2:top.minus1.level, function(mid.level){

      plogis(object$intercepts[mid.level] + xb.non.prop.odds[ , mid.level]) -
        plogis(object$intercepts[mid.level - 1] + xb.non.prop.odds[ , mid.level - 1])

    })

  }

  probs <- cbind(bottom.level.prob, middle.levels, top.level.prob)
  colnames(probs) <- y.new.levels
  return(probs)
}
