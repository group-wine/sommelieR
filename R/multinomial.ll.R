#function to yield joint multinomia log likelihood
multinomial.ll <- function(model.formula, in.data, int.vector, beta.vector){

  ######################################################
  #arguments;
  #   1. model.formula: a model formula (y ~ x1 + x2, etc.)
  #   2. in.data: the input data object
  #   3. int.vector: a vector of intercept estimates (one for each level of the outcome variable, except the top level).
  #         These need to be in the order of the levels of the outcome variable (from low to high).
  #   4. beta.vector: a vector of beta values (one for each predictor variable)
  #######################################################

  #extract y vector from model formula
  y <- in.data[ ,deparse(model.formula[[2]])]

  #determine how many levels y has
  y.levels <- sort(unique(y))

  #generate matrix of ordinal outcomes for each subject
  y.mat <- generate.multinomial(y, y.levels)

  #to get log likelihood, need to subtract j-1 level prob
  #so also get the intercepts for j-1 level
  #if the observed category is the lowest level of y, set to -99 as a placeholder
  y.minus1 <- ifelse(y - 1 >= min(y.levels), y-1, -99)
  y.minus1.levels <- sort(unique(y.minus1))
  y.minus1.mat <- generate.multinomial(y.minus1, y.minus1.levels)

  #create design matrix from model formula
  X <- model.matrix(model.formula, in.data)

  #get rid of common intercept (need separate ints for each level of y)
  X <- X[ , ! colnames(X) == "(Intercept)"]

  #generate intercepts for each subject
  #note that I'm adding 0 to int.vector as a place holder for the un-needed top level intercept
  alpha.j <- y.mat %*% c(int.vector, 0)

  #also for the j-1 category
  #again adding a zero as a placeholder for the non-existent j-1th category for the the lowest observed category
  int.minus1.vector <- c(0, int.vector)
  alpha.j.minus1 <- y.minus1.mat %*% int.minus1.vector

  #get XB for each subject
  #proportional odds model, so XB is the same across levels of y
  XB <- X %*% beta.vector

  #now put together the pieces of the log likelihood

  #first for the probability <= jth category
  alphaj.xb <- alpha.j + XB
  plogis.alphaj.xb <- exp(alphaj.xb)/(1 + exp(alphaj.xb))

  #set equal to 1 when we have the highest observed category
  plogis.alphaj.xb[y == max(y.levels)] <- 1

  #second for probability <= j-1th category
  alphaj.minus1.xb <- alpha.j.minus1 + XB
  plogis.alphaj.minus1.xb <- exp(alphaj.minus1.xb)/(1 + exp(alphaj.minus1.xb))

  #set equal to zero when the observed category is already the lowest
  plogis.alphaj.minus1.xb[y == min(y.levels)] <- 0

  #get probability for each y
  y.prob <- plogis.alphaj.xb - plogis.alphaj.minus1.xb

  #return the log-likelihood
  sum(log(y.prob))

}

#red <- read.table("winequality-red.csv", header = T, sep = ";")
#multinomial.ll(quality ~ fixed.acidity + volatile.acidity, red, c(-7.6264, -5.6630, -2.3039, -0.0839, 2.6271), c(0.03204, -4.58594))
