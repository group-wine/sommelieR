rm(list=ls())

### FUNCTIONS
# LINEAR BETA ESTIMATES
linear = function(dat,intercept=TRUE) {
  # dat is the data
  # dat$quality is the response
  # result : betahat
  
  if (intercept==TRUE) {
    X = as.matrix(cbind(1,dat[,colnames(dat)!='quality']),nr=nrow(dat),nc=ncol(dat))
  } else {
    X = as.matrix(dat[,colnames(dat)!='quality'],nr=nrow(dat),nc=ncol(dat)-1)
  }
  Y = as.matrix(dat$quality,nc=1)
  
  betahat = solve(crossprod(X),crossprod(X,Y))
  return(betahat)
}

### DATA ###
red = white = NULL
red = read.csv('/Users/csy/Dropbox/2019Spring/735/GroupProject/sommelieR-master/data/training_data/red_train.csv')
# white = read.csv('/Users/csy/Dropbox/2019Spring/735/GroupProject/sommelieR-master/data/training_data/white_train.csv')
dat = rbind(red,white)

linear(dat)
linear(dat, intercept=FALSE)



# fit = lm(quality ~ ., data=dat)
# summary(fit)
# 
# fit = lm(quality ~ .-fixed.acidity, data=dat)
# summary(fit)
# 
# fit = lm(quality ~ .-fixed.acidity-density, data=dat)
# summary(fit)
# 
# fit = lm(quality ~ .-fixed.acidity-density-citric.acid, data=dat)
# summary(fit)
# 
# fit = lm(quality ~ .-fixed.acidity-density-citric.acid-residual.sugar, data=dat)
# summary(fit)

