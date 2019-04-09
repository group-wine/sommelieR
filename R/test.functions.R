#testing functions
source("./R/generate.multinomial.R")
source("./R/max.partial.prop.odds.ll.R")
source("./R/partial.prop.odds.ll.R")
source("./R/partial.prop.odds.mod.R")

#read in data
red <- read.table("data/training_data/red_train.csv", header = T, sep = ",")

#collapse categories
red$quality3 <- ifelse(red$quality < 5, 0, ifelse(red$quality < 7, 1, 2))

library(optimx)

#run model
test <- partial.prop.odds.mod(y ="quality3", in.data = red,
                              prop.odds.formula = ~ citric.acid + residual.sugar,
                              non.prop.odds.formula =~ fixed.acidity + volatile.acidity,
                              method = "BFGS",
                              seed = c(14, 15))


#compare to existing package
library(ordinal)
red$quality3.factor <- as.factor(red$quality3)
test2 <- clm(quality3.factor ~ citric.acid + residual.sugar, nominal = ~ fixed.acidity + volatile.acidity, data = red)

#log-likelihoods
test$log.lik
logLik(test2)

#parameter estimates
summary(test2)$coef

test$intercepts
test$beta.hat.prop.odds
test$beta.hat.non.prop.odds

#same result for this case

#try just prop odds
test3 <- partial.prop.odds.mod(y ="quality3", in.data = red,
                              prop.odds.formula =~ fixed.acidity + volatile.acidity + citric.acid + residual.sugar,
                              method = "BFGS",
                              seed = c(14, 15))

test4 <- clm(quality3.factor ~ citric.acid + residual.sugar + fixed.acidity + volatile.acidity, data = red)

test3$log.lik
logLik(test4)

#only non-proportional odds
#note: BFGS doesn't seem to work here

test5 <- partial.prop.odds.mod(y ="quality3", in.data = red,
                               non.prop.odds.formula = ~ fixed.acidity + volatile.acidity,
                               method = "Nelder-Mead",
                               seed = c(1000, 10000),
                               itnmax = 1000)

test6 <- partial.prop.odds.mod(y ="quality3", in.data = red,
                               non.prop.odds.formula = ~ fixed.acidity + volatile.acidity,
                               method = "bobyqa")

test7 <- clm(quality3.factor ~ 1, nominal = ~ fixed.acidity + volatile.acidity, data = red)
summary(test7)$coef
logLik(test7)

#Nelder mead doesn't do as well
test5$log.lik
test5$intercepts
test5$beta.hat.non.prop.odds

#bobyqa does the best with all non-proportional coefficients
test6$log.lik
test6$intercepts
test6$beta.hat.non.prop.odds

#also note we can directly grab the predicted probabilities of each category:
head(test6$est.probs)



