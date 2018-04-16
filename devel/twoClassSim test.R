rm(list = ls())
library(caret)
library(rfPermute)

set.seed(2969)
imbal_train <- twoClassSim(2000, intercept = -20, linearVars = 20)
table(imbal_train$Class)

rp <- rfPermute(Class ~ ., data=imbal_train, ntree = 50, mtry = 4, 
               norm.votes = FALSE, nrep=100)

plot(rp.importance(rp))
plotNull(rp)
plotOOBtimes(rp)
impHeatmap(rp)
