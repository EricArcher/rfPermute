x <- system.time(randomForest(type ~ ., data = symb.metab, sampsize = rep(8, 4), replace = FALSE, proximity = TRUE))

x1 <- system.time(rfPermute(type ~ ., data = symb.metab, sampsize = rep(8, 4), replace = FALSE, proximity = TRUE, nrep = 100))

x2 <- system.time(rfPermute(type ~ ., data = symb.metab, sampsize = rep(8, 4), replace = FALSE, proximity = TRUE, nrep = 1000))

x3 <- system.time(rfPermute(type ~ ., data = symb.metab, sampsize = rep(8, 4), replace = FALSE, proximity = TRUE, nrep = 1000, num.cores = 3))

cbind(x, x1, x2, x3)
