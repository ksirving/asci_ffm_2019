# Simple edits to dismo::gbm.fixed
#
# Identical to dismo::gbm.fixed but have added "n.minobsinnode = n.minobsinnode" as parameter option (ln8), 
#  and ammended code to fit in lines 18 & 79 


My.gbm.fixed <- function (data, gbm.x, gbm.y, tree.complexity = 1, site.weights = rep(1, 
    nrow(data)), verbose = TRUE, learning.rate = 0.001, n.trees = 2000, 
    bag.fraction = 0.5, family = "bernoulli", keep.data = FALSE, 
    var.monotone = rep(0, length(gbm.x)),
    n.minobsinnode = n.minobsinnode) 
{
    train.fraction = 1
    if (!requireNamespace("gbm")) {
        stop("you need to install the gbm package to run this function")
    }
    x.data <- data[, gbm.x, drop = FALSE]
    y.data <- data[, gbm.y]
    sp.name <- names(data)[gbm.y]
    z1 <- unclass(Sys.time())
    gbm.call <- paste("gbm::gbm(y.data ~ .,n.trees = n.trees, data=x.data, verbose = F, interaction.depth = tree.complexity, \n    weights = site.weights, shrinkage = learning.rate, distribution = as.character(family),\n    var.monotone = var.monotone, bag.fraction = bag.fraction, keep.data = keep.data, n.minobsinnode = n.minobsinnode)", 
        sep = "")
    if (verbose) {
        print(paste("fitting gbm model with a fixed number of ", 
            n.trees, " trees for ", sp.name, sep = ""), quote = FALSE)
    }
    gbm.object <- eval(parse(text = gbm.call))
    best.trees <- n.trees
    fitted.values <- gbm::predict.gbm(gbm.object, x.data, n.trees = n.trees, 
        type = "response")
    gbm.summary <- summary(gbm.object, n.trees = n.trees, plotit = FALSE)
    y_i <- y.data
    u_i <- fitted.values
    if (family == "poisson") {
        deviance.contribs <- ifelse(y_i == 0, 0, (y_i * log(y_i/u_i))) - 
            (y_i - u_i)
        resid.deviance <- 2 * sum(deviance.contribs * site.weights)
        residuals <- sqrt(abs(deviance.contribs * 2))
        residuals <- ifelse((y_i - u_i) < 0, 0 - residuals, residuals)
        u_i <- sum(y.data * site.weights)/sum(site.weights)
        deviance.contribs <- ifelse(y_i == 0, 0, (y_i * log(y_i/u_i))) - 
            (y_i - u_i)
        total.deviance <- 2 * sum(deviance.contribs * site.weights)
    }
    if (family == "bernoulli") {
        deviance.contribs <- (y_i * log(u_i)) + ((1 - y_i) * 
            log(1 - u_i))
        resid.deviance <- -2 * sum(deviance.contribs * site.weights)
        residuals <- sqrt(abs(deviance.contribs * 2))
        residuals <- ifelse((y_i - u_i) < 0, 0 - residuals, residuals)
        u_i <- sum(y.data * site.weights)/sum(site.weights)
        deviance.contribs <- (y_i * log(u_i)) + ((1 - y_i) * 
            log(1 - u_i))
        total.deviance <- -2 * sum(deviance.contribs * site.weights)
    }
    if (family == "laplace") {
        resid.deviance <- sum(abs(y_i - u_i))
        residuals <- y_i - u_i
        u_i <- mean(y.data)
        total.deviance <- sum(abs(y_i - u_i))
    }
    if (family == "gaussian") {
        resid.deviance <- sum((y_i - u_i) * (y_i - u_i))
        residuals <- y_i - u_i
        u_i <- mean(y.data)
        total.deviance <- sum((y_i - u_i) * (y_i - u_i))
    }
    if (verbose) {
        print(paste("total deviance = ", round(total.deviance, 
            2), sep = ""), quote = F)
        print(paste("residual deviance = ", round(resid.deviance, 
            2), sep = ""), quote = F)
    }
    z2 <- unclass(Sys.time())
    elapsed.time.minutes <- round((z2 - z1)/60, 2)
    gbm.detail <- list(data = data, gbm.x = gbm.x, predictor.names = names(x.data), 
        gbm.y = gbm.y, reponse.name = names(y.data), family = family, 
        tree.complexity = tree.complexity, learning.rate = learning.rate, 
        bag.fraction = bag.fraction, cv.folds = 0, n.trees = n.trees, 
        best.trees = best.trees, train.fraction = train.fraction, 
        var.monotone = var.monotone, date = date(), elapsed.time.minutes = elapsed.time.minutes,
        n.minobsinnode = n.minobsinnode)
    gbm.object$gbm.call <- gbm.detail
    gbm.object$fitted <- fitted.values
    gbm.object$residuals <- residuals
    gbm.object$contributions <- gbm.summary
    gbm.object$self.statistics <- list(null.deviance = total.deviance, 
        resid.deviance = resid.deviance)
    gbm.object$weights <- site.weights
    return(gbm.object)
}
