# Simple edits to dismo::gbm.simplify
#
# Identical to dismo::gbm.simplify but have added "n.minobsinnode = .minobsinnode" as parameter option (ln 6) 
#  and referenced similarly altered function My.gbm.fixed(..., n.minobsinnode = n.minobsinnode, ...) in lines 65 & 161

My.gbm.simplify <- function (gbm.object, n.folds = 10, n.drops = "auto", alpha = 1, 
    prev.stratify = TRUE, eval.data = NULL, plot = TRUE,
    n.minobsinnode = n.minobsinnode) 
{
    if (!requireNamespace("gbm")) {
        stop("you need to install the gbm package to run this function")
    }
    requireNamespace("splines")
    gbm.call <- gbm.object$gbm.call
    data <- gbm.call$dataframe
    n.cases <- nrow(data)
    gbm.x <- gbm.call$gbm.x
    gbm.y <- gbm.call$gbm.y
    family <- gbm.call$family
    lr <- gbm.call$learning.rate
    tc <- gbm.call$tree.complexity
    start.preds <- length(gbm.x)
    max.drops <- start.preds - 2
    response.name <- gbm.call$response.name
    predictor.names <- gbm.call$predictor.names
    n.trees <- gbm.call$best.trees
    pred.list <- list(initial = gbm.x)
    weights <- gbm.object$weights
    if (n.drops == "auto") {
        auto.stop <- TRUE
    }
    else {
        auto.stop <- FALSE
    }
    orig.data <- data
    orig.gbm.x <- gbm.x
    original.deviance <- round(gbm.object$cv.statistics$deviance.mean, 
        4)
    original.deviance.se <- round(gbm.object$cv.statistics$deviance.se, 
        4)
    message("gbm.simplify - version 2.9 \nsimplifying gbm.step model for ", 
        response.name, " with ", start.preds, " predictors and ", 
        n.cases, " observations \noriginal deviance = ", original.deviance, 
        "(", original.deviance.se, ")")
    if (auto.stop) {
        message("variable removal will proceed until average change exceeds the original se")
        n.drops <- 1
    }
    else {
        if (n.drops > start.preds - 2) {
            message("value of n.drops (", n.drops, ") is greater than permitted\nresetting value to ", 
                start.preds - 2)
            n.drops <- start.preds - 2
        }
        else {
            message("a fixed number of ", n.drops, " drops will be tested")
        }
    }
    dev.results <- matrix(0, nrow = n.drops, ncol = n.folds)
    dimnames(dev.results) <- list(paste("drop.", 1:n.drops, sep = ""), 
        paste("rep.", 1:n.folds, sep = ""))
    drop.count <- matrix(NA, nrow = start.preds, ncol = n.folds)
    dimnames(drop.count) <- list(predictor.names, paste("rep.", 
        1:n.folds, sep = ""))
    original.deviances <- rep(0, n.folds)
    model.list <- list(paste("model", c(1:n.folds), sep = ""))
    gbm.call.string <- paste("try(My.gbm.fixed(data=train.data,gbm.x=gbm.new.x,gbm.y=gbm.y,n.minobsinnode = n.minobsinnode,", 
        sep = "")
    gbm.call.string <- paste(gbm.call.string, "family=family,learning.rate=lr,tree.complexity=tc,", 
        sep = "")
    gbm.call.string <- paste(gbm.call.string, "n.trees = ", n.trees, 
        ", site.weights = weights.subset,verbose=FALSE))", sep = "")
    if (prev.stratify & family == "bernoulli") {
        presence.mask <- data[, gbm.y] == 1
        absence.mask <- data[, gbm.y] == 0
        n.pres <- sum(presence.mask)
        n.abs <- sum(absence.mask)
        selector <- rep(0, n.cases)
        temp <- rep(seq(1, n.folds, by = 1), length = n.pres)
        temp <- temp[order(runif(n.pres, 1, 100))]
        selector[presence.mask] <- temp
        temp <- rep(seq(1, n.folds, by = 1), length = n.abs)
        temp <- temp[order(runif(n.abs, 1, 100))]
        selector[absence.mask] <- temp
    }
    else {
        selector <- rep(seq(1, n.folds, by = 1), length = n.cases)
        selector <- selector[order(runif(n.cases, 1, 100))]
    }
    message("creating initial models...")
    gbm.new.x <- orig.gbm.x
    for (i in 1:n.folds) {
        train.data <- orig.data[selector != i, ]
        weights.subset <- weights[selector != i]
        eval.data <- orig.data[selector == i, ]
        model.list[[i]] <- eval(parse(text = gbm.call.string))
        u_i <- eval.data[, gbm.y]
        y_i <- gbm::predict.gbm(model.list[[i]], eval.data, n.trees, 
            "response")
        original.deviances[i] <- round(calc.deviance(u_i, y_i, 
            family = family, calc.mean = TRUE), 4)
    }
    n.steps <- 1
    message("dropping predictor:", appendLF = FALSE)
    while (n.steps <= n.drops & n.steps <= max.drops) {
        message(" ", n.steps, appendLF = FALSE)
        for (i in 1:n.folds) {
            train.data <- orig.data[selector != i, ]
            eval.data <- orig.data[selector == i, ]
            weights.subset <- weights[selector != i]
            gbm.x <- model.list[[i]]$gbm.call$gbm.x
            n.preds <- length(gbm.x)
            these.pred.names <- model.list[[i]]$gbm.call$predictor.names
            contributions <- model.list[[i]]$contributions
            last.variable <- match(as.character(contributions[n.preds, 
                1]), these.pred.names)
            gbm.new.x <- gbm.x[-last.variable]
            last.variable <- match(as.character(contributions[n.preds, 
                1]), predictor.names)
            drop.count[last.variable, i] <- n.steps
            model.list[[i]] <- eval(parse(text = gbm.call.string))
            u_i <- eval.data[, gbm.y]
            y_i <- gbm::predict.gbm(model.list[[i]], eval.data, 
                n.trees, "response")
            deviance <- round(calc.deviance(u_i, y_i, family = family, 
                calc.mean = TRUE), 4)
            dev.results[n.steps, i] <- round(deviance - original.deviances[i], 
                4)
        }
        if (auto.stop) {
            delta.mean <- mean(dev.results[n.steps, ])
            if (delta.mean < (alpha * original.deviance.se)) {
                n.drops <- n.drops + 1
                dev.results <- rbind(dev.results, rep(0, n.folds))
            }
        }
        n.steps <- n.steps + 1
    }
    message("")
    dimnames(dev.results) <- list(paste("drop.", 1:n.drops, sep = ""), 
        paste("rep.", 1:n.folds, sep = ""))
    mean.delta <- apply(dev.results, 1, mean)
    se.delta <- sqrt(apply(dev.results, 1, var))/sqrt(n.folds)
    if (plot) {
        y.max <- 1.5 * max(mean.delta + se.delta)
        y.min <- 1.5 * min(mean.delta - se.delta)
        plot(seq(0, n.drops), c(0, mean.delta), xlab = "variables removed", 
            ylab = "change in predictive deviance", type = "l", 
            ylim = c(y.min, y.max))
        lines(seq(0, n.drops), c(0, mean.delta) + c(0, se.delta), 
            lty = 2)
        lines(seq(0, n.drops), c(0, mean.delta) - c(0, se.delta), 
            lty = 2)
        abline(h = 0, lty = 2, col = 3)
        min.y <- min(c(0, mean.delta))
        min.pos <- match(min.y, c(0, mean.delta)) - 1
        abline(v = min.pos, lty = 3, col = 2)
        abline(h = original.deviance.se, lty = 2, col = 2)
        title(paste("RFE deviance - ", response.name, " - folds = ", 
            n.folds, sep = ""))
    }
    message("processing final dropping of variables with full data")
    gbm.call.string <- paste("try(My.gbm.fixed(data=orig.data,gbm.x=gbm.new.x,gbm.y=gbm.y,n.minobsinnode = n.minobsinnode,", 
        sep = "")
    gbm.call.string <- paste(gbm.call.string, "family=family,learning.rate=lr,tree.complexity=tc,", 
        sep = "")
    gbm.call.string <- paste(gbm.call.string, "n.trees = ", n.trees, 
        ", site.weights = weights,verbose=FALSE))", sep = "")
    n.steps <- n.steps - 1
    final.model <- gbm.object
    train.data <- orig.data
    final.drops <- matrix(NA, nrow = start.preds, ncol = 1)
    dimnames(final.drops) <- list(predictor.names, "step")
    for (i in 1:n.steps) {
        gbm.x <- final.model$gbm.call$gbm.x
        n.preds <- length(gbm.x)
        these.pred.names <- final.model$gbm.call$predictor.names
        contributions <- final.model$contributions
        message(i, "-", as.character(contributions[n.preds, 1]))
        last.variable <- match(as.character(contributions[n.preds, 
            1]), these.pred.names)
        gbm.new.x <- gbm.x[-last.variable]
        last.variable <- match(as.character(contributions[n.preds, 
            1]), predictor.names)
        final.drops[last.variable] <- i
        final.model <- eval(parse(text = gbm.call.string))
    }
    removal.list <- dimnames(final.drops)[[1]]
    removal.list <- removal.list[order(final.drops)]
    removal.list <- removal.list[1:n.drops]
    removal.numbers <- rep(0, n.steps)
    for (i in 1:n.steps) {
        removal.numbers[i] <- match(removal.list[i], predictor.names)
        pred.list[[i]] <- orig.gbm.x[0 - removal.numbers[1:i]]
        names(pred.list)[i] <- paste("preds.", i, sep = "")
    }
    deviance.summary <- data.frame(mean = round(mean.delta, 4), 
        se = round(se.delta, 4))
    final.drops <- data.frame(preds = dimnames(final.drops)[[1]][order(final.drops)], 
        order = final.drops[order(final.drops)])
    return(list(deviance.summary = deviance.summary, deviance.matrix = dev.results, 
        drop.count = drop.count, final.drops = final.drops, pred.list = pred.list, 
        gbm.call = gbm.call))
}