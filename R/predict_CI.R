
#' Get predictions
#' 
#' Get predictions
#' 
#' @export
predict_CI <- function(object, ...){
    UseMethod("predict_CI")
}

#' Get predictions/CI from GLM
#' 
#' Gets predictions and confidence intervals on the 
#' response scale from a GLM object.
#' 
#' @param object The glm object
#' @param newdata A data frame containing the columns necessary
#' to make predictions from your glm object
#' @param ci.level Numeric - A value between 0 and 1 that gives the confidence
#' level for the confidence intervals you want created
#' @param ... Additional parameters to pass to predict
#' 
#' @method predict_CI glm
#' @S3method predict_CI glm
#' @export
#' 
#' @examples
#' # Generating some fake data
#' n <- 30
#' x <- sort(rnorm(n))
#' link <- .2 - .8*x
#' p <- 1/(1 + exp(-link))
#' y <- rbinom(n, 1, p)
#' dat <- data.frame(x = x, y = y)
#' 
#' # Fit a logistic regression
#' o <- glm(y ~ x, data = dat, family = binomial)
#' # Get the predictions and confidence intervals
#' out <- predict_CI(o)
#' \dontrun{
#' # Make a plot
#' plot(x, y)
#' # true relationship
#' lines(x, p)
#' # estimated relationship
#' lines(x, out$prediction, col = "red")
#' # confidence interval
#' lines(x, out$low.ci, col = "blue", lty = 2)
#' lines(x, out$up.ci, col = "blue", lty = 2)
#' }
predict_CI.glm <- function(object, newdata = NULL, ci.level = .95, ...){
    if(missing(newdata)){
        newdata <- object$data
    }
    pred <- predict(object, newdata, type = "link", se.fit = TRUE, ...)
    invlink <- object[["family"]][["linkinv"]]
    z <- qnorm(1 - (1-ci.level)/2)
    low.ci <- pred$fit - z * pred$se.fit
    up.ci <- pred$fit + z * pred$se.fit
    
    prediction <- invlink(pred$fit)
    low.ci.response <- invlink(low.ci)
    up.ci.response <- invlink(up.ci)
    
    pred.df <- data.frame(prediction = prediction, 
               low.ci = low.ci.response, 
               up.ci = up.ci.response)
    
    cbind(newdata, pred.df)
}

# newdat <- data.frame(x = seq(-3, 3, length.out = 10))
# 
# predict_CI(o, newdata = newdat)