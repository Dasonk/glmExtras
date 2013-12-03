\name{predict_CI.glm}
\alias{predict_CI.glm}
\title{Get predictions/CI from GLM}
\usage{
  \method{predict_CI}{glm} (object, newdata = NULL,
    ci.level = 0.95, ...)
}
\arguments{
  \item{object}{The glm object}

  \item{newdata}{A data frame containing the columns
  necessary to make predictions from your glm object}

  \item{ci.level}{Numeric - A value between 0 and 1 that
  gives the confidence level for the confidence intervals
  you want created}

  \item{...}{Additional parameters to pass to predict}
}
\description{
  Gets predictions and confidence intervals on the response
  scale from a GLM object.
}
\examples{
# Generating some fake data
n <- 30
x <- sort(rnorm(n))
link <- .2 - .8*x
p <- 1/(1 + exp(-link))
y <- rbinom(n, 1, p)
dat <- data.frame(x = x, y = y)

# Fit a logistic regression
o <- glm(y ~ x, data = dat, family = binomial)
# Get the predictions and confidence intervals
out <- predict_CI(o)
\dontrun{
# Make a plot
plot(x, y)
# true relationship
lines(x, p)
# estimated relationship
lines(x, out$prediction)
# confidence interval
lines(x, out$low.ci, col = "blue", lty = 2)
lines(x, out$up.ci, col = "blue", lty = 2)
}
}

