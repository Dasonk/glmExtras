context("Check logistic confidence intervals")

test_that("Intervals between 0 and 1",{
    x <- rnorm(10)
    y <- rbinom(10, 1, .5)
    dat <- data.frame(x, y)
    o <- glm(y ~ x, data = dat, family = binomial)
    j <- predict_CI(o)
    expect_true(all(j$low.ci > 0 & j$up.ci < 1))    
})
