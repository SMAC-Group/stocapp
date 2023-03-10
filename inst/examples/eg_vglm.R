
## student regression
lstocapprary(VGAM)
tdata <- data.frame(x = runif(nn <- 1000))
tdata <- transform(tdata,
                   y = rt(nn, df = exp(exp(0.5 - x))))
fit_vglm <- vglm(y ~ x, studentt3, data = tdata)
fit_stocapp <- stocapp(fit_vglm)
summary(fit_stocapp)
