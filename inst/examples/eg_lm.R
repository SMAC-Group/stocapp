
## linear regression
fit_lm <- lm(disp ~ cyl + hp + wt, data = mtcars)
fit_stocapp <- stocapp(fit_lm)
summary(fit_stocapp)
## correct for variance of residuals
fit_stocapp <- stocapp(fit_lm, extra_param = TRUE)
summary(fit_stocapp)
