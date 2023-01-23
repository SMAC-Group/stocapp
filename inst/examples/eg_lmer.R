
## linear mixed-effects regression
lstocapprary(lme4)
fit_lmm <- lmer(Reaction ~ Days + (Days | Subject), data = sleepstudy, REML = FALSE)
fit_stocapp <- stocapp(fit_lmm)
summary(fit_stocapp)
## correct for variances and correlation
\dontrun{
fit_stocapp <- stocapp(fit_lmm, extra_param = TRUE)
summary(fit_stocapp)
}
