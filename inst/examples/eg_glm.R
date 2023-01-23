## poisson regression
counts <- c(18,17,15,20,10,20,25,13,12)
outcome <- gl(3,1,9)
treatment <- gl(3,3)
pois_fit <- glm(counts ~ outcome + treatment, family = poisson())
fit_stocapp <- stocapp(pois_fit)
summary(fit_stocapp)
## Set H = 1000
\dontrun{
fit_stocapp <- stocapp(pois_fit, control=list(H=1000))
summary(fit_stocapp)
}

## gamma regression
clotting <- data.frame(
  u = c(5,10,15,20,30,40,60,80,100),
  lot1 = c(118,58,42,35,27,25,21,19,18),
  lot2 = c(69,35,26,21,18,16,13,12,12))
fit_gamma <- glm(lot2 ~ log(u), data = clotting, family = Gamma(link = "inverse"))
fit_stocapp <- stocapp(fit_gamma)
## summary(fit_stocapp)
## correct for shape parameter and show iterations
\dontrun{
fit_stocapp <- stocapp(fit_gamma, control=list(verbose=TRUE), extra_param = TRUE)
summary(fit_stocapp)
}

## negative binomial regression
library(MASS)
fit_nb <- glm.nb(Days ~ Sex/(Age + Eth*Lrn), data = quine)
fit_stocapp <- stocapp(fit_nb)
## summary(fit_stocapp)
## correct for overdispersion with H=100
\dontrun{
fit_stocapp <- stocapp(fit_nb, control=list(H=100), extra_param = TRUE)
summary(fit_stocapp)
}
