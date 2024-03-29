% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stocapp.R, R/stocapp_betareg.R,
%   R/stocapp_glm.R, R/stocapp_glmrob2.R, R/stocapp_lm.R, R/stocapp_lmer.R,
%   R/stocapp_nls.R, R/stocapp_vglm.R
\name{stocapp}
\alias{stocapp}
\alias{stocapp,betareg-method}
\alias{stocapp,glm-method}
\alias{stocapp,glmrob2-method}
\alias{stocapp,lm-method}
\alias{stocapp,lmerMod-method}
\alias{stocapp,nls-method}
\alias{stocapp,vglm-method}
\title{Bias correction via stochastic approximation}
\usage{
stocapp(
  object,
  thetastart = NULL,
  control = list(...),
  extra_param = FALSE,
  ...
)

\S4method{stocapp}{betareg}(
  object,
  thetastart = NULL,
  control = list(...),
  extra_param = FALSE,
  ...
)

\S4method{stocapp}{glm}(
  object,
  thetastart = NULL,
  control = list(...),
  extra_param = FALSE,
  ...
)

\S4method{stocapp}{glmrob2}(
  object,
  thetastart = NULL,
  control = list(...),
  extra_param = FALSE,
  ...
)

\S4method{stocapp}{lm}(
  object,
  thetastart = NULL,
  control = list(...),
  extra_param = FALSE,
  ...
)

\S4method{stocapp}{lmerMod}(
  object,
  thetastart = NULL,
  control = list(...),
  extra_param = FALSE,
  ...
)

\S4method{stocapp}{nls}(
  object,
  thetastart = NULL,
  control = list(...),
  extra_param = FALSE,
  ...
)

\S4method{stocapp}{vglm}(
  object,
  thetastart = NULL,
  control = list(...),
  extra_param = FALSE,
  ...
)
}
\arguments{
\item{object}{an \code{object} representing a fitted model (see 'Details').}

\item{thetastart}{an optional starting value for the iterative procedure.
If \code{NULL} (default), the procedure starts at the estimates in \code{object}.}

\item{control}{a \code{list} of parameters for controlling the iterative procedure
(see \code{\link{stocappControl}}).}

\item{extra_param}{if \code{TRUE}, the bias of estimation of extra parameters
is performed (see 'Details').}

\item{...}{additional optional arguments (see 'Details').}
}
\value{
A fitted model \code{object} of class \linkS4class{stocapp}.
}
\description{
\code{stocapp} is used to correct the bias of a fitted model \code{object}
with the stochastic approximation procedure.
}
\details{
The iterative bootstrap procedure is described in
\insertCite{kuk1995;textual}{stocapp} and further
studied by \insertCite{guerrier2019;textual}{stocapp} and
\insertCite{guerrier2020;textual}{stocapp}. The \emph{k}th iteration of this
algorithm is
\deqn{\hat{\theta}^{k} = \hat{\theta}^{k-1} + \hat{\pi} -
\frac{1}{H}\sum_{h=1}^H\hat{\pi}_h(\hat{\theta}^{k-1})}{%
\theta^(k)=\theta^(k-1) + \pi - \sum \pi_h(\theta^(k-1)) / H
}
for \eqn{k=1,2,\ldots} and where the sum is over \eqn{h=1,\ldots,H}.
The estimate \eqn{\hat{\pi}}{\pi} is provided by the \code{object}.
The value \eqn{\hat{\pi}_h(\hat{\theta})}{\pi_h(\theta)} is a parametric bootstrap
estimate where the bootstrap sample is generated from \eqn{\hat{\theta}}{\theta}
and a fixed \code{seed} (see \code{\link{stocappControl}}).
The greater the parameter value \eqn{H} generally the better bias correction
but the more computation it requires (see \code{\link{stocappControl}}).
If \code{thetastart=NULL}, the initial value of the procedure is \eqn{\hat{\theta}^{0}=\hat{\pi}}{\theta^{0}=\pi}.
The number of iterations are controlled by \code{maxit} parameter of \code{\link{stocappControl}}.

By default, the method correct \code{\link[stats:coef]{coefficients}} only. For
extra parameters, it depends on the model. These extra parameters may have
some constraints (e.g. positivity). If \code{constraint=TRUE} (see
\code{\link{stocappControl}}), then a transformation from the constraint space to the
real is used for the update.

For \link[betareg]{betareg}, \code{extra_param} is not available
as by default mean and precision parameters are corrected.
Currently the 'identity' link function is not supported for precision
parameters.

For \link[stats]{glm}, if \code{extra_param=TRUE}: the shape parameter for the
\code{\link[stats:family]{Gamma}}, the variance of the residuals in \code{\link[stats]{lm}} or
the overdispersion parameter of the negative binomial regression in \code{\link[MASS]{glm.nb}},
are also corrected. Note that the \code{\link[stats:family]{quasi}} families
are not supported for the moment as they have no simulation method
(see \code{\link[stats]{simulate}}). Bias correction for extra parameters
of the \code{\link[stats:family]{inverse.gaussian}} is not yet implemented.

For \link[stats]{glm}, if \code{extra_param=TRUE}: the shape parameter for the
\code{\link[stats:family]{Gamma}}, the variance of the residuals in \code{\link[stats]{lm}} or
the overdispersion parameter of the negative binomial regression in \code{\link[MASS]{glm.nb}},
are also corrected. Note that the \code{\link[stats:family]{quasi}} families
are not supported for the moment as they have no simulation method
(see \code{\link[stats]{simulate}}). Bias correction for extra parameters
of the \code{\link[stats:family]{inverse.gaussian}} is not yet implemented.

For \link[stats]{lm}, if \code{extra_param=TRUE}: the variance of the residuals is
also corrected. Note that using the \code{stocapp} is not useful as coefficients
are already unbiased, unless one considers different
data generating mechanism such as censoring, missing values
and outliers (see \code{\link{stocappControl}}).

For \code{\link[lme4]{lmer}}, by default, only the fixed effects are corrected.
If \code{extra_param=TRUE}: all the random effects
(variances and correlations) and the variance
of the residuals are also corrected.
Note that using the \code{stocapp} is
certainly not useful with the argument \code{REML=TRUE} in
\code{\link[lme4]{lmer}} as the bias of variance components is
already addressed, unless one considers different
data generating mechanism such as censoring, missing values
and outliers (see \code{\link{stocappControl}}).

For \link[stats]{nls}, if \code{extra_param=TRUE}: the variance of the residuals is
also corrected.

For \link[VGAM]{vglm}, \code{extra_param} is currently not used.
Indeed, the philosophy of a vector generalized linear model is to
potentially model all parameters of a distribution with a linear predictor.
Hence, what would be considered as an extra parameter in \code{\link[stats]{glm}}
for instance, may already be captured by the default \code{coefficients}.
However, correcting the bias of a \code{coefficients} does not imply
that the bias of the parameter of the distribution is corrected
(by \href{https://en.wikipedia.org/wiki/Jensen's_inequality}{Jensen's inequality}),
so we may use this feature in a future version of the package.
Note that we currently only support distributions
with a \code{simslot} (see \code{\link[VGAM]{simulate.vlm}}).
}
\examples{

## beta regression
library(betareg)
data("GasolineYield", package = "betareg")
## currently link.phi = "identity" is not supported
## fit_beta <- betareg(yield ~ batch + temp, data = GasolineYield)
fit_beta <- betareg(yield ~ batch + temp, link.phi = "log", data = GasolineYield)
fit_ib <- stocapp(fit_beta)

# precision parameter can also depend on covariates
fit_beta <- betareg(yield ~ batch + temp | temp, data = GasolineYield)
fit_ib <- stocapp(fit_beta)
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

## linear regression
fit_lm <- lm(disp ~ cyl + hp + wt, data = mtcars)
fit_stocapp <- stocapp(fit_lm)
summary(fit_stocapp)
## correct for variance of residuals
fit_stocapp <- stocapp(fit_lm, extra_param = TRUE)
summary(fit_stocapp)

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

## nonlinear regression
DNase1 <- subset(DNase, Run == 1)
fit_nls <- nls(density ~ SSlogis(log(conc), Asym, xmid, scal), data = DNase1)
fit_stocapp <- stocapp(fit_nls)
summary(fit_stocapp)

## student regression
lstocapprary(VGAM)
tdata <- data.frame(x = runif(nn <- 1000))
tdata <- transform(tdata,
                   y = rt(nn, df = exp(exp(0.5 - x))))
fit_vglm <- vglm(y ~ x, studentt3, data = tdata)
fit_stocapp <- stocapp(fit_vglm)
summary(fit_stocapp)
}
\references{
\insertAllCited{}
}
\seealso{
\code{\link[betareg]{betareg}}

\code{\link[stats]{glm}}, \code{\link[MASS]{glm.nb}}

\code{\link[stats]{glm}}, \code{\link[MASS]{glm.nb}}

\code{\link[stats]{lm}}

\code{\link[lme4]{lmer}}

\code{\link[stats]{nls}}

\code{\link[VGAM]{vglm}}
}
\author{
Samuel Orso
}
