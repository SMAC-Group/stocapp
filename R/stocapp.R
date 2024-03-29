# These functions are
# Copyright (C) 2022 S. Orso, University of Geneva
# All rights reserved.

#' @title
#' Bias correction via stochastic approximation
#' @description
#' \code{stocapp} is used to correct the bias of a fitted model \code{object}
#' with the stochastic approximation procedure.
#' @param object an \code{object} representing a fitted model (see 'Details').
#' @param thetastart an optional starting value for the iterative procedure.
#' If \code{NULL} (default), the procedure starts at the estimates in \code{object}.
#' @param control a \code{list} of parameters for controlling the iterative procedure
#' (see \code{\link{stocappControl}}).
#' @param extra_param if \code{TRUE}, the bias of estimation of extra parameters
#' is performed (see 'Details').
#' @param ... additional optional arguments (see 'Details').
#' @return
#' A fitted model \code{object} of class \linkS4class{stocapp}.
#' @details
#' The iterative bootstrap procedure is described in
#' \insertCite{kuk1995;textual}{stocapp} and further
#' studied by \insertCite{guerrier2019;textual}{stocapp} and
#' \insertCite{guerrier2020;textual}{stocapp}. The \emph{k}th iteration of this
#' algorithm is
#' \deqn{\hat{\theta}^{k} = \hat{\theta}^{k-1} + \hat{\pi} -
#' \frac{1}{H}\sum_{h=1}^H\hat{\pi}_h(\hat{\theta}^{k-1})}{%
#' \theta^(k)=\theta^(k-1) + \pi - \sum \pi_h(\theta^(k-1)) / H
#' }
#' for \eqn{k=1,2,\ldots} and where the sum is over \eqn{h=1,\ldots,H}.
#' The estimate \eqn{\hat{\pi}}{\pi} is provided by the \code{object}.
#' The value \eqn{\hat{\pi}_h(\hat{\theta})}{\pi_h(\theta)} is a parametric bootstrap
#' estimate where the bootstrap sample is generated from \eqn{\hat{\theta}}{\theta}
#' and a fixed \code{seed} (see \code{\link{stocappControl}}).
#' The greater the parameter value \eqn{H} generally the better bias correction
#' but the more computation it requires (see \code{\link{stocappControl}}).
#' If \code{thetastart=NULL}, the initial value of the procedure is \eqn{\hat{\theta}^{0}=\hat{\pi}}{\theta^{0}=\pi}.
#' The number of iterations are controlled by \code{maxit} parameter of \code{\link{stocappControl}}.
#'
#' By default, the method correct \code{\link[stats:coef]{coefficients}} only. For
#' extra parameters, it depends on the model. These extra parameters may have
#' some constraints (e.g. positivity). If \code{constraint=TRUE} (see
#' \code{\link{stocappControl}}), then a transformation from the constraint space to the
#' real is used for the update.
#' @references
#' \insertAllCited{}
#' @importFrom Rdpack reprompt
#' @importFrom stats coef model.matrix getCall predict model.frame is.empty.model model.offset var
#' @author Samuel Orso
#' @export
setGeneric("stocapp",
           function(object, thetastart = NULL, control=list(...), extra_param = FALSE, ...) standardGeneric("stocapp"),
           signature = "object",
           package = "stocapp")


#' @title Auxiliary for controlling stocapp
#' @description
#' Auxiliary function for \code{\link{stocapp}} bias correction.
#' @param tol positive convergence tolerance \eqn{\epsilon}.
#' The \code{\link{stocapp}} procedure converges when
#' \eqn{||\hat{\theta}^{k+1}-\hat{\theta}^k||_2/p<\epsilon}{||\theta^{k+1}-\theta^k||_2/p<\epsilon},
#' where \eqn{p} is the dimension of \eqn{\theta}.
#' @param maxit \code{integer} representing the maximal number of iterations.
#' @param verbose if \code{TRUE}, it prints some output in the console
#' at each iteration.
#' @param seed \code{integer} to set the seed (see \code{\link[base]{Random}}).
#' @param H \code{integer} representing the number of bootstrap estimates
#' (see \code{\link{stocapp}}).
#' @param constraint if \code{TRUE} (default), constraint for \code{extra_param}
#' is used in the iterative procedure (see 'Details' of \code{\link{stocapp}}).
#' @param early_stop if \code{TRUE} (default is \code{FALSE}), the iterative
#' procedure stops as soon as there is no improvment in the minimization of
#' the objective function (see 'Details' of \code{\link{stocapp}}).
#' @param cens if \code{TRUE} the simulated responses are censored according to
#' \code{left} and \code{right} values.
#' @param right \code{double} for right-censoring (only used if \code{cens=TRUE}).
#' @param left \code{double} for left-censoring (only used if \code{cens=TRUE}).
#' @param mis if \code{TRUE} the simulated responses have missing data at random.
#' @param prop \code{double} between 0 and 1 representing the proportion of
#' missing data (only used if \code{mis=TRUE}).
#' @param out if \code{TRUE} the simulated responses are also generated with a
#' contamination mechanism.
#' @param eps \code{double} between 0 and 1 representing the proportion of
#' outliers in the data (only used if \code{out=TRUE}).
#' @param G a \code{function} to generate outliers. It takes only
#' a sample size as argument.
#' @param func a \code{function} to reduce the \code{H} bootstrap estimates (rowwise).
#' By default, the average is computed. The user can supply a function.
#' One could imagine using other function such as the median or a trimmed mean.
#' @param sim a user-defined function for simulating responses (see 'Details')
#' @return a list with components named as the arguments.
#' @details
#' \code{sim} allows the user to provide its own function for generating
#' responses. Currently it is only supported for generalized linear models with
#' the prototype `fun(object, control, extra_param, ...)` (see \code{\link{stocapp}}).
#' @seealso \code{\link{stocapp}}, the iterative procedure for bias correction.
#' @export
stocappControl <- function(tol = 1e-5, maxit = 25, verbose = FALSE,
                      seed=123L,H=1L,constraint=TRUE,early_stop=FALSE,
                      cens=FALSE,right=NULL,left=NULL,
                      mis=FALSE,prop=NULL,
                      out=FALSE,eps=NULL,G=NULL,
                      func=function(x)rowMeans(x,na.rm=T),
                      sim=NULL){
  if(!is.numeric(tol)) stop("`tol` must be numeric")
  if(!is.numeric(maxit)) stop("`maxit` must be numeric")
  if(!is.logical(verbose)) stop("`verbose` must be a boolean")
  if(!is.logical(constraint)) stop("`constraint` must be a boolean")
  if(!is.logical(early_stop)) stop("`early_stop` must be a boolean")
  if(!is.numeric(seed)) stop("`seed` must be numeric")
  if(!is.numeric(H)) stop("`H` must be numeric")
  if(!is.logical(cens)) stop("`cens` must be a boolean")
  if(!is.logical(mis)) stop("`mis` must be a boolean")
  if(!is.logical(out)) stop("`out` must be a boolean")
  if(!is.function(func)) stop("`func` must be a function")
  list(tol=tol,maxit=maxit,verbose=verbose,constraint=constraint,early_stop=early_stop,
       cens=cens,right=right,left=left,seed=seed,
       H=H,func=func,mis=mis,prop=prop,out=out,eps=eps,G=G,sim=sim)
}

