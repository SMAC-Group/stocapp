# These functions are
# Copyright (C) 2022 S. Orso, University of Geneva
# All rights reserved.

stocapp.glmrob2 <- function(object, thetastart=NULL, control=list(...), ...){
  # controls
  control <- do.call("stocappControl",control)

  # initial estimator:
  # regression coefficients
  pi0 <- coef(object)
  p0 <- length(pi0)

  # extra parameters
  fam <- object$family$family
  p <- length(pi0)

  # starting value
  if(!is.null(thetastart)){
    if(is.numeric(thetastart) && length(thetastart) == length(pi0)){
      t0 <- thetastart
    } else {
      stop("`thetastart` must be a numeric vector of the same length as
           parameter of interest.", call.=FALSE)
    }
  } else {
    t0 <- pi0
  }

  # test diff between thetas
  test_theta <- control$tol + 1

  # iterator
  k <- 0L

  # create an environment for iterative bootstrap
  env_stocapp <- new.env(hash=F)

  # prepare data and formula for fit
  cl <- getCall(object)
  if(length(cl$formula)==1) cl$formula <- get(paste(cl$formula)) # get formula
  intercept_only <- cl$formula[[3]] == 1 # check for intercept only models
  mf <- model.frame(object)
  mt <- terms(object)
  if(!intercept_only){
    x0 <- if(!is.empty.model(mt)) model.matrix(mt, mf, object$contrasts)
    # check if model has an intercept
    has_intercept <- attr(mt,"intercept")
    if(has_intercept){
      # remove intercept from design
      x <- x0[,!grepl("Intercept",colnames(x0))]
      cl$formula <- quote(y~x)
    } else {
      cl$formula <- quote(y~x-1)
    }
    assign("x",x,env_stocapp)
  } else{
    cl$formula <- quote(y~1)
  }
  o <- as.vector(model.offset(mf))
  if(!is.null(o)) assign("o",o,env_stocapp)
  cl$data <- NULL
  # add an offset
  if(!is.null(o)) cl$offset <- quote(o)
  # FIXME: add support for weights, subset, na.action, start,
  #        etastart, mustart, contrasts

  # copy the object
  tmp_object <- object

  # copy the control
  control1 <- control
  control1$H <- 1L
  linkinv <- object$family$linkinv

  # initial values
  extra <- NULL
  diff <- rep(NA_real_, control$maxit)
  pi_star <- rep(NA_real_, p)

  # Iterative bootstrap algorithm:
  while(test_theta > control$tol && k < control$maxit){
    # update object for simulation
    if(k!=0){
      eta <- as.vector(x0 %*% t0[1:p0])
      mu <- linkinv(eta)
      tmp_object$fitted.values <- mu
      tmp_object$coefficients <- t0[1:p0]
    }

    # approximate
    control1$seed <- control$seed + k
    sim <- simulation(tmp_object, control1, extra)
    assign("y",sim,env_stocapp)
    fit_tmp <- tryCatch(error = function(cnd) NULL, {eval(cl,env_stocapp)})
    iter <- 1L
    while(is.null(fit_tmp) && iter < 10L){
      control1$seed <- control$seed + control$maxit * k + iter
      sim <- simulation(tmp_object,control1,extra)
      assign("y",sim,env_stocapp)
      fit_tmp <- tryCatch(error = function(cnd) NULL, {eval(cl,env_stocapp)})
      iter <- iter + 1L
    }
    if(is.null(fit_tmp)) next
    pi_star[1:p0] <- coef(fit_tmp)

    # update value
    delta <- pi0 - pi_star
    alpha <- 1.0 / (k + 1.0)
    t1 <- t0 + alpha * delta

    # test diff between thetas
    test_theta <- sum((t1-t0)^2)
    if(k>0) diff[k] <- test_theta

    # initialize test
    if(!k) tt_old <- test_theta+1

    # Alternative stopping criteria, early stop :
    if(control$early_stop){
      if(tt_old <= test_theta){
        warning("Algorithm stopped because the objective function does not reduce")
        break
      }
    }

    # update increment
    k <- k + 1L

    # Print info
    if(control$verbose){
      cat("Iteration:",k,"Norm between theta_k and theta_(k-1):",test_theta,"\n")
    }

    # update theta
    t0 <- t1

    # update test
    tt_old <- test_theta
  }
  # warning for reaching max number of iterations
  if(k>=control$maxit) warning("maximum number of iteration reached")

  # update glm object
  eta <- predict.glm(tmp_object) # FIXME: this does not return the "correct 'eta'"
  mu <- object$family$linkinv(eta)
  dev <- sum(object$family$dev.resids(object$y,mu,object$prior.weights))

  tmp_object$linear.predictors <- eta
  tmp_object$fitted.values <- mu
  tmp_object$residuals <- (object$y - mu)/object$family$mu.eta(eta)
  tmp_object$call <- object$call
  tmp_object$deviance <- dev
  tmp_object$aic <- object$family$aic(object$y, length(object$prior.weights)-sum(object$prior.weights == 0),
                                      mu, object$prior.weights, dev) + 2 * object$rank

  # additional metadata
  stocapp_extra <- list(
    iteration = k,
    of = sqrt(drop(crossprod(delta))),
    estimate = t0,
    test_theta = test_theta)

  new("StocappGlmrob2",
      object = tmp_object,
      stocapp_extra = stocapp_extra)
}

#' @rdname stocapp
#' @details
#' For \link[stats]{glm}, if \code{extra_param=TRUE}: the shape parameter for the
#' \code{\link[stats:family]{Gamma}}, the variance of the residuals in \code{\link[stats]{lm}} or
#' the overdispersion parameter of the negative binomial regression in \code{\link[MASS]{glm.nb}},
#' are also corrected. Note that the \code{\link[stats:family]{quasi}} families
#' are not supported for the moment as they have no simulation method
#' (see \code{\link[stats]{simulate}}). Bias correction for extra parameters
#' of the \code{\link[stats:family]{inverse.gaussian}} is not yet implemented.
#' @seealso \code{\link[stats]{glm}}, \code{\link[MASS]{glm.nb}}
#' @example /inst/examples/eg_glm.R
#' @export
setMethod("stocapp", className("glmrob2", "glmrob2"),
          definition = stocapp.glmrob2)

simulation.glmrob2 <- simulation.glm

#' @title Simulation for a Generalized Linear Model regression
#' @description simulation method for class \linkS4class{StocappGlm}
#' @param object an object of class \linkS4class{StocappGlm}
#' @param control a \code{list} of parameters for controlling the iterative procedure
#' (see \code{\link{stocappControl}}).
#' @param extra \code{NULL} by default; extra parameters to pass to simulation.
#' @param ... further arguments
#' @export
setMethod("simulation", signature = className("glmrob2", "glmrob2"),
          definition = simulation.glmrob2)
