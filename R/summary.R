# These functions are
# Copyright (C) 2022 S. Orso, University of Geneva
# All rights reserved.

## Define summary method for different classes of the union "SummaryStocapp"
show.summary.stocapp <- function(object){
  digits <- max(3L, getOption("digits")) - 3L
  print(object@summ)
  cat("\nIterative bootstrap procedure:")
  cat("\n\n     * number of iterations:", object@stocapp_extra$iteration)
  cat("\n     * objective function:", format(object@stocapp_extra$of, digits = digits))
  if(!is.null(object@stocapp_extra$stocapp_warn))
    cat("\n\nWarning while correcting the bias:", object@stocapp_extra$stocapp_warn)
  invisible(object)
}

#' @title Summarizing a fitted model corrected by the stochastic approximation procedure
#' @description Method for printing a \code{summary} of
#' class union \linkS4class{SummaryStocapp}.
#' @param object a summary object of member of \linkS4class{SummaryStocapp}
#' @seealso \linkS4class{SummaryStocapp}
#' @export
setMethod("show",
          "SummaryStocapp",
          definition = show.summary.stocapp)

## StocappBetareg
summaryStocappBetareg <- function(object, ...){
  summary.betareg <- getFromNamespace("summary.betareg", ns = "betareg")
  x <- getObject(object)
  y <- getExtra(object)
  summ <- summary.betareg(x, ...)
  new("SummaryStocappBetareg",
      summ = summ,
      stocapp_extra = y)
}

#' @title Summarizing a beta regression fit corrected by
#' the iterative bootstrap
#' @description summary method for class \linkS4class{StocappBetareg}
#' @param object an object of class \linkS4class{StocappBetareg}
#' @param ... further arguments passed to \code{summary.betareg}
#' @seealso \link[betareg]{summary.betareg}
#' @export
setMethod("summary",
          "StocappBetareg",
          definition = summaryStocappBetareg)

## StocappGlm
#' @importFrom stats summary.glm
summaryStocappGlm <- function(object, ...){
  x <- getObject(object)
  y <- getExtra(object)
  summ <- summary.glm(x, ...)
  new("SummaryStocappGlm",
      summ = summ,
      stocapp_extra = y)
}

#' @title Summarizing a Generalized Linear Model regression fit corrected by
#' the iterative bootstrap
#' @description summary method for class \linkS4class{StocappGlm}
#' @param object an object of class \linkS4class{StocappGlm}
#' @param ... further arguments passed to \code{summary.glm}
#' @seealso \link[stats]{summary.glm}
#' @export
setMethod("summary",
          "StocappGlm",
          definition = summaryStocappGlm)

## StocappLm
#' @importFrom stats summary.lm
summaryStocappLm <- function(object, ...){
  x <- getObject(object)
  y <- getExtra(object)
  summ <- summary.lm(x, ...)
  new("SummaryStocappLm",
      summ = summ,
      stocapp_extra = y)
}

#' @title Summarizing a linear regression fit corrected by
#' the iterative bootstrap
#' @description summary method for class \linkS4class{StocappLm}
#' @param object an object of class \linkS4class{StocappLm}
#' @param ... further arguments passed to \code{summary.lm}
#' @seealso \link[stats]{summary.lm}
#' @export
setMethod("summary",
          "StocappLm",
          definition = summaryStocappLm)

## StocappLmer
#' @importFrom utils getFromNamespace
summaryStocappLmer <- function(object, ...){
  summary.lmer <- getFromNamespace("summary.merMod", ns = "lme4")
  x <- getObject(object)
  y <- getExtra(object)
  summ <- summary.lmer(x, ...)
  new("SummaryStocappLmer",
      summ = summ,
      stocapp_extra = y)
}

#' @title Summarizing a linear mixed model regression fit corrected by
#' the iterative bootstrap
#' @description summary method for class \linkS4class{StocappLmer}
#' @param object an object of class \linkS4class{StocappLmer}
#' @param ... further arguments passed to \code{summary.merMod} of \pkg{lme4}
#' @export
setMethod("summary",
          "StocappLmer",
          definition = summaryStocappLmer)

## StocappNegbin
summaryStocappNegbin <- function(object, ...){
  summary.negbin <- getFromNamespace("summary.negbin", ns = "MASS")
  x <- getObject(object)
  y <- getExtra(object)
  summ <- summary.negbin(x, ...)
  new("SummaryStocappNegbin",
      summ = summ,
      stocapp_extra = y)
}

#' @title Summarizing a negative binomial regression fits corrected by
#' the iterative bootstrap
#' @description summary method for class \linkS4class{StocappNegbin}
#' @param object an object of class \linkS4class{StocappNegbin}
#' @param ... further arguments passed to \code{summary.negbin}
#' @seealso \link[MASS]{summary.negbin}
#' @export
setMethod("summary",
          "StocappNegbin",
          definition = summaryStocappNegbin)

## StocappNls
summaryStocappNls <- function(object, ...){
  summary.nls <- getFromNamespace("summary.nls", ns = "stats")
  x <- getObject(object)
  y <- getExtra(object)
  summ <- summary.nls(x, ...)
  new("SummaryStocappNls",
      summ = summ,
      stocapp_extra = y)
}

#' @title Summarizing a nonlinear regression fit corrected by
#' the iterative bootstrap
#' @description summary method for class \linkS4class{StocappNls}
#' @param object an object of class \linkS4class{StocappNls}
#' @param ... further arguments passed to \code{summary.nls} of \pkg{stats}
#' @export
setMethod("summary",
          "StocappNls",
          definition = summaryStocappNls)

## StocappVglm
#' @importFrom VGAM summaryvglm
summaryStocappVglm <- function(object, ...){
  x <- getObject(object)
  y <- getExtra(object)
  summ <- summaryvglm(x, ...)
  new("SummaryStocappVglm",
      summ = summ,
      stocapp_extra = y)
}

#' @title Summarizing a vector generalized linear model regression
#' fit corrected by the iterative bootstrap
#' @description summary method for class \linkS4class{StocappVglm}
#' @param object an object of class \linkS4class{StocappVglm}
#' @param ... further arguments passed to \code{summary.merMod} of \pkg{VGAM}
#' @export
setMethod("summary",
          "StocappVglm",
          definition = summaryStocappVglm)
