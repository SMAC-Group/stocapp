# These functions are
# Copyright (C) 2022 S. Orso, University of Geneva
# All rights reserved.

## Class definition for the package

## NOTE: due to compatibility issues between S3/S4 classes,
##       we consider supplying the fitted model in a new slot.

# FIXME: had to remove @describeIn due to roxygen2 `rd_section_minidesc` error
# Follow https://github.com/r-lib/roxygen2/issues/1421

## Stocapp union

setOldClass("betareg")
# @describeIn Stocapp fitted model by \code{betareg} from \pkg{betareg}
#' @export
setClass("StocappBetareg",
         slots = list(
           object = "betareg",
           stocapp_extra = "list"))


# @describeIn Stocapp fitted model by \code{glm} from \pkg{stats}
#' @export
setClass("StocappGlm",
         slots = list(
           object = "glm",
           stocapp_extra = "list"))

# @describeIn Stocapp fitted model by \code{lm} from \pkg{stats}
#' @export
setClass("StocappLm",
         slots = list(
           object = "lm",
           stocapp_extra = "list"))


# @describeIn Stocapp fitted model by \code{lmer} from \pkg{lme4}
#' @export
setClass("StocappLmer",
         slots = list(
           object = className("lmerMod","lme4"),
           stocapp_extra = "list"))

setOldClass("negbin")
# @describeIn Stocapp fitted model by \code{glm.nb} from \pkg{MASS}
#' @export
setClass("StocappNegbin",
         slots = list(
           object = "negbin",
           stocapp_extra = "list"))

setOldClass("nls")
# @describeIn Stocapp fitted model by \code{nls} from \pkg{stats}
#' @export
setClass("StocappNls",
         slots = list(
           object = "nls",
           stocapp_extra = "list"))

# @describeIn Stocapp fitted model by \code{vglm} from \pkg{VGAM}
#' @export
setClass("StocappVglm",
         slots = list(
           object = className("vglm","VGAM"),
           stocapp_extra = "list"))

#' @title
#' An S4 class union for \code{stocapp}
#' @description
#' Members of the union are \linkS4class{StocappBetareg}, \linkS4class{StocappGlm},
#' \linkS4class{StocappLm}, \linkS4class{StocappLmer}, \linkS4class{StocappNegbin},
#' \linkS4class{StocappNls}, \linkS4class{StocappVglm}
#' @details
#' The `Functions` section describes members of the class union.
#' @return
#' Each member of the union has a \code{slot} with the initial object
#' corrected by the \code{stocapp} (see \code{\link{getObject}}) and a second \code{slot} with
#' extra meta data from \code{stocapp} (see \code{\link{getExtra}}).
#' @author Samuel Orso
#' @seealso \code{\link{getExtra}}, \code{\link{getObject}}
#' @export
setClassUnion(name = "Stocapp",
              members = c("StocappBetareg",
                          "StocappGlm",
                          "StocappLm",
                          "StocappLmer",
                          "StocappNegbin",
                          "StocappNls",
                          "StocappVglm"))

## SummaryStocapp union

setOldClass("summary.betareg")
# @describeIn SummaryStocapp summary of class \code{summary.betareg} from \pkg{betareg}
#' @export
setClass("SummaryStocappBetareg",
         slots = list(
           summ = "summary.betareg",
           stocapp_extra = "list"))

setOldClass("summary.glm")
# @describeIn SummaryStocapp summary of class \code{summary.glm} from \pkg{stats}
#' @export
setClass("SummaryStocappGlm",
         slots = list(
           summ = "summary.glm",
           stocapp_extra = "list"))

setOldClass("summary.lm")
# @describeIn SummaryStocapp summary of class \code{summary.lm} from \pkg{stats}
#' @export
setClass("SummaryStocappLm",
         slots = list(
           summ = "summary.lm",
           stocapp_extra = "list"))

setOldClass("summary.merMod")
# @describeIn SummaryStocapp summary of class \code{summary.merMod} from \pkg{lme4}
#' @export
setClass("SummaryStocappLmer",
         slots = list(
           summ = "summary.merMod",
           stocapp_extra = "list"))

setOldClass("summary.negbin")
# @describeIn SummaryStocapp summary of class \code{summary.negbin} from \pkg{MASS}
#' @export
setClass("SummaryStocappNegbin",
         slots = list(
           summ = "summary.negbin",
           stocapp_extra = "list"))

setOldClass("summary.nls")
# @describeIn SummaryStocapp summary of class \code{summary.nls} from \pkg{stats}
#' @export
setClass("SummaryStocappNls",
         slots = list(
           summ = "summary.nls",
           stocapp_extra = "list"))

# @describeIn SummaryStocapp summary of class \code{summary.vglm} from \pkg{VGAM}
#' @export
setClass("SummaryStocappVglm",
         slots = list(
           summ = className("summary.vglm","VGAM"),
           stocapp_extra = "list"))

#' @title An S4 class union for \code{summary}
#' @description
#' Members of the union are \linkS4class{SummaryStocappBetareg}, \linkS4class{SummaryStocappGlm}, \linkS4class{SummaryStocappLm},
#' \linkS4class{SummaryStocappLmer}, \linkS4class{SummaryStocappNegbin}, \linkS4class{SummaryStocappNls},
#' \linkS4class{SummaryStocappVglm}
#' iterative bootstrap procedure
#' @details
#' The `Functions` section describes members of the class union.
#' @author Samuel Orso
#' @export
setClassUnion(name = "SummaryStocapp",
              members = c("SummaryStocappBetareg",
                          "SummaryStocappGlm",
                          "SummaryStocappLm",
                          "SummaryStocappLmer",
                          "SummaryStocappNegbin",
                          "SummaryStocappNls",
                          "SummaryStocappVglm"))
