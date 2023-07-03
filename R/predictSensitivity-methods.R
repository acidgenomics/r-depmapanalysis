## FIXME Add support for this.



#' Predict sensitivity
#'
#' @name predictSensitivity
#' @note Updated 2023-07-03.
#'
#' @inheritParams AcidRoxygen::params
#' @param ... Additional arguments.
#'
#' @param sensitive,insensitive `character`.
#' Cell line identifiers.
#'
#' @examples
#' data(rnaseq)
#'
#' ## DepMapExpression ====
#' object <- rnaseq
#' sensitive <- c("AAA", "BBB")
#' insensitive <- c("CCC", "DDD")
#' out <- predictSensitivity(
#'     object = object,
#'     sensitive = sensitive,
#'     insensitive = insensitive
#' )
#' print(out)
NULL



`predictSensitivity,DepMapExpression` <- # nolint
    function(object, sensitive, insensitive) {
        stop("IN PROGRESS")
    }
