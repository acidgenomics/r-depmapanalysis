## FIXME How to name the gene input here?



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
#' genes <- c("XXX", "YYY")
#' sensitive <- c("AAA", "BBB")
#' insensitive <- c("CCC", "DDD")
#' out <- predictSensitivity(
#'     object = object,
#'     genes = genes,
#'     sensitive = sensitive,
#'     insensitive = insensitive
#' )
#' print(out)
NULL



`predictSensitivity,DepMapExpression` <- # nolint
    function(object, sensitive, insensitive) {
        stop("FIXME IN PROGRESS")
    }
