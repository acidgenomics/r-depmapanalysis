#' @rdname DepMapCodependencies
#' @export
setGeneric(
    name = "DepMapCodependencies",
    def = function(object, ...) {
        standardGeneric("DepMapCodependencies")
    }
)

#' @rdname diffExp
#' @export
setGeneric(
    name = "diffExp",
    def = function(object, ...) {
        standardGeneric("diffExp")
    }
)

#' @rdname euclidean
#' @export
setGeneric(
    name = "euclidean",
    def = function(x, ...) {
        standardGeneric("euclidean")
    }
)

#' @rdname filterProblematic
#' @export
setGeneric(
    name = "filterProblematic",
    def = function(object, ...) {
        standardGeneric("filterProblematic")
    }
)

#' @export
#' @name humanize
#' @usage humanize(object, ...)
NULL

#' @rdname plotGeneEffect
#' @export
setGeneric(
    name = "plotGeneEffect",
    def = function(object, ...) {
        standardGeneric("plotGeneEffect")
    }
)

#' @rdname plotGeneEffectVsExpression
#' @export
setGeneric(
    name = "plotGeneEffectVsExpression",
    def = function(effect, expression, ...) {
        standardGeneric("plotGeneEffectVsExpression")
    }
)

#' @rdname plotTopGeneEffectPerCell
#' @export
setGeneric(
    name = "plotTopGeneEffectPerCell",
    def = function(object, ...) {
        standardGeneric("plotTopGeneEffectPerCell")
    }
)

#' @rdname plotTopGeneEffectPerGroup
#' @export
setGeneric(
    name = "plotTopGeneEffectPerGroup",
    def = function(object, ...) {
        standardGeneric("plotTopGeneEffectPerGroup")
    }
)

#' @rdname predictSensitivity
#' @export
setGeneric(
    name = "predictSensitivity",
    def = function(object, ...) {
        standardGeneric("predictSensitivity")
    }
)

#' @rdname selectCells
#' @export
setGeneric(
    name = "selectCells",
    def = function(object, ...) {
        standardGeneric("selectCells")
    }
)

#' @export
#' @name tpm
#' @usage tpm(object, ...)
NULL

#' @export
#' @name zscore
#' @usage zscore(object, ...)
NULL
