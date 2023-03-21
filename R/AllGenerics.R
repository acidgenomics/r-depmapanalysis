#' @rdname DepMapCodependencies
#' @export
setGeneric(
    name = "DepMapCodependencies",
    def = function(object, ...) {
        standardGeneric("DepMapCodependencies")
    }
)

#' @export
#' @name euclidean
#' @usage euclidean(x, y, ...)
NULL

#' @rdname findBiomarkers
#' @export
setGeneric(
    name = "findBiomarkers",
    def = function(object, ...) {
        standardGeneric("findBiomarkers")
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

#' @rdname selectCells
#' @export
setGeneric(
    name = "selectCells",
    def = function(object, ...) {
        standardGeneric("selectCells")
    }
)
