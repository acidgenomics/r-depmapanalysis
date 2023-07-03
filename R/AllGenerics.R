#' @rdname DepMapCodependencies
#' @export
setGeneric(
    name = "DepMapCodependencies",
    def = function(object, ...) {
        standardGeneric("DepMapCodependencies")
    }
)

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
