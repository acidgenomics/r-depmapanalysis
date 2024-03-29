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
