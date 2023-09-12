#' @rdname DepMapCodependencies
#' @export
setGeneric(
    name = "DepMapCodependencies",
    def = function(object, ...) {
        standardGeneric("DepMapCodependencies")
    }
)

#' @export
#' @name diffExp
#' @usage diffExp(object, ...)
NULL

#' @export
#' @name excludeContaminatedCells
#' @usage excludeContaminatedCells(object, ...)
NULL

## FIXME Export this in AcidGenerics.

#' @rdname plotGeneEffect
#' @export
setGeneric(
    name = "plotGeneEffect",
    def = function(object, ...) {
        standardGeneric("plotGeneEffect")
    }
)

## FIXME Export this in AcidGenerics.

#' @rdname plotGeneEffectVsExpression
#' @export
setGeneric(
    name = "plotGeneEffectVsExpression",
    def = function(effect, expression, ...) {
        standardGeneric("plotGeneEffectVsExpression")
    }
)

## FIXME Export this in AcidGenerics.

#' @rdname plotTopGeneEffectPerCell
#' @export
setGeneric(
    name = "plotTopGeneEffectPerCell",
    def = function(object, ...) {
        standardGeneric("plotTopGeneEffectPerCell")
    }
)

## FIXME Export this in AcidGenerics.

#' @rdname plotTopGeneEffectPerGroup
#' @export
setGeneric(
    name = "plotTopGeneEffectPerGroup",
    def = function(object, ...) {
        standardGeneric("plotTopGeneEffectPerGroup")
    }
)

#' @export
#' @name predictSensitivity
#' @usage predictSensitivity(object, ...)
NULL

# FIXME Need to move this to AcidGenerics.

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
