#' @rdname DepMapCodependencies
#' @export
setGeneric(
    name = "DepMapCodependencies",
    def = function(object, ...) {
        standardGeneric("DepMapCodependencies")
    }
)

# FIXME Need to use AcidGenerics for this.

#' @rdname diffExp
#' @export
setGeneric(
    name = "diffExp",
    def = function(object, ...) {
        standardGeneric("diffExp")
    }
)

# FIXME Rename this to `predictSensitivity`.
# FIXME Need to use AcidGenerics 

#' @rdname euclidean
#' @export
setGeneric(
    name = "euclidean",
    def = function(x, ...) {
        standardGeneric("euclidean")
    }
)

# FIXME Need to use AcidGenerics for this. Renamed to `excludeProblematicCells`.
# FIXME Consider just exporting `excludeContaminatedCells`.

#' @rdname filterProblematic
#' @export
setGeneric(
    name = "filterProblematic",
    def = function(object, ...) {
        standardGeneric("filterProblematic")
    }
)

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

# FIXME Rework our euclidean code to just use this.

#' @rdname predictSensitivity
#' @export
setGeneric(
    name = "predictSensitivity",
    def = function(object, ...) {
        standardGeneric("predictSensitivity")
    }
)

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
