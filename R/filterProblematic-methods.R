## FIXME Rework and rename to just `excludeContaminatedCells`, matching
## the naming conventions in Cellosaurus package.

## FIXME Class this on DepMapExperiment
## FIXME Consider only filtering "Problematic cell line: Contaminated." lines.
## Can use the comments to look for this.

## FIXME Rename this to filterContaminated instead.
## FIXME Update Cellosaurus to return `isContaminated` boolean column, which
## is stricter than the `isProblematic` check.



#' Filter problematic cell lines
#'
#' @name filterProblematic
#' @note Updated 2023-08-22.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @return Modified object, with problematic cell lines removed.
#'
#' @examples
#' data(crispr)
#'
#' ## DepMapExperiment ====
#' object <- crispr
#' object <- filterProblematic(object)
#' print(object)
NULL



## Updated 2023-08-22.
`filterProblematic,DepMapExperiment` <- # nolint
    function(object) {
        assert(
            validObject(object),
            isSubset(
                x = "cellosaurus",
                y = colnames(colData(object))
            ),
            isSubset(
                x = "isProblematic",
                y = colnames(colData(object)[["cellosaurus"]]))

        )
        keep <- !colData(object)[["cellosaurus"]][["isProblematic"]]
        object <- object[keep, , drop = FALSE]
        object
    }



#' @rdname filterProblematic
#' @export
setMethod(
    f = "filterProblematic",
    signature = signature(object = "DepMapExperiment"),
    definition = `filterProblematic,DepMapExperiment`
)
