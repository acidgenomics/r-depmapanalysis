## FIXME Class this on DepMapExperiment



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
#' crispr <- filterProblematic(crispr)
#' print(crispr)
NULL



## Updated 2023-08-22.
`filterProblematic,DepMapGeneEffect` <- # nolint
    function(object) {
        stop("FIXME In progress")
    }



#' @rdname filterProblematic
#' @export
setMethod(
    f = "filterProblematic",
    signature = signature(object = "DepMapGeneEffect"),
    definition = `filterProblematic,DepMapGeneEffect`
)
