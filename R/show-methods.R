## FIXME Add show method for CCLE classes...



#' @name show
#' @inherit AcidGenerics::show
#' @note Updated 2021-06-09.
#'
#' @examples
#' data(crispr)
#'
#' ## DepMapAnalysis ====
#' show(crispr)
NULL



## Updated 2021-06-09.
`show,DepMapAnalysis` <-  # nolint
    function(object) {
        validObject(object)
        showHeader(object)
        ## Metadata.
        m <- metadata(object)
        list <- list(
            "dataset" = m[["dataset"]],
            "project" = m[["project"]],
            "libraryType" = m[["libraryType"]],
            "scoringMethod" = m[["scoringMethod"]]
        )
        showSlotInfo(list)
        se <- as(object, "SummarizedExperiment")
        cat(capture.output(show(se)), sep = "\n")
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("DepMapAnalysis"),
    definition = `show,DepMapAnalysis`
)
