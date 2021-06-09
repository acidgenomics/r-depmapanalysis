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
`show,CCLEMutationData` <-  # nolint
    function(object) {
        validObject(object)
        showHeader(object)
        list <- list(
            "mutations" = nrow(object),
            "genes" = length(unique(object[["entrezGeneId"]])),
            "transcripts" = length(unique(object[["annotationTranscript"]])),
            "ncbiBuild" = unique(object[["ncbiBuild"]]),
            "variantClassification" = sort(unique(
                object[["variantClassification"]]
            ))
        )
        showSlotInfo(list)
    }



## Updated 2021-06-09.
`show,DepMapAnalysis` <-  # nolint
    function(object) {
        validObject(object)
        showHeader(object)
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
    signature = signature("CCLEMutationData"),
    definition = `show,CCLEMutationData`
)

#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature("DepMapAnalysis"),
    definition = `show,DepMapAnalysis`
)
