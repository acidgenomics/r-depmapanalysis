#' Show an object
#'
#' @name show
#' @note Updated 2022-09-21.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @return Console output.
#'
#' @examples
#' data(crispr)
#'
#' ## GeneEffect ====
#' show(crispr)
NULL



## Updated 2022-09-21.
`show,CCLEFusionData` <- # nolint
    function(object) {
        validObject(object)
        showHeader(object)
        list <- list(
            "fusions" = length(unique(object[["fusionName"]])),
            "cells" = length(unique(object[["depmapId"]]))
        )
        showSlotInfo(list)
    }



## Updated 2021-06-09.
`show,CCLEMutationData` <- # nolint
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



## Updated 2022-09-07.
`show,GeneEffect` <- # nolint
    function(object) {
        validObject(object)
        showHeader(object)
        m <- metadata(object)
        list <- list(
            "dataset" = m[["dataset"]],
            "libraryType" = m[["libraryType"]],
            "scoringMethod" = m[["scoringMethod"]],
            "date" = m[["date"]],
            "releaseDate" = m[["releaseDate"]]
        )
        showSlotInfo(list)
        se <- as(object, "SummarizedExperiment")
        cat(capture.output(show(se)), sep = "\n")
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature(object = "CCLEFusionData"),
    definition = `show,CCLEFusionData`
)



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature(object = "CCLEMutationData"),
    definition = `show,CCLEMutationData`
)



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature(object = "GeneEffect"),
    definition = `show,GeneEffect`
)
