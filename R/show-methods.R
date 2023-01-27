#' Show an object
#'
#' @name show
#' @note Updated 2023-01-27.
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



## Updated 2023-01-27.
`show,DepMapFusion` <- # nolint
    function(object) {
        validObject(object)
        showHeader(object)
        list <- list(
            "fusions" = length(unique(object[["fusionName"]])),
            "cells" = length(unique(object[["depmapId"]]))
        )
        showSlotInfo(list)
    }



## Updated 2023-01-27.
`show,DepMapMutation` <- # nolint
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



## Updated 2023-01-27.
`show,DepMapGeneEffect` <- # nolint
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
    signature = signature(object = "DepMapFusion"),
    definition = `show,DepMapFusion`
)



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature(object = "DepMapMutation"),
    definition = `show,DepMapMutation`
)



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature(object = "DepMapGeneEffect"),
    definition = `show,DepMapGeneEffect`
)
