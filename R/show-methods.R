#' Show an object
#'
#' @name show
#' @note Updated 2023-08-02.
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



## Updated 2023-08-02.
`show,DepMapFusions` <- # nolint
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



## Updated 2023-08-02.
`show,DepMapMutations` <- # nolint
    function(object) {
        validObject(object)
        showHeader(object)
        list <- list(
            "mutations" = nrow(object),
            "transcripts" = length(unique(object[["transcript"]])),
            "genes" = length(unique(object[["entrezGeneId"]]))
        )
        showSlotInfo(list)
    }



#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature(object = "DepMapFusions"),
    definition = `show,DepMapFusions`
)

#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature(object = "DepMapGeneEffect"),
    definition = `show,DepMapGeneEffect`
)

#' @rdname show
#' @export
setMethod(
    f = "show",
    signature = signature(object = "DepMapMutations"),
    definition = `show,DepMapMutations`
)
