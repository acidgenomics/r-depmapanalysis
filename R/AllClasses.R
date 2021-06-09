## FIXME Use Chronos as default method instead of CERES.
## FIXME Need to rework and add in support for DEMETER2 here.
## FIXME Need to add support for show method, indicating CRISPR here.

#' Cancer cell line dependency map analysis
#'
#' @details
#' Inherits from `SummarizedExperiment`.
#' Cells in columns, genes in rows.
#'
#' @note Updated 2021-06-09.
#' @export
#'
#' @return `CERES`.
#'
#' @seealso
#' - https://depmap.org/portal/achilles/
#' - https://depmap.org/ceres/
setClass(
    Class = "DepMapAnalysis",
    contains = "SummarizedExperiment"
)
setValidity(
    Class = "DepMapAnalysis",
    method = function(object) {
        assayNames <- assayNames(object)
        metadata <- metadata(object)
        packageVersion <- metadata[["packageVersion"]]
        ok <- validate(
            isSubset(c("effect", "probability"), assayNames),
            is(packageVersion, "package_version"),
            hasDimnames(object)
        )
        if (!isTRUE(ok)) return(ok)
        ok <- validateClasses(
            object = metadata,
            expected = list(
                "commonEssentials" = "character",
                "controlCommonEssentials" = "character",
                "controlNonessentials" = "character",
                "dataset" = "character",
                "date" = "Date",
                "packageVersion" = "package_version",
                "sessionInfo" = "session_info",
                "wd" = "character"
            ),
            subset = TRUE
        )
        if (!isTRUE(ok)) return(ok)
        TRUE
    }
)



#' CCLE copy number data
#'
#' @details
#' Inherits from `SummarizedExperiment`.
#' Cells in columns, genes in rows.
#'
#' @note Updated 2021-02-25.
#' @export
#'
#' @return `CCLECopyNumberData`.
setClass(
    Class = "CCLECopyNumberData",
    contains = "SummarizedExperiment"
)



#' CCLE expression data
#'
#' @details
#' Inherits from `SummarizedExperiment`.
#' Cells in columns, genes in rows.
#'
#' @note Updated 2021-02-25.
#' @export
#'
#' @return `CCLEExpressionData`.
setClass(
    Class = "CCLEExpressionData",
    contains = "SummarizedExperiment"
)



#' CCLE mutation data
#'
#' @details
#' Inherits from `SummarizedExperiment`.
#' Cells in columns, genes in rows.
#'
#' @note Updated 2021-02-25.
#' @export
#'
#' @return `CCLEMutationData`.
setClass(
    Class = "CCLEMutationData",
    contains = "DataFrame"
)
