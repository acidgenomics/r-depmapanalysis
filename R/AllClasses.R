## FIXME Consider defining Harmonia object (should this extend Achilles?)



#' Achilles CRISPR screening data
#'
#' @details
#' Scored using CERES algorithm.
#'
#' Inherits from `SummarizedExperiment`.
#' Cells in columns, genes in rows.
#'
#' @note Updated 2021-02-25.
#' @export
#'
#' @return `Achilles`.
#'
#' @seealso
#' - https://depmap.org/portal/achilles/
#' - https://depmap.org/ceres/
setClass(
    Class = "Achilles",
    contains = "SummarizedExperiment"
)
setValidity(
    Class = "Achilles",
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
                "date" = "Date",
                "packageVersion" = "package_version",
                "release" = "character",
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



#' DEMETER2 RNAi gene effect data
#'
#' @details
#' Inherits from `SummarizedExperiment`.
#' Cells in columns, genes in rows.
#'
#' @note Updated 2021-02-25.
#' @export
#'
#' @return `DEMETER2`.
setClass(
    Class = "DEMETER2",
    contains = "SummarizedExperiment"
)
