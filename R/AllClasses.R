#' Achilles CRISPR screening data
#'
#' @details
#' Scored using CERES algorithm.
#'
#' Cells in columns, genes in rows.
#'
#' @note Updated 2020-10-07.
#' @export
#'
#' @return `Achilles`.
#'
#' @seealso
#' - https://depmap.org/portal/achilles/
#' - https://depmap.org/ceres/
setClass(
    Class = "Achilles",
    contains = "RangedSummarizedExperiment"
)
setValidity(
    Class = "Achilles",
    method = function(object) {
        metadata <- metadata(object)
        version <- metadata[["version"]]
        ok <- validate(
            is(version, "package_version"),
            version >= "0.0.2"
        )
        if (!isTRUE(ok)) return(ok)
        ok <- validate(
            is(object, "RangedSummarizedExperiment"),
            hasDimnames(object)
        )
        if (!isTRUE(ok)) return(ok)

        ## Metadata ------------------------------------------------------------
        ok <- validateClasses(
            object = metadata,
            expected = list(
                commonEssentials = "character",
                controlCommonEssentials = "character",
                controlNonessentials = "character",
                date = "Date",
                release = "character",
                sessionInfo = "session_info",
                version = "package_version",
                wd = "character"
            ),
            subset = TRUE
        )
        if (!isTRUE(ok)) return(ok)

        ## Assays --------------------------------------------------------------
        assayNames <- assayNames(object)
        ok <- validate(isSubset(
            x = c("effect", "probability"),
            y = assayNames
        ))
        if (!isTRUE(ok)) return(ok)

        TRUE
    }
)



#' CCLE copy number data
#'
#' @note Updated 2020-09-30.
#' @export
#'
#' @return `CCLECopyNumberData`.
setClass(
    Class = "CCLECopyNumberData",
    contains = "DataFrame"
)



#' CCLE expression data
#'
#' @note Updated 2020-09-30.
#' @export
#'
#' @return `CCLEExpressionData`.
setClass(
    Class = "CCLEExpressionData",
    contains = "DataFrame"
)



#' CCLE mutation data
#'
#' @note Updated 2020-09-30.
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
#' Cells in columns, genes in rows.
#'
#' @note Updated 2020-10-01.
#' @export
#'
#' @return `DEMETER2`.
setClass(
    Class = "DEMETER2",
    contains = "SummarizedExperiment"
)
