## FIXME Use Chronos as default method instead of CERES.
## FIXME Need to rework and add in support for DEMETER2 here.
## FIXME Need to add support for show method, indicating CRISPR here.



## FIXME Want to return this type of metadata.
## CRISPR (DepMap 21Q2 Public+Score); CERES
## RNAi (Achilles+DRIVE+Marcotte); DEMETER2



#' Cancer cell line dependency map analysis
#'
#' @details
#' Inherits from `SummarizedExperiment`.
#' Cells in columns, genes in rows.
#'
#' @note Updated 2021-06-09.
#' @export
#'
#' @return `DepMapAnalysis`.
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
                ## FIXME Might not be able to return all of these for RNAi.
                "commonEssentials" = "character",
                "controlCommonEssentials" = "character",
                "controlNonessentials" = "character",
                "dataset" = "character",
                "date" = "Date",
                "packageVersion" = "package_version",
                ## Can allow: Chronos, CERES, DEMETER2
                "scoringMethod" = "FIXME",
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
setValidity(
    Class = "CCLECopyNumberData",
    method = function(object) {
        ok <- validate(
            isSubset(c("log2CopyNumber"), assayNames(object))
        )
        if (!isTRUE(ok)) return(ok)
        ok <- validateClasses(
            object = metadata(object),
            expected = list(
                ## > "missingCells" = "character",
                "dataset" = "character",
                "date" = "Date",
                "packageName" = "character",
                "packageVersion" = "numeric_version",
                "retiredGenes" = "character",
                "sessionInfo" = "session_info",
                "wd" = "character"
            ),
            subset = TRUE
        )
        if (!isTRUE(ok)) return(ok)
        TRUE
    }
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

## FIXME This needs validity checks.



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
setValidity(
    Class = "CCLEMutationData",
    method = function(object) {
        ok <- validate(
            isSubset(
                x = c(
                    "annotationTranscript",
                    "cDnaChange",
                    "cgaWesAc",
                    "chromosome",
                    "codonChange",
                    "cosmiChsCnt",
                    "dbSnpRs",
                    "dbSnpValStatus",
                    "depMapId",
                    "endPosition",
                    "entrezGeneId",
                    "exAcAf",
                    "genomeChange",
                    "hcAc",
                    "hugoSymbol",
                    "isCosmiChotspot",
                    "isDeleterious",
                    "isTcgAhotspot",
                    "ncbiBuild",
                    "proteinChange",
                    "rdAc",
                    "referenceAllele",
                    "rnAseqAc",
                    "sangerWesAc",
                    "startPosition",
                    "strand",
                    "tcgAhsCnt",
                    "tumorSeqAllele1",
                    "variantAnnotation",
                    "variantClassification",
                    "variantType",
                    "wgsAc"
                ),
                y = colnames(object)
            )
        )
        if (!isTRUE(ok)) return(ok)
        ok <- validateClasses(
            object = metadata(object),
            expected = list(
                dataset = "character",
                packageName = "character",
                packageVersion = "package_version"
            ),
            subset = TRUE
        )
        if (!isTRUE(ok)) return(ok)
        TRUE
    }
)
