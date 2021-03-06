#' Achilles
#'
#' Legacy object that is no longer recommended.
#'
#' Use `DepMapAnalysis` object instead.
#'
#' @export
#' @keywords internal
#' @note Updated 2021-06-20.
#'
#' @return `Achilles`.
setClass(
    Class = "Achilles",
    contains = "SummarizedExperiment"
)



#' CCLE copy number data
#'
#' @details
#' Gene level copy number data, log2 transformed with a pseudo count of 1.
#' This is generated by mapping genes onto the segment level calls.
#'
#' Inherits from `SummarizedExperiment`.
#' Cells in columns, genes in rows.
#'
#' @export
#' @note Updated 2021-06-09.
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
            isSubset("log2CopyNumber", assayNames(object)),
            isSubset(
                x = c(
                    "chromosome",
                    "dbXrefs",
                    "description",
                    "featureType",
                    "geneId",
                    "geneName",
                    "geneSynonyms",
                    "mapLocation",
                    "modificationDate",
                    "nomenclatureStatus",
                    "otherDesignations",
                    "typeOfGene",
                    "xTaxId"
                ),
                y = colnames(rowData(object))
            ),
            isSubset(
                x = c(
                    "achillesNReplicates",
                    "age",
                    "alias",
                    "cas9Activity",
                    "ccleName",
                    "cellLineName",
                    "cellLineNnmd",
                    "cosmicid",
                    "cultureMedium",
                    "cultureType",
                    "depMapId",
                    "depmapPublicComments",
                    "lineage",
                    "lineageMolecularSubtype",
                    "lineageSubSubtype",
                    "lineageSubtype",
                    "primaryDisease",
                    "primaryOrMetastasis",
                    "rrid",
                    "sampleCollectionSite",
                    "sangerModelId",
                    "sex",
                    "source",
                    "strippedCellLineName",
                    "subtype",
                    "wtsiMasterCellId"
                ),
                y = colnames(colData(object))
            )
        )
        if (!isTRUE(ok)) return(ok)
        ok <- validateClasses(
            object = metadata(object),
            expected = list(
                ## > "missingCells" = "character",
                ## > "retiredGenes" = "character",
                "dataset" = "character",
                "date" = "Date",
                "packageName" = "character",
                "packageVersion" = "numeric_version",
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
#'
#' RNA-seq TPM gene expression data for just protein coding genes using RSEM.
#' Log2 transformed, using a pseudo-count of 1.
#'
#' Inherits from `SummarizedExperiment`.
#' Cells in columns, genes in rows.
#'
#' @export
#' @note Updated 2021-06-09.
#'
#' @return `CCLEExpressionData`.
setClass(
    Class = "CCLEExpressionData",
    contains = "SummarizedExperiment"
)
setValidity(
    Class = "CCLEExpressionData",
    method = function(object) {
        ok <- validate(
            isSubset("log2Tpm", assayNames(object)),
            isSubset(
                x = c(
                    "chromosome",
                    "dbXrefs",
                    "description",
                    "featureType",
                    "geneId",
                    "geneName",
                    "geneSynonyms",
                    "mapLocation",
                    "modificationDate",
                    "nomenclatureStatus",
                    "otherDesignations",
                    "typeOfGene",
                    "xTaxId"
                ),
                y = colnames(rowData(object))
            ),
            isSubset(
                x = c(
                    "achillesNReplicates",
                    "age",
                    "alias",
                    "cas9Activity",
                    "ccleName",
                    "cellLineName",
                    "cellLineNnmd",
                    "cosmicid",
                    "cultureMedium",
                    "cultureType",
                    "depMapId",
                    "depmapPublicComments",
                    "lineage",
                    "lineageMolecularSubtype",
                    "lineageSubSubtype",
                    "lineageSubtype",
                    "primaryDisease",
                    "primaryOrMetastasis",
                    "rrid",
                    "sampleCollectionSite",
                    "sangerModelId",
                    "sex",
                    "source",
                    "strippedCellLineName",
                    "subtype",
                    "wtsiMasterCellId"
                ),
                y = colnames(colData(object))
            )
        )
        if (!isTRUE(ok)) return(ok)
        ok <- validateClasses(
            object = metadata(object),
            expected = list(
                ## > "missingCells" = "character",
                ## > "retiredGenes" = "character",
                "dataset" = "character",
                "date" = "Date",
                "packageName" = "character",
                "packageVersion" = "numeric_version",
                "sessionInfo" = "session_info",
                "wd" = "character"
            ),
            subset = TRUE
        )
        if (!isTRUE(ok)) return(ok)
        TRUE
    }
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
                "dataset" = "character",
                "packageName" = "character",
                "packageVersion" = "package_version"
            ),
            subset = TRUE
        )
        if (!isTRUE(ok)) return(ok)
        TRUE
    }
)



#' Cancer cell line dependency map analysis
#'
#' @details
#' Inherits from `SummarizedExperiment`.
#' Cells in columns, genes in rows.
#'
#' @export
#' @note Updated 2021-06-09.
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
        ok <- validate(
            hasDimnames(object),
            isSubset("effect", assayNames(object))
        )
        if (!isTRUE(ok)) return(ok)
        ok <- validateClasses(
            object = metadata(object),
            expected = list(
                ## > "missingCells" = "character",
                ## > "retiredGenes" = "character",
                "dataset" = "character",
                "date" = "Date",
                "libraryType" = "character",
                "packageVersion" = "package_version",
                "project" = "character",
                "scoringMethod" = "character",
                "sessionInfo" = "session_info",
                "wd" = "character"
            ),
            subset = TRUE
        )
        if (!isTRUE(ok)) return(ok)
        switch(
            EXPR = metadata(object)[["libraryType"]],
            "crispr" = {
                ok <- validate(
                    isSubset("probability", assayNames(object)),
                    isSubset(
                        x = c(
                            "achillesNReplicates",
                            "age",
                            "alias",
                            "cas9Activity",
                            "ccleName",
                            "cellLineName",
                            "cellLineNnmd",
                            "cosmicid",
                            "cultureMedium",
                            "cultureType",
                            "depMapId",
                            "depmapPublicComments",
                            "lineage",
                            "lineageMolecularSubtype",
                            "lineageSubSubtype",
                            "lineageSubtype",
                            "primaryDisease",
                            "primaryOrMetastasis",
                            "rrid",
                            "sampleCollectionSite",
                            "sangerModelId",
                            "sex",
                            "source",
                            "strippedCellLineName",
                            "subtype",
                            "wtsiMasterCellId"
                        ),
                        y = colnames(colData(object))
                    )
                )
                if (!isTRUE(ok)) return(ok)
                ok <- validateClasses(
                    object = metadata(object),
                    expected = list(
                        "commonEssentials" = "character",
                        "controlCommonEssentials" = "character",
                        "controlNonessentials" = "character"
                    ),
                    subset = TRUE
                )
                if (!isTRUE(ok)) return(ok)
            },
            "rnai" = {
                ok <- validate(
                    isSubset(
                        x = c(
                            "ccleId",
                            "cellLineName",
                            "disease",
                            "diseaseSubSubtype",
                            "diseaseSubtype",
                            "inAchilles",
                            "inDrive",
                            "inMarcotte",
                            "marcotteName",
                            "marcotteSubtypeIntrinsic",
                            "marcotteSubtypeNeve",
                            "marcotteSubtypeThreeReceptor",
                            "novartisName",
                            "novartisPathologistAnnotation",
                            "novartisPrimarySite"
                        ),
                        y = colnames(colData(object))
                    )
                )
                if (!isTRUE(ok)) return(ok)
            }
        )
        TRUE
    }
)
