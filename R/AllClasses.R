## Internal validity methods ===================================================

## Updated 2023-09-14.
.validateNcbiGeneIds <- function(object) {
    ok <- validate(
        allAreMatchingRegex(
            x = rownames(object),
            pattern = "^[0-9]+$"
        )
    )
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- validateClasses(
        object = rowData(object),
        expected = list(
            "chromosome" = "Rle",
            "dbXrefs" = "CompressedCharacterList",
            "description" = "Rle",
            "featureType" = "Rle",
            "geneId" = "Rle",
            "geneName" = "Rle",
            "geneSynonyms" = "CompressedCharacterList",
            "mapLocation" = "Rle",
            "modificationDate" = "Rle",
            "nomenclatureStatus" = "Rle",
            "otherDesignations" = "CompressedCharacterList",
            "taxonomyId" = "Rle",
            "typeOfGene" = "Rle"
        ),
        subset = TRUE
    )
    if (!isTRUE(ok)) {
        return(ok)
    }
    TRUE
}



## Updated 2023-09-14.
.validateEnsemblTxIds <- function(object) {
    ok <- validate(
        allAreMatchingRegex(
            x = rownames(object),
            pattern = "^ENST[0-9]{11}$"
        )
    )
    ok <- validateClasses(
        object = rowData(object),
        expected = list(
            "broadClass" = "Rle",
            "canonicalTranscript" = "Rle",
            "description" = "Rle",
            "gcContent" = "Rle",
            "geneBiotype" = "Rle",
            "geneId" = "Rle",
            "geneIdVersion" = "Rle",
            "geneName" = "Rle",
            "geneSeqEnd" = "Rle",
            "geneSeqStart" = "Rle",
            "geneSynonyms" = "CompressedCharacterList",
            "ncbiGeneId" = "CompressedIntegerList",
            "seqCoordSystem" = "Rle",
            "txBiotype" = "Rle",
            "txCdsSeqEnd" = "Rle",
            "txCdsSeqStart" = "Rle",
            "txExternalName" = "Rle",
            "txId" = "Rle",
            "txIdVersion" = "Rle",
            "txIsCanonical" = "Rle",
            "txName" = "Rle",
            "txSupportLevel" = "Rle"
        ),
        subset = TRUE
    )
    if (!isTRUE(ok)) {
        return(ok)
    }
    TRUE
}



## Updated 2023-09-14.
.validateSE <- function(object) {
    ok <- validate(
        hasRownames(object),
        hasColnames(object),
        allAreMatchingRegex(x = colnames(object), pattern = "^CVCL_.+$")
    )
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- validateClasses(
        object = colData(object),
        expected = list(
            "broad" = "DFrame",
            "cellosaurus" = "Cellosaurus"
        ),
        subset = TRUE
    )
    if (!isTRUE(ok)) {
        return(ok)
    }
    ok <- validateClasses(
        object = metadata(object),
        expected = list(
            "dataset" = "character",
            "date" = "Date",
            "json" = "list",
            "packageName" = "character",
            "packageVersion" = "package_version",
            "sessionInfo" = "sessionInfo",
            "wd" = "character"
        ),
        subset = TRUE
    )
    if (!isTRUE(ok)) {
        return(ok)
    }
    TRUE
}



## Virtual classes =============================================================

#' `DepMapGeneEffect` virtual class
#'
#' @export
#' @note Updated 2023-09-14.
#'
#' @details
#' Not intended to be used directly.
#'
#' `DepMapCrisprGeneEffect` and `DepMapRnaiGeneEffect` extend this class.
#'
#' @return `DepMapGeneEffect`.
setClass(
    Class = "DepMapGeneEffect",
    contains = "SummarizedExperiment"
)
setValidity(
    Class = "DepMapGeneEffect",
    method = function(object) {
        ok <- .validateSE(object)
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- isSubset("effect", assayNames(object))
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- .validateNcbiGeneIds(object)
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- validateClasses(
            object = metadata(object),
            expected = list(
                "libraryType" = "character",
                "scoringMethod" = "character"
            ),
            subset = TRUE
        )
        if (!isTRUE(ok)) {
            return(ok)
        }
        if (identical(metadata(object)[["libraryType"]], "crispr")) {
            ok <- validate(
                isSubset("probability", assayNames(object))
            )
            if (!isTRUE(ok)) {
                return(ok)
            }
            ok <- validateClasses(
                object = metadata(object),
                expected = list(
                    "commonEssentials" = "DFrame",
                    "controlCommonEssentials" = "DFrame",
                    "controlNonessentials" = "DFrame"
                ),
                subset = TRUE
            )
            if (!isTRUE(ok)) {
                return(ok)
            }
        }
        TRUE
    }
)



## Primary classes =============================================================

#' DepMap CRISPR gene effect
#'
#' @details
#' Inherits from `DepMapGeneEffect`, which extends `SummarizedExperiment`.
#' Cells in columns, genes in rows.
#'
#' @export
#' @note Updated 2023-08-09.
#'
#' @return `DepMapCrisprGeneEffect`.
#'
#' @seealso
#' - https://depmap.org/portal/achilles/
#' - https://depmap.org/ceres/
#' - https://score.depmap.sanger.ac.uk/
setClass(
    Class = "DepMapCrisprGeneEffect",
    contains = "DepMapGeneEffect"
)



#' DepMap gene effect co-dependencies
#'
#' @details
#' Inherits from `DFrame`.
#'
#' @note Updated 2023-08-09.
#' @export
#'
#' @return `DepMapCodependencies`.
setClass(
    Class = "DepMapCodependencies",
    contains = "DFrame"
)
setValidity(
    Class = "DepMapCodependencies",
    method = function(object) {
        ok <- validate(
            identical(
                x = c("geneName1", "geneName2", "pearson"),
                y = colnames(object)
            )
        )
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- validateClasses(
            object = metadata(object),
            expected = .expectedMetadata,
            subset = TRUE
        )
        if (!isTRUE(ok)) {
            return(ok)
        }
        TRUE
    }
)


## FIXME Check for NCBI genes here.

#' DepMap copy number data
#'
#' @details
#' Gene level copy number data, log2 transformed with a pseudo count of 1.
#' This is generated by mapping genes onto the segment level calls.
#'
#' Inherits from `SummarizedExperiment`.
#' Cells in columns, genes in rows.
#'
#' @export
#' @note Updated 2023-09-14.
#'
#' @return `DepMapCopyNumber`.
setClass(
    Class = "DepMapCopyNumber",
    contains = "SummarizedExperiment"
)
setValidity(
    Class = "DepMapCopyNumber",
    method = function(object) {
        ok <- .validateSE(object)
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- isSubset("log2CopyNumber", assayNames(object))
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- .validateNcbiGeneIds(object)
        if (!isTRUE(ok)) {
            return(ok)
        }
        TRUE
    }
)



## FIXME Check the class structure of columns here.

#' DepMap fusion call data
#'
#' @details
#' Inherits from `DFrame`.
#'
#' @note Updated 2023-08-02.
#' @export
#'
#' @return `DepMapFusions`.
setClass(
    Class = "DepMapFusions",
    contains = "DFrame"
)
setValidity(
    Class = "DepMapFusions",
    method = function(object) {
        ok <- validate(
            hasRownames(object),
            hasColnames(object),
            isSubset(
                x = c(
                    "annots",
                    "ccleCount",
                    "depmapId",
                    "ffpm",
                    "fusionName",
                    "junctionReadCount",
                    "largeAnchorSupport",
                    "leftBreakDinuc",
                    "leftBreakEntropy",
                    "leftBreakpoint",
                    "leftGene",
                    "rightBreakDinuc",
                    "rightBreakEntropy",
                    "rightBreakpoint",
                    "rightGene",
                    "spanningFragCount",
                    "spliceType"
                ),
                y = colnames(object)
            )
        )
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- validateClasses(
            object = metadata(object),
            expected = append(
                x = .expectedMetadata,
                values = list("filtered" = "logical")
            ),
            subset = TRUE
        )
        if (!isTRUE(ok)) {
            return(ok)
        }
        TRUE
    }
)


## FIXME Check for NCBI genes here.s

#' DepMap RNA-seq gene expression
#'
#' @details
#' Inherits from `SummarizedExperiment`.
#' Cells in columns, genes in rows.
#'
#' @export
#' @note Updated 2023-09-14.
#'
#' @return `DepMapGeneExpression`.
setClass(
    Class = "DepMapGeneExpression",
    contains = "SummarizedExperiment"
)
setValidity(
    Class = "DepMapGeneExpression",
    method = function(object) {
        ok <- .validateSE(object)
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- isSubset("log2Tpm", assayNames(object))
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- .validateNcbiGeneIds(object)
        if (!isTRUE(ok)) {
            return(ok)
        }
        TRUE
    }
)



#' DepMap somatic mutation data
#'
#' @details
#' Inherits from `DFrame`.
#'
#' @note Updated 2023-08-02.
#' @export
#'
#' @return `DepMapMutations`.
setClass(
    Class = "DepMapMutations",
    contains = "DFrame"
)
setValidity(
    Class = "DepMapMutations",
    method = function(object) {
        ok <- validate(
            isSubset(
                x = c(
                    "af",
                    "alt",
                    "altCount",
                    "associatedWith",
                    "cancerMolecularGenetics",
                    "ccleDeleterious",
                    "chrom",
                    "civicDescription",
                    "civicId",
                    "civicScore",
                    "cosmicHotspot",
                    "cosmicOverlappingMutations",
                    "cscapeScore",
                    "dannScore",
                    "dbsnpFilter",
                    "dbsnpId",
                    "didaId",
                    "didaName",
                    "dnaChange",
                    "driver",
                    "entrezGeneId",
                    "funseq2Score",
                    "gcContent",
                    "gt",
                    "gtexGene",
                    "gwasDisease",
                    "gwasPmid",
                    "hessDriver",
                    "hessSignature",
                    "hgncFamily",
                    "hgncName",
                    "hugoSymbol",
                    "issues",
                    "likelyDriver",
                    "likelyGof",
                    "likelyLof",
                    "lineageAssociation",
                    "lof",
                    "modelId",
                    "pharmgkbId",
                    "popaf",
                    "pos",
                    "proteinChange",
                    "ps",
                    "ref",
                    "refCount",
                    "revelScore",
                    "str",
                    "structuralRelation",
                    "transcript",
                    "transcriptExon",
                    "transcriptLikelyLof",
                    "transcriptStrand",
                    "uniprotId",
                    "variantInfo",
                    "variantType"
                ),
                y = colnames(object)
            )
        )
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- validateClasses(
            object = metadata(object),
            expected = .expectedMetadata,
            subset = TRUE
        )
        if (!isTRUE(ok)) {
            return(ok)
        }
        TRUE
    }
)



## FIXME Need to add a validity method here.

#' DepMap protein expression data
#'
#' @details
#' Inherits from `SummarizedExperiment`.
#' Cells in columns, peptides in rows.
#'
#' @export
#' @note Updated 2023-09-11.
#'
#' @return `DepMapProteomics`.
setClass(
    Class = "DepMapProteomics",
    contains = "SummarizedExperiment"
)



#' DepMap RNAi gene effect (DEMETER2)
#'
#' @details
#' Inherits from `DepMapGeneEffect`, which extends `SummarizedExperiment`.
#' Cells in columns, genes in rows.
#'
#' @export
#' @note Updated 2023-08-09.
#'
#' @return `DepMapRnaiGeneEffect`.
setClass(
    Class = "DepMapRnaiGeneEffect",
    contains = "DepMapGeneEffect"
)



#' DepMap RNA-seq transcript expression
#'
#' @details
#' Inherits from `RangedSummarizedExperiment`.
#' Cells in columns, transcripts in rows.
#'
#' @export
#' @note Updated 2023-09-14.
#'
#' @return `DepMapTxExpression`.
setClass(
    Class = "DepMapTxExpression",
    contains = "RangedSummarizedExperiment"
)
setValidity(
    Class = "DepMapTxExpression",
    method = function(object) {
        ok <- .validateSE(object)
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- isSubset("log2Tpm", assayNames(object))
        if (!isTRUE(ok)) {
            return(ok)
        }
        ok <- .validateEnsemblTxIds(object)
        if (!isTRUE(ok)) {
            return(ok)
        }
        TRUE
    }
)
