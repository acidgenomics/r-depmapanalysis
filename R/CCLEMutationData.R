## FIXME Need to relax casing on:
## - dbsnp
## - isTCGAhotspot
## - isCOSMIChotspot
## - TCGAhsCnt
## - ExAC
## - SangerWES_AC



#' Import CCLE mutation data
#'
#' @export
#' @note Updated 2022-09-21.
#'
#' @inheritParams params
#'
#' @return `CCLEMutationData`.
#'
#' @examples
#' object <- CCLEMutationData()
#' dim(object)
CCLEMutationData <- # nolint
    function(dataset) {
        dataset <- match.arg(dataset)
        url <- datasets[[dataset]][["files"]][["ccle"]][["mutations"]][["url"]]
        assert(isAURL(url))
        df <- .importDataFile(url = url, format = "csv", rownamesCol = NULL)
        assert(is(df, "DataFrame"))
        ## FIXME This is camel casing to `isTcgAhotspot`, which we don't want...
        ## FIXME May need to update syntactic to address this edge case.
        ##  [1] "Hugo_Symbol"            "Entrez_Gene_Id"
        ##  [3] "NCBI_Build"             "Chromosome"
        ##  [5] "Start_position"         "End_position"
        ##  [7] "Strand"                 "Variant_Classification"
        ##  [9] "Variant_Type"           "Reference_Allele"
        ## [11] "Alternate_Allele"       "dbSNP_RS"
        ## [13] "dbSNP_Val_Status"       "Genome_Change"
        ## [15] "Annotation_Transcript"  "DepMap_ID"
        ## [17] "cDNA_Change"            "Codon_Change"
        ## [19] "Protein_Change"         "isDeleterious"
        ## [21] "isTCGAhotspot"          "TCGAhsCnt"
        ## [23] "isCOSMIChotspot"        "COSMIChsCnt"
        ## [25] "ExAC_AF"                "Variant_annotation"
        ## [27] "CGA_WES_AC"             "HC_AC"
        ## [29] "RD_AC"                  "RNAseq_AC"
        ## [31] "SangerWES_AC"           "WGS_AC"
        ## FIXME Need to manually fix these columns:
        ## dbSNP_RS
        colnames(df) <- camelCase(colnames(df), strict = TRUE)
        ##  [1] "hugoSymbol"            "entrezGeneId"          "ncbiBuild"
        ##  [4] "chromosome"            "startPosition"         "endPosition"
        ##  [7] "strand"                "variantClassification" "variantType"
        ## [10] "referenceAllele"       "alternateAllele"       "dbSnpRs"
        ## [13] "dbSnpValStatus"        "genomeChange"          "annotationTranscript"
        ## [16] "depMapId"              "cDnaChange"            "codonChange"
        ## [19] "proteinChange"         "isDeleterious"         "isTcgAhotspot"
        ## [22] "tcgAhsCnt"             "isCosmiChotspot"       "cosmiChsCnt"
        ## [25] "exAcAf"                "variantAnnotation"     "cgaWesAc"
        ## [28] "hcAc"                  "rdAc"                  "rnAseqAc"
        ## [31] "sangerWesAc"           "wgsAc"
        df <- encode(df)
        metadata(df) <- list(
            "date" = Sys.Date(),
            "dataset" = dataset,
            "packageName" = .pkgName,
            "packageVersion" = .pkgVersion
        )
        new("CCLEMutationData", df)
    }

formals(CCLEMutationData)[["dataset"]] <- # nolint
    .formalsList[["depmapDataset"]]
