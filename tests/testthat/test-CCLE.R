context("CCLE")

test_that("CCLECopyNumberData", {
    object <- CCLECopyNumberData(release = depMapRelease)
    expect_s4_class(object, "CCLECopyNumberData")
})

test_that("CCLEExpressionData", {
    object <- CCLEExpressionData(release = depMapRelease)
    expect_s4_class(object, "CCLEExpressionData")
})

test_that("CCLEMutationData", {
    object <- CCLEMutationData(release = depMapRelease)
    expect_s4_class(object, "CCLEMutationData")
    expect_identical(
        object = dim(object),
        expected = c(1288288L, 32L)
    )
    ## This step can take a long time to load the object into memory.
    x <- as(object, "DFrame")
    x <- x[1L, , drop = FALSE]
    x[["hcAc"]] <- NULL
    metadata(x) <- list()
    expect_equal(
        object = x,
        expected = DataFrame(
            "hugoSymbol" = "VPS13D",
            "entrezGeneId" = 55187,
            "ncbiBuild" = 37,
            "chromosome" = "1",
            "startPosition" = 12359347,
            "endPosition" = 12359347,
            "strand" = "+",
            "variantClassification" = "Nonsense_Mutation",
            "variantType" = "SNP",
            "referenceAllele" = "C",
            "tumorSeqAllele1" = "A",
            "dbSnpRs" = NA_character_,
            "dbSnpValStatus" = NA_character_,
            "genomeChange" = "g.chr1:12359347C>A",
            "annotationTranscript" = "ENST00000358136.3",
            "depMapId" = "ACH-000001",
            "cDnaChange" = "c.6122C>A",
            "codonChange" = "c.(6121-6123)tCa>tAa",
            "proteinChange" = "p.S2041*",
            "isDeleterious" = TRUE,
            "isTcgAhotspot" = FALSE,
            "tcgAhsCnt" = NA_integer_,
            "isCosmiChotspot" = FALSE,
            "cosmiChsCnt" = 0,
            "exAcAf" = NA_integer_,
            "variantAnnotation" = "damaging",
            "cgaWesAc" = "34:213",
            "rdAc" = NA,
            "rnAseqAc" = NA_character_,
            "sangerWesAc" = NA_character_,
            "wgsAc" = NA_character_
        )
    )
})
