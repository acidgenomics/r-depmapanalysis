test_that("GeneEffect : all co-dependencies for a single gene", {
    object <- crispr
    geneNames <- as.character(rowData(object)[["geneName"]])
    geneName1 <- geneNames[[1L]]
    geneName2 <- geneNames[[2L]]
    ## Calculate all co-dependencies for a gene of interest.
    x <- Codependencies(
        object = object,
        geneName1 = geneName1,
        geneName2 = NULL
    )
    expect_s4_class(x, "Codependencies")
    output <- capture.output(x)
    expect_true(grepl(pattern = "Codependencies", x = output[[1L]]))
    expect_identical(
        object = dim(x),
        expected = c(97L, 3L)
    )
    df <- as.data.frame(x[1L, ])
    df[["pearson"]] <- round(df[["pearson"]], digits = 3L)
    expect_identical(
        object = df,
        expected = data.frame(
            "geneName1" = "A1BG",
            "geneName2" = "ABCF1",
            "pearson" = 0.379
        )
    )
})

test_that("Gene Effect: lineage restrict and compare 2 genes", {
    object <- crispr
    diseases <- as.character(colData(object)[["cellosaurusNcItDisease"]])
    disease <- diseases[[1L]]
    keep <- colData(object)[["cellosaurusNcItDisease"]] %in% disease
    object <- object[, keep]
    x <- Codependencies(
        object = object,
        geneName1 = geneName1,
        geneName2 = geneName2
    )
    expect_s4_class(x, "Codependencies")
    output <- capture.output(x)
    expect_true(grepl(pattern = "Codependencies", x = output[[1L]]))
    expect_identical(
        object = dim(x),
        expected = c(1L, 3L)
    )
    df <- as.data.frame(x)
    expect_identical(
        object = df,
        expected = data.frame(
            "geneName1" = "A1BG",
            "geneName2" = "A1CF",
            "pearson" = -1 # nolint
        )
    )
})
