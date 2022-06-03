test_that("GeneEffect", {
    for (object in list(crispr, rnai)) {
        cells <- head(colnames(object), n = 6L)
        p <- plotTopGeneEffectPerCell(object, cells = cells, n = 5L)
        expect_s3_class(p, "ggplot")
    }
})
