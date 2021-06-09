context("plotGeneEffect")

test_that("CRISPR and RNAi", {
    for (object in list(crispr, rnai)) {
        genes <- rownames(object)[seq_len(5L)]
        p <- plotGeneEffect(
            object = object,
            genes = genes
        )
        expect_s3_class(p, "ggplot")
    }
})
