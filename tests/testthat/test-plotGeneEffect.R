context("plotGeneEffect")

test_that("Achilles and DEMETER2", {
    for (object in list(ach, dem)) {
        genes <- rownames(object)[seq_len(5L)]
        p <- plotGeneEffect(
            object = object,
            genes = genes
        )
        expect_s3_class(p, "ggplot")
    }
})
