context("plotTopGeneEffectPerGroup")

test_that("GeneEffect", {
    for (object in list(crispr, rnai)) {
        gene <- rownames(object)[[1L]]
        group <- switch(
            EXPR = metadata(object)[["libraryType"]],
            "rnai" = "disease",
            "subtype"
        )
        p <- plotTopGeneEffectPerGroup(object, gene = gene, group = group)
        expect_s3_class(p, "ggplot")
    }
})
