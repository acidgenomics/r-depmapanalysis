context("plotGeneEffectVsExpression")

test_that("GeneEffect", {
    effect <- GeneEffect()
    expression <- CCLEExpressionData()
    gene <- as.character(rowData(effect)[["geneName"]])[[1L]]
    subtype <- as.character(colData(effect)[["subtype"]])[[1L]]
    p <- plotGeneEffectVsExpression(
        effect = effect,
        expression = expression,
        gene = gene,
        subtype = subtype
    )
    expect_s3_class(p, "ggplot")
})
