test_that("GeneEffect", {
    ## FIXME Speed this up by using our minimal example objects.
    effect <- GeneEffect()
    expression <- CCLEExpressionData()
    gene <- as.character(rowData(effect)[["geneName"]])[[1L]]
    subtype <- as.character(colData(effect)[["subtype"]])[[1L]]
    p <- plotGeneEffectVsExpression(
        effect = effect,
        expression = expression,
        gene = gene,
        ## FIXME This is suboptimal that we've hard coded to subtype??
        subtype = subtype
    )
    expect_s3_class(p, "ggplot")
})
