context("CCLE")

test_that("CCLECopyNumberData", {
    object <- CCLECopyNumberData(release = depMapRelease)
    expect_s4_class(object, "SummarizedExperiment")
})

test_that("CCLEExpressionData", {
    object <- CCLEExpressionData(release = depMapRelease)
    expect_s4_class(object, "SummarizedExperiment")
})

test_that("CCLEMutationData", {
    object <- CCLEMutationData(release = depMapRelease)
    expect_s4_class(object, "SummarizedExperiment")
})
