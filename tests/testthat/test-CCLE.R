context("CCLE")

test_that("CCLECopyNumberData", {
    object <- CCLECopyNumberData(release = depMapRelease)
})

test_that("CCLEExpressionData", {
    object <- CCLEExpressionData(release = depMapRelease)
})

test_that("CCLEMutationData", {
    object <- CCLEMutationData(release = depMapRelease)
})
