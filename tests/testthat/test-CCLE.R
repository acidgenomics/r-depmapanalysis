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
})
