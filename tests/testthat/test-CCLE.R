context("CCLE")

test_that("CCLECopyNumberData", {
    object <- CCLECopyNumberData(dataset = dataset)
    expect_s4_class(object, "CCLECopyNumberData")
})

test_that("CCLEExpressionData", {
    object <- CCLEExpressionData(dataset = dataset)
    expect_s4_class(object, "CCLEExpressionData")
})

test_that("CCLEMutationData", {
    object <- CCLEMutationData(dataset = dataset)
    expect_s4_class(object, "CCLEMutationData")
})
