test_that("CCLECopyNumberData", {
    datasets <- eval(formals(CCLECopyNumberData)[["dataset"]])
    for (dataset in datasets) {
        object <- CCLECopyNumberData(dataset = dataset)
        expect_s4_class(object, "CCLECopyNumberData")
    }
})

test_that("CCLEExpressionData", {
    datasets <- eval(formals(CCLEExpressionData)[["dataset"]])
    for (dataset in datasets) {
        object <- CCLEExpressionData(dataset = dataset)
        expect_s4_class(object, "CCLEExpressionData")
    }
})

test_that("CCLEFusionData", {
    datasets <- eval(formals(CCLEFusionData)[["dataset"]])
    for (dataset in datasets) {
        object <- CCLEFusionData(dataset = dataset)
        expect_s4_class(object, "CCLEFusionData")
    }
})

test_that("CCLEMutationData", {
    datasets <- eval(formals(CCLEMutationData)[["dataset"]])
    for (dataset in datasets) {
        object <- CCLEMutationData(dataset = dataset)
        expect_s4_class(object, "CCLEMutationData")
    }
})
