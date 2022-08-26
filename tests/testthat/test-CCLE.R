## Test for any malformed DepMap flat files.
options("acid.import.engine" = "base")

test_that("CCLECopyNumberData", {
    datasets <- eval(formals(CCLECopyNumberData)[["dataset"]])
    for (dataset in datasets) {
        object <- CCLECopyNumberData(dataset = dataset)
        expect_s4_class(object, "CCLECopyNumberData")
        output <- capture.output(object)
        expect_true(grepl(pattern = "CCLECopyNumberData", x = output[[1L]]))
    }
})

test_that("CCLEExpressionData", {
    datasets <- eval(formals(CCLEExpressionData)[["dataset"]])
    for (dataset in datasets) {
        object <- CCLEExpressionData(dataset = dataset)
        expect_s4_class(object, "CCLEExpressionData")
        output <- capture.output(object)
        expect_true(grepl(pattern = "CCLEExpressionData", x = output[[1L]]))
    }
})

test_that("CCLEFusionData", {
    datasets <- eval(formals(CCLEFusionData)[["dataset"]])
    for (dataset in datasets) {
        object <- CCLEFusionData(dataset = dataset)
        expect_s4_class(object, "CCLEFusionData")
        output <- capture.output(object)
        expect_true(grepl(pattern = "CCLEFusionData", x = output[[1L]]))
    }
})

test_that("CCLEMutationData", {
    datasets <- eval(formals(CCLEMutationData)[["dataset"]])
    for (dataset in datasets) {
        object <- CCLEMutationData(dataset = dataset)
        expect_s4_class(object, "CCLEMutationData")
        output <- capture.output(object)
        expect_true(grepl(pattern = "CCLEMutationData", x = output[[1L]]))
    }
})
