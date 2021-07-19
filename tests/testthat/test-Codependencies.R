context("Codependencies")

test_that("All datasets", {
    gene <- "SOX10"
    datasets <- eval(formals(Codependencies)[["dataset"]])
    for (dataset in datasets) {
        object <- Codependencies(gene = gene, dataset = dataset)
        expect_s4_class(object, "Codependencies")
    }
})
