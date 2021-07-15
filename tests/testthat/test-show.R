context("show")

test_that("GeneEffect", {
    object <- crispr
    output <- capture.output(object)
    expect_true(
        grepl(
            pattern = "^GeneEffect",
            x = output[[1L]]
        )
    )
})
