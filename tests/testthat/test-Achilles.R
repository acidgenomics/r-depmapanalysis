context("Achilles")

test_that("21Q1", {
    object <- Achilles(release = "21Q1")
    expect_identical(
        object = metadata(object)[["release"]],
        expected = "depmap_public_21q1"
    )
    expect_true(
        isSubset(
            x = c(
                "AKAP2_11217",
                "C10orf113_387638",
                "C12orf74_338809",
                "C17orf47_284083",
                "CRIPAK_285464",
                "KIAA1107_23285",
                "OCLM_10896",
                "PALM2_114299",
                "SPHAR_10638"
            ),
            y = metadata(object)[["retired"]]
        )
    )
})
