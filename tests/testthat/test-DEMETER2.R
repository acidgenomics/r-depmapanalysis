context("DEMETER2")

test_that("demeter2_data_v6", {
    object <- DEMETER2()
    expect_identical(
        object = head(colnames(object), n = 3L),
        expected = c(
            "a101d_skin",
            "a1207_central_nervous_system",
            "a172_central_nervous_system"
        )
    )
    expect_identical(
        object = head(rownames(object), n = 3L),
        expected = c("a1bg_1", "a1cf_29974", "a2m_2")
    )
    expect_identical(
        object = names(assays(object)),
        expected = "effect"
    )
    expect_identical(
        object = colData(object)["a101d_skin", ],
        expected = DataFrame(
            "ccleId" = "A101D_SKIN",
            "disease" = "skin",
            "diseaseSubSubtype" = NA_character_,
            "diseaseSubtype" = "melanoma",
            "inAchilles" = FALSE,
            "inDrive" = TRUE,
            "inMarcotte" = FALSE,
            "marcotteName" = NA_character_,
            "marcotteSubtypeIntrinsic" = NA_character_,
            "marcotteSubtypeNeve" = NA_character_,
            "marcotteSubtypeThreeReceptor" = NA_character_,
            "novartisName" = "a101d",
            "novartisPathologistAnnotation" = "Skin:Melanoma",
            "novartisPrimarySite" = "skin",
            row.names = "a101d_skin"
        )
    )
    rowData <- decode(rowData(object)["a1bg_1", c("geneId", "geneName")])
    metadata(rowData) <- list()
    expect_identical(
        object = rowData,
        expected = DataFrame(
            "geneId" = 1L,
            "geneName" = "A1BG",
            row.names = "a1bg_1"
        )
    )
    expect_identical(
        object = metadata(object)[["release"]],
        expected = "demeter2_data_v6"
    )
    expect_true(
        isSubset(
            x = c(
                "AKAP2_11217",
                "C10orf113_387638",
                "C16orf47_388289",
                "C2orf48_348738",
                paste(
                    "LINC01001",
                    "LOC105377826",
                    "LOC107984841_100133161",
                    "105377826",
                    "107984841",
                    sep = "_and_"
                ),
                paste(
                    "LOC100133050",
                    "LOC102725009_100133050",
                    "102725009",
                    sep = "_and_"
                )
            ),
            y = metadata(object)[["retired"]]
        )
    )
})
