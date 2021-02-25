context("Achilles")

test_that("21Q1", {
    object <- Achilles(release = depMapRelease)
    expect_s4_class(object, "Achilles")
    expect_identical(
        object = head(colnames(object), n = 3L),
        expected = c("ach_000004", "ach_000005", "ach_000007")
    )
    expect_identical(
        object = head(rownames(object), n = 3L),
        expected = c("a1bg_1", "a1cf_29974", "a2m_2")
    )
    expect_identical(
        object = names(assays(object)),
        expected = c("effect", "probability")
    )
    ## nolint start
    expect_equal(
        object = colData(object)["ach_000004", ],
        expected = DataFrame(
            "achillesNReplicates" = 2,
            "age" = 30,
            "alias" = NA_character_,
            "cas9Activity" = 52.4,
            "ccleName" = "HEL_HAEMATOPOIETIC_AND_LYMPHOID_TISSUE",
            "cellLineName" = "HEL",
            "cellLineNnmd" = -3.079202,  # NOTE causes `identical()` to fail.
            "cosmicid"= 907053,
            "cultureMedium" = "RPMI + 10% FBS",
            "cultureType" = "Suspension",
            "depMapId" = "ACH-000004",
            "depmapPublicComments" = NA,
            "lineage" = "blood",
            "lineageMolecularSubtype" = NA_character_,
            "lineageSubSubtype" = "M6",
            "lineageSubtype" = "AML",
            "primaryDisease" = "Leukemia",
            "primaryOrMetastasis" = NA_character_,
            "rrid" = "CVCL_0001",
            "sampleCollectionSite" = "haematopoietic_and_lymphoid_tissue",
            "sangerModelId" = "SIDM00594",
            "sex" = "Male",
            "source" = "DSMZ",
            "strippedCellLineName" = "HEL",
            "subtype" = paste(
                "Acute Myelogenous Leukemia (AML),",
                "M6 (Erythroleukemia)"
            ),
            "wtsiMasterCellId" = 783,
            row.names = "ach_000004"
        )
    )
    ## nolint end
    rowData <- rowData(object)[
        "a1bg_1",
        ,
        drop = FALSE
    ]
    rowData <- decode(rowData)
    metadata(rowData) <- list()
    attr(class(rowData[["dbXrefs"]]), "package") <- "IRanges"
    attr(class(rowData[["geneSynonyms"]]), "package") <- "IRanges"
    attr(class(rowData[["otherDesignations"]]), "package") <- "IRanges"
    expect_identical(
        object = rowData,
        expected = DataFrame(
            "chromosome" = "19",
            "dbXrefs" = CharacterList(c(
                "Ensembl:ENSG00000121410",
                "HGNC:HGNC:5",
                "MIM:138670"
            )),
            "description" = "alpha-1-B glycoprotein",
            "featureType" = NA_character_,
            "geneId" = 1L,
            "geneName" = "A1BG",
            "geneSynonyms" = CharacterList(c(
                "A1B", "ABG", "GAB", "HYST2477"
            )),
            "mapLocation" = "19q13.43",
            "modificationDate" = 20210129,  # nolint
            "nomenclatureStatus" = "O",
            "otherDesignations" = CharacterList(c(
                "alpha-1B-glycoprotein",
                "epididymis secretory sperm binding protein Li 163pA",
                "HEL-S-163pA"
            )),
            "typeOfGene" = "protein-coding",
            "xTaxId" = 9606,  # nolint
            row.names = "a1bg_1"
        )
    )
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
            y = metadata(object)[["retiredGenes"]]
        )
    )
})
