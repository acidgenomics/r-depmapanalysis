test_that("All datasets", {
    datasets <- eval(formals(GeneEffect)[["dataset"]])
    for (dataset in datasets) {
        object <- GeneEffect(dataset = dataset)
        expect_s4_class(object, "GeneEffect")
    }
})

test_that("CRISPR : depmap_public_21q2", {
    dataset <- "depmap_public_21q2"
    object <- GeneEffect(dataset = dataset)
    expect_s4_class(object, "GeneEffect")
    expect_identical(
        object = head(colnames(object), n = 3L),
        expected = c("ACH_000001", "ACH_000004", "ACH_000005")
    )
    expect_identical(
        object = head(rownames(object), n = 3L),
        expected = c("A1BG_1", "A1CF_29974", "A2M_2")
    )
    expect_identical(
        object = assayNames(object),
        expected = c("effect", "probability")
    )
    ## nolint start
    expect_equal(
        object = colData(object)["ACH_000004", ],
        expected = DataFrame(
            "achillesNReplicates" = 2,
            "age" = 30,
            "alias" = NA_character_,
            "cas9Activity" = 52.4,
            "ccleName" = "HEL_HAEMATOPOIETIC_AND_LYMPHOID_TISSUE",
            "cellLineName" = "HEL",
            "cellLineNnmd" = -3.079202, # NOTE causes `identical()` to fail.
            "cosmicid" = 907053,
            "cultureMedium" = "RPMI + 10% FBS",
            "cultureType" = "Suspension",
            "depMapId" = "ACH-000004",
            "depmapPublicComments" = NA_character_,
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
            row.names = "ACH_000004"
        )
    )
    ## nolint end
    rowData <- rowData(object)["A1BG_1", , drop = FALSE]
    rowData <- decode(rowData)
    metadata(rowData) <- list()
    attr(class(rowData[["dbXrefs"]]), "package") <- "IRanges"
    attr(class(rowData[["geneSynonyms"]]), "package") <- "IRanges"
    attr(class(rowData[["otherDesignations"]]), "package") <- "IRanges"
    expect_equal(
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
            "modificationDate" = 20210518, # nolint
            "nomenclatureStatus" = "O",
            "otherDesignations" = CharacterList(c(
                "alpha-1B-glycoprotein",
                "epididymis secretory sperm binding protein Li 163pA",
                "HEL-S-163pA"
            )),
            "typeOfGene" = "protein-coding",
            "xTaxId" = 9606, # nolint
            row.names = "A1BG_1"
        )
    )
    expect_identical(
        object = metadata(object)[["dataset"]],
        expected = dataset
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
                "MICALCL_84953",
                "PALM2_114299"
            ),
            y = metadata(object)[["retiredGenes"]]
        )
    )
})

test_that("RNAi : demeter2_data_v6", {
    dataset <- "demeter2_data_v6"
    object <- GeneEffect(dataset = dataset)
    expect_s4_class(object, "GeneEffect")
    expect_identical(
        object = head(colnames(object), n = 3L),
        expected = c(
            "A101D_SKIN",
            "A1207_CENTRAL_NERVOUS_SYSTEM",
            "A172_CENTRAL_NERVOUS_SYSTEM"
        )
    )
    expect_identical(
        object = head(rownames(object), n = 3L),
        expected = c("A1BG_1", "A1CF_29974", "A2M_2")
    )
    expect_identical(
        object = assayNames(object),
        expected = "effect"
    )
    expect_identical(
        object = colData(object)["A101D_SKIN", ],
        expected = DataFrame(
            "ccleId" = "A101D_SKIN",
            "cellLineName" = "A101D",
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
            row.names = "A101D_SKIN"
        )
    )
    rowData <- decode(rowData(object)["A1BG_1", c("geneId", "geneName")])
    metadata(rowData) <- list()
    expect_identical(
        object = rowData,
        expected = DataFrame(
            "geneId" = 1L,
            "geneName" = "A1BG",
            row.names = "A1BG_1"
        )
    )
    expect_identical(
        object = metadata(object)[["dataset"]],
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
            y = metadata(object)[["retiredGenes"]]
        )
    )
})
