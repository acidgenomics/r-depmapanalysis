.pkgName <- packageName()
.pkgVersion <- packageVersion(.pkgName)



.datasetNames <- c(
    "depmap_public_21q2",
    "depmap_public_21q1",
    "depmap_public_20q4v2",
    "depmap_public_20q3",
    "depmap_public_20q2",
    "depmap_public_20q1",
    "sanger_project_score_2021_05",
    "sanger_project_score_2019_08",
    "demeter2_data_v6"
)



.formalsList <- list(
    "dataset" = .datasetNames,
    "depmapDataset" = grep(
        pattern = "^depmap_",
        x = .datasetNames,
        value = TRUE
    )
)
