.pkgName <- packageName()
.pkgVersion <- packageVersion(.pkgName)

.datasetNames <- c(
    "depmap_public_23q2",
    ## > "depmap_public_22q4",
    ## > "depmap_public_22q2",
    ## > "depmap_public_22q1",
    ## > "depmap_public_21q4",
    ## > "depmap_public_21q3",
    ## > "depmap_public_21q2",
    ## > "depmap_public_21q1",
    ## > "depmap_public_20q4v2",
    ## > "depmap_public_20q3",
    ## > "depmap_public_20q2",
    ## > "depmap_public_20q1",
    "demeter2_data_v6"
)

.extdataUrl <- pasteURL(
    "r.acidgenomics.com",
    "extdata",
    tolower(.pkgName),
    protocol = "https"
)

.formalsList <- list(
    "dataset" = .datasetNames,
    "depmapDataset" = grep(
        pattern = "^depmap_",
        x = .datasetNames,
        value = TRUE
    )
)
