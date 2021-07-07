.pkgName <- packageName()
.pkgVersion <- packageVersion(.pkgName)



.formalsList <- list(
    "dataset" = names(.datasets),
    "depmapDataset" = grep(
        pattern = "^depmap_",
        x = names(.datasets),
        value = TRUE
    )
)
