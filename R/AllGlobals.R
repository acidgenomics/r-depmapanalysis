.pkgName <- packageName()
.pkgVersion <- packageVersion(.pkgName)

.currentBroadDataset <- "depmap_public_23q4"

.extdataUrl <- pasteUrl(
    "r.acidgenomics.com",
    "extdata",
    tolower(.pkgName),
    protocol = "https"
)
