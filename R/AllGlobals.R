.pkgName <- packageName()
.pkgVersion <- packageVersion(.pkgName)

.currentDataset <- "depmap_public_23q2"

.extdataUrl <- pasteURL(
    "r.acidgenomics.com",
    "extdata",
    tolower(.pkgName),
    protocol = "https"
)
