## Updated 2021-02-24.
.rowDataFromEntrez <- function(assays) {
    assert(is.list(assays))
    retired <- NULL
    ## Extract the NCBI Entrez identifiers from the row names.
    match <- str_match(
        string = rownames(assays[[1L]]),
        pattern = "^(.+)_([0-9]+)$"
    )
    entrezIds <- as.integer(match[, 3L, drop = TRUE])
    assert(!any(is.na(entrezIds)))
    rowData <- EntrezGeneInfo(
        organism = "Homo sapiens",
        taxonomicGroup = "Mammalia"
    )
    assert(
        is(rowData, "EntrezGeneInfo"),
        isSubset(c("geneId", "geneName"), colnames(rowData))
    )
    rowData <- as(rowData, "DataFrame")
    ## Retired NCBI Entrez gene identifiers will return NA here.
    idx <- match(
        x = entrezIds,
        table = as.integer(rowData[["geneId"]])
    )
    keep <- !is.na(idx)
    ## Inform the user regarding any retired gene identifiers.
    if (!all(keep)) {
        retired <- sort(match[!keep, 1L, drop = TRUE])
        alertWarning(sprintf(
            "%d retired NCBI Entrez %s in data set: %s.",
            length(retired),
            ngettext(
                n = length(retired),
                msg1 = "identifier",
                msg2 = "identifiers"
            ),
            toString(retired, width = 200L)
        ))
    }
    assays <- lapply(
        X = assays,
        keep = keep,
        FUN = function(x, keep) {
            x[keep, , drop = FALSE]
        }
    )
    match <- str_match(
        string = rownames(assays[[1L]]),
        pattern = "^(.+)_([0-9]+)$"
    )
    entrezIds <- as.integer(match[, 3L, drop = TRUE])
    assert(!any(is.na(entrezIds)))
    idx <- match(
        x = entrezIds,
        table = as.integer(rowData[["geneId"]])
    )
    assert(!any(is.na(idx)))
    rowData <- rowData[idx, , drop = FALSE]
    rownames(rowData) <- match[, 1L, drop = TRUE]
    list(
        "assays" = assays,
        "retired" = retired,
        "rowData" = rowData
    )
}
