## Updated 2021-06-09.
.makeCcle <- function(
    class,
    assayName,
    fileName,
    dataset
) {
    assert(
        isString(class),
        isString(assayName),
        isString(fileName),
        isString(dataset)
    )
    assays <- list(
        .importDataFile(
            dataset = dataset,
            keys = "ccle",
            fileName = fileName,
            rownamesCol = 1L,
            return = "matrix"
        )
    )
    names(assays) <- assayName
    ## Cells in columns, genes in rows.
    assays <- lapply(X = assays, FUN = t)
    ## Cell line metadata.
    missingCells <- NULL
    colData <- .importCellLineSampleData(dataset = dataset)
    assert(areIntersectingSets(colnames(assays[[1L]]), rownames(colData)))
    if (!isSubset(colnames(assays[[1L]]), rownames(colData))) {
        missingCells <- setdiff(colnames(assays[[1L]]), rownames(colData))
        alertWarning(sprintf(
            "%d missing cell %s in {.var %s}: %s.",
            length(missingCells),
            ngettext(
                n = length(missingCells),
                msg1 = "line",
                msg2 = "lines"
            ),
            "colData",
            toString(missingCells, width = 100L)
        ))
        colData[missingCells, ] <- NA
        assert(isSubset(colnames(assays[[1L]]), rownames(colData)))
    }
    ## Gene metadata.
    l <- .rowDataFromEntrez(assays = assays)
    assert(
        is.list(l),
        identical(
            x = names(l),
            y = c("assays", "retiredGenes", "rowData")
        )
    )
    assays <- l[["assays"]]
    retiredGenes <- l[["retiredGenes"]]
    rowData <- l[["rowData"]]
    metadata <- list(
        "dataset" = dataset,
        "missingCells" = missingCells,
        "packageName" = .pkgName,
        "packageVersion" = .pkgVersion,
        "retiredGenes" = retiredGenes
    )
    .makeSummarizedExperiment(
        assays = assays,
        rowData = rowData,
        colData = colData,
        metadata = metadata,
        class = class
    )
}
