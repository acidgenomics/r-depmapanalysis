## Updated 2021-02-25.
.makeCcle <- function(
    class,
    assayName,
    fileName,
    dataset,
    rowData,
    colData
) {
    assert(
        isString(class),
        isString(assayName),
        isString(fileName),
        isString(dataset),
        isFlag(rowData),
        isFlag(colData)
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
    if (isTRUE(colData)) {
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
    } else {
        colData <- NULL
    }
    ## Gene metadata.
    if (isTRUE(rowData)) {
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
    } else {
        retiredGenes <- NULL
        rowData <- NULL
    }
    metadata <- list(
        "missingCells" = missingCells,
        "retiredGenes" = retiredGenes,
        "dataset" = dataset
    )
    .makeSummarizedExperiment(
        assays = assays,
        rowData = rowData,
        colData = colData,
        metadata = metadata,
        class = class
    )
}
