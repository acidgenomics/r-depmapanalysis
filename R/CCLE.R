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
            fileName = fileName,
            dataset = dataset,
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



#' Import CCLE copy number data
#'
#' @export
#' @note Updated 2021-02-25.
#'
#' @inheritParams params
#'
#' @return `CCLECopyNumberData`.
#'
#' @examples
#' object <- CCLECopyNumberData()
#' print(object)
CCLECopyNumberData <-  # nolint
    function(
        dataset = NULL,
        rowData = TRUE,
        colData = TRUE
    ) {
        .makeCcle(
            class = "CCLECopyNumberData",
            assayName = "copyNumber",
            fileName = "ccle_gene_cn.csv",
            dataset = dataset,
            rowData = rowData,
            colData = colData
        )
    }

formals(CCLECopyNumberData)[["dataset"]] <- .formalsList[["dataset"]]



#' Import CCLE expression data
#'
#' @export
#' @note Updated 2021-02-25.
#'
#' @inheritParams params
#'
#' @return `CCLEExpressionData`.
#'
#' @examples
#' object <- CCLEExpressionData()
#' dim(object)
CCLEExpressionData <-  # nolint
    function(
        dataset = NULL,
        rowData = TRUE,
        colData = TRUE
    ) {
        .makeCcle(
            class = "CCLEExpressionData",
            assayName = "expression",
            fileName = "ccle_expression.csv",
            dataset = dataset,
            rowData = rowData,
            colData = colData
        )
    }

formals(CCLEExpressionData)[["dataset"]] <- .formalsList[["dataset"]]



#' Import CCLE mutation data
#'
#' @export
#' @note Updated 2021-02-25.
#'
#' @inheritParams params
#'
#' @return `CCLEMutationData`.
#'
#' @examples
#' object <- CCLEMutationData()
#' dim(object)
CCLEMutationData <-  # nolint
    function(dataset = NULL) {
        df <- .importDataFile(
            fileName = "ccle_mutations.csv",
            format = "csv",
            dataset = dataset,
            rownamesCol = NULL
        )
        assert(is(df, "DataFrame"))
        colnames(df) <- camelCase(colnames(df), strict = TRUE)
        metadata(df) <- list(
            "packageVersion" = .pkgVersion,
            "dataset" = dataset
        )
        new("CCLEMutationData", df)
    }

formals(CCLEMutationData)[["dataset"]] <- .formalsList[["dataset"]]
