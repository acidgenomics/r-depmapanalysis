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
        release = NULL,
        rowData = TRUE,
        colData = TRUE
    ) {
        ## e.g. returns "depmap_public_21q1".
        release <- .matchDepMapRelease(release)
        assert(
            isString(release),
            isFlag(rowData),
            isFlag(colData)
        )
        assays <- list(
            "copyNumber" = .importDataFile(
                fileName = "ccle_gene_cn.csv",
                release = release,
                rownamesCol = 1L,
                return = "matrix"
            )
        )
        ## Cells in columns, genes in rows.
        assays <- lapply(X = assays, FUN = t)
        ## Cell line metadata.
        if (isTRUE(colData)) {
            colData <- .importCellLineSampleData(release = release)
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
                    y = c("assays", "retired", "rowData")
                )
            )
            assays <- l[["assays"]]
            retired <- l[["retired"]]
            rowData <- l[["rowData"]]
        } else {
            retired <- NULL
            rowData <- NULL
        }
        metadata <- list(
            "retired" = retired,
            "release" = release
        )
        .makeSummarizedExperiment(
            assays = assays,
            rowData = rowData,
            colData = colData,
            metadata = metadata,
            class = "CCLECopyNumberData"
        )
    }

formals(CCLECopyNumberData)[["release"]] <- .currentDepMapRelease



#' Import CCLE expression data
#'
#' @export
#' @note Updated 2021-02-24.
#'
#' @inheritParams params
#'
#' @return `CCLEExpressionData`.
#'
#' @examples
#' object <- CCLEExpressionData()
#' dim(object)
CCLEExpressionData <-  # nolint
    function(release = NULL) {
        df <- .importDataFile(
            fileName = "ccle_expression.csv",
            release = .matchDepMapRelease(release),
            rownamesCol = 1L
        )
        assert(is(df, "DataFrame"))
        new("CCLEExpressionData", df)
    }

formals(CCLEExpressionData)[["release"]] <- .currentDepMapRelease



#' Import CCLE mutation data
#'
#' @export
#' @note Updated 2021-02-24.
#'
#' @inheritParams params
#'
#' @return `CCLEMutationData`.
#'
#' @examples
#' object <- CCLEMutationData()
#' dim(object)
CCLEMutationData <-  # nolint
    function(release = NULL) {
        df <- .importDataFile(
            fileName = "ccle_mutations.csv",
            format = "tsv",
            release = .matchDepMapRelease(release),
            rownamesCol = NULL
        )
        assert(is(df, "DataFrame"))
        new("CCLEMutationData", df)
    }

formals(CCLEMutationData)[["release"]] <- .currentDepMapRelease
