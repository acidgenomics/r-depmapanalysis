#' Import DEMETER2 RNAi screen gene effect data
#'
#' @export
#' @note Updated 2021-02-24.
#'
#' @inheritParams params
#'
#' @return `DEMETER2`.
#'
#' @examples
#' object <- DEMETER2()
#' dim(object)
DEMETER2 <-  # nolint
    function(rowData = TRUE, colData = TRUE) {
        release <- "demeter2_data_v6"
        assert(
            isFlag(rowData),
            isFlag(colData)
        )
        ## CSV formatting: cells in columns, genes in rows.
        assays <- list(
            "effect" = .importDataFile(
                fileName = "d2_combined_gene_dep_scores.csv",
                release = release,
                rownamesCol = 1L,
                return = "matrix"
            )
        )
        ## Sample (i.e. cell line) metadata.
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
            retired = NULL
            rowData <- NULL
        }
        metadata <- list(
            "packageVersion" = .pkgVersion,
            "release" = release,
            "retired" = retired
        )
        args <- list(
            "assays" = assays,
            "colData" = colData,
            "metadata" = metadata,
            "rowData" = rowData
        )
        args <- Filter(Negate(is.null), args)
        se <- do.call(what = makeSummarizedExperiment, args = args)
        assert(is(se, "SummarizedExperiment"))
        rownames(se) <- tolower(rownames(se))
        colnames(se) <- tolower(colnames(se))
        validObject(se)
        new("DEMETER2", se)
    }
