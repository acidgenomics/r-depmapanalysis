## FIXME Rename this to RNAi???



#' Import DEMETER2 RNAi screen gene effect data
#'
#' @export
#' @note Updated 2021-02-25.
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
            "release" = release,
            "retiredGenes" = retiredGenes
        )
        .makeSummarizedExperiment(
            assays = assays,
            rowData = rowData,
            colData = colData,
            metadata = metadata,
            class = "DEMETER2"
        )
    }
