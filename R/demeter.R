#' Import DEMETER2 RNAi screen gene effect data
#'
#' @export
#' @note Updated 2020-10-01.
#'
#' @return `DEMETER2`.
#'
#' @examples
#' object <- DEMETER2()
#' dim(object)
DEMETER2 <-  # nolint
    function() {
        ## CSV formatting: cells in columns, genes in rows.
        assays <- list(
            "effect" = .importDataFile(
                fileName = "d2_combined_gene_dep_scores.csv",
                release = "demeter2_data_v6",
                rownamesCol = 1L,
                return = "matrix"
            )
        )
        assert(is.matrix(mat))
        ## Need to map this to cell data in a future release.
        ## > colData <- .importCellLineSampleData(
        ## >     release = .currentDEMETERRelease
        ## > )
        se <- makeSummarizedExperiment(assays = assays)
        new("DEMETER2", se)
    }
