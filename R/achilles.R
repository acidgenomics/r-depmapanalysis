#' Import Project Achilles CRISPR gene effect data
#'
#' @section Assays:
#'
#' - `effect`: **CERES data** with principle components strongly related to
#'   known batch effects removed, then shifted and scaled per cell line so the
#'   median nonessential KO effect is 0 and the median essential KO effect is
#'   -1.
#' - `probability`: **Probability** that knocking out the gene has a real
#'   depletion effect using `gene_effect`.
#'
#' @export
#' @note Updated 2020-10-01.
#'
#' @inheritParams params
#'
#' @return `Achilles`.
#'
#' @examples
#' object <- Achilles()
#' dim(object)
Achilles <-  # nolint
    function(
        release = NULL,
        rowRanges = TRUE,
        colData = TRUE
    ) {
        if (is.null(release)) release <- .currentRelease
        assert(
            isString(release),
            isFlag(rowRanges),
            isFlag(colData)
        )
        ## CSV formatting: genes in columns, cells in rows. Transposing here to
        ## match DEMETER2 formatting, and standard SummarizedExperiment
        ## conventions for NGS data.
        assays <- list(
            "effect" = .importDataFile(
                fileName = "achilles_gene_effect.csv",
                release = release,
                rownamesCol = 1L,
                return = "matrix"
            ),
            "probability" = .importDataFile(
                fileName = "achilles_gene_dependency.csv",
                release = release,
                rownamesCol = 1L,
                return = "matrix"
            )
        )
        assays <- lapply(X = assays, FUN = t)
        if (isTRUE(colData)) {
            colData <- .importCellLineSampleData(release = release)
        } else {
            colData <- NULL
        }
        assert(
            identical(
                x = dimnames(assays[["effect"]]),
                y = dimnames(assays[["probability"]])
            ),
            isSubset(
                x = colnames(assays[[1L]]),
                y = rownames(colData)
            )
        )
        if (isTRUE(rowRanges)) {
            ## FIXME IMPORT FROM INTERNAL MAPPINGS.
        } else {
            rowRanges <- NULL
        }
        metadata <- list(
            release = release,
            version = .version
        )
        args <- list(
            assays = assays,
            rowRanges = rowRanges,
            colData = colData,
            metadata = metadata
        )
        args <- Filter(Negate(is.null), args)
        se <- do.call(what = makeSummarizedExperiment, args = args)
        rownames(se) <- tolower(rownames(se))
        colnames(se) <- tolower(colnames(se))
        new("Achilles", se)
    }
