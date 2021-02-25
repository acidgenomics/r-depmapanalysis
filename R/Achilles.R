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
#' @note Updated 2021-02-25.
#'
#' @inheritParams params
#'
#' @return `Achilles`.
#'
#' @examples
#' object <- Achilles()
#' print(object)
Achilles <-  # nolint
    function(
        release = NULL,
        rowData = TRUE,
        colData = TRUE
    ) {
        ## e.g. "depmap_public_21q1".
        release <- .matchDepMapRelease(release)
        assert(
            isString(release),
            isFlag(rowData),
            isFlag(colData)
        )
        ## CSV formatting: genes in columns, cells in rows.
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
            "commonEssentials" =
                .importCommonEssentials(release = release),
            "controlCommonEssentials" =
                .importControlCommonEssentials(release = release),
            "controlNonessentials" =
                .importControlNonessentials(release = release),
            "retired" = retired,
            "release" = release
        )
        .makeSummarizedExperiment(
            assays = assays,
            rowData = rowData,
            colData = colData,
            metadata = metadata,
            class = "Achilles"
        )
    }

#' @include AllGlobals.R
formals(Achilles)[["release"]] <- .currentDepMapRelease
