## FIXME Rename this to CRISPR...

## FIXME Need to add support for scoringMethod
## ceres or chronos




#' Import cancer cell line dependency map data
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
#' @note Updated 2021-06-09.
#'
#' @inheritParams params
#' @param project `character(1)`.
#'   Project name.
#'   Defaults to a combination of multiple projects (`"combined"`):
#'   - CRISPR: Broad DepMap Public, Sanger Project Score
#'     (e.g. for `depmap_public_21q2`).
#'   - RNAi: Achilles, DRIVE, Marcotte
#'     (e.g. for `demeter2_data_v6`).
#'
#' @param scoringMethod `character(1)`.
#'
#' @return `DepMapAnalysis`.
#'
#' @examples
#' object <- Achilles()
#' print(object)
DepMapAnalysis <-  # nolint
    function(
        dataset,
        project = c(
            "combined",
            "achilles"
        ),
        scoringMethod = c(
            "chronos",
            "ceres",
            "demeter2"
        ),
        rowData = TRUE,
        colData = TRUE
    ) {
        assert(
            isFlag(rowData),
            isFlag(colData)
        )
        dataset <- match.arg(dataset)
        project <- match.arg(project)
        ## FIXME Need to add support for this.
        scoringMethod <- match.arg(scoringMethod)
        ## CSV formatting: genes in columns, cells in rows.
        ## FIXME Need to work the variable input here...
        assays <- list(
            "effect" = .importDataFile(
                fileName = "achilles_gene_effect.csv",
                dataset = dataset,
                rownamesCol = 1L,
                return = "matrix"
            ),
            "probability" = .importDataFile(
                fileName = "achilles_gene_dependency.csv",
                dataset = dataset,
                rownamesCol = 1L,
                return = "matrix"
            )
        )
        ## Cells in columns, genes in rows.
        assays <- lapply(X = assays, FUN = t)
        ## Cell line metadata.
        if (isTRUE(colData)) {
            colData <- .importCellLineSampleData(dataset = dataset)
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
            "commonEssentials" =
                .importCommonEssentials(dataset = dataset),
            "controlCommonEssentials" =
                .importControlCommonEssentials(dataset = dataset),
            "controlNonessentials" =
                .importControlNonessentials(dataset = dataset),
            "dataset" = dataset,
            "retiredGenes" = retiredGenes
        )
        .makeSummarizedExperiment(
            assays = assays,
            rowData = rowData,
            colData = colData,
            metadata = metadata,
            class = "DepMapAnalysis"
        )
    }

#' @include AllGlobals.R
formals(DepMapAnalysis)[["dataset"]] <- .formalsList[["dataset"]]
