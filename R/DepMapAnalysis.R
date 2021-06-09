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
#' @param scoringMethod `character(1)`.
#'   Scoring method name to use.
#'   - CRISPR: `"chronos"` (as of 2021 Q1) or `"ceres"`
#'     (e.g. `depmap_public_21q2` dataset).
#'   - RNAi: `"demeter2"`
#'     (e.g. `demeter2_data_v6` dataset).
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
        )
    ) {
        dataset <- match.arg(dataset)
        project <- match.arg(project)
        scoringMethod <- match.arg(scoringMethod)
        keys <- c(project, scoringMethod)
        ## FIXME This needs to add support for DEMETER2 RNAi dataset.
        if (isTRUE(grepl(pattern = "^depmap_", x = dataset))) {
            switch(
                EXPR = project,
                "combined" = {
                    switch(
                        EXPR = scoringMethod,
                        "chronos" = {
                            ceFile <- "crispr_common_essentials_chronos.csv"
                            effectFile <- "crispr_gene_effect_chronos.csv"
                            probFile <- "crispr_gene_dependency_chronos.csv"
                        },
                        "ceres" = {
                            ceFile <- "crispr_common_essentials.csv"
                            effectFile <- "crispr_gene_effect.csv"
                            probFile <- "crispr_gene_dependency.csv"
                        }
                    )
                },
                "achilles" = {
                    switch(
                        EXPR = scoringMethod,
                        "chronos" = {
                            ceFile <- "achilles_common_essentials_chronos.csv"
                            effectFile <- "achilles_gene_effect_chronos.csv"
                            probFile <- "achilles_gene_dependency_chronos.csv"
                        },
                        "ceres" = {
                            ceFile <- "achilles_common_essentials.csv"
                            effectFile <- "achilles_gene_effect.csv"
                            probFile <- "achilles_gene_dependency.csv"
                        }
                    )
                }
            )
        }
        ## CSV formatting: genes in columns, cells in rows.
        ## FIXME Need to rethink the naming here...
        assays <- list(
            "effect" = .importDataFile(
                dataset = dataset,
                keys = keys,
                fileName = effectFile,
                rownamesCol = 1L,
                return = "matrix"
            ),
            "probability" = .importDataFile(
                dataset = dataset,
                keys = keys,
                fileName = probFile,
                rownamesCol = 1L,
                return = "matrix"
            )
        )
        ## Cells in columns, genes in rows.
        assays <- lapply(X = assays, FUN = t)
        ## Cell line metadata.
        colData <- .importCellLineSampleData(dataset = dataset)
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

        ## FIXME Need to support retiredCells here too...
        ## FIXME Failing here due to ACH_002315 not in sample_info.csv.
        ## Is this due to combined with Sanger?
        ## Removed in 2021-02-01.
        ## Need to handle this edge case better?
        ## https://depmap.org/portal/announcements/

        metadata <- list(
            "commonEssentials" =
                .importGeneDataFile(
                    dataset = dataset,
                    keys = keys,
                    fileName = ceFile
                ),
            "controlCommonEssentials" =
                .importControlCommonEssentials(dataset = dataset),
            "controlNonessentials" =
                .importControlNonessentials(dataset = dataset),
            "dataset" = dataset,
            "project" = project,
            "retiredGenes" = retiredGenes,
            "scoringMethod" = scoringMethod
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
