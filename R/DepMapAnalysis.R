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
#' object <- DepMapAnalysis()
#' print(object)
DepMapAnalysis <-  # nolint
    function(
        dataset,
        project = c(
            "combined",
            "achilles",
            "drive"
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
        ## Handle DEMETER2 scoring edge case for RNAi dataset gracefully.
        if (isTRUE(grepl(pattern = "demeter2_", x = dataset))) {
            scoringMethod <- "demeter2"
        }
        keys <- c(project, scoringMethod)
        if (isTRUE(grepl(pattern = "^depmap_", x = dataset))) {
            libraryType <- "crispr"
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
        } else if (isTRUE(grepl(pattern = "^demeter2_", x = dataset))) {
            libraryType <- "rnai"
            effectFile <- paste0("d2_", project, "_gene_dep_scores.csv")
        }
        ## CSV formatting: genes in columns, cells in rows.
        assays <- list()
        assays[["effect"]] <- .importDataFile(
            dataset = dataset,
            keys = keys,
            fileName = effectFile,
            rownamesCol = 1L,
            return = "matrix"
        )
        if (identical(libraryType, "crispr")) {
            assays[["probability"]] <- .importDataFile(
                dataset = dataset,
                keys = keys,
                fileName = probFile,
                rownamesCol = 1L,
                return = "matrix"
            )
        }
        ## Cells in columns, genes in rows.
        ## Don't need to transpose for DEMETER2 RNAi dataset.
        if (identical(libraryType, "crispr")) {
            assays <- lapply(X = assays, FUN = t)
        }
        ## Cell line metadata.
        missingCells <- NULL
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
        metadata <- list(
            "dataset" = dataset,
            "libraryType" = libraryType,
            "missingCells" = missingCells,
            "project" = project,
            "retiredGenes" = retiredGenes,
            "scoringMethod" = scoringMethod
        )
        if (identical(libraryType, "crispr")) {
            metadata[["commonEssentials"]] <-
                .importGeneDataFile(
                    dataset = dataset,
                    keys = keys,
                    fileName = ceFile
                )
            metadata[["controlCommonEssentials"]] <-
                .importControlCommonEssentials(dataset = dataset)
            metadata[["controlNonessentials"]] <-
                .importControlNonessentials(dataset = dataset)
        }
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
