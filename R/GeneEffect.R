## FIXME Need to pick recommended methods here by default.

# FIXME Need to update R code to use:
# - transpose_assays
# - default scoring method and project



#' Gene effect in cancer cell lines
#'
#' @section Assays:
#'
#' - `effect`: **Chronos or CERES data** with principle components strongly
#'   related to known batch effects removed, then shifted and scaled per cell
#'   line so the median nonessential KO effect is 0 and the median essential KO
#'   effect is -1.
#' - `probability`: **Probability** that knocking out the gene has a real
#'   depletion effect using `gene_effect`.
#'
#' @export
#' @note Updated 2021-07-07.
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
#' @return `GeneEffect`.
#'
#' @examples
#' object <- GeneEffect()
#' print(object)
GeneEffect <-  # nolint
    function(
        dataset,
        ## FIXME Set this to "default" and pick recommended for the dataset.
        project = c(
            "achilles+score",  # CRISPR combined
            "achilles+drive+marcotte",  # RNAi combined
            "achilles",
            "score",
            "drive"
        ),
        ## FIXME Set this to "default" and pick recommended for the dataset.
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
        ## FIXME Should be able to take this out once we switch to "default"
        ## approach instead.
        if (
            isTRUE(grepl(pattern = "demeter2_", x = dataset)) &&
            !identical(scoringMethod, "demeter2")
        ) {
            scoringMethod <- "demeter2"
            alertInfo(sprintf(
                "Setting {.var %s} to {.var %s}.",
                "scoringMethod", scoringMethod
            ))
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
        } else if (isTRUE(grepl(pattern = "project_score", x = dataset))) {
            libraryType <- "crispr"
            ceFile <- "common_essentials.csv"
            effectFile <- "gene_effect.csv"
            probFile <- "gene_dependency.csv"
        } else if (isTRUE(grepl(pattern = "^demeter2_", x = dataset))) {
            libraryType <- "rnai"
            effectFile <- paste0("d2_", project, "_gene_dep_scores.csv")
        }
        assays <- list(
            "effect" = .importDataFile(
                dataset = dataset,
                keys = keys,
                fileName = effectFile,
                rownamesCol = 1L,
                return = "matrix"
            )
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
        metadata <- list(
            "libraryType" = libraryType,
            "project" = project,
            "scoringMethod" = scoringMethod
        )
        if (identical(libraryType, "crispr")) {
            metadata <- append(
                x = metadata,
                values = list(
                    "commonEssentials" =
                        .importGeneDataFile(
                            dataset = dataset,
                            keys = keys,
                            fileName = ceFile
                        )
                )
            )
            ## NOTE Not supported for Sanger Project Score 2021.
            if (isSubset(
                x = "controls",
                y = names(.datasets[[project]])
            )) {
                metadata <- append(
                    x = metadata,
                    values = list(
                        "controlCommonEssentials" =
                            .importControlCommonEssentials(dataset = dataset),
                        "controlNonessentials" =
                            .importControlNonessentials(dataset = dataset)
                    )
                )
            }
        }
        ## FIXME This is failing for "sanger_project_score_2021_05" due to
        ## subset mismatch -- too long?
        .makeSummarizedExperiment(
            dataset = dataset,
            assays = assays,
            transposeAssays = switch(
                EXPR = libraryType,
                "crispr" = TRUE,
                "rnai" = FALSE
            ),
            metadata = metadata,
            class = "GeneEffect"
        )
    }

formals(GeneEffect)[["dataset"]] <- .formalsList[["dataset"]]
