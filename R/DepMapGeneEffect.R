#' DepMap gene effect in cancer cell lines
#'
#' @section Assays:
#'
#' - `effect`: **Chronos or CERES data** with principle components strongly
#' related to known batch effects removed, then shifted and scaled per cell
#' line so the median nonessential KO effect is 0 and the median essential KO
#' effect is -1.
#' - `probability`: **Probability** that knocking out the gene has a real
#' depletion effect using `gene_effect`.
#'
#' @export
#' @note Updated 2023-03-08.
#'
#' @inheritParams params
#'
#' @return `DepMapGeneEffect`.
#'
#' @examples
#' ## CRISPR (default).
#' object <- DepMapGeneEffect()
#' print(object)
#'
#' ## RNAi (DEMETER2).
#' ## > object <- DepMapGeneEffect(dataset = "demeter2_data_v6")
DepMapGeneEffect <- # nolint
    function(dataset) {
        dataset <- match.arg(dataset)
        assert(isSubset(dataset, names(datasets)))
        json <- datasets[[dataset]]
        urls <- unlist(x = json[["files"]], recursive = FALSE, use.names = TRUE)
        dict <- list(
            "libraryType" = json[["metadata"]][["library_type"]],
            "releaseDate" = json[["metadata"]][["date"]],
            "scoringMethod" = json[["metadata"]][["scoring_method"]],
            "transposeAssays" = json[["metadata"]][["transpose_assays"]]
        )
        assert(
            isString(dict[["libraryType"]]),
            isString(dict[["releaseDate"]]),
            isString(dict[["scoringMethod"]]),
            isFlag(dict[["transposeAssays"]]),
            allAreURLs(urls)
        )
        h1(sprintf("{.cls %s}: {.var %s}", "DepMapGeneEffect", dataset))
        dl(c(
            "libraryType" = dict[["libraryType"]],
            "scoringMethod" = dict[["scoringMethod"]],
            "releaseDate" = dict[["releaseDate"]]
        ))
        switch(
            EXPR = dataset,
            "depmap_public_22q4" = {
                urls <- list(
                    "assays" = list(
                        "effect" =
                            urls[["CRISPRGeneEffect.csv"]],
                        "probability" =
                            urls[["CRISPRGeneDependency.csv"]]
                    ),
                    "metadata" = list(
                        "commonEssentials" =
                            urls[["CRISPRInferredCommonEssentials.csv"]],
                        "controlCommonEssentials" =
                            urls[["AchillesCommonEssentialControls.csv"]],
                        "controlNonessentials" =
                            urls[["AchillesNonessentialControls.csv"]]
                    )
                )
            },
            "depmap_public_22q2" = {
                urls <- list(
                    "assays" = list(
                        "effect" =
                            urls[["CRISPR_gene_effect.csv"]],
                        "probability" =
                            urls[["CRISPR_gene_dependency.csv"]]
                    ),
                    "metadata" = list(
                        "commonEssentials" =
                            urls[["CRISPR_common_essentials.csv"]],
                        "controlCommonEssentials" =
                            urls[["common_essentials.csv"]],
                        "controlNonessentials" =
                            urls[["nonessentials.csv"]]
                    )
                )
            },
            "demeter2_data_v6" = {
                urls <- list(
                    "assays" = list(
                        "effect" =
                            urls[["D2_combined_gene_dep_scores.csv"]],
                        "sd" =
                            urls[["D2_combined_seed_dep_score_SDs.csv"]]
                    ),
                    "metadata" = list(
                        "controlCommonEssentials" =
                            urls[["Hart-pos-controls.csv"]],
                        "controlNonessentials" =
                            urls[["Hart-neg-controls.csv"]]
                    )
                )
            }
        )
        ## Assays --------------------------------------------------------------
        assays <- lapply(
            X = urls[["assays"]],
            FUN = .importDataFile,
            format = "csv",
            rownameCol = 1L,
            colnames = TRUE,
            return = "matrix"
        )
        ## Metadata ------------------------------------------------------------
        metadata <- lapply(
            X = urls[["metadata"]],
            FUN = .importGeneDataFile
        )
        metadata <- append(
            x = metadata,
            values = list(
                "libraryType" = dict[["libraryType"]],
                "releaseDate" = dict[["releaseDate"]],
                "scoringMethod" = dict[["scoringMethod"]]
            )
        )
        ## Return --------------------------------------------------------------
        se <- .makeDepMapSE(
            dataset = dataset,
            assays = assays,
            transposeAssays = dict[["transposeAssays"]],
            metadata = metadata,
            class = "DepMapGeneEffect"
        )
        se
    }

formals(DepMapGeneEffect)[["dataset"]] <- # nolint
    .formalsList[["dataset"]]
