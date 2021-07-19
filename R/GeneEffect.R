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
#' @note Updated 2021-07-08.
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
        project = "default",
        scoringMethod = "default"
    ) {
        data(list = "datasets", package = .pkgName, envir = environment())
        assert(is.list(datasets))
        dataset <- match.arg(dataset)
        assert(
            isSubset(dataset, names(datasets)),
            isSubset(
                x = "screen",
                y = names(datasets[[dataset]])
            ),
            msg = sprintf("Invalid '%s': '%s'.", "dataset", dataset)
        )
        yaml <- datasets[[dataset]]
        assert(
            isSubset(
                x = "defaults",
                y = names(yaml[["screen"]])
            )
        )
        defaults <- yaml[["screen"]][["defaults"]]
        if (identical(project, "default")) {
            project <- defaults[["project"]]
        }
        if (identical(scoringMethod, "default")) {
            scoringMethod <- defaults[["scoring_method"]]
        }
        assert(
            isString(project),
            isSubset(
                x = project,
                y = names(yaml[["screen"]][["project"]])
            ),
            msg = sprintf(
                "Invalid '%s': '%s'.",
                "project", project
            )
        )
        assert(
            isString(scoringMethod),
            isSubset(
                x = scoringMethod,
                y = names(yaml[["screen"]][["project"]][[
                    project]][["scoring_method"]])
            ),
            msg = sprintf(
                "Invalid '%s': '%s'.",
                "scoringMethod", scoringMethod
            )
        )
        libraryType <- yaml[["screen"]][["library_type"]]
        releaseDate <- yaml[["metadata"]][["date"]]
        transposeAssays <- yaml[["screen"]][["transpose_assays"]]
        assert(
            isString(libraryType),
            isString(releaseDate),
            isFlag(transposeAssays)
        )
        h1(sprintf("Preparing {.var %s} gene effect dataset.", dataset))
        dl(c(
            "libraryType" = libraryType,
            "project" = project,
            "releaseDate" = releaseDate,
            "scoringMethod" = scoringMethod
        ))
        urls <- list(
            "assays" = list(
                "effect" =
                    yaml[["screen"]][[
                        "project"]][[project]][[
                            "scoring_method"]][[scoringMethod]][[
                                "gene_effect"]][["url"]],
                "probability" =
                    yaml[["screen"]][[
                        "project"]][[project]][[
                            "scoring_method"]][[scoringMethod]][[
                                "gene_dependency"]][["url"]]
            ),
            "metadata" = list(
                "commonEssentials" =
                    yaml[["screen"]][[
                        "project"]][[project]][[
                            "scoring_method"]][[scoringMethod]][[
                                "common_essentials"]][["url"]],
                "controlCommonEssentials" =
                    yaml[["screen"]][["controls"]][[
                        "common_essentials"]][["url"]],
                "controlNonessentials" =
                    yaml[["screen"]][["controls"]][[
                        "nonessentials"]][["url"]]
            )
        )
        ## Assays --------------------------------------------------------------
        urls[["assays"]] <- Filter(
            f = Negate(is.null),
            x = urls[["assays"]]
        )
        assays <- lapply(
            X = urls[["assays"]],
            FUN = .importDataFile,
            rownamesCol = 1L,
            return = "matrix"
        )
        assert(identical(names(urls[["assays"]]), names(assays)))
        ## Metadata ------------------------------------------------------------
        urls[["metadata"]] <- Filter(
            f = Negate(is.null),
            x = urls[["metadata"]]
        )
        metadata <- lapply(
            X = urls[["metadata"]],
            FUN = .importGeneDataFile
        )
        assert(identical(names(urls[["metadata"]]), names(metadata)))
        metadata <- append(
            x = metadata,
            values = list(
                ## "dataset" and "yaml" are added in call below.
                "libraryType" = libraryType,
                "project" = project,
                "releaseDate" = releaseDate,
                "scoringMethod" = scoringMethod
            )
        )
        .makeSummarizedExperiment(
            dataset = dataset,
            assays = assays,
            transposeAssays = transposeAssays,
            metadata = metadata,
            class = "GeneEffect"
        )
    }

formals(GeneEffect)[["dataset"]] <- .formalsList[["dataset"]]
