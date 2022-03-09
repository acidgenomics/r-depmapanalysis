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
#' @note Updated 2022-03-09.
#'
#' @inheritParams params
#'
#' @return `GeneEffect`.
#'
#' @examples
#' object <- GeneEffect()
#' print(object)
GeneEffect <-  # nolint
    function(dataset) {
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
        libraryType <- yaml[["screen"]][["library_type"]]
        project <- yaml[["screen"]][["defaults"]][["project"]]
        releaseDate <- yaml[["metadata"]][["date"]]
        scoringMethod <- yaml[["screen"]][["defaults"]][["scoring_method"]]
        transposeAssays <- yaml[["screen"]][["transpose_assays"]]
        assert(
            isString(libraryType),
            isString(project),
            isString(releaseDate),
            isString(scoringMethod),
            isFlag(transposeAssays)
        )
        h1(sprintf("Preparing {.var %s} gene effect dataset.", dataset))
        dl(c(
            "libraryType" = libraryType,
            "project" = project,
            "scoringMethod" = scoringMethod,
            "releaseDate" = releaseDate
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
        se <- .makeSummarizedExperiment(
            dataset = dataset,
            assays = assays,
            transposeAssays = transposeAssays,
            metadata = metadata,
            class = "GeneEffect"
        )
        return(se)
    }

formals(GeneEffect)[["dataset"]] <- .formalsList[["dataset"]]
