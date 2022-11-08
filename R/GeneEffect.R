#' Gene effect in cancer cell lines
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
#' @note Updated 2022-11-08.
#'
#' @inheritParams params
#'
#' @return `GeneEffect`.
#'
#' @examples
#' ## CRISPR (default).
#' object <- GeneEffect()
#' print(object)
#'
#' ## RNAi.
#' ## > object <- GeneEffect(dataset = "demeter2_data_v6")
GeneEffect <- # nolint
    function(dataset) {
        dataset <- match.arg(dataset)
        assert(isSubset(dataset, names(datasets)))
        json <- datasets[[dataset]]
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
            isFlag(dict[["transposeAssays"]])
        )
        h1(sprintf("{.cls %s}: {.var %s}", "GeneEffect", dataset))
        dl(c(
            "libraryType" = dict[["libraryType"]],
            "scoringMethod" = dict[["scoringMethod"]],
            "releaseDate" = dict[["releaseDate"]]
        ))
        urls <- list(
            "assays" = list(
                "effect" = json[["files"]][["screen"]][[
                    "gene_effect"]][["url"]],
                "probability" = json[["files"]][["screen"]][[
                    "gene_dependency"]][["url"]]
            ),
            "metadata" = list(
                "commonEssentials" = json[["files"]][["screen"]][[
                    "common_essentials"]][["url"]],
                "controlCommonEssentials" = json[["files"]][["screen"]][[
                    "positive_control_genes"]][["url"]],
                "controlNonessentials" = json[["files"]][["screen"]][[
                    "negative_control_genes"]][["url"]]
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
                "libraryType" = dict[["libraryType"]],
                "releaseDate" = dict[["releaseDate"]],
                "scoringMethod" = dict[["scoringMethod"]]
            )
        )
        se <- .makeSummarizedExperiment(
            dataset = dataset,
            assays = assays,
            transposeAssays = dict[["transposeAssays"]],
            metadata = metadata,
            class = "GeneEffect"
        )
        se
    }

formals(GeneEffect)[["dataset"]] <- # nolint
    .formalsList[["dataset"]]
