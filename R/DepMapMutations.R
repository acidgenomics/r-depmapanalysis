#' Import DepMap somatic mutation data
#'
#' @export
#' @note Updated 2023-08-02.
#'
#' @inheritParams params
#'
#' @return `DepMapMutations`.
#'
#' @examples
#' object <- DepMapMutations()
#' dim(object)
DepMapMutations <- # nolint
    function(dataset) {
        dataset <- match.arg(dataset)
        url <- datasets[[dataset]][["files"]][["OmicsSomaticMutations.csv"]]
        assert(isAURL(url))
        df <- .importDataFile(url = url, format = "csv")
        assert(is(df, "DataFrame"))
        colnames(df)[colnames(df) == "GTexGene"] <- "GtexGene"
        colnames(df)[colnames(df) == "GwasPmID"] <- "GwasPmid"
        ## Coerce "GoF", "LoF" to proper camel case.
        colnames(df) <- sub(
            pattern = "([GL])oF$",
            replacement = "\\1of",
            x = colnames(df)
        )
        colnames(df) <- camelCase(colnames(df), strict = TRUE)
        sanitize <- function(x) {
            ## Remove unnecessary quoting.
            x <- sub(
                pattern = "^[\"]+",
                replacement = "",
                x = x
            )
            x <- sub(
                pattern = "[\"]+$",
                replacement = "",
                x = x
            )
            ## Fix malformed ";".
            x <- gsub(
                pattern = "%3B",
                replacement = ";",
                x = x,
                fixed = TRUE
            )
            x <- sub(pattern = paste0(";$"), replacement = "", x = x)
            ## Remove trailing spaces.
            x <- sub(pattern = "[[:space:]]+$", replacement = "", x = x)
            x
        }
        split <- function(x, sep) {
            x <- strsplit(x = x, split = sep, fixed = TRUE)
            x <- CharacterList(x)
            x
        }
        df[["associatedWith"]] <- sanitize(df[["associatedWith"]])
        df[["associatedWith"]] <- split(df[["associatedWith"]], sep = ";")
        df[["cancerMolecularGenetics"]] <- sanitize(df[["associatedWith"]])
        df[["cosmicOverlappingMutations"]] <-
            split(df[["cosmicOverlappingMutations"]], sep = " ")
        df[["dbsnpId"]] <- split(df[["dbsnpId"]], sep = " ")
        df[["lineageAssociation"]] <- sanitize(df[["lineageAssociation"]])
        df[["lineageAssociation"]] <- gsub(
            pattern = ";",
            replacement = ", ",
            x = df[["lineageAssociation"]],
            fixed = TRUE
        )
        df[["lineageAssociation"]] <-
            split(df[["lineageAssociation"]], sep = ", ")
        ## - structuralRelation -- funky (e.g. `""ABL1,  FGFR1, JAK2 ""`).
        ##     Note that some of these have two spaces, so need to sanitize.
        ## - transcriptLikelyLof (by `;`).
        ## - uniprotId (by `, `).
        df <- encode(df)
        metadata(df) <- list(
            "date" = Sys.Date(),
            "dataset" = dataset,
            "packageName" = .pkgName,
            "packageVersion" = .pkgVersion
        )
        new(Class = "DepMapMutations", df)
    }

formals(DepMapMutations)[["dataset"]] <- # nolint
    .formalsList[["depmapDataset"]]
