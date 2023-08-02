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
        ## FIXME Need to split these columns:
        ## - associatedWith (by `;`).
        ## - cancerMolecularGenetics -- funky (e.g. `""Dom, Rec""`).
        ## - cosmicOverlappingMutations (by ` `).
        ## - dbsnpId (by ` `).
        ## - lineageAssociation -- funky (e.g. `""E, L, M""`).
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
