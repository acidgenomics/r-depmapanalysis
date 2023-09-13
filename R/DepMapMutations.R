## FIXME Consider reworking using OmicsSomaticMutationsMAFProfile.maf,
## which we can import via maftools.
##
## Consider filtering dataset to only include drivers.



#' Import DepMap somatic mutation data
#'
#' @export
#' @note Updated 2023-08-09.
#'
#' @return `DepMapMutations`.
#'
#' @examples
#' object <- DepMapMutations()
#' dim(object)
DepMapMutations <- # nolint
    function() {
        dataset <- .currentBroadDataset
        assert(isString(dataset))
        url <- datasets[[dataset]][["files"]][["OmicsSomaticMutations.csv"]]
        assert(isAURL(url))
        df <- .importBroadDataFile(url = url, format = "csv")
        assert(is(df, "DFrame"))
        colnames(df)[colnames(df) == "GTexGene"] <- "GtexGene"
        colnames(df)[colnames(df) == "GwasPmID"] <- "GwasPmid"
        ## Coerce "GoF", "LoF" to proper camel case.
        colnames(df) <- sub(
            pattern = "([GL])oF$",
            replacement = "\\1of",
            x = colnames(df)
        )
        colnames(df) <- camelCase(colnames(df), strict = TRUE)
        sanitizeCol <- function(x) {
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
        splitCol <- function(x, sep) {
            x <- strsplit(x = x, split = sep, fixed = TRUE)
            x <- CharacterList(x)
            x
        }
        df[["associatedWith"]] <- sanitizeCol(df[["associatedWith"]])
        df[["associatedWith"]] <- splitCol(df[["associatedWith"]], sep = ";")
        df[["cancerMolecularGenetics"]] <- sanitizeCol(df[["associatedWith"]])
        df[["cosmicOverlappingMutations"]] <-
            splitCol(df[["cosmicOverlappingMutations"]], sep = " ")
        df[["dbsnpId"]] <- splitCol(df[["dbsnpId"]], sep = " ")
        df[["lineageAssociation"]] <- sanitizeCol(df[["lineageAssociation"]])
        df[["lineageAssociation"]] <- gsub(
            pattern = ";",
            replacement = ", ",
            x = df[["lineageAssociation"]],
            fixed = TRUE
        )
        df[["lineageAssociation"]] <-
            splitCol(df[["lineageAssociation"]], sep = ", ")
        df[["structuralRelation"]] <- sanitizeCol(df[["structuralRelation"]])
        df[["structuralRelation"]] <-
            splitCol(df[["structuralRelation"]], sep = ", ")
        df[["transcriptLikelyLof"]] <- sanitizeCol(df[["transcriptLikelyLof"]])
        df[["transcriptLikelyLof"]] <-
            splitCol(df[["transcriptLikelyLof"]], sep = ";")
        df[["uniprotId"]] <- splitCol(df[["uniprotId"]], sep = ", ")
        df <- encode(df)
        metadata(df) <- list(
            "date" = Sys.Date(),
            "dataset" = dataset,
            "packageName" = .pkgName,
            "packageVersion" = .pkgVersion
        )
        new(Class = "DepMapMutations", df)
    }
