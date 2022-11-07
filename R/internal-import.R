#' Cache URL into package
#'
#' @note Updated 2021-07-08.
#' @noRd
.cacheURL <-
    function(url, ...) {
        alert(sprintf("Downloading DepMap file {.url %s}.", url))
        cacheURL(url, pkg = .pkgName, ...)
    }



## FIXME This is currently erroring with DEMETER2 dataset...

## FIXME Now we're hitting this issue...need to rethink our GeneEffect
## dataset import approach, arg....
## → Importing /Users/mike/.cache/R/DepMapAnalysis/145d6f7b73c4_35020903 using data.table::`fread()`.
## Error:
##     ! Assert failure.
## [1] isSubset(x = c("ccleId", "cellLineName"), y = colnames(cd[["x"]])) is
## not TRUE.
## Cause: `c("ccleId", "cellLineName")` has elements not in
## `colnames(cd[["x"]])`: cellLineName
## Backtrace:
##     ▆
## 1. └─DepMapAnalysis::GeneEffect("demeter2_data_v6")
## 2.   └─DepMapAnalysis:::.makeSummarizedExperiment(...) at r-depmapanalysis/R/GeneEffect.R:91:8
## 3.     └─DepMapAnalysis:::.standardizeDemeter2(se) at r-depmapanalysis/R/internal-SE.R:185:12
## 4.       └─goalie::assert(...) at r-depmapanalysis/R/internal-SE.R:206:4
## 5.         └─AcidCLI (local) stop(...)
## 6.           └─cli::cli_abort(x, call = NULL)
## 7.             └─rlang::abort(...)



#' Import cell line sample metadata
#'
#' Sample metadata now indicates that there are merged cells we should drop
#' from analysis (e.g. ACH-002260).
#'
#' @note Updated 2022-09-21.
#' @noRd
#'
#' @seealso
#' - `.standardizeDemeter2` for DEMETER2 RNAi dataset handling.
.importCellLineSampleData <- # nolint
    function(dataset) {
        url <- datasets[[dataset]][["files"]][["sample_info"]][["url"]]
        df <- .importDataFile(
            url = url,
            rownamesCol = 1L,
            return = "DataFrame"
        )
        assert(is(df, "DataFrame"))
        if (identical(dataset, "demeter2_data_v6")) {
            return(df)
        }
        ## DEMETER2:
        ##  [1] "CCLE_ID"                         "disease"
        ##  [3] "disease_subtype"                 "disease_sub_subtype"
        ##  [5] "in_DRIVE"                        "in_Achilles"
        ##  [7] "in_Marcotte"                     "Novartis_name"
        ##  [9] "Novartis_Primary_site"           "Novartis_Pathologist_Annotation"
        ## [11] "Marcotte_name"                   "Marcotte_subtype_three_receptor"
        ## [13] "Marcotte_subtype_neve"           "Marcotte_subtype_intrinsic"
        ## FIXME Need to rethink this for DEMETER2.
        ## FIXME We need to merge in the canonical metadata from DepMap...
        assert(
            isSubset(
                x = c(
                    "Cellosaurus_NCIt_disease",
                    "Cellosaurus_NCIt_id",
                    "COSMICID",
                    "DepMap_ID",
                    "RRID"
                ),
                y = colnames(df)
            )
        )
        colnames(df)[colnames(df) == "Cellosaurus_NCIt_disease"] <-
            "ncitDiseaseName"
        colnames(df)[colnames(df) == "Cellosaurus_NCIt_id"] <-
            "ncitDiseaseId"
        colnames(df)[colnames(df) == "COSMICID"] <- "cosmicId"
        colnames(df)[colnames(df) == "DepMap_ID"] <- "depmapId"
        colnames(df)[colnames(df) == "RRID"] <- "cellosaurusId"
        colnames(df) <- camelCase(colnames(df), strict = TRUE)
        if (isSubset("alias", colnames(df))) {
            x <- df[["alias"]]
            x <- strsplit(x = x, split = ", ", fixed = TRUE)
            x <- CharacterList(x)
            df[["alias"]] <- x
        }
        if (
            !isSubset("cellLineName", colnames(df)) &&
                isSubset("strippedCellLineName", colnames(df))
        ) {
            ## e.g. "depmap_public_20q3".
            df[["cellLineName"]] <- df[["strippedCellLineName"]]
        } else if (
            !isSubset("cellLineName", colnames(df)) &&
                isSubset("ccleId", colnames(df))
        ) {
            x <- df[["ccleId"]]
            x <- vapply(
                X = strsplit(x = x, split = "_", fixed = TRUE),
                FUN = `[`,
                i = 1L,
                FUN.VALUE = character(1L)
            )
            df[["cellLineName"]] <- x
        }
        assert(isSubset("cellLineName", colnames(df)))
        if (anyNA(df[["cellLineName"]])) {
            assert(isSubset("strippedCellLineName", colnames(df)))
            idx <- which(is.na(df[["cellLineName"]]))
            df[["cellLineName"]][idx] <- df[["strippedCellLineName"]][idx]
        }
        ## There can be some problematic cell lines that still persist here
        ## (e.g. ACH-002260).
        keep <- !is.na(df[["cellLineName"]])
        df <- df[keep, ]
        df <- factorize(df)
        df <- encode(df)
        df
    }



#' Import a DepMap data file
#'
#' @note Updated 2022-08-10.
#' @noRd
.importDataFile <-
    function(url,
             format = c("csv", "tsv"),
             rownamesCol = NULL,
             engine = getOption(
                 x = "acid.import.engine",
                 default = ifelse(
                     test = unname(isInstalled("data.table")),
                     yes = "data.table",
                     no = "base"
                 )
             ),
             return = c("DataFrame", "matrix")) {
        assert(
            isAURL(url),
            isScalar(rownamesCol) || is.null(rownamesCol),
            isString(engine)
        )
        format <- match.arg(format)
        return <- match.arg(return)
        ## Engine overrides for malformed DepMap flat file downloads.
        malformedIds <- c(
            31316011L,
            35020903L
        )
        if (as.integer(basename(url)) %in% malformedIds) {
            requireNamespaces("data.table")
            engine <- "data.table"
        }
        tmpfile <- .cacheURL(url = url)
        df <- import(
            con = tmpfile,
            format = format,
            engine = engine
        )
        if (isScalar(rownamesCol)) {
            if (!isString(rownamesCol)) {
                rownamesCol <- colnames(df)[[rownamesCol]]
            }
            assert(isSubset(rownamesCol, colnames(df)))
            rownames(df) <- df[[rownamesCol]]
        }
        out <- switch(
            EXPR = return,
            "DataFrame" = as(df, "DataFrame"),
            "matrix" = {
                if (hasRownames(df)) df[[rownamesCol]] <- NULL
                as.matrix(df)
            }
        )
        out <- makeDimnames(out)
        out
    }



#' Import a DepMap file containing gene identifiers
#'
#' @note Updated 2022-08-05.
#' @noRd
.importGeneDataFile <-
    function(url) {
        df <- .importDataFile(
            url = url,
            engine = "base",
            return = "DataFrame"
        )
        colnames(df) <- camelCase(colnames(df))
        if (identical(colnames(df), c("geneSymbol", "geneId"))) {
            ## e.g. DEMETER2 control files.
            vec <- paste0(df[["geneSymbol"]], " (", df[["geneId"]], ")")
        } else {
            ## e.g. DepMap current public release files.
            assert(
                identical(colnames(df), "gene"),
                isCharacter(df[["gene"]])
            )
            vec <- df[["gene"]]
        }
        vec <- sort(vec)
        vec
    }
