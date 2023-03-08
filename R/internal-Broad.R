#' Cache URL into package
#'
#' @note Updated 2023-03-08.
#' @noRd
.cacheURL <-
    function(url) {
        cacheURL(url = url, pkg = .pkgName)
    }



## FIXME Ensure that this works for all our datasets.
## FIXME Consider renaming "modelId" to "broadModelId".

#' Import cell line sample metadata
#'
#' Sample metadata now indicates that there are merged cells we should drop
#' from analysis (e.g. ACH-002260).
#'
#' @note Updated 2023-03-08.
#' @noRd
.importCellLineSampleData <-
    function(dataset) {
        key <- switch(
            EXPR = dataset,
            "depmap_public_22q4" = "Model.csv",
            "depmap_public_22q2" = "sample_info.csv",
            "demeter2_data_v6" = "sample_info.csv"
        )
        url <- datasets[[dataset]][["files"]][[key]]
        df <- .importDataFile(
            url = url,
            format = "csv",
            rownameCol = NULL,
            colnames = TRUE,
            return = "DataFrame"
        )
        rownames(df) <- makeNames(df[[1L]])
        colnames(df)[colnames(df) == "Cellosaurus_NCIt_disease"] <-
            "ncitDiseaseName"
        colnames(df)[colnames(df) == "Cellosaurus_NCIt_id"] <-
            "ncitDiseaseId"
        colnames(df)[colnames(df) == "COSMICID"] <- "cosmicId"
        colnames(df)[colnames(df) == "DepMap_ID"] <- "broadModelId"
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
            ## e.g. "demeter2_data_v6".
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



## FIXME We're running into issues with 22Q4 formatting here.
## FIXME But this works for 22Q2...consider how to rework here.

#' Import a DepMap data file
#'
#' @note Updated 2023-03-08.
#' @noRd
.importDataFile <-
    function(url,
             format = c("csv", "tsv"),
             colnames = TRUE,
             rownameCol = NULL,
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
            isFlag(colnames),
            isScalar(rownameCol) || is.null(rownameCol),
            isString(engine)
        )
        format <- match.arg(format)
        return <- match.arg(return)
        ## Engine overrides for malformed DepMap flat file downloads.
        malformedIds <- c(31316011L, 35020903L)
        if (isSubset(as.integer(basename(url)), malformedIds)) {
            requireNamespaces("data.table")
            engine <- "data.table"
        }
        tmpfile <- .cacheURL(url = url)
        ## FIXME 22Q4 doesn't contain colnames...need to rethink.
        ## FIXME We can just set rownameCol here instead.
        df <- import(
            con = tmpfile,
            format = format,
            rownameCol = rownameCol,
            colnames = colnames,
            engine = engine
        )
        out <- switch(
            EXPR = return,
            "DataFrame" = as(df, "DataFrame"),
            "matrix" = as.matrix(df)
        )
        out <- makeDimnames(out)
        out
    }



## FIXME Consider returning a DataFrame instead of a vector?
## Consider using geneName and ncbiGeneId columns?

#' Import a DepMap file containing gene identifiers
#'
#' @note Updated 2022-08-05.
#' @noRd
.importGeneDataFile <-
    function(url) {
        df <- .importDataFile(
            url = url,
            colnames = TRUE,
            engine = "base",
            return = "DataFrame"
        )
        if (
            identical(colnames(df), "Essentials") ||
            identical(colnames(df), "Gene") ||
            identical(colnames(df), "gene")
        ) {
            # e.g. DepMap 22Q4 ("Essentials", "Gene") and 22Q2 ("gene").
            df <- stringi::stri_split_fixed(
                str = df[[1L]],
                pattern = " ",
                n = 2L,
                simplify = TRUE
            )
            df <- as(df, "DataFrame")
            colnames(df) <- c("geneName", "ncbiGeneId")
            df[["ncbiGeneId"]] <- gsub(
                pattern = "[\\(\\)]",
                replacement = "",
                x = df[["ncbiGeneId"]]
            )
        } else if (identical(colnames(df), c("Gene_Symbol", "Gene_ID"))) {
            ## e.g. DEMETER2.
            colnames(df) <- c("geneName", "ncbiGeneId")
        } else {
            abort("Unsupported file.")
        }
        assert(
            is(df, "DataFrame"),
            identical(colnames(df), c("geneName", "ncbiGeneId"))
        )
        df[["ncbiGeneId"]] <- as.integer(df[["ncbiGeneId"]])
        df <- df[order(df), , drop = FALSE]
        df
    }
