#' Cache URL into package
#'
#' @note Updated 2023-03-08.
#' @noRd
.cacheURL <-
    function(url) {
        cacheURL(url = url, pkg = .pkgName)
    }



## FIXME Work off the DepMap ID as input here and ensure we match to the
## Cellosaurus database...don't assume the RRIDs are correct.
##
## FIXME Only return lines that map to Cellosaurus.
## FIXME Use Cellosaurus to return the cell line metadata and join.

#' Import Broad DepMap cell line model info
#'
#' Sample metadata now indicates that there are merged cells we should drop
#' from analysis (e.g. ACH-002260).
#'
#' @note Updated 2023-03-08.
#' @noRd
.importBroadModelInfo <-
    function(dataset, cello = NULL) {
        if (is.null(cello)) {
            cello <- Cellosaurus()
        }
        assert(
            is(cello, "Cellosaurus"),
            isSubset("depmapId", colnames(cello))
        )
        key <- switch(
            EXPR = dataset,
            "depmap_public_22q4" = "Model.csv",
            "depmap_public_22q2" = "sample_info.csv",
            "demeter2_data_v6" = "sample_info.csv"
        )
        url <- datasets[[dataset]][["files"]][[key]]
        assert(isAURL(url))
        broad <- .importDataFile(
            url = url,
            format = "csv",
            rownameCol = NULL,
            colnames = TRUE,
            return = "DataFrame"
        )
        assert(allAreMatchingFixed(x = broad[[1L]], pattern = "ACH-"))
        depmapIds <- intersect(x = broad[[1L]], y = decode(cello[["depmapId"]]))
        broad <- broad[
            match(x = depmapIds, table = broad[[1L]]),
            ,
            drop = FALSE
        ]
        cello <- cello[
            match(x = depmapIds, table = cello[["depmapId"]]),
            ,
            drop = FALSE
        ]
        cello <- droplevels2(cello)
        df <- DataFrame(
            "cellLineName" = cello[["cellLineName"]],
            "cellosaurusId" = cello[["accession"]],
            "depmapId" = cello[["depmapId"]],
            "sangerModelId" = cello[["sangerModelId"]],
            "cellosaurus" = I(cello),
            "broad" = I(broad),
            row.names = makeNames(decode(cello[["depmapId"]]))
        )
        ## FIXME This step is currently failing, need to harden.
        df <- droplevels2(df)
        df
    }

formals(.importBroadModelInfo)[["dataset"]] <-
    .formalsList[["dataset"]][[1L]]



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
