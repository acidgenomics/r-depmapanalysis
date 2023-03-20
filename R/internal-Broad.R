#' Import a Broad DepMap file containing gene identifiers
#'
#' @note Updated 2023-03-08.
#' @noRd
.importBroadGeneDataFile <-
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



#' Import Broad DepMap cell line model info
#'
#' Sample metadata now indicates that there are merged cells we should drop
#' from analysis (e.g. ACH-002260).
#'
#' @note Updated 2023-03-08.
#' @noRd
.importBroadModelInfo <-
    function(dataset) {
        if (identical(dataset, "demeter2_data_v6")) {
            df <- .importDemeter2ModelInfo()
            return(df)
        }
        fileKey <- switch(
            EXPR = dataset,
            "depmap_public_22q4" = "Model.csv",
            "depmap_public_22q2" = "sample_info.csv",
            stop("Unsupported dataset.")
        )
        cello <- Cellosaurus()
        assert(
            is(cello, "Cellosaurus"),
            isSubset(
                c(
                    "accession",
                    "cellLineName",
                    "depmapId",
                    "isProblematic",
                    "sangerModelId"
                ),
                colnames(cello)
            )
        )
        alert("Filtering cell lines annotated as 'problematic' by Cellosaurus.")
        cello <- cello[!cello[["isProblematic"]], , drop = FALSE]
        url <- datasets[[dataset]][["files"]][[fileKey]]
        broad <- .importDataFile(
            url = url,
            format = "csv",
            rownameCol = NULL,
            colnames = TRUE,
            return = "DataFrame"
        )
        ids <- list()
        ids[["broad"]] <- broad[[1L]]
        ids[["cello"]] <- decode(cello[["depmapId"]])
        ids[["intersect"]] <- sort(intersect(
            x = na.omit(ids[["broad"]]),
            y = na.omit(ids[["cello"]])
        ))
        ids[["setdiff"]] <- sort(setdiff(
            x = na.omit(ids[["broad"]]),
            y = na.omit(ids[["cello"]])
        ))
        assert(allAreMatchingFixed(x = ids[["intersect"]], pattern = "ACH-"))
        broad <- broad[
            match(x = ids[["intersect"]], table = ids[["broad"]]),
            ,
            drop = FALSE
        ]
        cello <- cello[
            match(x = ids[["intersect"]], table = ids[["cello"]]),
            ,
            drop = FALSE
        ]
        cello <- droplevels2(cello)
        df <- DataFrame(
            "cellLineName" = decode(cello[["cellLineName"]]),
            "cellosaurusId" = decode(cello[["accession"]]),
            "depmapId" = decode(cello[["depmapId"]]),
            "sangerModelId" = decode(cello[["sangerModelId"]]),
            "cellosaurus" = I(cello),
            "broad" = I(broad),
            row.names = makeNames(decode(cello[["depmapId"]]))
        )
        metadata(df) <- list("missingCells" = ids[["setdiff"]])
        df
    }

formals(.importBroadModelInfo)[["dataset"]] <-
    .formalsList[["dataset"]][[1L]]



#' Import Broad DEMETER2 RNAi cell line model info
#'
#' @note Updated 2023-03-08.
#' @noRd
#'
#' @details
#' Matching the cell lines here by CCLE identifier.
.importDemeter2ModelInfo <- function() {
    url <- datasets[["demeter2_data_v6"]][["files"]][["sample_info.csv"]]
    d2 <- .importDataFile(
        url = url,
        format = "csv",
        rownameCol = NULL,
        colnames = TRUE,
        return = "DataFrame"
    )
    broad <- .importBroadModelInfo()
    ids <- list()
    ids[["d2"]] <- d2[[1L]]
    ids[["d2"]][ids[["d2"]] == "COLO699_LUNG"] <-
        "CHL1DM_SKIN"
    ids[["d2"]][ids[["d2"]] == "GISTT1_GASTROINTESTINAL_TRACT"] <-
        "GISTT1_STOMACH"
    ids[["d2"]][ids[["d2"]] == "MB157_BREAST"] <-
        "MDAMB157_BREAST"
    ids[["broad"]] <- broad[["broad"]][["CCLEName"]]
    ids[["intersect"]] <- sort(intersect(
        x = na.omit(ids[["d2"]]),
        y = na.omit(ids[["broad"]])
    ))
    ids[["setdiff"]] <- sort(setdiff(
        x = na.omit(ids[["d2"]]),
        y = na.omit(ids[["broad"]])
    ))
    d2 <- d2[
        match(x = ids[["intersect"]], table = ids[["d2"]]),
        ,
        drop = FALSE
    ]
    broad <- broad[
        match(x = ids[["intersect"]], table = ids[["broad"]]),
        ,
        drop = FALSE
    ]
    df <- broad
    df[["demeter2"]] <- d2
    rownames(df) <- makeNames(d2[[1L]])
    metadata(df) <- list("missingCells" = ids[["setdiff"]])
    df
}
