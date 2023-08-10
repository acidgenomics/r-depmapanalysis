#' Import Sanger CellModelPassports cell line model info
#'
#' @note Updated 2023-03-08.
#' @noRd
.importSangerModelInfo <-
    function(date = "2023-01-10") {
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
        alert(paste(
            "Filtering cell lines annotated as \"problematic\"",
            "by Cellosaurus."
        ))
        cello <- cello[!cello[["isProblematic"]], , drop = FALSE]
        date2 <- gsub(pattern = "-", replacement = "", x = date)
        url <- pasteURL(
            "cog.sanger.ac.uk",
            "cmp",
            "download",
            paste0("model_list_", date2, ".csv"),
            protocol = "https"
        )
        ## FIXME Can we use the base engine here instead?
        sanger <- import(
            con = .cacheURL(url),
            format = "csv",
            engine = "data.table"
        )
        sanger <- as(sanger, "DFrame")
        ids <- list()
        ids[["sanger"]] <- sanger[[1L]]
        ids[["cello"]] <- decode(cello[["sangerModelId"]])
        ids[["intersect"]] <- sort(intersect(
            x = na.omit(ids[["sanger"]]),
            y = na.omit(ids[["cello"]])
        ))
        ids[["setdiff"]] <- sort(setdiff(
            x = na.omit(ids[["sanger"]]),
            y = na.omit(ids[["cello"]])
        ))
        assert(allAreMatchingFixed(x = ids[["intersect"]], pattern = "SIDM"))
        sanger <- sanger[
            match(x = ids[["intersect"]], table = ids[["sanger"]]),
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
            "sanger" = I(sanger),
            row.names = decode(cello[["sangerModelId"]])
        )
        metadata(df) <- list(
            "date" = date,
            "missingCells" = ids[["setdiff"]]
        )
        df
    }
