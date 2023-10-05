#' Import Sanger CellModelPassports cell line model info
#'
#' @note Updated 2023-09-14.
#' @noRd
.importSangerModelInfo <-
    function(date = "2023-08-01") {
        url <- pasteUrl(
            "cog.sanger.ac.uk",
            "cmp",
            "download",
            paste0(
                "model_list_",
                gsub(pattern = "-", replacement = "", x = date),
                ".csv"
            ),
            protocol = "https"
        )
        sanger <- import(con = .cacheUrl(url), format = "csv")
        sanger <- as(sanger, "DFrame")
        cello <- Cellosaurus()
        cello <- excludeContaminatedCells(cello)
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
        df <- as.DataFrame(list(
            "cellosaurus" = cello,
            "sanger" = sanger
        ))
        rownames(df) <-
            makeNames(decode(df[["cellosaurus"]][["sangerModelId"]]))
        metadata(df) <- list(
            "date" = date,
            "excludedCells" = ids[["setdiff"]]
        )
        df
    }
