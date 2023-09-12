#' Import Sanger CellModelPassports cell line model info
#'
#' @note Updated 2023-09-12.
#' @noRd
.importSangerModelInfo <-
    function(date = "2023-08-01") {
        date2 <- gsub(pattern = "-", replacement = "", x = date)
        url <- pasteURL(
            "cog.sanger.ac.uk",
            "cmp",
            "download",
            paste0("model_list_", date2, ".csv"),
            protocol = "https"
        )
        sanger <- import(con = .cacheURL(url), format = "csv")
        sanger <- as(sanger, "DFrame")
        cello <- Cellosaurus()
        assert(
            is(cello, "Cellosaurus"),
            isSubset(
                c(
                    "accession",
                    "cellLineName",
                    "depmapId",
                    "sangerModelId"
                ),
                colnames(cello)
            )
        )
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
        assert(
            is(cello, "Cellosaurus"),
            is(sanger, "DFrame"),
            validObject(cello)
        )
        df <- as.DataFrame(list(
            "cellLineName" = decode(cello[["cellLineName"]]),
            "cellosaurusId" = decode(cello[["accession"]]),
            "depmapId" = decode(cello[["depmapId"]]),
            "sangerModelId" = decode(cello[["sangerModelId"]]),
            "cellosaurus" = cello,
            "sanger" = sanger
        ))
        rownames(df) <- decode(df[["sangerModelId"]])
        metadata(df) <- list(
            "date" = date,
            "excludedCells" = ids[["setdiff"]]
        )
        df
    }
