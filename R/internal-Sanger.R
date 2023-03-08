## FIXME Ensure we use "broadModelId".
## FIXME rename modelId to sangerModelId
## FIXME rename rrid to cellosaurusId
## FIXME rename modelName to cellLineName
## FIXNE rename cancerTypeNcitId
## FIXME rename broadId to depmapId
## FIXME Split the synonyms by semicolon.
## FIXME Add "strippedCellLineName" column.
## FIXME Add "ccleName" column.
## FIXME Add ncitDiseaseId column.
## FIXME Need to map ncitDiseaseName here -- use Cellosaurus code.
## FIXME Only return lines that map to Cellosaurus.



## FIXME Ignore cells that are contaminated (problematic).

#' Import Sanger CellModelPassports cell line model info
#'
#' @note Updated 2023-03-08.
#' @noRd
#'
#' @details
#' Alternatively, can use "model_list_latest.csv.gz".
.importSangerModelInfo <-
    function(date = "2023-01-10", cello = NULL) {
        if (is.null(cello)) {
            cello <- Cellosaurus()
        }
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
        keep <- !cello[["isProblematic"]]
        cello <- cello[keep, , drop = FALSE]
        date2 <- gsub(pattern = "-", replacement = "", x = date)
        url <- pasteURL(
            "cog.sanger.ac.uk",
            "cmp",
            "download",
            paste0("model_list_", date2, ".csv"),
            protocol = "https"
        )
        sanger <- import(
            con = .cacheURL(url),
            format = "csv",
            engine = "data.table"
        )
        sanger <- as(sanger, "DataFrame")
        assert(allAreMatchingFixed(x = sanger[[1L]], pattern = "SIDM"))
        modelIds <- sort(intersect(
            x = sanger[[1L]],
            y = decode(cello[["sangerModelId"]])
        ))
        sanger <- sanger[
            match(x = modelIds, table = sanger[[1L]]),
            ,
            drop = FALSE
        ]
        cello <- cello[
            match(x = modelIds, table = decode(cello[["sangerModelId"]])),
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
        df
    }
