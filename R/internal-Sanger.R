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
        date2 <- gsub(pattern = "-", replacement = "", x = date)
        url <- pasteURL(
            "cog.sanger.ac.uk",
            "cmp",
            "download",
            paste0("model_list_", date2, ".csv"),
            protocol = "https"
        )
        con <- .cacheURL(url)
        extra <- import(con = con, format = "csv", engine = "data.table")
        extra <- as(extra, "DataFrame")
        colnames(extra) <- camelCase(tolower(colnames(extra)))

        ## FIXME It doesn't look like Sanger is mapping RRID to all cells correctly.
        ## Check with ACH- ids against our current Cello annotations.
        ## Let's just use our Cello mapping IDS (1925 vs. 1585).

        df <- DataFrame()

        ## Step 1: Figure out which cells with Sanger modelId map to Cello.
        ## Get these from the Cellosaurus object
        ## cellosaurusId
        ## sangerModelId
        ## cellLineName
        ## ...



        ## Nest a column named "extra" here.
        ## broadId -> depmapId
        ## rrid -> cellosaurusId
        ## modelId -> sangerModelId
        ## modelName -> cellLineName
        ## cancerTypeNcitId -> ncitDiseaseId
        ## cancerType -> ncitDiseaseName
        ## sampleId -> sangerSampleId
        ## patientId -> sangerPatientId
        ## parentId -> sangerParentId
        ## FIXME Consider collapsing this other metadata into a DataFrame named
        ## Sanger?
        ## FIXME Work on standardizing annoying names first.

        ## FIXME Return with cellosaurusId, depmapId, sangerModelId, cellLineName
        ## Also include strippedCellLineName?
        df
    }
