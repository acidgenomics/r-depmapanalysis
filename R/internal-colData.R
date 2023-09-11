#' Simple column data
#'
#' @note Updated 2023-09-11.
#' @noRd
#'
#' @return `DFrame`.
#'
#' @examples
#' data(rnaseq)
#' cd <- .simpleColData(object)
.simpleColData <- function(object) {
    df <- colData(object)[["cellosaurus"]]
    df <- as(df, "DFrame")
    df <- decode(df)
    colnames(df)[colnames(df) == "accession"] <- "cellosaurusId"
    out <- df[
        ,
        c(
            "cellLineName",
            "cellosaurusId",
            "atccId",
            "depmapId",
            "sangerModelId",
            "isContaminated",
            "isProblematic",
            "ncitDiseaseId",
            "ncitDiseaseName",
            "oncotreeCode",
            "oncotreeLevel",
            "oncotreeMainType",
            "oncotreeName",
            "oncotreeParent",
            "oncotreeTissue"
        )
    ]
    out
}
