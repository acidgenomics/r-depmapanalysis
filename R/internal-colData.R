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
    cols <- c(
        "cellLineName",
        "accession",
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
    df <- colData(object)[["cellosaurus"]]
    assert(isSubset(cols, colnames(df)))
    df <- as(df, "DFrame")
    df <- decode(df)
    colnames(df)[colnames(df) == "accession"] <- "cellosaurusId"
    df
}
