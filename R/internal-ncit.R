#' Add NCI thesaurus disease name metadata, using OBO ontology file
#'
#' @note Updated 2023-03-01.
#' @noRd
#'
#' @details
#' Alternative versioned release:
#' https://github.com/NCI-Thesaurus/thesaurus-obo-edition/releases/
#' download/v2022-08-19/ncit.obo
#'
#' May be able to speed this up by setting "minimal" instead of "everything" in
#' ontologyIndex call, which would need to be added to pipette in the future.
#'
#' @seealso
#' - https://evs.nci.nih.gov/evs-download/thesaurus-downloads
#' - https://obofoundry.org/ontology/ncit.html
#' - https://github.com/NCI-Thesaurus/thesaurus-obo-edition/
#' - https://www.ebi.ac.uk/ols/ontologies/ncit
#' - BiocOncoTK package
.ncitMetadata <- function(cache = TRUE) {
    assert(isFlag(cache))
    url <- pasteURL(
        "purl.obolibrary.org",
        "obo",
        "ncit.obo",
        protocol = "http"
    )
    if (isTRUE(cache)) {
        con <- cacheURL(url = url, pkg = .pkgName)
    } else {
        con <- url
    }
    obo <- import(con, format = "obo")
    df <- DataFrame(
        "ncitDiseaseId" = names(obo[["name"]]),
        "ncitDiseaseName" = unname(obo[["name"]])
    )
    df[["ncitDiseaseId"]] <- sub(
        pattern = "^NCIT:",
        replacement = "",
        x = df[["ncitDiseaseId"]]
    )
    assert(hasNoDuplicates(df[["ncitDiseaseId"]]))
    df <- df[order(df[["ncitDiseaseId"]]), , drop = FALSE]
    df
}
