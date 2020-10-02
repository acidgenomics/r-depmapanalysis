#' Import a DepMap file containing gene identifiers
#'
#' @note Updated 2020-10-02
#' @noRd
.importGeneDataFile <-
    function(fileName, release) {
        df <- .importDataFile(
            fileName = fileName,
            release = release,
            return = "DataFrame"
        )
        assert(isCharacter(df[["gene"]]))
        vec <- sort(df[["gene"]])
        vec
    }



#' Import common essential genes
#'
#' @note Updated 2020-10-02.
#' @noRd
.importCommonEssentials <-
    function(release) {
        .importGeneDataFile(
            fileName = "achilles_common_essentials.csv",
            release = release
        )
    }



#' Import control essential genes
#'
#' @note Updated 2020-10-02.
#' @noRd
.importControlCommonEssentials <-
    function(release) {
        .importGeneDataFile(
            fileName = "common_essentials.csv",
            release = release
        )
    }



#' Import control non-essential genes
#'
#' @note Updated 2020-10-02.
#' @noRd
.importControlNonessentials <-
    function(release) {
        .importGeneDataFile(
            fileName = "nonessentials.csv",
            release = release
        )
    }
