#' @name mapCells
#' @inherit acidgenerics::mapCells
#' @note Updated 2020-10-01.
NULL



#' @rdname mapCells
#' @name mapCells
#' @importFrom acidgenerics mapCells
#' @usage mapCells(object, ...)
#' @export
NULL



`mapCells,Achilles` <-  # nolint
    function(object) {
        print("FIXME")
    }



#' @rdname mapCells
#' @export
setMethod(
    f = "mapCells",
    signature = signature("Achilles"),
    definition = `mapCells,Achilles`
)
