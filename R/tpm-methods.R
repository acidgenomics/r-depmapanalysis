#' Transcripts per million (log2)
#'
#' @name tpm
#' @inherit AcidGenerics::tpm
#' @note Updated 2023-09-11.
#'
#' @examples
#' data(rnaseq)
#' tpm <- tpm(rnaseq)
#' print(tpm)
NULL



## Updated 2023-09-11.
`tpm,DepMapGeneExpression` <- # nolint
    function(object) {
        assert(validObject(object))
        mat <- assay(object, i = "log2Tpm")
        assert(is.matrix(mat))
        mat
    }



#' @rdname tpm
#' @export
setMethod(
    f = "tpm",
    signature = signature(object = "DepMapGeneExpression"),
    definition = `tpm,DepMapGeneExpression`
)
