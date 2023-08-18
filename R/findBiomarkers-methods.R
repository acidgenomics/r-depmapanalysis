## FIXME I think it might be cleaner to just call this `diffExp` instead.

## FIXME Need to also add a DepMapTxExpression method here.
## Can use the new Bioconductor DifferentialRegulation package here?
## https://www.biorxiv.org/content/10.1101/2023.08.17.553679v1

## FIXME This is the t-test code.
## FIXME Add filters for percentage of sign flips.
## FIXME Add filter for fold change cutoff, p value cutoff.




#' Find biomarkers
#'
#' @name findBiomarkers
#' @note Updated 2023-08-02.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param insensitiveCells,sensitiveCells `character`.
#' Cell line names.
#'
#' @return FIXME Varies, depending on method.
#'
#' @seealso
#' - limma User's Guide, Section 15.4.
#'
#' @examples
#' data(rnaseq)
#'
#' ## DepMapExpression ====
#' object <- rnaseq
#' sensitiveCells <- colnames(object)[c(1L:5L)]
#' insensitiveCells <- colnames(object)[c(6L:10L)]
#' x <- findBiomarkers(
#'     object = object,
#'     sensitiveCells = sensitiveCells,
#'     insensitiveCells = insensitiveCells
#' )
#' print(x)
NULL



## FIXME Need to rework by porting our Python code.

## Updated 2023-01-26.
`findBiomarkers,DepMapExpression` <-
    function(
        object,
        sensitiveCells,
        insensitiveCells
    ) {
        stop("FIXME Still a work in progress.")
        assert(
            validObject(object),
            requireNamespaces("limma"),
            isCharacter(sensitiveCells),
            isCharacter(insensitiveCells)
        )
        ## FIXME Need to run .mapCellsToColnames here.
        log2Tpm <- assay(object, i = "log2Tpm")
        fit <- limma::lmFit(log2Tpm, design)
        fit <- limma::eBayes(fit, trend=TRUE)
        ## FIXME Add an expression cutoff (e.g. baseMean) here.
        topTable(fit, coef=ncol(design))
    }



#' @rdname findBiomarkers
#' @export
setMethod(
    f = "findBiomarkers",
    signature = signature(object = "DepMapExpression"),
    definition = `findBiomarkers,DepMapExpression`
)
