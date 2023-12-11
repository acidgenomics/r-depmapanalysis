## FIXME Return this classed, so we can also include a corresponding plot
## function (similar to our approach in Prism).

## FIXME Rework this as a general method that we can inherit in
## cBioPortalAnalysis package. Consider exporting for SummarizedExperiment?



#' @name predictSensitivity
#' @author Nitesh Turaga, Teresa Rice, Michael Steinbaugh
#' @inherit AcidGenerics::predictSensitivity
#' @note Updated 2023-09-12.
#'
#' @details
#' Predict cell line sensitivity by calculation of Euclidean distance between
#' known biomarker genes up- or down-regulated by a treatment of interest.
#'
#' @inheritParams AcidRoxygen::params
#'
#' @param upregulated `character`.
#' Genes observed to be upregulated by treatment.
#'
#' @param downregulated `character`.
#' Genes observed to be downregulated by treatment.
#'
#' @return `DFrame`.
#'
#' @examples
#' data(rnaseq)
#'
#' ## DepMapGeneExpression ====
#' object <- rnaseq
#' genes <- rownames(object)
#' upregulated <- genes[1L:5L]
#' downregulated <- genes[6L:10L]
#' df <- predictSensitivity(
#'     object = object,
#'     upregulated = upregulated,
#'     downregulated = downregulated
#' )
#' print(df)
NULL



## Updated 2023-09-12.
`predictSensitivity,DepMapGeneExpression` <- # nolint
    function(object,
             upregulated,
             downregulated) {
        assert(
            validObject(object),
            isCharacter(upregulated),
            isCharacter(downregulated),
            hasNoDuplicates(upregulated),
            hasNoDuplicates(downregulated),
            areDisjointSets(x = upregulated, y = downregulated)
        )
        up <- mapGenesToRownames(
            object = object,
            genes = upregulated,
            strict = TRUE
        )
        down <- mapGenesToRownames(
            object = object,
            genes = downregulated,
            strict = TRUE
        )
        object <- object[c(up, down), , drop = FALSE]
        ## We are returning genes in rows, cells in columns. Some of our legacy
        ## Python code uses a transposed matrix here.
        mat <- zscore(object)
        ## Determine the maximum and minimum values per gene across the cells.
        geneMax <- rowMaxs(mat)
        geneMin <- rowMins(mat)
        ## Calculate SI and RI per gene for up- and down-regulated genes.
        siUp <- geneMax[names(geneMax) %in% up]
        riUp <- geneMin[names(geneMin) %in% up]
        siDown <- geneMin[names(geneMin) %in% down]
        riDown <- geneMax[names(geneMax) %in% down]
        si <- c(siUp, siDown)
        ri <- c(riUp, riDown)
        ## Create SI and RI matrix to get max.
        siMat <- do.call(
            what = cbind,
            args = replicate(
                n = ncol(mat),
                expr = si,
                simplify = FALSE
            )
        )
        riMat <- do.call(
            what = cbind,
            args = replicate(
                n = ncol(mat),
                expr = ri,
                simplify = FALSE
            )
        )
        colnames(siMat) <- colnames(mat)
        colnames(riMat) <- colnames(mat)
        ## Calculate the summation of euclidean distances.
        sigmaS <- colSums2((mat - siMat) ** 2L)
        sigmaR <- colSums2((mat - riMat) ** 2L)
        ## Euclidean distance of sensitivity model.
        dsj <- sqrt(sigmaS)
        ## Euclidean distance of insensitivity model.
        drj <- sqrt(sigmaR)
        ratio <- dsj / drj
        ## Indicate whether we predict sensitive.
        pred <- ifelse(
            test = dsj <= drj,
            yes = "sensitive",
            no = "insensitive"
        )
        out <- DataFrame(
            "drj" = drj,
            "dsj" = dsj,
            "ratio" = ratio,
            "prediction" = pred,
            row.names = colnames(object)
        )
        ## Return with additional metadata useful for biologists.
        cd <- .simpleColData(object)
        out <- cbind(out, cd)
        ## Ensure we stash the user input in the output.
        metadata(out) <- list(
            "upregulated" = upregulated,
            "downregulated" = downregulated,
            "releaseDate" = metadata(object)[["releaseDate"]]
        )
        out
    }



#' @rdname predictSensitivity
#' @export
setMethod(
    f = "predictSensitivity",
    signature = signature(object = "DepMapGeneExpression"),
    definition = `predictSensitivity,DepMapGeneExpression`
)
