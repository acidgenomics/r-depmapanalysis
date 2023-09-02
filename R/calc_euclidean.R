#' @param input_df input data.frame in the format Cells x Genes
#' @param up list of genes to be up-regulated
#' @param down list of genes to be down-regulated
#'
#' @importFrom goalie isCharacter assert
#' @importFrom matrixStats colMaxs colMins rowSums2
#'
calc_euclidean <- function(input_df,
                           up_genes,
                           down_genes) {
    ## Input Validation
    assert(is.data.frame(input_df),
           isCharacter(up_genes),
           isCharacter(down_genes))

    ## Make sure there are no duplicated genes
    assert(!any(duplicated(colnames(input_df))))

    ## Convert to data matrix
    gene_matrix <- as.matrix(input_df[, -1])

    ## Get colMax and colMin
    gene_max <- matrixStats::colMaxs(gene_matrix)
    gene_min <- matrixStats::colMins(gene_matrix)

    ## SI and RI for up and down genes
    si_up <- gene_max[names(gene_max) %in% up]
    ri_up <- gene_min[names(gene_min) %in% up]
    si_down <- gene_min[names(gene_min) %in% down]
    ri_down <- gene_max[names(gene_max) %in% down]

    si_vector <- c(si_up, si_down)
    ri_vector <- c(ri_up, ri_down)

    ## Create SI and RI matrix to get max
    si_matrix <- do.call("rbind",
                         replicate(nrow(input_df), si_vector, simplify = FALSE))
    ri_matrix <- do.call("rbind",
                         replicate(nrow(input_df), ri_vector, simplify = FALSE))

    ## Get Summation of euclidean distances
    sigma_s <- matrixStats::rowSums2((gene_matrix - si_matrix) ** 2)
    sigma_r <- matrixStats::rowSums2((gene_matrix - ri_matrix) ** 2)

    ## Euclidean distance of sensitivity model
    dsj <- sqrt(sigma_s)
    ## Euclidean distance of insensitivity model
    drj <- sqrt(sigma_r)
    ratio <- dsj / drj

    ## Predict call
    pred <- ifelse(dsj <= drj, "sensitive", "insensitive")

    result <- data.frame(
        Cells = input_df$X,
        drj = drj,
        dsj = dsj,
        ratio = ratio,
        prediction = pred
    )

    print(result)

    ## Write results
    output_file <- file.path(getwd(), "calc_euclidean.csv")
    message("Writing results to :", output_file)
    write.csv(result,file = output_file)

    ## Print sensitivity count
    cat("sensitive count: ", sum(result$prediction %in% "sensitive"))

}
