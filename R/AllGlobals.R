#' Current DepMap (quarterly) release
#'
#' @note Updated 2020-09-30.
#' @noRd
.currentRelease <- "20Q3"



#' Current DEMETER2 release
#'
#' @note Updated 2020-10-01.
#' @noRd
.currentDEMETERRelease <- "demeter2_data_v6"



#' Popular (starred) DepMap file downloads
#'
#' @note Updated 2020-10-01.
#' @noRd
#'
#' @seealso https://depmap.org/portal/download/
.depmap <- list(
    "url_stem" = "https://ndownloader.figshare.com/files/",
    ## CRISPR screens.
    "20q3" = list(
        ## List of genes identified as dependencies in all lines, one per line.
        "achilles_common_essentials.csv" = "24613283",
        "achilles_gene_dependency.csv" = "24613298",
        "achilles_gene_effect.csv" = "24613292",
        "ccle_expression.csv" = "24613325",
        "ccle_gene_cn.csv" = "24613352",
        "ccle_mutations.csv" = "24613355",
        ## List of genes used as positive controls, intersection of Biomen
        ## (2014) and Hart (2015) essentials in the format "HUGO (Entrez)". Each
        ## entry is separated by a newline.The scores of these genes are used as
        ## the dependent distribution for inferring dependency probability.
        "common_essentials.csv" = "24613385",
        ## List of genes used as negative controls (Hart (2014) nonessentials)
        ## in the format "HUGO (Entrez)". Each entry is separated by a newline.
        "nonessentials.csv" = "24613388",
        "sample_info.csv" = "24613394"
    ),
    ## RNAi screens.
    "demeter2_data_v6" = list(
        "d2_combined_gene_dep_scores.csv" = "13515395",
        "sample_info.csv" = "11489717"
    )
)



#' Current package versions
#'
#' @note Updated 2020-10-01.
#' @noRd
.version <- packageVersion(packageName())
