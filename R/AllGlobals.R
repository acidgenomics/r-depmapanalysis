#' Current DepMap (quarterly) release
#'
#' @note Updated 2021-05-19.
#' @noRd
.currentDepMapRelease <- "21Q2"



#' Popular (starred) DepMap file downloads
#'
#' @note Updated 2021-02-10.
#' @noRd
#'
#' @details
#' List of genes identified as dependencies in all lines, one per line:
#'
#' - `achilles_common_essentials.csv`
#' - `achilles_gene_dependency.csv`
#' - `achilles_gene_effect.csv`
#' - `ccle_expression.csv`
#' - `ccle_gene_cn.csv`
#' - `ccle_mutations.csv`
#'
#' List of genes used as positive controls, intersection of Biomen
#' (2014) and Hart (2015) essentials in the format "HUGO (Entrez)". Each
#' entry is separated by a newline.The scores of these genes are used as
#' the dependent distribution for inferring dependency probability:
#'
#' - `common_essentials.csv`
#'
#' List of genes used as negative controls (Hart (2014) nonessentials)
#' in the format "HUGO (Entrez)". Each entry is separated by a newline:
#'
#' - `nonessentials.csv`
#' - `sample_info.csv`
#'
#' @seealso https://depmap.org/portal/download/
.depmap <- list(
    "url_stem" = "https://ndownloader.figshare.com/files/",
    ## CRISPR screens.
    "depmap_public_21q2" = list(
        "achilles_common_essentials.csv" = "27902028",
        "achilles_gene_dependency.csv" = "27902040",
        "achilles_gene_effect.csv" = "27902046",
        "ccle_expression.csv" = "27902091",
        "ccle_gene_cn.csv" = "27902124",
        "ccle_mutations.csv" = "27902118",
        "common_essentials.csv" = "27902160",
        "nonessentials.csv" = "27902370",
        "readme.txt" = "27902373",
        "sample_info.csv" = "27902376"
    ),

    "depmap_public_21q1" = list(
        "achilles_common_essentials.csv" = "26261275",
        "achilles_gene_dependency.csv" = "26261290",
        "achilles_gene_effect.csv" = "26261293",
        "ccle_expression.csv" = "26261476",
        "ccle_gene_cn.csv" = "26261524",
        "ccle_mutations.csv" = "26261527",
        "common_essentials.csv" = "26261545",
        "nonessentials.csv" = "26261557",
        "readme.txt" = "26261566",
        "sample_info.csv" = "26261569"
    ),
    "depmap_public_20q4v2" = list(
        "achilles_common_essentials.csv" = "25770002",
        "achilles_gene_dependency.csv" = "25770032",
        "achilles_gene_effect.csv" = "25770029",
        "ccle_expression.csv" = "25797011",
        "ccle_gene_cn.csv" = "25770017",
        "ccle_mutations.csv" = "25494419",
        "common_essentials.csv" = "25494434",
        "nonessentials.csv" = "25494437",
        "readme.txt" = "25797005",
        "sample_info.csv" = "25494443"
    ),
    "depmap_public_20q4" = list(
        "achilles_common_essentials.csv" = "XXX",
        "achilles_gene_dependency.csv" = "XXX",
        "achilles_gene_effect.csv" = "XXX",
        "ccle_expression.csv" = "XXX",
        "ccle_gene_cn.csv" = "XXX",
        "ccle_mutations.csv" = "XXX",
        "common_essentials.csv" = "XXX",
        "nonessentials.csv" = "XXX",
        ## NOTE "readme.txt"
        "sample_info.csv" = "XXX"
    ),
    "depmap_public_20q3" = list(
        "achilles_common_essentials.csv" = "24613283",
        "achilles_gene_dependency.csv" = "24613298",
        "achilles_gene_effect.csv" = "24613292",
        "ccle_expression.csv" = "24613325",
        "ccle_gene_cn.csv" = "24613352",
        "ccle_mutations.csv" = "24613355",
        "common_essentials.csv" = "24613385",
        "nonessentials.csv" = "24613388",
        ## NOTE "readme.txt"
        "sample_info.csv" = "24613394"
    ),
    ## NOTE depmap_public_20q3
    ## NOTE depmap_public_20q2
    ## NOTE depmap_public_20q1
    ## NOTE depmap_public_19q4
    ## NOTE depmap_public_19q3
    ## NOTE depmap_public_19q2
    ## NOTE depmap_public_19q1
    ## NOTE depmap_public_18q4
    ## NOTE depmap_public_18q3
    ## NOTE depmap_public_18q2
    ## NOTE depmap_public_18q1
    ## RNAi screens.
    "demeter2_data_v6" = list(
        "d2_combined_gene_dep_scores.csv" = "13515395",
        "sample_info.csv" = "11489717"
    )
)



.pkgName <- packageName()
.pkgVersion <- packageVersion(.pkgName)
