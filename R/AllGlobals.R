.pkgName <- packageName()
.pkgVersion <- packageVersion(.pkgName)



#' DepMap Figshare URL file download stem
#'
#' @note Updated 2021-06-09.
#' @noRd
.urlStem <- "https://ndownloader.figshare.com/files"



#' DepMap dataset URLs
#'
#' @note Updated 2021-06-08.
#' @noRd
#'
#' @section Combination of Broad and Sanger scoring data:
#'
#' Combined Achilles and Sanger SCORE Chronos data using Harmonia. This batch
#' correction pipeline is described in:
#' https://www.biorxiv.org/content/10.1101/2020.05.22.110247v3.
#'
#' @section Chronos vs. CERES scoring:
#'
#' Chronos was added as the recommended scoring method over CERES in the 2021
#' Q2 update.
#'
#' Blog post describing difference:
#' https://cancerdatascience.org/blog/posts/ceres-chronos/
#'
#' Pre-print manuscript:
#' https://www.biorxiv.org/content/10.1101/2021.02.25.432728v1.abstract
#'
#' @section Descriptions of file downloads:
#'
#' Harmonia data (Broad Achilles and Sanger Project Score combined);
#' post-Chronos scoring (current preferred method):
#'
#' - `crispr_common_essentials_chronos.csv`:
#'       List of genes identified as dependencies in all lines, one per line.
#' - `crispr_gene_dependency_chronos.csv`:
#'       Probability that knocking out the gene has a real depletion effect
#'       using 'CRISPR_gene_effect_Chronos'.
#'       - Columns: genes in the format "HUGO (Entrez)".
#'       - Rows: cell lines (Broad IDs).
#' - `crispr_gene_effect_chronos.csv`:
#'       Combined Achilles and Sanger SCORE Chronos data using Harmonia.
#'       - Columns: genes in the format "HUGO (Entrez)".
#'       - Rows: cell lines (Broad IDs).
#'
#' Harmonia data (Broad Achilles and Sanger Project Score combined);
#' post-CERES scoring (deprecated in favor of Chronos):
#'
#' - `crispr_common_essentials.csv`:
#'       List of genes identified as dependencies in all lines, one per line.
#' - `crispr_gene_dependency.csv`:
#'       Probability that knocking out the gene has a real depletion effect
#'       using 'CRISPR_gene_effect'.
#'       - Columns: genes in the format "HUGO (Entrez)".
#'       - Rows: cell lines (Broad IDs).
#' - `crispr_gene_effect.csv`:
#'       Combined Achilles and Sanger SCORE Chronos data using Harmonia.
#'       - Columns: genes in the format "HUGO (Entrez)".
#'       - Rows: cell lines (Broad IDs).
#'
#' Achilles data (deprecated in favor of Harmonia);
#' post-Chronos scoring:
#'
#' - `achilles_common_essentials_chronos.csv`:
#'       List of genes identified as pan-essentials using Chronos.
#' - `achilles_gene_dependency_chronos.csv`:
#'       Probability that knocking out the gene has a real depletion effect
#'       using 'gene_effect_Chronos'.
#'       - Columns: genes in the format "HUGO (Entrez)".
#'       - Rows: cell lines (Broad IDs).
#' - `achilles_gene_effect_chronos.csv`:
#'       Chronos data, copy number corrected.
#'       - Columns: genes in the format "HUGO (Entrez)".
#'       - Rows: cell lines (Broad IDs).
#'
#' Achilles data (deprecated in favor of Harmonia);
#' post-CERES scoring (deprecated in favor of Chronos):
#'
#' - `achilles_common_essentials.csv`:
#'       List of genes identified as dependencies in all lines, one per line.
#' - `achilles_gene_dependency.csv`:
#'       Probability that knocking out the gene has a real depletion effect
#'       using 'gene_effect'.
#'       - Columns: genes in the format "HUGO (Entrez)".
#'       - Rows: cell lines (Broad IDs).
#' - `achilles_gene_effect.csv`:
#'       CERES data with principle components strongly related to known batch
#'       effects removed, then shifted and scaled per cell line so the median
#'       nonessential KO effect is 0 and the median essential KO effect is -1.
#'       - Columns: genes in the format "HUGO (Entrez)".
#'       - Rows: cell lines (Broad IDs).
#'
#' Pre-CERES (and Chronos) scoring control files:
#'
#' - `common_essentials.csv`:
#'       List of genes used as positive controls, intersection of
#'       Biomen (2014) and Hart (2015) essentials in the format "HUGO (Entrez)".
#'       Each entry is separated by a newline. The scores of these genes are
#'       used as the dependent distribution for inferring dependency
#'       probability.
#' - `nonessentials.csv`:
#'       List of genes used as negative controls (Hart (2014) nonessentials)
#'       in the format "HUGO (Entrez)". Each entry is separated by a newline.
#'
#' RNAi screening with DEMETER2 scoring:
#'
#' - `d2_combined_gene_dep_scores.csv`:
#'       Source: Broad Institute, Novartis, Marcotte et al.
#'       - 17309 Genes
#'       - 712 Cell Lines
#'       - 31 Primary Diseases
#'       - 31 Lineages
#'
#' CCLE files:
#'
#' - `ccle_expression.csv`:
#'       RNAseq TPM gene expression data for just protein coding genes using
#'       RSEM. Log2 transformed, using a pseudo-count of 1.
#'       - Rows: cell lines (Broad IDs).
#'       - Columns: genes (HGNC symbol and Entrez ID).
#' - `ccle_gene_cn.csv`:
#'       Gene level copy number data, log2 transformed with a pseudo count
#'       of 1. This is generated by mapping genes onto the segment level calls.
#'       - Rows: cell lines (Broad IDs).
#'       - Columns: genes (HGNC symbol and Entrez ID).
#' - `ccle_mutations.csv`:
#'       MAF of gene mutations. For all columns with AC, the allelic ratio is
#'       presented as [ALTERNATE:REFERENCE].
#'       Columns:
#'       - CGA_WES_AC:
#'             the allelic ratio for this variant in all our WES/WGS(exon only)
#'             using a cell line adapted version of the 2019 CGA pipeline that
#'             includes germline filtering.
#'       - SangerWES_AC:
#'             in Sanger WES (called by sanger) (legacy).
#'       - SangerRecalibWES_AC:
#'             in Sanger WES after realignment at Broad (legacy).
#'       - RNAseq_AC:
#'             in Broad RNAseq data from the CCLE2 project (legacy).
#'       - HC_AC:
#'             in Broad Hybrid capture data from the CCLE2 project (legacy).
#'       - RD_AC: in Broad Raindance data from the CCLE2 project (legacy).
#'       - legacy_wgs_exon_only:
#'             in Broad WGS data from the CCLE2 project (legacy).
#'       - isTCGAhotspot:
#'             is this mutation commonly found in TCGA.
#'       - TCGAhsCnt:
#'             number of times this mutation is observed in TCGA.
#'       - isCOSMIChotspot:
#'             is this mutation commonly found in COSMIC.
#'       - COSMIChsCnt:
#'             number of samples in COSMIC with this mutation.
#'       - ExAC_AF:
#'             the allelic frequency in the Exome Aggregation
#'             Consortium (ExAC).
#'       Descriptions of the remaining columns in the MAF can be found here:
#'       https://docs.gdc.cancer.gov/Data/File_Formats/MAF_Format/
#'
#' Other files:
#'
#' - `readme.txt`:
#'       Description of all files contained in this release.
#' - `sample_info.csv`:
#'       Cell line information definitions:
#'       - [1] "DepMap_ID"
#'                 Static primary key assigned by DepMap to each cell line.
#'       - [2] "cell_line_name"
#'       - [3] "stripped_cell_line_name"
#'                  Cell line name with alphanumeric characters only.
#'       - [4] "CCLE_Name"
#'                  Previous naming system that used the stripped cell line name
#'                  followed by the lineage; no longer assigned to new cell
#'                  lines.
#'       - [5] "alias"
#'                  Additional cell line identifiers (not a comprehensive list).
#'       - [6] "COSMICID"
#'                  Cell line ID used in Cosmic cancer database
#'       - [7] "sex"
#'                  Sex of tissue donor if known.
#'       - [8] "source"
#'                  Source of cell line vial used by DepMap.
#'       - [9] "Achilles_n_replicates"
#'                 Number of replicates used in Achilles CRISPR screen
#'                 passing QC.
#'      -  [10] "cell_line_NNMD"
#'                 Difference in the means of positive and negative controls
#'                 normalized by the standard deviation of the negative control
#'                 distribution.
#'      -  [11] "culture_type"
#'                 Growth pattern of cell line (Adherent, Suspension, Mixed
#'                 adherent and suspension, 3D, or Adherent (requires
#'                 laminin coating)).
#'      -  [12] "culture_medium"
#'                 Medium used to grow cell line.
#'      -  [13] "cas9_activity"
#'                 Percentage of cells remaining GFP positive on days 12-14 of
#'                 cas9 activity assay as measured by FACS.
#'      -  [14] "RRID"
#'                 Cellosaurus research resource identifier.
#'      -  [15] "WTSI_Master_Cell_ID"
#'      -  [16] "sample_collection_site"
#'                 Tissue collection site.
#'      -  [17] "primary_or_metastasis"
#'                 Indicates whether tissue sample is from primary or
#'                 metastatic site.
#'      -  [18] "primary_disease"
#'                 General cancer lineage category.
#'      -  [19] "Subtype"
#'                 Subtype of disease; specific disease name.
#'      -  [20] "age"
#'                 If known, age of tissue donor at time of sample collection.
#'      -  [21] "Sanger_Model_ID"
#'                 Sanger Institute Cell Model Passport ID.
#'      -  [22] "depmap_public_comments"
#'      -  [23] "lineage"
#'      -  [24] "lineage_subtype"
#'      -  [25] "lineage_sub_subtype"
#'      -  [26] "lineage_molecular_subtype"
#'                  Cancer type classifications in a standardized form.
#'
#' @seealso
#' - https://depmap.org/portal/download/
.datasets <- list(
    ## CRISPR screens.
    "depmap_public_21q2" = list(
        "combined" = list(
            "ceres" = list(
                "crispr_common_essentials.csv" = 27902163L,
                "crispr_gene_dependency.csv" = 27902169L,
                "crispr_gene_effect.csv" = 27902226L
            ),
            "chronos" = list(
                "crispr_common_essentials_chronos.csv" = 27902166L,
                "crispr_gene_dependency_chronos.csv" = 27902175L,
                "crispr_gene_effect_chronos.csv" = 27902229L
            )
        ),
        "achilles" = list(
            "ceres" = list(
                "achilles_common_essentials.csv" = 27902028L,
                "achilles_gene_dependency.csv" = 27902040L,
                "achilles_gene_effect.csv" = 27902046L
            ),
            "chronos" = list(
                "achilles_common_essentials_chronos.csv" = 27902031L,
                "achilles_gene_dependency_chronos.csv" = 27902049L,
                "achilles_gene_effect_chronos.csv" = 27902043L
            )
        ),
        "controls" = list(
            "common_essentials.csv" = 27902160L,
            "nonessentials.csv" = 27902370L
        ),
        "ccle" = list(
            "ccle_expression.csv" = 27902091L,
            "ccle_gene_cn.csv" = 27902124L,
            "ccle_mutations.csv" = 27902118L
        ),
        "metadata" = list(
            "readme.txt" = 27902373L,
            "sample_info.csv" = 27902376L
        )
    ),
    "depmap_public_21q1" = list(
        "combined" = list(
            "ceres" = list(
                "crispr_common_essentials.csv" = 26261551L,
                "crispr_gene_dependency.csv" = 26261554L,
                "crispr_gene_effect.csv" = 26261563L
            )
        ),
        "achilles" = list(
            "ceres" = list(
                "achilles_common_essentials.csv" = 26261275L,
                "achilles_gene_dependency.csv" = 26261290L,
                "achilles_gene_effect.csv" = 26261293L
            )
        ),
        "controls" = list(
            "common_essentials.csv" = 26261545L,
            "nonessentials.csv" = 26261557L
        ),
        "ccle" = list(
            "ccle_expression.csv" = 26261476L,
            "ccle_gene_cn.csv" = 26261524L,
            "ccle_mutations.csv" = 26261527L
        ),
        "metadata" = list(
            "readme.txt" = 26261566L,
            "sample_info.csv" = 26261569L
        )
    ),
    "depmap_public_20q4v2" = list(
        "achilles" = list(
            "ceres" = list(
                "achilles_common_essentials.csv" = 25770002L,
                "achilles_gene_dependency.csv" = 25770032L,
                "achilles_gene_effect.csv" = 25770029L
            )
        ),
        "controls" = list(
            "common_essentials.csv" = 25494434L,
            "nonessentials.csv" = 25494437L
        ),
        "ccle" = list(
            "ccle_expression.csv" = 25797011L,
            "ccle_gene_cn.csv" = 25770017L,
            "ccle_mutations.csv" = 25494419L
        ),
        "metadata" = list(
            "readme.txt" = 25797005L,
            "sample_info.csv" = 25494443L
        )
    ),
    "depmap_public_20q3" = list(
        "achilles" = list(
            "ceres" = list(
                "achilles_common_essentials.csv" = 24613283L,
                "achilles_gene_dependency.csv" = 24613298L,
                "achilles_gene_effect.csv" = 24613292L
            )
        ),
        "controls" = list(
            "common_essentials.csv" = 24613385L,
            "nonessentials.csv" = 24613388L
        ),
        "ccle" = list(
            "ccle_expression.csv" = 24613325L,
            "ccle_gene_cn.csv" = 24613352L,
            "ccle_mutations.csv" = 24613355L
        ),
        "metadata" = list(
            "readme.txt" = 24613391L,
            "sample_info.csv" = 24613394L
        )
    ),
    "depmap_public_20q2" = list(
        "achilles" = list(
            "ceres" = list(
                "achilles_common_essentials.csv" = 22629059L,
                "achilles_gene_dependency.csv" = 22629071L,
                "achilles_gene_effect.csv" = 22629068L
            )
        ),
        "controls" = list(
            "common_essentials.csv" = 22629128L,
            "nonessentials.csv" = 22629131L
        ),
        "ccle" = list(
            ## This references the "CCLE_expression_v2.csv" file.
            "ccle_expression.csv" = 22897976L,
            "ccle_gene_cn.csv" = 22629107L,
            "ccle_mutations.csv" = 22629110L
        ),
        "metadata" = list(
            ## This references the "README_v2.txt" file.
            "readme.txt" = 22897970L,
            "sample_info.csv" = 22629137L
        )
    ),
    "depmap_public_20q1" = list(
        "achilles" = list(
            "ceres" = list(
                "achilles_common_essentials.csv" = 21521865L,
                ## This references the "Achilles_gene_dependency_v2.csv" file.
                "achilles_gene_dependency.csv" = 22543826L,
                ## This references the "Achilles_gene_effect_v2.csv" file.
                "achilles_gene_effect.csv" = 22543691L
            )
        ),
        "controls" = list(
            "common_essentials.csv" = 21521991L,
            "nonessentials.csv" = 21521994L
        ),
        "ccle" = list(
            "ccle_expression.csv" = 21521940L,
            "ccle_gene_cn.csv" = 21521964L,
            "ccle_mutations.csv" = 21521967L
        ),
        "metadata" = list(
            ## This references the "README_v2.txt" file.
            "readme.txt" = 22543694L,
            ## This references the "sample_info_v2.csv" file.
            "sample_info.csv" = 21522000L
        )
    ),
    ## RNAi screens.
    "demeter2_data_v6" = list(
        "combined" = list(
            "demeter2" = list(
                "d2_combined_gene_dep_scores.csv" = 13515395L
            )
        ),
        "achilles" = list(
            "demeter2" = list(
                "d2_achilles_gene_dep_scores.csv" = 11489669L
            )
        ),
        "drive" = list(
            "demeter2" = list(
                "d2_drive_gene_dep_scores.csv" = 11489693L
            )
        ),
        "metadata" = list(
            "readme.txt" = 13515380L,
            "sample_info.csv" = 11489717L
        )
    )
)



.formalsList <- list(
    "dataset" = names(.datasets),
    "depmapDataset" = grep(
        pattern = "^depmap_",
        x = names(.datasets),
        value = TRUE
    )
)
