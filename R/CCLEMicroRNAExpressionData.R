#' Import CCLE microRNA expression data
#'
#' @details
#' NanoString microRNA panel data.
#'
#' @export
#' @note Updated 2022-11-16.
#'
#' @return `CCLEMicroRNAExpressionData`.
#'
#' @examples
#' object <- CCLEMicroRNAExpressionData()
#' dim(object)
CCLEMicroRNAExpressionData <- # nolint
    function() {
        url <- pasteURL(
            "depmap.org",
            "portal",
            "download",
            "api",
            paste0(
                "download?",
                "file_name", "=",
                "ccle%2Fccle_2018%2FCCLE_miRNA_20180525.gct",
                "&",
                "bucket", "=",
                "depmap-external-downloads"
            ),
            protocol = "https"
        )
        tmpfile <- .cacheURL(url)
        assay <- import(
            con = tmpfile,
            format = "gct",
            return = "matrix",
            quiet = FALSE
        )
        ## Need to reimport to map NanoString identifiers back to actual
        ## microRNA names.
        df <- import(
            con = tmpfile,
            format = "gct",
            return = "data.frame",
            quiet = TRUE
        )
        assert(isSubset(c("Name", "Description"), colnames(df)))
        df <- df[, c("Name", "Description")]
        ## Import miRBase annotations as rowRanges.
        url <- pasteURL(
            "www.mirbase.org",
            "ftp",
            "CURRENT",
            "genomes",
            "hsa.gff3",
            protocol = "https"
        )
        tmpfile <- .cacheURL(url)
        rowRanges <- import(tmpfile)
        assert(
            is(rowRanges, "GenomicRanges"),
            identical(
                x = colnames(mcols(rowRanges)),
                y = c(
                    "source",
                    "type",
                    "score",
                    "phase",
                    "ID",
                    "Alias",
                    "Name",
                    "Derives_from"
                )
            ),
            hasDuplicates(mcols(rowRanges)[["Name"]])
        )
        ## Need to correct "hsa-mir" to "hsa-miR".
        mcols(rowRanges)[["Name"]] <- gsub(
            pattern = "\\bmir\\b",
            replacement = "miR",
            x = mcols(rowRanges)[["Name"]]
        )
        f <- as.factor(vapply(
            X = strsplit(
                x = mcols(rowRanges)[["Name"]],
                split = "-",
                fixed = TRUE
            ),
            FUN = function(x) {
                paste(x[1L:3L], collapse = "-")
            },
            FUN.VALUE = character(1L)
        ))
        rowRangesList <- split(rowRanges, f = f)
        assert(is(rowRangesList, "GenomicRangesList"))
        keep <- df[["Description"]] %in% names(rowRangesList)
        df[["Description"]][!keep]
        ## microRNA mismatches...
        ##   [1] "hsa-miR-103"
        ##   [2] "hsa-miR-106a+hsa-miR-17"
        ##   [3] "hsa-miR-1201"
        ##   [4] "hsa-miR-1225-3p"
        ##   [5] "hsa-miR-1245"
        ##   [6] "hsa-miR-1254"
        ##   [7] "hsa-miR-1259"
        ##   [8] "hsa-miR-125a-3p"
        ##   [9] "hsa-miR-125a-5p"
        ##  [10] "hsa-miR-1260"
        ##  [11] "hsa-miR-1268"
        ##  [12] "hsa-miR-1269"
        ##  [13] "hsa-miR-127-3p"
        ##  [14] "hsa-miR-127-5p"
        ##  [15] "hsa-miR-1274a"
        ##  [16] "hsa-miR-1274b"
        ##  [17] "hsa-miR-1280"
        ##  [18] "hsa-miR-129-3p"
        ##  [19] "hsa-miR-129-5p"
        ##  [20] "hsa-miR-1308"
        ##  [21] "hsa-miR-139-5p"
        ##  [22] "hsa-miR-140-3p"
        ##  [23] "hsa-miR-140-5p"
        ##  [24] "hsa-miR-142-3p"
        ##  [25] "hsa-miR-142-5p"
        ##  [26] "hsa-miR-146b-3p"
        ##  [27] "hsa-miR-146b-5p"
        ##  [28] "hsa-miR-147"
        ##  [29] "hsa-miR-151-3p"
        ##  [30] "hsa-miR-151-5p"
        ##  [31] "hsa-miR-181b+hsa-miR-181d"
        ##  [32] "hsa-miR-188-3p"
        ##  [33] "hsa-miR-188-5p"
        ##  [34] "hsa-miR-190"
        ##  [35] "hsa-miR-193a-3p"
        ##  [36] "hsa-miR-193a-5p"
        ##  [37] "hsa-miR-1973"
        ##  [38] "hsa-miR-1974"
        ##  [39] "hsa-miR-1975"
        ##  [40] "hsa-miR-1977"
        ##  [41] "hsa-miR-1978"
        ##  [42] "hsa-miR-1979"
        ##  [43] "hsa-miR-199a-3p+hsa-miR-199b-3p"
        ##  [44] "hsa-miR-199a-5p"
        ##  [45] "hsa-miR-199b-5p"
        ##  [46] "hsa-miR-203"
        ##  [47] "hsa-miR-20a+hsa-miR-20b"
        ##  [48] "hsa-miR-219-1-3p"
        ##  [49] "hsa-miR-219-2-3p"
        ##  [50] "hsa-miR-219-5p"
        ##  [51] "hsa-miR-220a"
        ##  [52] "hsa-miR-220b"
        ##  [53] "hsa-miR-220c"
        ##  [54] "hsa-miR-28-3p"
        ##  [55] "hsa-miR-28-5p"
        ##  [56] "hsa-miR-296-3p"
        ##  [57] "hsa-miR-296-5p"
        ##  [58] "hsa-miR-299-3p"
        ##  [59] "hsa-miR-299-5p"
        ##  [60] "hsa-miR-323-3p"
        ##  [61] "hsa-miR-323-5p"
        ##  [62] "hsa-miR-324-3p"
        ##  [63] "hsa-miR-324-5p"
        ##  [64] "hsa-miR-330-3p"
        ##  [65] "hsa-miR-330-5p"
        ##  [66] "hsa-miR-331-3p"
        ##  [67] "hsa-miR-331-5p"
        ##  [68] "hsa-miR-337-3p"
        ##  [69] "hsa-miR-337-5p"
        ##  [70] "hsa-miR-338-5p"
        ##  [71] "hsa-miR-339-3p"
        ##  [72] "hsa-miR-339-5p"
        ##  [73] "hsa-miR-342-3p"
        ##  [74] "hsa-miR-342-5p"
        ##  [75] "hsa-miR-34c-3p"
        ##  [76] "hsa-miR-34c-5p"
        ##  [77] "hsa-miR-361-3p"
        ##  [78] "hsa-miR-361-5p"
        ##  [79] "hsa-miR-362-3p"
        ##  [80] "hsa-miR-362-5p"
        ##  [81] "hsa-miR-365"
        ##  [82] "hsa-miR-369-3p"
        ##  [83] "hsa-miR-369-5p"
        ##  [84] "hsa-miR-371-3p"
        ##  [85] "hsa-miR-371-5p"
        ##  [86] "hsa-miR-378"
        ##  [87] "hsa-miR-409-3p"
        ##  [88] "hsa-miR-409-5p"
        ##  [89] "hsa-miR-423-3p"
        ##  [90] "hsa-miR-423-5p"
        ##  [91] "hsa-miR-450b-3p"
        ##  [92] "hsa-miR-450b-5p"
        ##  [93] "hsa-miR-451"
        ##  [94] "hsa-miR-453"
        ##  [95] "hsa-miR-455-3p"
        ##  [96] "hsa-miR-455-5p"
        ##  [97] "hsa-miR-483-3p"
        ##  [98] "hsa-miR-483-5p"
        ##  [99] "hsa-miR-485-3p"
        ## [100] "hsa-miR-485-5p"
        ## [101] "hsa-miR-486-3p"
        ## [102] "hsa-miR-490-3p"
        ## [103] "hsa-miR-490-5p"
        ## [104] "hsa-miR-491-3p"
        ## [105] "hsa-miR-491-5p"
        ## [106] "hsa-miR-499-3p"
        ## [107] "hsa-miR-499-5p"
        ## [108] "hsa-miR-500+hsa-miR-501-5p"
        ## [109] "hsa-miR-501-3p"
        ## [110] "hsa-miR-508-3p"
        ## [111] "hsa-miR-508-5p"
        ## [112] "hsa-miR-509-3-5p"
        ## [113] "hsa-miR-509-3p"
        ## [114] "hsa-miR-509-5p"
        ## [115] "hsa-miR-512-3p"
        ## [116] "hsa-miR-512-5p"
        ## [117] "hsa-miR-513a-3p"
        ## [118] "hsa-miR-513a-5p"
        ## [119] "hsa-miR-514"
        ## [120] "hsa-miR-515-3p"
        ## [121] "hsa-miR-515-5p"
        ## [122] "hsa-miR-516a-3p"
        ## [123] "hsa-miR-516a-5p"
        ## [124] "hsa-miR-517c+hsa-miR-519a"
        ## [125] "hsa-miR-518d-3p"
        ## [126] "hsa-miR-519b-3p"
        ## [127] "hsa-miR-519c-3p"
        ## [128] "hsa-miR-520a-3p"
        ## [129] "hsa-miR-520a-5p"
        ## [130] "hsa-miR-520c-3p"
        ## [131] "hsa-miR-520d-3p"
        ## [132] "hsa-miR-520d-5p+hsa-miR-527+hsa-miR-518a-5p"
        ## [133] "hsa-miR-524-3p"
        ## [134] "hsa-miR-525-3p"
        ## [135] "hsa-miR-525-5p"
        ## [136] "hsa-miR-526a+hsa-miR-518d-5p+hsa-miR-520c-5p"
        ## [137] "hsa-miR-532-3p"
        ## [138] "hsa-miR-532-5p"
        ## [139] "hsa-miR-542-3p"
        ## [140] "hsa-miR-542-5p"
        ## [141] "hsa-miR-544"
        ## [142] "hsa-miR-548a-3p"
        ## [143] "hsa-miR-548a-5p"
        ## [144] "hsa-miR-548b-3p"
        ## [145] "hsa-miR-548b-5p"
        ## [146] "hsa-miR-548c-5p"
        ## [147] "hsa-miR-548d-3p"
        ## [148] "hsa-miR-548d-5p"
        ## [149] "hsa-miR-549"
        ## [150] "hsa-miR-550"
        ## [151] "hsa-miR-556-3p"
        ## [152] "hsa-miR-556-5p"
        ## [153] "hsa-miR-566"
        ## [154] "hsa-miR-574-3p"
        ## [155] "hsa-miR-574-5p"
        ## [156] "hsa-miR-576-3p"
        ## [157] "hsa-miR-576-5p"
        ## [158] "hsa-miR-582-3p"
        ## [159] "hsa-miR-582-5p"
        ## [160] "hsa-miR-590-3p"
        ## [161] "hsa-miR-590-5p"
        ## [162] "hsa-miR-615-3p"
        ## [163] "hsa-miR-615-5p"
        ## [164] "hsa-miR-628-5p"
        ## [165] "hsa-miR-642"
        ## [166] "hsa-miR-644"
        ## [167] "hsa-miR-654-3p"
        ## [168] "hsa-miR-654-5p"
        ## [169] "hsa-miR-663"
        ## [170] "hsa-miR-664"
        ## [171] "hsa-miR-671-3p"
        ## [172] "hsa-miR-671-5p"
        ## [173] "hsa-miR-720"
        ## [174] "hsa-miR-767-3p"
        ## [175] "hsa-miR-767-5p"
        ## [176] "hsa-miR-769-3p"
        ## [177] "hsa-miR-769-5p"
        ## [178] "hsa-miR-770-5p"
        ## [179] "hsa-miR-875-3p"
        ## [180] "hsa-miR-875-5p"
        ## [181] "hsa-miR-876-3p"
        ## [182] "hsa-miR-876-5p"
        ## [183] "hsa-miR-885-3p"
        ## [184] "hsa-miR-885-5p"
        ## [185] "hsa-miR-886-3p"
        ## [186] "hsa-miR-886-5p"
        ## [187] "bkv-miR-B1-3p+jcv-miR-J1-3p"
        ## [188] "bkv-miR-B1-5p"
        ## [189] "ebv-miR-BART1-3p"
        ## [190] "ebv-miR-BART1-5p"
        ## [191] "ebv-miR-BART10"
        ## [192] "ebv-miR-BART11-3p"
        ## [193] "ebv-miR-BART11-5p"
        ## [194] "ebv-miR-BART12"
        ## [195] "ebv-miR-BART13"
        ## [196] "ebv-miR-BART14"
        ## [197] "ebv-miR-BART15"
        ## [198] "ebv-miR-BART16"
        ## [199] "ebv-miR-BART17-3p"
        ## [200] "ebv-miR-BART17-5p"
        ## [201] "ebv-miR-BART18-3p"
        ## [202] "ebv-miR-BART18-5p"
        ## [203] "ebv-miR-BART19-3p"
        ## [204] "ebv-miR-BART19-5p"
        ## [205] "ebv-miR-BART2-3p"
        ## [206] "ebv-miR-BART2-5p"
        ## [207] "ebv-miR-BART20-3p"
        ## [208] "ebv-miR-BART20-5p"
        ## [209] "ebv-miR-BART21-3p"
        ## [210] "ebv-miR-BART21-5p"
        ## [211] "ebv-miR-BART22"
        ## [212] "ebv-miR-BART3"
        ## [213] "ebv-miR-BART4"
        ## [214] "ebv-miR-BART5"
        ## [215] "ebv-miR-BART6-3p"
        ## [216] "ebv-miR-BART6-5p"
        ## [217] "ebv-miR-BART7"
        ## [218] "ebv-miR-BART8"
        ## [219] "ebv-miR-BART9"
        ## [220] "ebv-miR-BHRF1-1"
        ## [221] "ebv-miR-BHRF1-2"
        ## [222] "ebv-miR-BHRF1-3"
        ## [223] "hcmv-miR-UL112"
        ## [224] "hcmv-miR-UL148D"
        ## [225] "hcmv-miR-UL22A"
        ## [226] "hcmv-miR-UL36"
        ## [227] "hcmv-miR-UL70-3p"
        ## [228] "hcmv-miR-UL70-5p"
        ## [229] "hcmv-miR-US25-1"
        ## [230] "hcmv-miR-US25-2-3p"
        ## [231] "hcmv-miR-US25-2-5p"
        ## [232] "hcmv-miR-US33-3p"
        ## [233] "hcmv-miR-US33-5p"
        ## [234] "hcmv-miR-US4"
        ## [235] "hcmv-miR-US5-1"
        ## [236] "hcmv-miR-US5-2"
        ## [237] "hiv1-miR-H1"
        ## [238] "hiv1-miR-N367"
        ## [239] "hiv1-miR-TAR-3p"
        ## [240] "hsv1-miR-H1"
        ## [241] "hsv1-miR-H2-3p"
        ## [242] "hsv1-miR-H2-5p"
        ## [243] "hsv1-miR-H3"
        ## [244] "hsv1-miR-H4-3p"
        ## [245] "hsv1-miR-H4-5p"
        ## [246] "hsv1-miR-H7"
        ## [247] "hsv1-miR-H8"
        ## [248] "hsv2-miR-H2"
        ## [249] "hsv2-miR-H3"
        ## [250] "hsv2-miR-H4-3p"
        ## [251] "hsv2-miR-H4-5p"
        ## [252] "kshv-miR-K12-10a+kshv-miR-K12-10b"
        ## [253] "kshv-miR-K12-11"
        ## [254] "kshv-miR-K12-12"
        ## [255] "kshv-miR-K12-2"
        ## [256] "kshv-miR-K12-3"
        ## [257] "kshv-miR-K12-4-3p"
        ## [258] "kshv-miR-K12-4-5p"
        ## [259] "kshv-miR-K12-5"
        ## [260] "kshv-miR-K12-6-3p"
        ## [261] "kshv-miR-K12-6-5p"
        ## [262] "kshv-miR-K12-7"
        ## [263] "kshv-miR-K12-8"
        ## [264] "kshv-miR-K12-9"
        ## [265] "mcv-miR-M1-3p"
        ## [266] "mcv-miR-M1-5p"





        ## Censor values that are a combination of multple microRNAs.
        keep <- !grepl(
            pattern = "+",
            x = df[["Description"]],
            fixed = TRUE
        )


        TRUE
    }
