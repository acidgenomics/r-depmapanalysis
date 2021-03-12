## DepMapAnalysis 0.0.6 (2021-03-12)

### Minor changes

- Updated basejump dependencies, and removed now unnecessary stringr import.

## DepMapAnalysis 0.0.5 (2021-02-25)

### Major changes

- Improved metadata support for some CCLE functions, returning as
  `SummarizedExperiment` instead of `DataFrame`, when possible.

## DepMapAnalysis 0.0.4 (2021-02-10)

### Major changes

- Added support for 21Q1, 20Q4 v2, and 20Q4 releases.
- Switched `Achilles` object to inherit from `SummarizedExperiment` instead of
  `RangedSummarizedExperiment`.
- Gene metadata now comes from NCBI FTP server.
- `DEMETER2` class objects now support `colData` and `rowData`.

## DepMapAnalysis 0.0.3 (2020-10-07)

### Minor changes

- `Achilles`: Gene synonyms are now fetched automatically, based on the new
  `makeGRangesFromEnsembl` update in basejump.
- Simplified internal BiocFileCache handling, using new `cacheURL` function
  defined in pipette package.

## DepMapAnalysis 0.0.2 (2020-10-02)

### New functions

- `plotGeneEffect`: Plot effect of gene knockdown as a boxplot or density plot.

### Major changes

- `Achilles` object now requires slotting of common essential genes from DepMap.
  These are now imported automatically in the main `Achilles()` call.

## DepMapAnalysis 0.0.1 (2020-10-02)

Initial release.
