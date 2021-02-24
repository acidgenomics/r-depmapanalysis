library(usethis)
ach <- Achilles(rowData = FALSE, colData = FALSE)
ach <- ach[seq_len(100L), seq_len(50L)]
use_data(ach, overwrite = TRUE)
