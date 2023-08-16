## OncoTree ontology mappings
##
## @note Updated 2023-08-16.
## @seealso
## - https://github.com/cBioPortal/oncotree/tree/master/scripts/
##     ontology_to_ontology_mapping_tool
library(pipette)
ontologyMappings <- import("ontologyMappings.tsv")
use_data(ontologyMappings, overwrite = TRUE, internal = TRUE)
