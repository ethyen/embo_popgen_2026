# 1. read files
FST_NAM_EAS <- read.table("input/Part_1_HumanDiversity/Chr2_NAM_EAS.weir.fst", header = F, skip = 1)
FST_NAM_EUR <- read.table("input/Part_1_HumanDiversity/Chr2_NAM_EUR.weir.fst", header = F, skip = 1)
FST_EUR_EAS <- read.table("input/Part_1_HumanDiversity/Chr2_EUR_EAS.weir.fst", header = F, skip = 1)

# 2. Remove duplicates
FST_NAM_EAS_filter <- FST_NAM_EAS[!duplicated(FST_NAM_EAS$BIN_START), ]
FST_NAM_EUR_filter <- FST_NAM_EUR[!duplicated(FST_NAM_EUR$BIN_START), ]
FST_EUR_EAS_filter <- FST_EUR_EAS[!duplicated(FST_EUR_EAS$BIN_START), ]

# 3. Remove NAs
FST_NAM_EAS_data <- FST_NAM_EAS_filter[!is.na(FST_NAM_EAS_filter$WEIGHTED_FST), ]
FST_NAM_EUR_data <- FST_NAM_EUR_filter[!is.na(FST_NAM_EUR_filter$WEIGHTED_FST), ]
FST_EUR_EAS_data <- FST_EUR_EAS_filter[!is.na(FST_EUR_EAS_filter$WEIGHTED_FST), ]

# 4. Overlap SNPs
overlap_NAM <- FST_NAM_EAS_data[FST_NAM_EAS_data$POS %in% FST_NAM_EUR_data$POS, ]
overlap_NAM_all <- FST_NAM_EAS_data[FST_NAM_EAS_data$POS %in% FST_NAM_EUR_data$POS & FST_NAM_EAS_data$POS %in% FST_EUR_EAS_data$POS, ]

# ...
