# Import FST files
AFR_EAS_FST <- read.table("./input/Part_1_HumanDiversity/AFR_EAS.weir.fst",sep = "\t", header = T)
AFR_EUR_FST <- read.table("./input/Part_1_HumanDiversity/AFR_EUR.weir.fst",sep = "\t", header = T)
EAS_EUR_FST <- read.table("./input/Part_1_HumanDiversity/EAS_EUR.weir.fst",sep = "\t", header = T)


# Filter the FST files to remove duplicated row
AFR_EAS_FST_filtered <- AFR_EAS_FST[!duplicated(AFR_EAS_FST),]
AFR_EUR_FST_filtered <- AFR_EUR_FST[!duplicated(AFR_EUR_FST),]
EAS_EUR_FST_filtered <- EAS_EUR_FST[!duplicated(EAS_EUR_FST),]

# Filter the FST files to remove rows with NA values in the FST column
AFR_EAS_FST_filtered <- AFR_EAS_FST_filtered[!is.na(AFR_EAS_FST_filtered[,3]),]
AFR_EUR_FST_filtered <- AFR_EUR_FST_filtered[!is.na(AFR_EUR_FST_filtered[,3]),]
EAS_EUR_FST_filtered <- EAS_EUR_FST_filtered[!is.na(EAS_EUR_FST_filtered[,3]),]

# Merge the filtered FST files based on the first two columns (chromosome and position)
FST_merged <- merge(AFR_EAS_FST_filtered, AFR_EUR_FST_filtered, by = c("CHROM", "POS"))
FST_merged <- merge(FST_merged, EAS_EUR_FST_filtered, by = c("CHROM", "POS"))
names(FST_merged) <- c("CHROM", "POS", "FST_AFR_EAS", "FST_AFR_EUR", "FST_EAS_EUR")

# Set negative FST values to zero
FST_merged_positive <- FST_merged
FST_merged_positive[FST_merged < 0] <- 0

# Check Fst values at position 109513601
FST_merged_positive[FST_merged_positive$POS == 109513601, ]

# Calculate distribution quantiles to determine POS 109513601  is an outlier
outlier_fct <- function(x) {
  Q1 <- quantile(x, 0.25)
  Q3 <- quantile(x, 0.75)
  IQR <- Q3 - Q1
  outliers_threshold <- Q3 + 1.5 * IQR
  return(outliers_threshold)
}
outliers_threshold_EUR_EAS <- outlier_fct(FST_merged_positive$FST_EAS_EUR)
outliers_threshold_AFR_EAS <- outlier_fct(FST_merged_positive$FST_AFR_EAS)
outliers_threshold_AFR_EUR <- outlier_fct(FST_merged_positive$FST_AFR_EUR)

FST_merged_positive[FST_merged_positive$POS == 109513601, ] > outliers_threshold

# Plot pairwise Fst around 109513601 in a 10kb window, highlighting the candidate SNP

upper_range <-  109513601 + 5000
lower_range <- 109513601 - 5000

par(mfrow = c(1, 3))

for (pop in c("AFR_EAS", "AFR_EUR", "EAS_EUR")) {
  
  plot(FST_merged_positive$POS, FST_merged_positive[[paste0("FST_",fst_plot)]], 
       main = paste("Pairwise Fst:", pop), 
       xlab = "Position", ylab = "Fst", 
       xlim = c(upper_range, lower_range), 
       ylim = c(0, max(FST_merged_positive[[paste0("FST_",fst_plot)]], na.rm = TRUE)))
  
  # Highlight the candidate SNP at position 109513601
  points(109513601, FST_merged_positive[FST_merged_positive$POS == 109513601, fst_plot], 
         col = "red", pch = 16)
  # Put outlier treshold as a horizonal dashed line
  abline(h = get(paste0("outliers_threshold_",pop)), col = "blue", lty = 2)
  
}






# Answer to the questions

#1. monomorphic sites, missing data, ...
#2. yes, EUR vs EAS 
#3. less rare alleles and more polymorphic sites
#4 doesn't show direction
#5: direction
