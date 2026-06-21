# Load eigenvalues and eigenvectors from PLINK PCA output 
eigenvector <- read.table("input/Part_2_CanidDiversity/plink_pca.eigenvec", header = FALSE)
eigenvalues <- read.table("input/Part_2_CanidDiversity/plink_pca.eigenval", header = FALSE)
metadata <- read.table("input/Part_2_CanidDiversity/sample_info.txt", header = TRUE, sep = "\t")

#Merge them with the sample metadata (input/Part_2_CanidDiversity/sample_info.txt)
PCA_data <- merge(eigenvector, eigenvalues, by = 0)
PCA_data <- merge(PCA_data, metadata, by.x = "V2", by.y = "sampleName")

# Calculate the percentage of variance explained by PC1 and PC2
PC1_explaned <- (eigenvalues$V1[1] / sum(eigenvalues$V1)) * 100
PC2_explaned <- (eigenvalues$V1[2] / sum(eigenvalues$V1)) * 100

# Generate a scatter plot of PC1 vs. PC2, coloring the points by breed and shaping them by size group (small vs. large)

library(ggplot2)

PCA_plot <- ggplot(PCA_data, aes(x = V3, y = V4, color = breed, shape = group)) +
  geom_point(size = 3) +
  labs(x = paste0("PC1 (", round(PC1_explaned, 2), "%)"), 
       y = paste0("PC2 (", round(PC2_explaned, 2), "%)"), 
       title = "PCA of Canid Diversity") +
  theme_minimal() +
  theme(legend.title = element_blank())



# 4 Genomic outlier detection using PCAdapt

library(pcadapt)

# ...

# important: filter LD
# advantages of pcadapt: 
# - if you don't know much about the species
# - no assuptions
# ==> test deviation of neutrality: not directly selection

