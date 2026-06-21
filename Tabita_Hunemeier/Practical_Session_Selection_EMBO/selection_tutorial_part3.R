library(rehh)
CHS <- data2haplohh(hap_file = "input/Part_1_HumanDiversity/Chr2_EDAR_CHS_500K.recode.vcf", 
                 chr.name = 2, 
                 polarize_vcf = FALSE)
LWK <- data2haplohh(hap_file = "input/Part_1_HumanDiversity/Chr2_EDAR_LWK_500K.recode.vcf", 
                    chr.name = 2, 
                    polarize_vcf = FALSE)

CHS_decay <- calc_ehh(CHS, mrk = "rs3827760", include_nhaplo = TRUE)
LWK_decay <- calc_ehh(LWK, mrk = "rs3827760", include_nhaplo = TRUE)

#Plot the EHH decay and furcation trees for both AFR and EAS.
par(mfrow=c(1,2))
plot(CHS_decay)
plot(LWK_decay)

# calc furcation
CHS_furcation <- calc_furcation(CHS, mrk = "rs3827760")
LWK_furcation <- calc_furcation(LWK, mrk = "rs3827760")

# plot furcation
par(mfrow=c(1,2))
plot(CHS_furcation, main = "CHS Furcation")
plot(LWK_furcation, main = "LWK Furcation")

### ==> middle: core haplotype, branches: random variation breaking the haplotype
### ==> fat branches: strong LD, thin branches: weak LD
### ==> we need a recombination map to perform this analyses


#  iHS & XP-EHH (Window-based)

# Perform a genome-wide scan of homozygosity using scan_hh() for AFR and EAS
CHS_scan <- scan_hh(CHS)
LWK_scan <- scan_hh(LWK)

# Calculate iHS scores for both populations using ihh2ihs()
CHS_iHS <- ihh2ihs(CHS_scan)
LWK_iHS <- ihh2ihs(LWK_scan)

# Check the iHS score at rs3827760 and generate a single-site iHS plot in EAS.
plot(LWK_iHS$ihs$POSITION, LWK_iHS$ihs$IHS, col= ifelse(LWK_iHS$ihs$POSITION == 109513601, "red", "black"),pch=13) 

# Create a function to estimate the average absolute iHS in sliding windows (50 SNPs/40 step) and plot the results.
slidefct <- function(data,window,step){
  total <- length(data)
  spots <- seq(from = 1, to = (total-window+1),by = step)
  result <- vector(length = length(spots))
  for (i in 1:length(spots)){
    result[i] <- data[spots[i]]
  }
  return(result)
}

# average iHS over window of 50 SNP with steps of 40 SNPs
mean_iHS <- slidefct(abs(LWK_iHS$ihs$IHS), window = 50, step = 40)

# ...

