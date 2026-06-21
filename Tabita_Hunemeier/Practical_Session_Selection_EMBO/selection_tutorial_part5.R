# here: don't need phased data, but need to polarized

# 1. Load the normalized SNP-level XP-nSL scores from input/Part_2_CanidDiversity/xpnsl_phased.xpnsl.out.norm
XPscores <- read.table("input/Part_2_CanidDiversity/xpnsl_phased.xpnsl.out.norm", header = T)

# 2. Load the 100 Kb window-level data from input/Part_2_CanidDiversity/xpnsl_phased.xpnsl.out.norm.100kb.windows
windows <- read.table("input/Part_2_CanidDiversity/xpnsl_phased.xpnsl.out.norm.100kb.windows", header = T)

# Highlight the IGF1 region (between 41 Mb and 45.5 Mb).
IGF1_region <- windows[windows$BIN_START >= 41000000 & windows$BIN_START < 45500000, ]

# Generate two Manhattan plots: one for the raw SNP-level scores (calculating -log10(p-value) from the normalized Z-scores), and another for the window-based fraction of extreme positive SNPs (frac_top).