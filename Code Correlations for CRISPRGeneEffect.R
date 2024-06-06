# -----------------------------------------------------
# LAB: Emanuele Lab of UNC Lineberger Center
# PROJECT: Calculating Pearson Correlations Between Genes
# SOURCE: DEPMAP Portal
# AUTHOR: Veronica Upadhyay

# PACKAGES
# -----------------------------------------------------
# highlight these and run once at the start of creating the file; comment out
# install.packages("readr")
library(readr) 
# Display information about the loaded packages, if you need it:
# sessionInfo()

# READ FILES
# -----------------------------------------------------
# Highlight + run once at the start of file- creation
# To Import the CSV file:
# Session >> Set Working Directory >> Pick file to save project in
# getwd() # To double checking working directory
# Paste file name into quotations; file should be in wd
CRISPRGeneEffect <-read.csv("/Users/veronicaupadhyay/Desktop/Emanuele Lab/CRISPR_gene_effect.csv",header= TRUE, sep= ",")
# Should show up in top right pane; double click on it to open the csv file in R
ConditionSource <- read.csv("/Users/veronicaupadhyay/Desktop/Emanuele Lab/ExceptionsTeller.csv",header= TRUE, sep= ",")


# ⭐ FILTER Myeloid, Lymphoid
filter_out <- ConditionSource$Depmap.ID[ConditionSource$Lineage %in% c("Myeloid", "Lymphoid")]
# Remember to change names of these omitted- types in output files' names*

# Create a new, filtered, CRISPRGeneEffect 
CRISPRGeneEffect_filtered <- CRISPRGeneEffect[!CRISPRGeneEffect$DepMap_ID %in% filter_out, ]



# ⭐ CALCULATE CORRELATION COEFFICIENTS FOR EACH GENE
# -----------------------------------------------------
# Get total num of cols in CRISPRGeneEffect
num_cols_sourceset <- 2:ncol(CRISPRGeneEffect_filtered)  # 2 cause the first column is just cell-line names
# Data frame of all the genes (columns)
gene_names <- names(CRISPRGeneEffect_filtered[num_cols_sourceset])

# Control Number of Files Produced:
genes_to_process <-gene_names[1:2] 

# ⭐ KEY GENE ITERATION
# nested for in loops: https://www.educba.com/nested-for-loop-in-r/ 
for (gene in genes_to_process) {
# Iterate over each gene name:
  key_gene_value <- as.numeric(CRISPRGeneEffect_filtered[[gene]])
  
# Establish empty vector to hold the correlation coefficients:
  cor_coefs <- vector("numeric", length(num_cols_sourceset))
  
# ⭐ SECONDARY GENE ITERATION  
# Calculate correlation of key_gene_value (main) versus each secondary_gene_values (other)
  for (i in num_cols_sourceset) { # for in loop, so no +=1 needed
    
# Check if either key_gene_value or secondary_gene_values is empty, and skip if true
    if (any(is.na(key_gene_value)) || any(is.na(CRISPRGeneEffect_filtered[[i]]))) {
      cor_coefs[i - 1] <- NA
      next
    }
    
    secondary_gene_values <- CRISPRGeneEffect_filtered[[i]] # double brackets cause its a whole column
    correlation <- cor(key_gene_value, secondary_gene_values, method = "pearson")
    cor_coefs[i - 1] <- correlation # idk why but the -1 is the only thing that parallels it) rn
  }
  
# Data frame for correlation coefficients:
  gene_corcoefs <- data.frame(Gene = gene_names, Correlation = cor_coefs) # titles
  gene_corcoefs$Correlation <- sprintf("%.7f", gene_corcoefs$Correlation) # 7 decimal places
  
# ⭐ MANY MANY CSV FILES
# -----------------------------------------------------
  resulting_files <- paste0(gene, " (Omit: Myeloid, Lymphoid)", " Correlation Coefficients.csv")
# *include names of omited output files!
  write_csv(gene_corcoefs, resulting_files) 
}
