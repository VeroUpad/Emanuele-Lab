# -----------------------------------------------------
# LAB: Emanuele Lab of UNC Lineberger Center
# PROJECT: Calculating Pearson Correlations Between Genes
# SOURCE: DEPMAP Portal
# MAIN FILE: NF2 CRISPR (DepMap Public 23Q2+Score Chronos).csv
# AUTHOR: Veronica Upadhyay

# PACKAGES
# -----------------------------------------------------
# highlight these and run once at the start of creating the file; comment out then
# install.packages("readr")
library(readr) 
# install.packages("openxlsx")
library(openxlsx)

# Display information about the loaded packages, if you need it:
# sessionInfo()

# READ MAIN FILE
# -----------------------------------------------------
# highlight these and run once at the start of creating the file
# To Import the CSV file:
# Session >> Set Working Directory >> Choose the file you want to save your project in
# getwd() # For double checking your working directory
# Paste file name into quotations; file should be in wd
DataSource<-read.csv("NF2 CRISPR (DepMap Public 23Q2+Score Chronos).csv",header= TRUE, sep= ",")
# Should show up in top right pane; double click on it to open the csv file in R





# ⭐ CALCULATE CORRELATION COEFFICIENTS FOR EACH GENE
# -----------------------------------------------------
# Get total number of columns in DataSource:
num_cols_sourceset<-2:ncol(DataSource)  # 2 cause the first column is just cell-line names
# Data frame of all the columns aka genes:
gene_names <-names(DataSource[num_cols_sourceset]) 

# VERY IMPORTANT: Just so the program doesn't crash, this line of code makes sure that
# only a set number of Excel files are at a time. 17000 Excel files at once would be
# hard, so try maybe 300 at a time?
genes_to_process <-gene_names[1:5] 

# ⭐ KEY GENE ITERATION
# guide on nested for in loops: https://www.educba.com/nested-for-loop-in-r/ 

for (gene in genes_to_process) {
  # itterate over each gene name:
  key_gene_value<- as.numeric(DataSource[[gene]])
  # have the as numeric here, in case the data is in like char form or something
  
  # Initialize empty vector to hold the corr coefs:
  cor_coefs<- vector("numeric", length(num_cols_sourceset))
  
  # ⭐ SECONDARY GENE ITERATION  
  # calculate corr coefs of key_gene_value (main) versus each secondary_gene_values (other)
  for (i in num_cols_sourceset) { # for in loop, so no +=1 needed
    secondary_gene_values<-as.numeric(DataSource[[i]]) # remember: double brackets cause its a whoel column
    correlation <-cor(key_gene_value, secondary_gene_values, method= "pearson")
    cor_coefs[i-1]<-correlation # -1 is the only thing that makes it work (parralels it) rn
  }
  
  #  make  data frame for cor coefs:
  gene_corcoefs<-data.frame(Gene= gene_names, Correlation= cor_coefs) # titles
  gene_corcoefs$Correlation<-sprintf("%.7f", gene_corcoefs$Correlation) # 7 decimal places
  
# ⭐ MANY MANY CSV FILES
# -----------------------------------------------------
# using csv files instead of excel cause it might lighten the load on my system
  
# resulting_files <- paste0(gene, " Correlation Coefficients.csv")
# write_csv(gene_corcoefs, resulting_files) 
  
# }

# ⭐ MANY MANY EXCEL FILES 
# -----------------------------------------------------
#  about making Excel files: http://www.sthda.com/english/wiki/r-xlsx-package-a-quick-start-guide-to-manipulate-excel-files-in-r
# IF YOU WANNA USE THIS AGAIN, REMEMBER THE XLSX PACKAGES HAS TO BE DOWNLOADED

# make a new workbook
# you're usign workbooks because youre makign Excel files; workbooks are like a 
# "copy" of excel file in the r file
wb<-createWorkbook()

# put a worksheet in the notebook
addWorksheet(wb, "Correlation Co")

# write data to the worksheet
writeData(wb, sheet= "Correlation Co", x= gene_corcoefs, startRow= 1, startCol= 1)

# save  workbook as an Excel file
file_name <- paste0(gene, " Correlation Coefficients.xlsx")
saveWorkbook(wb, file_name, overwrite= TRUE)
}
# TO DO: Maybe incorporate a way for these to be saved into folders by Alphabet
