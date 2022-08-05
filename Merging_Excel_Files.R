# Merging Excel Files with readxl ----

rm(list=ls()) # Clean all objects from memory
ls()          # List the current variables in memory

library(readxl)
library(data.table)

# Paths
path            <- "data_merging_files"                             # Location of Source files
merged_file <- "data_merging_files//result_table.csv" # Location of resulting file

# Read sample files using full path and regex
filenames_list <- list.files(path= path, full.names=TRUE, pattern="^PatientList.*?\\.xlsx")
filenames_list

# Function: Show names and open read files
fx_readfiles <- function(filename){
  print(paste("Merging",filename,sep = " "))
  read_excel(filename)}

# Apply defined function to list
tibbles <- lapply(filenames_list,fx_readfiles)

merged <- do.call(rbind, tibbles)
merged

# Write / Export Results to a CSV file
data.table::fwrite(merged,merged_file)
