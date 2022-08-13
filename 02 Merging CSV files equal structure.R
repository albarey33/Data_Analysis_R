
#########################################################################.
##################### CARE MANAGEMENT - MONTHLY ENROLLMENT  #############.
# SCRIPT / FUNCTION TO MERGE CSV FILES SHOWING ROWS AND COLUMNS PER FILE
# USE CASE: TO BUILD THE MONTHLY ENROLLMENT LIST DATA CSV FILE 
# FOR DATA ANALYSIS OF CARE MANAGEMENT RESULTS
# Data Source: PATIENT HEALTH DASHBOARD (PHD) - POPULATION SUMMARY
# FREQUENCY: Execute this script after enrollment ONCE A MONTH
###################################################################.

# 0 PREPARE INSTALL CALL PACKAGES -----------------------------------------

rm(list=ls()) # Delete all objects
ls() # List variables in memory

st <- Sys.time()

# Load-multiple-packages-at-once
 required_packages <- c("dplyr") # , "data.table", "tidyverse")
 lapply(required_packages, library, character.only = TRUE)

# 1 PARAMETERS CHANGE NAMES / UPDATE --------------------------------------------------

currPPL <- "202106"     # Update 

# Paths
# Location of Source files
path            <- "PopulationSamples"       

# 2 FUNCTIONS -------------------

######### FUNCTION APPEND PPL CSV FILES  <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
fx_append_csvfiles <- function(list_of_csv_files, PPLini, PPLfin){
  mytypelist <- list() # Creation of empty list
  for(i in seq_along(list_of_csv_files)){
    df.list <- lapply(list_of_csv_files[i], read.csv)  ######### Convert list to Data frame 
    df.list[[1]]$PPL <- substr(list_of_csv_files[i], PPLini, PPLfin)
    print(paste(i," - Number of records in ",list_of_csv_files[i]," = ",
                nrow(df.list[[1]])," ; columns = ",length(df.list[[1]]),sep=""))
    mytypelist <- append(mytypelist, df.list)
  }
  df <- data.frame(dplyr::bind_rows(mytypelist))
}

# 3 READ THE DOWNLOADED DATA CSV FILES ------------------------------------

# Read sample files using full path and regex
filenames_list <- list.files(path= path, full.names=TRUE, 
                             pattern=paste0("^Patient.*?",currPPL,"_1000pts.csv"))
filenames_list

# Locate the data month on the name
locPPL <- regexpr(currPPL,filenames_list[1])[1]  # Locate pattern in string

# Apply function
df_PPL_PHD <- fx_append_csvfiles(filenames_list, locPPL, locPPL+5)

#head(df_PPL_PHD,1)
dim(df_PPL_PHD)
#str(df_PPL_PHD,1)

# 4 WRITE RESULTING DATA TABLE -------------------------------------------
# 
# Resulting file same as Procedure "01 Merging Excel files with equal structure.R"

Sys.time() - st

#################### END ------ 

