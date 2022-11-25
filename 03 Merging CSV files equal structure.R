
#########################################################################.
##################### CARE MANAGEMENT - MONTHLY ENROLLMENT  #############.
# SCRIPT / FUNCTION TO MERGE CSV FILES SHOWING ROWS AND COLUMNS PER FILE
# USE CASE: To collect data from many csv files into one csv file
# FOR DATA ANALYSIS OF CARE MANAGEMENT RESULTS
# Data Source: CSV files
# Example Algorithms: Add field with the source data file name
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
  mytypelist

}

# 3 READ THE DOWNLOADED DATA CSV FILES ------------------------------------

# Read sample files using full path and regex
filenames_list <- list.files(path= path, full.names=TRUE, 
                             pattern=paste0("^Patient.*?",currPPL,"_1000pts.csv"))
filenames_list

# Locate the data month on the name
locPPL <- regexpr(currPPL,filenames_list[1])[1]  # Locate pattern in string

# Apply function
tibbles <- fx_append_csvfiles(filenames_list, locPPL, locPPL+5)

# 4 ADD FIELD WITH THE DATA SOURCE FILE NAME -------------
# Extract name from paths: basename

for( i in seq_along(filenames_list)){
  tibbles[[i]]$FileName <- basename(filenames_list[i]) # list_files[i]
  }

# 5 WRITE RESULTING DATA TABLE -------------------------------------------

df <- data.frame(dplyr::bind_rows(tibbles))
str(df)

Sys.time() - st

#################### END ------ 
