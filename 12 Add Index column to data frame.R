
###################################################################.
# SCRIPT: ADD INDEX COLUMN TO DATA FRAME
# USE CASE: To de-identify patients and not showing PHI
###################################################################.

# 0 PREPARE INSTALL CALL PACKAGES -----------------------------------------

rm(list=ls()) # Delete all objects
ls() # List variables in memory

# options(header=T, sep=",", stringsAsFactors = FALSE, 
#         str = strOptions(strict.width = "cut"), 
#         readr.show_progress=FALSE)

st <- Sys.time()

# Load-multiple-packages-at-once
required_packages <- c("dplyr", "stringr")
lapply(required_packages, library, character.only = TRUE)

# 1 PARAMETERS CHANGE NAMES / UPDATE --------------------------------------------------

# Paths # Location of Source files
path            <- "PopulationSamples"       

# 2 READ THE DOWNLOADED DATA EXCEL FILES ------------------------------------
# Read sample files using full path and regex (known pattern)

filenames_list <- list.files(path= path, full.names=TRUE, 
                             pattern=paste0("^MergedFile.*?.csv"))
filenames_list

# Function: Read CSV file showing number of rows and columns

fx_readfiles <- function(filename){
  csvfile <- read.csv(filename)
  print(paste("Number of records in ",filename," = ",nrow(csvfile),
              " ; columns = ",length(csvfile),sep=""))
  dfcsvfile <- data.frame(csvfile)
  dfcsvfile 
}

# Apply defined function to list
PPL_df <- fx_readfiles(filenames_list)
class(PPL_df)
dim(PPL_df)

 
# 3 INDEX UNIQUE ROWS ENROLLMENT  E0000000 ---------

PPL_df$Index <- paste0('E',stringr::str_pad(1:nrow(PPL_df), 7, side="left", pad="0"))

# To move Index field at the left of the data table
PPL_df <- PPL_df %>% select(Index, everything())
head(PPL_df)

str(PPL_df[1:10])

