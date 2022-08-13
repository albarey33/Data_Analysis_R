
###################################################################.
# SCRIPT: READ EXCEL FILES WITH EQUAL STRUCTURE
# USE CASE: TO BUILD THE MONTHLY ENROLLMENT LIST DATA CSV FILE 
# FOR DATA ANALYSIS OF CARE MANAGEMENT RESULTS
# Data Source: PATIENT HEALTH DASHBOARD (PHD) - POPULATION SUMMARY
# FREQUENCY: Execute this script after enrollment ONCE A MONTH
###################################################################.

# 0 PREPARE INSTALL CALL PACKAGES -----------------------------------------

rm(list=ls()) # Delete all objects
ls() # List variables in memory

options(header=T, sep=",", stringsAsFactors = FALSE, 
        str = strOptions(strict.width = "cut"), 
        readr.show_progress=FALSE)

st <- Sys.time()

# .libPaths("C:/R_Libraries")     # Defined as Environmental Variable
# .libPaths() ###  Check the default folder where packages are installed

# Load-multiple-packages-at-once
required_packages <- c("dplyr", "readxl", "data.table")
lapply(required_packages, library, character.only = TRUE)

# 1 PARAMETERS CHANGE NAMES / UPDATE --------------------------------------------------

currPPL <- "202106"     # Update 

# Paths
# Location of Source files
path            <- "PopulationSamples"       
#resultingfile   <- "data_merging_files//result_table.csv" 

# Location of resulting file ----
resultingfile   <- paste0("PopulationSamples//MergedFile", currPPL, ".csv")

# 2 READ THE DOWNLOADED DATA EXCEL FILES ------------------------------------
# Read sample files using full path and regex (known pattern)

filenames_list <- list.files(path= path, full.names=TRUE, 
                 pattern=paste0("^Patient.*?",currPPL,"_1000pts.xlsx"))

# * 2.1 Change field types ----
# For this particular data, they all have to be either "numeric" or "text", 
# that means all the others has to be changed to this two;
# For this purpose the first file will provide a vector of types
# This is necessary before merging Excel data files and avoid discrepancy of types and warnings
# "There were 50 or more warnings (use warnings() to see the first 50)"
# Some fields are recognized as "logical" although they have to be as "Text" for later processing steps.

# Method with Across - Where
# https://stackoverflow.com/questions/27668266/dplyr-change-many-data-types
# convert all logical type fields to character
# dat %>% mutate(across(where(is.factor), as.character))
vectortypes <- read_excel(filenames_list[1]) %>% 
  mutate(across(where(is.logical), as.character)) %>%   # as.character
  summarise_all(class) %>% slice(1) %>% unlist(., use.names=FALSE)
vectortypes
# Convert character and logical to text
vectortypes[vectortypes %in% c("character", "logical")] <- "text"
#vectortypes[vectortypes == "character" | vectortypes == "logical"] <- "text"  # Option
vectortypes

# * 2.2 Function: Read Excel files with equal structure --------

fx_readfiles <- function(filename){
  print(paste("Merging",filename,sep = " "))
  xlfile <- readxl::read_excel(filename, col_types = vectortypes)
  print(paste("Number of records in ",
              filename," = ",nrow(xlfile),
              " ; columns = ",length(xlfile),sep=""))
  dfXLfile <- data.frame(xlfile)
  dfXLfile 
  }

# Apply defined function to list
tibbles <- lapply(filenames_list,fx_readfiles)

#merged <- do.call(rbind, tibbles) # Alternative
PPL_df <- data.frame(do.call(dplyr::bind_rows, tibbles))

head(PPL_df,1)
dim(PPL_df)
str(PPL_df)

# 3 CHANGE DF FIELD NAMES WITHOUT NUMBERS USING pattern + REGEX ---------------
# PREFIXED WITH X FIELD NAMES AUTOMATICALLY WHEN MERGED 
names(PPL_df) <- sub("X[0-9]{1,2}..","",names(PPL_df))  # Regular expressions
PPL_df$PPL <- currPPL  # Add column current month
str(PPL_df)


# 4 WRITE RESULTING DATA TABLE -------------------------------------------

str(PPL_df)
dim(PPL_df)

data.table::fwrite(PPL_df,resultingfile)

Sys.time() - st

############## END ------ 

