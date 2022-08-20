
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
#path <- "PopulationSamples//DATA_Interactions_by_CM//" # if sub-folder

# Location of resulting file ----
resultingfile   <- paste0("PopulationSamples//MergedFile", currPPL, ".csv")

# 2 READ THE DOWNLOADED DATA EXCEL FILES ------------------------------------
# Read sample files using full path and regex (known pattern)

filenames_list <- list.files(path= path, full.names=TRUE, 
                 pattern=paste0("^Patient.*?",currPPL,"_1000pts.xlsx"))
filenames_list

# * 2.2 Function: Read Excel files with equal structure --------

fx_readfiles <- function(filename){
  print(paste("Merging",filename,sep = " "))
  xlfile <- readxl::read_excel(filename)#, col_types = vectortypes)
  print(paste("Number of records in ",
              filename," = ",nrow(xlfile),
              " ; columns = ",length(xlfile),sep=""))
  xlfile[] <- lapply(xlfile, function(x) {        # Change field types
    if (inherits(x, "logical")) as.character(x) else x
  })
  dfXLfile <- data.frame(xlfile)
  dfXLfile
  }

# Apply defined function to list
tibbles <- lapply(filenames_list,fx_readfiles)
tibbles
str(tibbles[1])

#merged <- do.call(rbind, tibbles) # Alternative
PPL_df <- data.frame(do.call(dplyr::bind_rows, tibbles))

head(PPL_df,1)
dim(PPL_df)
str(PPL_df)

# 3 CHANGE DF FIELD NAMES WITHOUT NUMBERS USING pattern + REGEX ---------------
# PREFIXED WITH X FIELD NAMES AUTOMATICALLY WHEN MERGED 
names(PPL_df) <- sub("X[0-9]{1,2}..","",names(PPL_df))  # Regular expressions

# 4 Add column current month ---------------
PPL_df$PPL <- currPPL  
str(PPL_df)

# 5 WRITE RESULTING DATA TABLE -------------------------------------------

str(PPL_df)
dim(PPL_df)

data.table::fwrite(PPL_df,resultingfile)

Sys.time() - st

# NOTES: 
#CM_List <- read_excel("t_VH_Assigned_CM_in_Care_Team.xlsm", 
#                      sheet='CC4C_OB_CMs', range="K7:K60")

############## END ------ 

