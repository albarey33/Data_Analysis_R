
###################################################################.
# SCRIPT: COUNT OF ENROLLEES BY TYPE OF PRACTICES
# For this example, the table Practices-Type is imported
# USE CASE: One Step to arrange the data frames
###################################################################.

# 0 PREPARE INSTALL CALL PACKAGES -----------------------------------------

rm(list=ls()) # Delete all objects
ls() # List variables in memory

options(header=T, sep=",", stringsAsFactors = FALSE, 
        str = strOptions(strict.width = "cut"), 
        readr.show_progress=FALSE)

st <- Sys.time()

# Load-multiple-packages-at-once
required_packages <- c("dplyr", "readxl", "stringr", "data.table")
lapply(required_packages, library, character.only = TRUE)

# 1 PARAMETERS CHANGE NAMES / UPDATE --------------------------------------------------

# Paths # Location of Source files
path            <- "PopulationSamples"
listadoexport            <- "PopulationSamples//listadoexp.csv"

# 2 READ THE DOWNLOADED DATA EXCEL FILES ------------------------------------
# Read sample files using full path and regex (known pattern)

filenames_list <- list.files(path= path, full.names=TRUE, 
                 pattern=paste0("^OnlyPractices.*?.csv"))
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

dim(PPL_df)

# Enrolles by Practice
# listado <- PPL_df %>% group_by(Practice_Name) %>% tally() %>% arrange(desc(n))
# listado

# 3 IMPORT TABLE PRACTICES TYPE  ------------------------------

practices_type <- list.files(path= path, full.names=TRUE, 
                             pattern=paste0("^Practices_Type.*?.csv"))

practices_type <- fx_readfiles(practices_type)

head(practices_type)

# 4 MERGE TABLES - LEFT JOIN  ------

PPL_df <- PPL_df %>% left_join(practices_type, by = c('Practice_Name' = 'Practice.Name'))

# 5 COUNT OF ENROLLEES BY PRACTICE TYPE  ------

PPL_df %>% group_by(Practice.Type) %>% tally() %>% arrange(desc(n))

Sys.time() - st

########################### END ------ 
