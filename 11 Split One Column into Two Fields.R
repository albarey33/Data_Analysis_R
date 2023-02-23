
###################################################################.
# SCRIPT: SPLIT ONE COLUMN INTO TWO FIELDS
# USE CASE: One Step to arrange the data frames
###################################################################.

# 0 PREPARE INSTALL CALL PACKAGES -----------------------------------------

rm(list=ls()) # Delete all objects
ls() # List variables in memory

# options(header=T, sep=",", stringsAsFactors = FALSE, 
#         str = strOptions(strict.width = "cut"), 
#         readr.show_progress=FALSE)

st <- Sys.time()

# Load-multiple-packages-at-once
required_packages <- c("dplyr", "readxl", "stringr")
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
head(PPL_df[,c("Medicaid.ID", "Practice...NPI")],10)

# Structure of Fields with "Practice" in their names
str(PPL_df[c(grep("Practice",names(PPL_df)))])

# 3 SPLIT COLUMN PRACTICE + NPI USING DIVIDER ": "  -------

#split(NOMIDNA$Client, ", ")
# str_split_fixed(NOMIDNA$Client, ", ", n = 2)[,1]
# str_split_fixed(NOMIDNA$Client, ", ", n = 2)[,2]

head(str_split_fixed(PPL_df$Practice...NPI, ": ", n = 2),5) # Field to split

dfSplitPracticeNPI <- str_split_fixed(PPL_df$Practice...NPI, ": ", n = 2) 

head(dfSplitPracticeNPI, 15)

dfSplitPracticeNPI<- data.frame(dfSplitPracticeNPI) # Convert matrix to dataframe

# 
names(dfSplitPracticeNPI) <- c("Practice_Name", "Practice_NPI")

# Append resulting two new fields to main data
PPL_df <- PPL_df %>% dplyr::bind_cols(dfSplitPracticeNPI)

str(PPL_df)
class(PPL_df)

# 4 CHECK RESULTING NEW FIELDS IN DATA TABLE ------

str(PPL_df[c(grep("Practice",names(PPL_df)))])

# 5 WRITE RESULTING DATA TABLE -------------------------------------------
# EXPORT SUB-TABLE

OnlyPractices <- PPL_df %>% select(c("Medicaid.ID",
                                     "Practice_Name"))

head(OnlyPractices,10)

resultingfile   <- "PopulationSamples//OnlyPractices.csv" 

data.table::fwrite(OnlyPractices,resultingfile)

Sys.time() - st

############## END ------ 
