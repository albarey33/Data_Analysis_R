
###################################################################.
# SCRIPT: APPLY CHANGES TO A GROUP OF FIELDS BY BATCH
# Example: Batch changes: Example, Cost fields, Medical conditions
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

# 3 FUNCTIONS  ------------------------------------
tablex       <- function(x){print(table(x, useNA = 'always'))}
tablexy      <- function(x,y){print(table(x,y, useNA = 'always'))}

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
head(PPL_df)


################################################################################.
# 4 APPLY CHANGES TO GROUPS OF FIELDS - MEDICAL CONDITIONS ----

str(PPL_df)

# * 4.1 MEDICAL CONDITIONS: IDENTIFY RANGE -------------

# First Condition Column: Any.Mental.Health.Condition
First_Cond_Col <- match("Any.Mental.Health.Condition",names(PPL_df))
First_Cond_Col  # Location of First field related to Condition
# Last Sickle Cell
Last_Cond_Col <- match("Sickle.Cell",names(PPL_df))
Last_Cond_Col   # Location of Last field related to Condition
paste0("Range of Conditions: From ",First_Cond_Col," TO: ", Last_Cond_Col)
str(PPL_df[,First_Cond_Col:Last_Cond_Col])
RangeConditions <- c(First_Cond_Col:Last_Cond_Col)
str(PPL_df[,RangeConditions])

# * 4.2 APPLY CHANGES: Replace values NAs or Blanks ("") by "No" -------------

PPL_df[,RangeConditions][PPL_df[,RangeConditions]==""] <- "No"
PPL_df[,RangeConditions][is.na(PPL_df[,RangeConditions])] <- "No"
head(PPL_df,3)[,RangeConditions]
str(PPL_df[,RangeConditions])

# * 4.3 Replace values in other fields only --------
grep('Dual', names(PPL_df), value = T)

tablex(PPL_df$Dual.Medicare.Medicaid.Eligible)   # Verify Yes, No

PPL_df <- PPL_df %>% rename("Dual" = "Dual.Medicare.Medicaid.Eligible") %>% 
                      mutate(Dual = case_when(Dual == ""    ~ "No", 
                                              Dual == "YES" ~ "Yes"))
tablex(PPL_df$Dual)

# Other Option
# PPL_df[,'Dual'][PPL_df[,'Dual']==""] <- "No"          
# PPL_df[,'Dual'][PPL_df[,'Dual'] == 'YES'] <- "Yes"
# tablex(PPL_df$Dual)

tablex(PPL_df$.ABD)   # Verify Yes, No
PPL_df <- PPL_df %>% mutate(.ABD = case_when(.ABD == "Non-ABD"  ~ "No", 
                                             .ABD == "ABD"      ~ "Yes"))
tablex(PPL_df$.ABD)   # Verify Yes, No

# PPL_df[,'.ABD'][PPL_df[,'.ABD'] == 'Non-ABD'] <- "No"
# PPL_df[,'.ABD'][PPL_df[,'.ABD'] == 'ABD'] <- "Yes"

str(PPL_df)

str(PPL_df %>% select(c("Dual", RangeConditions)))

# 5 WRITE RESULTING DATA TABLE -------------------------------------------

# fwrite(PPL_df,resultingfile)
Sys.time() - st

########################### END ------ 
