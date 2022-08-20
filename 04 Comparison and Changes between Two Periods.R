
#######################################################################################.
######### COMPARISON OF ENROLLMENT IN TWO MONTHS
# SCRIPT: USE DPLYR TO VIEW AND SUMMARISE SPECIFIC CHANGES
# USE CASE: COMPARE TWO ENROLLMENT MONTHS AND TALLY CHANGES IN ENROLLMENT AND PRACTICES PER PATIENT
# Identify New and Unenrolled Patients and Changes in Practice
# PURPOSE: FOR DATA ANALYSIS OF CARE MANAGEMENT RESULTS
######################################################################################.

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
required_packages <- c("dplyr", "data.table")#, "lubridate", "tidyverse")
lapply(required_packages, library, character.only = TRUE)

# 1 PARAMETERS CHANGE NAMES / UPDATE --------------------------------------------------

# Location of files: Paths / Filenames

path            <- "PopulationSamples"   

# 2 FUNCTIONS -------------------

# Function Convert Data to Money values without commas
 tablex       <- function(x){print(table(x, useNA = 'always'))}
 tablexy      <- function(x,y){print(table(x,y, useNA = 'always'))}

# 3 READ INPUT FILES - TWO MONTHS OF DATA  ----------

# Read csv file
filenames_list <- list.files(path= path, full.names=TRUE, 
                             pattern=paste0("^Enrollment_2mo.*?\\.csv"))
filenames_list

# 4 UPLOAD TWO MONTHS ENROLLMENT DATA ------------------------------------------------

# Function: Show names and open read files
fx_readfiles <- function(filename){
  print(paste("Reading",filename,sep = " "))
  read.csv(filename)}

# Apply defined function to list
dfMonth1 <- data.frame(lapply(filenames_list[1],fx_readfiles))
dfMonth2 <- data.frame(lapply(filenames_list[2],fx_readfiles))

head(dfMonth1,5)
head(dfMonth2,5)

dfMonth1 <- dfMonth1 %>% rename(Practice_Previous_Month = Practice_Name)
dfMonth2 <- dfMonth2 %>% rename(Practice_Current_Month = Practice_Name)


# 5 FULL JOIN AND COMPARE TWO MONTHS --------------------------------------------

comparison <- dfMonth1 %>% full_join(dfMonth2, #by=c('Medicaid.ID'), 
                    by = c('Medicaid.ID','Name','DOB','Age','Gender','Race'))

head(comparison,5)

# All Combinations by Practices
comparison %>% group_by(Practice_Previous_Month, Practice_Current_Month) %>% tally()

comparison$Prev_Month <- ifelse(is.na(comparison$Practice_Previous_Month), 'NewPt', 'Practice')
comparison$Curr_Month <- ifelse(is.na(comparison$Practice_Current_Month), 'Unenrolled', 'Practice')

head(comparison,5)

# Table Recap New and Unenrolled patients
tablexy(comparison$Prev_Month, comparison$Curr_Month)

comparison %>% group_by(Prev_Month, Curr_Month) %>% tally()

# 6 CHECK THE MOST FREQUENT CHANGES OF PRACTICES -----
tchanges <- comparison %>% 
  filter(Practice_Previous_Month != Practice_Current_Month) %>% 
  group_by(Practice_Previous_Month, Practice_Current_Month) %>% 
  tally() %>% arrange(desc(n)) %>% filter(n>0)

tchanges

### Check use of dplyr::setdiff(x, y, ) 
### Rows that appear in x but not y. 
### PtEngDsh <- dplyr::setdiff(PtEngDsh,CCNCNewPrev)  

# 7 IDENTIFY PATIENTS WHO CHANGED PRACTICES (ONE EXAMPLE) -----

comparison %>% 
  dplyr::filter(Practice_Previous_Month == "CAPE FEAR FAMILY MED CARE" & 
                Practice_Current_Month == "CAROLINA URGENT AND FAMILY CARE")


# 8 WRITE RESULTS -------------------------------

# outputfile
# fwrite(AllEnrollees, outputfile)

Sys.time() - st

################## END ----------


