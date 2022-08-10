
###################################################################.
############### CARE MANAGEMENT - MONTHLY ENROLLMENT  #############.
# SCRIPT: READ EXCEL FILES WITH EQUAL STRUCTURE
# USE CASE: TO BUILD THE MONTHLY ENROLLMENT LIST DATA CSV FILE 
# From PATIENT HEALTH DASHBOARD (PHD) - POPULATION SUMMARY
# Execute this script after enrollment ONCE A MONTH
# This report contains Demographic, Condition, Utilization & Cost information
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
required_packages <- c("dplyr", "readxl")
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

# Change field types ----
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

# Function: Read Excel files with equal structure

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

# 4 CHANGE DF FIELD NAMES WITHOUT NUMBERS USING REGEX -----------------------
# PREFIXED WITH X FIELD NAMES AUTOMATICALLY WHEN MERGED 
names(PPL_df) <- sub("X[0-9]{1,2}..","",names(PPL_df))  # Regular expressions
PPL_df$PPL <- currPPL  # Add column current month
str(PPL_df)


# 4 WRITE RESULTING DATA TABLE -------------------------------------------

str(PPL_df)
dim(PPL_df)

fwrite(PPL_df,resultingfile)

Sys.time() - st

########################### END ------ 


# 5 SPLIT COLUMN PRACTICE + NPI -----------------------------

dfSplitPracticeNPI <- str_split_fixed(PPL_df$Practice...NPI, ": ", n = 2) 
head(dfSplitPracticeNPI, 15)
dfSplitPracticeNPI<- data.frame(dfSplitPracticeNPI)
names(dfSplitPracticeNPI) <- c("Practice_Name", "Practice_NPI")
PPL_df <- PPL_df %>% bind_cols(dfSplitPracticeNPI)

# 6 SPLIT COLUMN PROVIDER + NPI ---------------------------------------------

dfSplitProviderNPI <- str_split_fixed(PPL_df$.Provider...NPI, ": ", n = 2) 
class(dfSplitProviderNPI)
dfSplitProviderNPI<- data.frame(dfSplitProviderNPI)
names(dfSplitProviderNPI) <- c("Provider_Name", "Provider_NPI")
PPL_df <- PPL_df %>% bind_cols(dfSplitProviderNPI)

# 7 RENAME PCP PROVIDERS  ------------------------------
# Renaming some Providers (Special cases) Change values

# * 7.1 FUNCTION CHANGE PRACTICE NAMES -------
fx_change_Practice_Names <- function(PCP_field_name, df, dfChanges){
  LocPCP <- match(PCP_field_name,names(df))
  for(i in 1:nrow(dfChanges)){
    print(paste0("Records with ",dfChanges[i,1],": ", df %>% 
                   filter(df[ ,LocPCP] == dfChanges[i,1]) %>% tally()))
    df[ ,LocPCP][df[ ,LocPCP] == dfChanges[i,1]] <- dfChanges[i,2]
    print(paste0("  -------->   CHANGED TO :", dfChanges[i,2]))
  }
  df
}

# * 7.2 Create a table with name changes (hardcoding) -----
PCPch <- data.frame(OriginalName= character(0), ChangedName= character(0))
str(PCPch)
PCPch[nrow(PCPch)+1,] <- c("CAPE FEAR FAMILY MEDICAL CARE 1340 WALTER REED",
                           "CAPE FEAR FAMILY MED CARE")
PCPch[nrow(PCPch)+1,] <- c("CAPE FEAR FAMILY MEDICAL CARE 413 OWEN DR", 
                           "CAPE FEAR FAMILY MED CARE")
PCPch[nrow(PCPch)+1,] <- c("CAPE FEAR FAMILY MEDICAL CARE 405 OWEN DR", 
                           "CAPE FEAR FAMILY MED CARE")
PCPch[nrow(PCPch)+1,] <- c("CAROLINA RHEUMATOLOGY AND INTERNAL MEDICINE", 
                           "CAPE FEAR VALLEY PRIMARY CARE - JOHN SMITH")
PCPch[nrow(PCPch)+1,] <- c("WOMEN'S HEALTH HAVEN", 
                           "CFVMC DIVISION OF DUKE OB/GYN")
PCPch[nrow(PCPch)+1,] <- c("FAYETTEVILLE GERIATRIC & INTERNAL MEDICINE",
                           "CAROLINA PRIMARY & INTERNAL MEDICINE")
PCPch[nrow(PCPch)+1,] <- c("WADE FAMILY MEDICAL CENTER",
                           "STEDMAN - WADE HEALTH SERVICES")
PCPch[nrow(PCPch)+1,] <- c("CAPE FEAR VALLEY PRIMARY CARE - SKIBO ROAD",
                           "CAPE FEAR VALLEY PRIMARY CARE - FAYETTEVILLE FAMILY")
PCPch
str(PCPch)

# * 7.3 Apply change of names ----

PPL_df <- fx_change_Practice_Names("Practice_Name", PPL_df, PCPch)

# 8 APPLY CHANGES TO GROUPS OF FIELDS -----

# * 8.1 COST - CONVERT DATA FROM STRING TO NUMERIC VALUES --------------

# Location of Cost fields

str(PPL_df)
# First Column related to cost
FirstCost <- match('Total.Healthcare.Cost..Last.12.mos.....', names(PPL_df))
FirstCost    # Location of First Column related to cost
# Last Cost related to cost
LastCost <- match('Capitation.Cost..Last.12.mos....', names(PPL_df)) 
LastCost    # Location of Last Column related to cost
paste0("Range of Cost info : from ",FirstCost," TO: ", LastCost)
str(PPL_df[,FirstCost:LastCost])

# Apply fx to Convert Cost in text with commas "$0,000" to numeric
fx_convmoney <- function(x){as.numeric(gsub("[\\$,]", "", x))}
PPL_df[,FirstCost:LastCost] <- lapply(PPL_df[,FirstCost:LastCost], 
                                      fx_convmoney )
str(PPL_df[,FirstCost:LastCost])
str(PPL_df)

# * 8.2 MEDICAL CONDITIONS: REPLACE BLANK OR NULL  FOR NO -------------

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
PPL_df[,RangeConditions][is.na(PPL_df[,RangeConditions])] <- "No"
# PPL_df[,RangeConditions][PPL_df[,RangeConditions] == ""] <- "No"
head(PPL_df,3)[,RangeConditions]
str(PPL_df[,RangeConditions])
str(PPL_df)
grep('Dual', names(PPL_df))
grep('Dual', names(PPL_df), value = T)
PPL_df[,'Dual.Medicare.Medicaid.Eligible'][is.na(PPL_df[,'Dual.Medicare.Medicaid.Eligible'])] <- "No"
PPL_df[,'Dual.Medicare.Medicaid.Eligible'][PPL_df[,'Dual.Medicare.Medicaid.Eligible'] == 'YES'] <- "Yes"

PPL_df[,'.ABD'][PPL_df[,'.ABD'] == 'Non-ABD'] <- "No"
PPL_df[,'.ABD'][PPL_df[,'.ABD'] == 'ABD'] <- "Yes"

names(PPL_df)
PPL_df$.ABD   # Verify Yes, No
grep('Dual', names(PPL_df), value = T)
tablex(PPL_df$Dual.Medicare.Medicaid.Eligible)

# 9 RENAME / CHANGE OF CONDITION FIELD NAMES ---------------
str(PPL_df)

PPL_df <- PPL_df %>% dplyr::rename(
  Name = Patient.Name, 
  ABD = .ABD,
  Any.MH = Any.Mental.Health.Condition, 
  Chr.GastroInt.Dis = Chronic.GI.Disease,
  Chr.Kidney.Dis = Chronic.Kidney.Disease,
  Chr.Liver.Dis = Chronic.Liver.Disease,
  Chr.Neurological.Dis = Chronic.Neurological.Disease,
  Dev.Disab = Developmental.Disability,
  Dual = Dual.Medicare.Medicaid.Eligible,
  Eating.Dis = Eating.Disorder,
  History.Myoc.Inf = History.of.Myocardial.Infarction,
  Ischemic.Vascular.Dis = Ischemic.Vascular.Disease,
  Musculoskeletal.Connective.Tissue.Dis = Musculoskeletal.Disease,
  Personality = Personality.Disorders
)

#paste(names(PPL_df), collapse = ", ")

# 10 DELETE NOT NEEDED FIELDS -------------------------------------------

dim(PPL_df)
PPL_df <- PPL_df %>% dplyr::select(-c('Practice...NPI', 
                               '.Provider...NPI', 
                               'Care.Manager.Phone.Number', 
                               '.Dual.Eligibility'))

# 11 WRITE RESULTING DATA TABLE -------------------------------------------

str(PPL_df)
dim(PPL_df)

fwrite(PPL_df,resultingfile)

Sys.time() - st

########################### END ------ 
