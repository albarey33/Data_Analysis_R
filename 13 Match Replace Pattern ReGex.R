
###################################################################.
# SCRIPT: REGULAR EXPRESSIONS UTILIZED IN SCRIPTS
# Example: Change old names of Practices by new ones defined in table
# USE CASE: One Step to arrange the data frames
###################################################################.

# Substrings of a Character Vector
# Extract or replace substrings in a character vector.

df <- data.frame(Zip = c("28377-0000","28306-0000","28314-0000",
                         "28342-0000","28303-0000","27332-0000"))
df

substr(df$Zip,1,5)


# grep - Pattern Matching and Replacement 
x <- "$12,543.43"
as.numeric(gsub("[\\$,]", "", x))

df <- data.frame(Int_Date = c("12/9/2019 12:00:00 AM",
                              "2/20/2020 12:00:00 AM",
                              "5/4/2020 12:00:00 AM"))
gsub('([0-9]+) .*', '\\1', df$Int_Date)
sub(" 12:00:00 AM","",df$Int_Date)

# Values with a string in pattern
grep("2020",df$Int_Date, value = T )

# Remove comma in 1000 value
df <- data.frame(Score = c("1,000","702","785","843","757"))
as.integer(gsub("[\\,]", "", df$Score))





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
# 4 OTHER CHANGES TO GROUPS OF FIELDS - COST FIELDS ----

# * 4.1 COST - CHECK RANGE --------------

# Location of Cost fields

str(PPL_df)

# Relocate (move) fields to the beginning
PPL_df <- PPL_df %>% relocate(Medicaid.ID, Patient.Name)

PPL_df$Primary.Care.Manager


tablex(nchar(PPL_df$DOB))
# tablex(nchar(PPL_df$DOB)) <- as.character(PPL_df$DOB, "%m/%d/%Y") # FIX
# as.character(PPL_df$DOB, "%m/%d/%Y")
# as.character(PPL_df$DOB, "%d/%m/%Y")

PPL_df %>% summarise(EDVs = sum(ED.Visits..Last.12.mos., na.rm = T))
PPL_df %>% summarise(IPVs = sum(Inpatient.Admissions..Last.12.mos., na.rm = T))


# First Column related to cost
FirstCost <- match('Total.Healthcare.Cost..Last.12.mos.....', names(PPL_df))
FirstCost    # Location of First Column related to cost
# Last Cost related to cost
LastCost <- match('Capitation.Cost..Last.12.mos....', names(PPL_df)) 
LastCost    # Location of Last Column related to cost
paste0("Range of Cost info : from ",FirstCost," TO: ", LastCost)
str(PPL_df[,FirstCost:LastCost])

# * 4.2 COST - CONVERT DATA FROM STRING TO NUMERIC VALUES --------------

# Apply fx to Convert Cost in text with commas "$0,000" to numeric
fx_convmoney <- function(x){as.numeric(gsub("[\\$,]", "", x))}
PPL_df[,FirstCost:LastCost] <- lapply(PPL_df[,FirstCost:LastCost], 
                                      fx_convmoney )
str(PPL_df[,FirstCost:LastCost])

PPL_df %>% group_by(Gender) %>% summarise(Cost = sum(Total.Healthcare.Cost..Last.12.mos.....))
PPL_df %>% group_by(Gender) %>% tally()

# 5 APPLY CHANGES TO GROUPS OF FIELDS - MEDICAL CONDITIONS ----

str(PPL_df)

# * 5.1 MEDICAL CONDITIONS: IDENTIFY RANGE -------------

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

# * 5.2 APPLY CHANGES: Replace values NAs or Blanks ("") by "No" -------------

PPL_df[,RangeConditions][PPL_df[,RangeConditions]==""] <- "No"
PPL_df[,RangeConditions][is.na(PPL_df[,RangeConditions])] <- "No"
head(PPL_df,3)[,RangeConditions]
str(PPL_df[,RangeConditions])

# * 5.3 Replace values in other fields only --------
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


                                
# Change from Family, First to FIRST FAMILY names ---------------
# PriPro_Cx$Primary.Care.Manager <- toupper(sub("(\\w+),\\s(\\w+)","\\2 \\1", PriPro_Cx$Primary.Care.Manager))



# 6 WRITE RESULTING DATA TABLE -------------------------------------------

# fwrite(PPL_df,resultingfile)
Sys.time() - st

########################### END ------ 
