
###################################################################.
######### PURPOSE: CONDITIONS / BH CONDITIONS LABEL PER PATIENT
# Example: "For Patient_001 - > COPD, Diabetes, Hypertension"
# Execute this script after enrollment ONCE A MONTH
# This report contains Medical Conditions information
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
required_packages <- c("dplyr", "data.table", "lubridate", 
                       "tidyverse", "readxl")
lapply(required_packages, library, character.only = TRUE)

# 1 PARAMETERS CHANGE NAMES / UPDATE --------------------------------------------------

currPPL <- "202106"     # Update 

# Location of files: Paths / Filenames

path            <- "Monthly_Enrollment"   
inputfile   <- paste0("Enrollment_PPL_", currPPL, ".csv")
outputfile  <- paste0("Monthly_Enrollment//MedicalConditions_", currPPL, ".csv")

# 2 FUNCTIONS -------------------

# Function Convert Data to Money values without commas
tablex       <- function(x){print(table(x, useNA = 'always'))}
tablexy      <- function(x,y){print(table(x,y, useNA = 'always'))}

# 3 READ THE GENERATED CSV DATA FILE ----------

# Read csv file
filenames_list <- list.files(path= path, full.names=TRUE, inputfile)
filenames_list

# 4 UPLOAD ENROLLMENT DATA ------------------------------------------------

AllEnrollees <- data.frame(read.csv(file=filenames_list)) # full.names=TRUE, 
str(AllEnrollees)
dim(AllEnrollees)

# 5 RENAME COLUMN NAMES ----------------------------
# Necessary that column names match with the replacement names
# in order to execute the function

str(AllEnrollees)

str(AllEnrollees)
dim(AllEnrollees)

# 5 DEFINE COLUMN NAMES GROUPS DEMOGRAPHICS MEDICAL BH CONDITIONS ------

# DEMOGRAPHICS
demographics <- c("Medicaid.ID",  "Name", "DOB",
                  "Age", "Gender", "Patient.County",  "Practice.County" )

demographics %in% names(AllEnrollees)

#### VERIFY: IF MISSING
demographics[!demographics %in% names(AllEnrollees)]#### MISSING 

# SPECIAL CONDITIONS
special_conditions <- c(  "Dual", "ABD" )
              #"Foster_Care_Indicator", #"Palliative_Care_Indicator"

special_conditions %in% names(AllEnrollees)

#### VERIFY: IF MISSING
special_conditions[!special_conditions %in%  names(AllEnrollees)]   

# CLINICAL CONDITIONS

clinical_conditions <- c(
  "Asthma",
  "Cancer",
  "CVA.Stroke",
  "Chr.GastroInt.Dis",
  "Chr.Kidney.Dis",
  "Chronic.Pain", 
  "COPD",
  "Diabetes",
  "CHF",
  "HIV",
  "Hypertension",
  "Ischemic.Vascular.Dis",
  "Chr.Liver.Dis",
  "History.Myoc.Inf",  # "Post_MI",
  "Musculoskeletal.Connective.Tissue.Dis",
  "Chr.Neurological.Dis",
  #  "Pressure_Ulcer_Or_Stasis_Ulcer",
  "Sickle.Cell"
  # "Three.or.More.Chronic.Conditions"
)
clinical_conditions[clinical_conditions %in% names(AllEnrollees)]

#### VERIFY: IF MISSING
clinical_conditions[!clinical_conditions %in%  names(AllEnrollees)] 

# BH CONDITIONS
BH_Conditions <- c(
  # "MH_Indicator",
  "Schizophrenia.Schizoaffective",
  "Bipolar.Disorder",
  "ADHD",
  "Anxiety",
  "Autism",
  "Conduct.Disorder",
  "Dementia",
  "Depression",
  "Eating.Dis",
  "OCD",
  "Personality",
  "PTSD",
  "Substance.Abuse",
  "Dev.Disab",
  #"Opioid.Safety.Indicator",
  # "SPMI"
  "Any.MH"
)

BH_Conditions[BH_Conditions %in% names(AllEnrollees)]

#### VERIFY: IF MISSING
BH_Conditions[!BH_Conditions %in%  names(AllEnrollees)] #### MISSING
dim(AllEnrollees)

# REORDER BY GROUPS
AllEnrollees <- AllEnrollees %>% select(c(all_of(demographics), 
                                          all_of(special_conditions), 
                                          all_of(clinical_conditions), 
                                          all_of(BH_Conditions)))

head(AllEnrollees)

as.vector(t(names(AllEnrollees)))
paste(t(names(AllEnrollees)), collapse = ", ")

# 6 SELECT FIELDS THAT WILL CHANGE YES BY FIELD NAME ------

ChangeName <- c(special_conditions,clinical_conditions,BH_Conditions)
ChangeName   # ALL FIELDS THAT WILL CHANGE YES BY FIELD NAME
ChangeName %in% names(AllEnrollees)
head(AllEnrollees[,ChangeName],5)

# Vector_from_ifelse: Names that replace the original "Yes" value
# Necessary that column names match with the replacement names
# in order to execute the function
# ONLY FOR VERIFICATION

Verification_from_ifelse <- c("Dual", "ABD", "Asthma", "Cancer", "CVA.Stroke",
                              "Chr.GastroInt.Dis", "Chr.Kidney.Dis", "Chronic.Pain",
                              "COPD", "Diabetes", "CHF", "HIV", "Hypertension", 
                              "Ischemic.Vascular.Dis", "Chr.Liver.Dis", "History.Myoc.Inf", 
                              "Musculoskeletal.Connective.Tissue.Dis", 
                              "Chr.Neurological.Dis", "Sickle.Cell", 
                              "Any.MH", "Schizophrenia.Schizoaffective", "Bipolar.Disorder", "ADHD", 
                              "Anxiety", "Autism", "Conduct.Disorder", "Dementia", 
                              "Depression", "Eating.Dis", "OCD", "Personality",
                              "PTSD", "Substance.Abuse", "Dev.Disab"
)

ChangeName[!ChangeName %in% Verification_from_ifelse]
Verification_from_ifelse %in%  ChangeName

# 7 REPLACE YES / NO VALUES BY THE CONDITION NAME OR BLANK ------
# "Yes" -> Condition name
# "No  -> Blank

AllEnrollees[,"PTSD"]

for(i in seq_along(ChangeName)){
  AllEnrollees[,ChangeName][i]  <- ifelse(AllEnrollees[,ChangeName][i]  == "Yes",
                                          ChangeName[i],"")
}

dim(AllEnrollees)
head(AllEnrollees, 20)
str(AllEnrollees)
class(AllEnrollees)


# 8 PASTE COLLAPSE CONDITIONS ---------------------------------------------
# Paste all Conditions Columns into One  - Columns 49, 50
AllEnrollees$Clinical_Dxs <- apply( AllEnrollees[,clinical_conditions],1,paste,collapse = ",")
AllEnrollees$BH_Dxs       <- apply( AllEnrollees[,BH_Conditions]      ,1,paste,collapse = ",")
AllEnrollees$All_Conditions <- apply( AllEnrollees[,c(clinical_conditions, BH_Conditions)],1,paste,collapse = ",")
AllEnrollees$All_Conditions
dim(AllEnrollees)
str(AllEnrollees)
table(AllEnrollees$Any.MH, useNA = 'always')


# 9 REGEX ELIMINATE CONSECUTIVE COMMAS -----------------------------

#                              ^,*|(?<=,),|,*$
#                        gsub("^,*|(?<=,),|,*$", "",                     x, perl=T)
gsub("^,*|(?<=,),|,*$","", AllEnrollees$Clinical_Dxs, perl=T)
AllEnrollees$Clinical_Dxs <- gsub("^,*|(?<=,),|,*$","", AllEnrollees$Clinical_Dxs, perl=T)
AllEnrollees$BH_Dxs       <- gsub("^,*|(?<=,),|,*$","", AllEnrollees$BH_Dxs      , perl=T)
AllEnrollees$All_Conditions <- gsub("^,*|(?<=,),|,*$","", AllEnrollees$All_Conditions      , perl=T)

AllEnrollees$Clinical_Dxs   <- gsub(",", ", ", AllEnrollees$Clinical_Dxs)
AllEnrollees$BH_Dxs         <- gsub(",", ", ", AllEnrollees$BH_Dxs)
AllEnrollees$All_Conditions <- gsub(",", ", ", AllEnrollees$All_Conditions)

AllEnrollees$Clinical_Dxs
AllEnrollees$BH_Dxs
AllEnrollees$All_Conditions

# help(gsub)
# AllEnrollees[,56]  <- ifelse(AllEnrollees[,56]  == ", , ","Chronic Neurological Disease","")
#x <- c("a,b,c", ",a,b,,c", ",,,a,,,b,c,,,")    
#gsub("^,*|(?<=,),|,*$", "", x, perl=T)
# [1] "a,b,c" "a,b,c" "a,b,c"

str(AllEnrollees)

#AllEnrollees <- AllEnrollees %>% rename(Name=Patient.Name)
#AllEnrollees[ ,5]

# 10 CHANGE VALUES GENDER FIELD ----------
AllEnrollees$Gender[ AllEnrollees$Gender == "FEMALE" ] <- "F"
AllEnrollees$Gender[ AllEnrollees$Gender == "MALE" ] <- "M"
AllEnrollees
str(data.table(AllEnrollees))

# TEST

t(AllEnrollees[sample(1:nrow(AllEnrollees),1),])

# 11 WRITE DATA TABLE ------------------------------------------------------

outputfile
fwrite(AllEnrollees, outputfile)

ls()
Sys.time() - st

