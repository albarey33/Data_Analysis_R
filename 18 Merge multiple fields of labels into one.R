
###################################################################.
#### MERGE MULTIPLE FIELDS OF LABELS INTO ONE
# Example: Conditions / BH conditions into one label per patient
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

path            <- "PopulationSamples"   
inputfile   <- paste0("Enrollment_PPL_", currPPL, ".csv")

# 2 FUNCTIONS -------------------

# Function Convert Data to Money values without commas
tablex       <- function(x){print(table(x, useNA = 'always'))}

# 3 READ THE GENERATED CSV DATA FILE ----------

# Read csv file
filenames_list <- list.files(path= path, full.names=TRUE, inputfile)
filenames_list

# 4 UPLOAD ENROLLMENT DATA ------------------------------------------------

AllEnrollees <- data.frame(read.csv(file=filenames_list)) # full.names=TRUE, 
str(AllEnrollees)
dim(AllEnrollees)

# 4.1 Data Frame for Post -----

clinical_conditions2 <- c("Asthma","CVA.Stroke",
                          "Chr.Kidney.Dis",  "Chronic.Pain", 
                          "COPD",  "Diabetes",  "CHF")

dfclinicalconditions <- data.frame(AllEnrollees[c(192, 111, 128, 163, 87, 74, 37, 7, 396, 311, 306),clinical_conditions2])
dfclinicalconditions

# 5 RENAME COLUMN NAMES ----------------------------
# Necessary that column names match with the replacement names
# in order to execute the function

str(AllEnrollees)
dim(AllEnrollees)

# 6 DEFINE COLUMN NAMES GROUPS DEMOGRAPHICS MEDICAL BH CONDITIONS ------

# DEMOGRAPHICS
demographics <- c("Medicaid.ID",  "Name", "DOB",
                  "Age", "Gender", "Patient.County",  "Practice.County" )

demographics %in% names(AllEnrollees)

#### VERIFY: IF MISSING
# Check vector of values in another vector
demographics[!demographics %in% names(AllEnrollees)]#### MISSING 

# SPECIAL CONDITIONS
special_conditions <- c(  "Dual", "ABD" )
              #"Foster_Care_Indicator", #"Palliative_Care_Indicator"

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

# 7 REORDER BY GROUPS -------------
AllEnrollees <- AllEnrollees %>% select(c(all_of(demographics), 
                                          all_of(special_conditions), 
                                          all_of(clinical_conditions), 
                                          all_of(BH_Conditions)))

head(AllEnrollees)

# Vector of names
as.vector(names(AllEnrollees))
paste(names(AllEnrollees), collapse = ", ")
class(t(names(AllEnrollees))) # t() Matrix Transpose


# 7 FIELD NAMES AS VALUES ------

# * 7.1 Select fields that will change YES by field name  -----

ChangeName <- c(special_conditions,clinical_conditions,BH_Conditions)
ChangeName   # ALL FIELDS THAT WILL CHANGE YES BY FIELD NAME
tablex(ChangeName %in% names(AllEnrollees))
head(AllEnrollees[,ChangeName],3)
AllEnrollees

# Vector_from_ifelse: Names that replace the original "Yes" value
# Necessary that column names match with the replacement names
# in order to execute the function
# ONLY FOR VERIFICATION

# * 7.2 Verification - Match names -------------

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

# * 7.3 Replace YES / NO values by the condition name or blank ------
# "Yes" -> Condition name
# "No  -> Blank

tablex(ifelse(AllEnrollees[,"PTSD"]=="Yes",1,0))

fxonlyfieldnamestochangevalues <- function(df){
  for(ii in seq_along(df)){
    df[,ii] <- ifelse(df[,ii]  == "Yes", names(df)[ii],"")
    #print(df[,ii])
  }
  df
  }

AllEnrollees[,c(ChangeName)] <- fxonlyfieldnamestochangevalues(AllEnrollees[,c(ChangeName)])

tablex(AllEnrollees$PTSD) # Verification

# for(i in seq_along(ChangeName)){
#   AllEnrollees[,ChangeName][i]  <- ifelse(AllEnrollees[,ChangeName][i]  == "Yes",
#                                    ChangeName[i],"")}

str(AllEnrollees)
class(AllEnrollees)


# 8 PASTE COLLAPSE CONDITIONS BY GROUPS ------------------------------------

# Paste all Conditions Columns into One  - Columns 49, 50
AllEnrollees$Clinical_Dxs <- apply( AllEnrollees[,clinical_conditions],1,paste,collapse = ",")
AllEnrollees$BH_Dxs       <- apply( AllEnrollees[,BH_Conditions]      ,1,paste,collapse = ",")
AllEnrollees$All_Conditions <- apply( AllEnrollees[,c(clinical_conditions, BH_Conditions)],1,paste,collapse = ",")
AllEnrollees$All_Conditions
dim(AllEnrollees)
str(AllEnrollees)
tablex(AllEnrollees$Any.MH)


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

str(data.table(AllEnrollees))

# RANDOM TEST
t(AllEnrollees[sample(nrow(AllEnrollees),1),])

# 11 WRITE DATA TABLE ------------------------------------------------------

#outputfile
#fwrite(AllEnrollees, outputfile)

ls()
Sys.time() - st

# 12 Brief example for post - From 4.1  ------------------

dfclinicalconditions

# Defined function to convert the values "Yes" into the name of the condition
fxonlyfieldnamestochangevalues <- function(df){
  for(ii in seq_along(df)){
    df[,ii] <- ifelse(df[,ii]  == "Yes", names(df)[ii],"")
  }
  df
}

# Applying Function
dfclinicalconditions <- fxonlyfieldnamestochangevalues(dfclinicalconditions)
dfclinicalconditions

# Mergin the values
allconditions <- apply( dfclinicalconditions,1,paste,collapse = ",")
allconditions
allconditions <- gsub("^,*|(?<=,),|,*$","", allconditions, perl=T)
allconditions <- gsub(",", ", ", allconditions)
dfclinicalconditions$onecolumn <- allconditions
dfclinicalconditions <- dfclinicalconditions %>% select(onecolumn, everything())
dfclinicalconditions
