# PREPARE INSTALL CALL PACKAGES -----------------------------

rm(list=ls())    # Delete all objects
options(header=T, sep=",", 
        stringsAsFactors = FALSE, 
        str = strOptions(strict.width = "cut"))

st <- Sys.time()

#devtools::session_info()
# .libPaths("C:/R_Libraries")
# .libPaths() ###  Check the default folder where packages are installed

# Load-multiple-packages-at-once
required_packages <- c("dplyr", "data.table", "lubridate", 
                       "tidyverse", "readxl", "stringr", 
                       "bit64", "tidyr")
lapply(required_packages, library, character.only = TRUE)
rm(required_packages)

# PATH
fileslocation            <- "Three_Years_Data"



# FUNCTIONS --------------

######### FUNCTION APPEND PPL CSV FILES 
fx_append_csvfiles <- function(list_of_csv_files, PPLini, PPLfin){
  mytypelist <- list() # Creation of empty list
  for(i in seq_along(list_of_csv_files)){
    df.list <- lapply(list_of_csv_files[i], read.csv)  ######### Convert list to Data frame 
    df.list[[1]]$PPL <- substr(list_of_csv_files[i], PPLini, PPLfin)
    print(paste(i," - Number of records in ",list_of_csv_files[i]," = ",nrow(df.list[[1]])," ; columns = ",length(df.list[[1]]),sep=""))
    mytypelist <- append(mytypelist, df.list)
  }
  df <- data.frame(bind_rows(mytypelist))
}

######### FUNCTION APPEND EXCEL FILES
fx_append_excel_data <- function(DataFilesADT){
  options(message = FALSE)
  mytypelist <- list() # Creation of empty list
  for(i in seq_along(DataFilesADT)){   # Data in Three Groups or Files
    details <- file.info(DataFilesADT)
    details <- details[with(details, order(as.POSIXct(mtime), decreasing = TRUE)),]
    file.list <- rownames(details[1])  # Only name column[1]
    df.list <- lapply(file.list[i], read_excel)  ######### Convert list to Data frame 
    df.list[[1]]$FileName <- file.list[i]               # R will recycle values
    #    df.list[[1]]$AddedCol <- rep(vectorvh[i],nrow(df.list[[1]])) # Not needed # Not needed
    print(paste(i," ","Number of records in file ",file.list[i]," = ",nrow(df.list[[1]])," ; columns = ",length(df.list[[1]]),sep=""))
    mytypelist <- append(mytypelist, df.list)
  }
  data.frame(bind_rows(mytypelist))
}


######### FUNCTION COMPARISON TWO DATA FRAMES STRUCTURE
fx_comparison_two_DFs_Fields <- function(df_x, df_y){
  LeftW <- data.frame(field=names(df_x),match=match(names(df_x), names(df_y)), "L in R")
  RightW <- data.frame(field=names(df_y),match=match(names(df_y), names(df_x)),"R in L")
  compardf <- LeftW %>% full_join(RightW, by= 'field') %>% filter(is.na(match.x) | is.na(match.x) )
  print(compardf)
}

# FUNCTION TO ELIMINATE GROUP OF FIELDS BY WORDS
fx_eliminate_groups_fields <- function(fields_eliminate, df){
  for (i in seq_along(fields_eliminate)){
    print("DELETE NEXT GROUP OF FIELDS: ")
    print(names(df)[grepl(fields_eliminate[i], names(df))])
    df <- df[,!grepl(fields_eliminate[i], names(df))]
  }
  df
}

# FUNCTION CHANGE PRACTICE NAMES
fx_change_Practice_Names <- function(PCP_field_name, df, dfChanges){
  LocPCP <- match(PCP_field_name,names(df))
  for(i in 1:nrow(dfChanges)){
    print(paste0("Records with ",dfChanges[i,1],": ", df %>% filter(df[ ,LocPCP] == dfChanges[i,1]) %>% tally()))
    df[ ,LocPCP][df[ ,LocPCP] == dfChanges[i,1]] <- dfChanges[i,2]
    print(paste0("  -------->   CHANGED TO :", dfChanges[i,2]))
  }
  df
}

# FUNCTION CONVERT DATA FROM NULL TO ZERO
fx_nulltozero <- function(x){ifelse(is.na(x),0,x)} # 

# Function Convert Data to Money values
fx_convmoney <-   function(x){as.numeric(gsub("[\\$,]", "", x))} # 

# Function Convert Yes to Unos
fx_convert_Yes_to_Unos <-   function(x){ifelse(x == "Yes",1L,0L)}

# TABLES USENA = 'always'
tablex <- function(x){print(table(x, useNA = 'always'))}
tablexy <- function(x,y){print(table(x,y, useNA = 'always'))}

###########################################################################o
###########################################################################o
###########################################################################o

# ENROLLMENT CONDITIONS AND PRACTICES IN THE LAST MONTHS ----

# 1 PPL SINCE 202101 -------------
# DF CONDITIONS & DEMOGRAPHICS AFTER JANUARY 2021

# Location of files: Paths / Filenames
getwd()
list_PPL_PHD <- list.files(path=fileslocation, pattern="^5Enrollment_PHD_PPL_", 
                           full.names=TRUE)
list_PPL_PHD
locPPL <- regexpr("202106",list_PPL_PHD[1])[1]
#substr(list_PPL_PHD[1], 38, 43)  # Location of PPL in name

df_PPL_PHD <- fx_append_csvfiles(list_PPL_PHD, locPPL, locPPL+5)
rm(list_PPL_PHD)

#df_PPL_PHD <- list_PPL_PHD %>% map_df(~fread(.)) # "data.table" "data.frame"
str(df_PPL_PHD)
dim(df_PPL_PHD)

tablexy(df_PPL_PHD$PPL, df_PPL_PHD$Any.MH)
#tablexy(df_PPL_PHD$PPL, df_PPL_PHD$CCNC_Priority) # NOT IN 202104
tablexy(df_PPL_PHD$PPL, df_PPL_PHD$ABD)
tablexy(df_PPL_PHD$PPL, df_PPL_PHD$New_CCNC_Priority_since_prior_month)
df_PPL_PHD %>% filter(New_CCNC_Priority_since_prior_month == 'Yes')
names(df_PPL_PHD)
# delete Patient.Region only has data in 202007 - 202012: 'Region 5'
# delete Primary.Insurance.Provider only has data in January 2021 'NCMEDICAID'

# * 1.1 DELETE NOT NEEDED FIELDS ---------
df_PPL_PHD <- df_PPL_PHD %>% select(-c('Medicare.ID', 'Commercial.ID',
                                       #         ".Dual.Eligibility", ### TWO DUAL FIELDS
                                       # 'Phone', 
                                       #        'Care.Manager.Phone.Number',
                                       #        'Practice...NPI', '.Provider...NPI', 
                                       'Patient.Region'))
tablexy(df_PPL_PHD$PPL, df_PPL_PHD$ED.Visits..Last.12.mos.)
tablexy(df_PPL_PHD$PPL, df_PPL_PHD$Inpatient.Admissions..Last.12.mos.)

# * 1.2 RENAME TO MATCH NAMES PHD -----------------------------------------------

#df_PPL_PHD <- df_PPL_PHD %>% rename(ABD = .ABD)

# * 1.3 CHANGE OF FORMATS -----------------------------------------------

df_PPL_PHD$Zip <- as.character(df_PPL_PHD$Zip)
str(df_PPL_PHD)
tablexy(df_PPL_PHD$PPL, df_PPL_PHD$Risk.Stratification) # ALL NA

#####################################################################o
#####################################################################o
#####################################################################o

# 2 PPL 202007 - 202012  ----------------------

list_PPL_MbD <- list.files(path=fileslocation, pattern="^4Enrollment_PPL", 
                           full.names=TRUE)

list_PPL_MbD <- sort(list_PPL_MbD,decreasing = TRUE,) # 18 MONTHS from the last PPL
list_PPL_MbD
seq_along(list_PPL_MbD)
length(list_PPL_MbD)
list_PPL_MbD

# * 2.1 Read csv files  -------------------------------

substr(list_PPL_MbD[1], 34, 39) # Location of PPL in Name ini,fin
locPPL <- regexpr("202012",list_PPL_MbD[1])[1]
df_PPL_MbD <- fx_append_csvfiles(list_PPL_MbD, locPPL, locPPL+5)

rm(list_PPL_MbD)
table(df_PPL_MbD$PPL, useNA = 'always')
head(df_PPL_MbD,2)
#df_PPL_MbD <- list_PPL_MbD %>% map_df(~fread(.))
# df_PPL_MbD_files$PPLMID <- paste(df_PPL_MbD_files$PPL,df_PPL_MbD_files$Current.MID,sep="/")
table(df_PPL_MbD$CCNC_Priority, useNA = 'always')
#table(df_PPL_MbD_files$PPL, df_PPL_MbD_files$MH_Indicator_Cost, useNA = 'always')
table(df_PPL_MbD$PPL, df_PPL_MbD$Asthma, useNA = 'always')
table(df_PPL_MbD$PPL, df_PPL_MbD$MH_Indicator, useNA = 'always')
table(df_PPL_MbD$PPL, df_PPL_MbD$Post_MI, useNA = 'always')
table(df_PPL_MbD$PPL, df_PPL_MbD$MH_Indicator_Cost, useNA = 'always')
table(df_PPL_MbD$PPL, df_PPL_MbD$ABD, useNA = 'always')
table(df_PPL_MbD$ED.Visits..Last.12.mos., useNA = 'always')
df_PPL_MbD$Zip <- as.character(df_PPL_MbD$Zip)
str(df_PPL_MbD)

# * 2.2 DELETE FIELDS ------------

# VERIFICATE DUPLICATED COLUMN TO DELETE
table(df_PPL_MbD$Patient.Name == df_PPL_MbD$Client.Name, useNA = 'always')

# Delete PCP Data Not Needed Columns
df_PPL_MbD <- df_PPL_MbD %>%  select(-c("Client.Name",
                                        "Patient.Region",
                                        "PCP_Address",
                                        "PCP_City",
                                        "PCP_County",
                                        "PCP_State",
                                        "PCP_Zip",
                                        "Practice...NPI", # Already as CA_PCP
                                        "MH_Indicator_Cost",
                                        "Blank",
                                        'Medicare.ID',
                                        'Commercial.ID',
                                        # 'Phone',
                                        'Care.Manager.Phone.Number')) #

table(df_PPL_PHD$Primary.Insurance.Provider, useNA = 'always')
table(df_PPL_MbD$PPL, df_PPL_MbD$SPMI, useNA = 'always')
table(df_PPL_MbD$PPL, df_PPL_MbD$Insurance.Provider, useNA = 'always') # Health Choice / Medicaid

# * 2.2 RENAME TO MATCH NAMES PHD -----------------------------------------------
str(df_PPL_MbD)

df_PPL_MbD <- df_PPL_MbD %>% rename(Medicaid.ID = Current.MID,
                                    Name = Patient.Name, 
                                    Primary.Insurance.Provider = Insurance.Provider,
                                    Any.Mental.Health.Condition = MH_Indicator,
                                    Any.MH = MH_Indicator, 
                                    Durable.Medical.Equipment.Cost..Last.12.mos.... = DME.Cost..Last.12.mos....,
                                    #Eating.Disorder = Eating,
                                    Eating.Dis = Eating, 
                                    #History_MI = Post_MI,
                                    History.Myoc.Inf = Post_MI, 
                                    #Chronic.GI.Disease = ,
                                    Chr.GastroInt.Dis = Chronic_GI_Disease, 
                                    #Chronic.Kidney.Disease = Chronic_Kidney_Disease, 
                                    Chr.Kidney.Dis = Chronic_Kidney_Disease, 
                                    Sickle.Cell = Sickle_Cell_Indicator, 
                                    #Chronic.Liver.Disease = Chronic_Liver_Disease,
                                    Chr.Liver.Dis = Chronic_Liver_Disease, 
                                    Patient.County = County,
                                    Chronic.Pain = Chronic_Pain, 
                                    Bipolar.Disorder = Bipolar, 
                                    Schizophrenia.Schizoaffective = Schizo, 
                                    Substance.Abuse = SA, 
                                    Dev.Disab = DD_Indicator, 
                                    CVA.Stroke = CVA, 
                                    Conduct.Disorder = Conduct, 
                                    #Chronic.Neurological.Disease = ,
                                    Chr.Neurological.Dis = Chronic_Neurological_Disease, 
                                    Hypertension = HTN, 
                                    Ischemic.Vascular.Dis = IVD, 
                                    Musculoskeletal.Connective.Tissue.Dis = Musculoskeletal_Connective_Tissue_Disease, 
                                    Therapy.Services..PT.RT.ST..Cost..Last.12.mos.... = ST.PT.OT.Therapy.Cost..Last.12.mos....,
                                    # Mental.Health.Cost..Last.12.mos.... = MH_Indicator_Cost,
                                    Practice_Name = CA_PCP)

# * 2.3 COMPARISON FIELDS COLUMN NAMES PPL_PHD PPL_MbD -------------------------------------

fx_comparison_two_DFs_Fields(df_PPL_PHD, df_PPL_MbD)

df_PPL_PHD %>% group_by(PPL) %>% summarise(Cost = sum(Total.Healthcare.Cost..Last.12.mos.....))
df_PPL_PHD %>% group_by(PPL) %>% summarise(Cost = sum(Capitation.Cost..Last.12.mos....))
df_PPL_MbD %>% group_by(PPL) %>% summarise(Cost = sum(Ambulance.Cost..Last.12.mos....))
df_PPL_PHD %>% group_by(PPL) %>% summarise(OtherInst = sum(Other.Institutional.Cost..Last.12.mos....))
df_PPL_MbD %>% group_by(PPL) %>% summarise(RHC = sum(RHC.FQHC.Health.Dept.Cost..Last.12.mos....))

class(df_PPL_PHD$Mental.Health.Cost..Last.12.mos....)

# * 2.4 BIND ROWS ALL PPL SINCE 202007 ------------------------------------

ALL_PPL <- bind_rows(df_PPL_PHD, df_PPL_MbD)
rm(df_PPL_PHD, df_PPL_MbD)
str(ALL_PPL)
table(ALL_PPL$PPL)
tablexy(ALL_PPL$PPL, ALL_PPL$CCNC_Priority)
ALL_PPL %>% group_by(PPL) %>% summarise(Cost = sum(Total.Healthcare.Cost..Last.12.mos.....))
ALL_PPL %>% group_by(PPL) %>% summarise(Cost = sum(Capitation.Cost..Last.12.mos....))
ALL_PPL %>% group_by(PPL) %>% summarise(Cost = sum(Ambulance.Cost..Last.12.mos....))
tablexy(ALL_PPL$PPL, ALL_PPL$New_CCNC_Priority_since_prior_month)

###########################################################################o
###########################################################################o
###########################################################################o

# 3 PPL 202006 WITHOUT COST INFO ----------------------
# FROM MEMBER DASHBOARD CONDITIONS & DEMOGRAPHICS JUNE 2020
# Note Next Line: full.names() <- Better FALSE
list_PPL_202006 <- list.files(pattern="^Enrollment_202006_wcost_and_util", full.names=FALSE)
list_PPL_202006 <- sort(list_PPL_202006,decreasing = TRUE,) # 18 MONTHS from the last PPL
seq_along(list_PPL_202006)
length(list_PPL_202006)
list_PPL_202006

# * 3.1 Read csv files  -------------------------------
substr("Enrollment_202006.csv", 12, 17) # Location of PPL in Name ini,fin
df_PPL_202006 <- fx_append_csvfiles(list_PPL_202006, 12, 17)
rm(list_PPL_202006)
str(df_PPL_202006)
dim(df_PPL_202006)
#df_PPL_202006 <- list_PPL_202006 %>% map_df(~fread(.))
#df_PPL_202006$PPL <- as.character(df_PPL_202006$PPL)
table(df_PPL_202006$PPL, useNA = 'always')
# df_PPL_202006$PPLMID <- paste(df_PPL_MbD_files$PPL,df_PPL_MbD_files$Current.MID,sep="/")
table(df_PPL_202006$CCNC_Priority, useNA = 'always')
#table(df_PPL_202006$PPL, df_PPL_202006$MH_Indicator_Cost, useNA = 'always')
table(df_PPL_202006$PPL, df_PPL_202006$MH_Indicator, useNA = 'always')
table(df_PPL_202006$PPL, df_PPL_202006$Post_MI, useNA = 'always')
table(df_PPL_202006$PPL, df_PPL_202006$Chronic_Pain, useNA = 'always')
df_PPL_202006$DOB <- as.character(df_PPL_202006$DOB, "%m/%d/%Y") # FIX
df_PPL_202006$Zip <- as.character(df_PPL_202006$Zip) # FIX

# Delete PCP Data Not Needed Columns
df_PPL_202006 <- df_PPL_202006 %>%  select(-c("PCP_Address",
                                              "PCP_City",
                                              "PCP_County",
                                              "PCP_State",
                                              # 'Practice...NPI',
                                              #'Phone',
                                              "PCP_Zip")) # MH_Indicator_Cost

# * 3.2 RENAME TO MATCH NAMES PHD -----------------------------------------------
str(df_PPL_202006)

df_PPL_202006 <- df_PPL_202006 %>% rename(Medicaid.ID = Current.MID,
                                          Primary.Insurance.Provider = Insurance.Provider,
                                          Any.MH = MH_Indicator,
                                          #Durable.Medical.Equipment.Cost..Last.12.mos.... = DME.Cost..Last.12.mos....,
                                          #Eating_Disorder = Eating,
                                          # History_MI = ,
                                          Name = Patient.Name, 
                                          History.Myoc.Inf = Post_MI, 
                                          Musculoskeletal.Connective.Tissue.Dis = Musculoskeletal_Connective_Tissue_Disease, 
                                          Patient.County = County,
                                          Chr.GastroInt.Dis = Chronic_GI_Disease, 
                                          Chr.Kidney.Dis = Chronic_Kidney_Disease, 
                                          Chr.Liver.Dis = Chronic_Liver_Disease,
                                          Chronic.Pain = Chronic_Pain,
                                          Chr.Neurological.Dis = Chronic_Neurological_Disease,
                                          CVA.Stroke = CVA, 
                                          Hypertension = HTN, 
                                          Dev.Disab = DD_Indicator, 
                                          Bipolar.Disorder = Bipolar, 
                                          Ischemic.Vascular.Dis = IVD, 
                                          Schizophrenia.Schizoaffective = Schizo, 
                                          Sickle.Cell = Sickle_Cell_Indicator, 
                                          Management.Fee.Cost..Last.12.mos.... = Management.Fees,
                                          # Therapy.Services..PT.RT.ST..Cost..Last.12.mos.... = ST.PT.OT.Therapy.Cost..Last.12.mos....,
                                          # Mental.Health.Cost..Last.12.mos.... = MH_Indicator_Cost,
                                          Practice_Name = CA_PCP)
dim(df_PPL_202006)

# * 3.3 COMPARISON FIELDS COLUMN NAMES PPL_PHD PPL_MbD -------------------------------------

fx_comparison_two_DFs_Fields(ALL_PPL,df_PPL_202006)


# * 3.4 BIND ROWS ALL PPL SINCE 202006 ------------------------------------

ALL_PPL <- bind_rows(ALL_PPL, df_PPL_202006)
rm(df_PPL_202006)
str(ALL_PPL)
names(ALL_PPL)
table(ALL_PPL$ED.Visits..Last.12.mos., useNA = 'always')
table(ALL_PPL$PPL)
tablex(nchar(ALL_PPL$Date.of.Last.CM.Interaction))
tablexy(ALL_PPL$PPL, nchar(ALL_PPL$Date.of.Last.CM.Interaction))

###########################################################################o
###########################################################################o
###########################################################################o

# 4 PPL 201903 - 202005 (15 MONTHS) DF CONDITIONS UNTIL MAY 2020 -----------------------

setwd("C:/Users/bbacareyes/")
setwd("./OneDrive - Carolina Collaborative Community Care/")
setwd("./DATA_Reports/")
setwd("./REPORT_4C_MONTHLY_ENROLLMENT/")
setwd("./DATA_PPL_2019_03_to_2020_05") # SET WORKING DIRECTORY

# * 4.1 DF CONDITIONS 15 MONTHS UNTIL MAY 2020 ------

list_MemDash <- list.files(path=getwd(), pattern="PPL_FLTM_DATA_", full.names=FALSE)
list_MemDash <- sort(list_MemDash,decreasing = TRUE,) # 18 MONTHS from the last PPL
list_MemDash

#### Upload Data 18 MONTHS as Data.Frame Data.Table
substr("PPL_FLTM_DATA_202005_May.csv", 15, 20) # Location of PPL in Name ini,fin
df_MemDash <- fx_append_csvfiles(list_MemDash, 15, 20)
rm(list_MemDash)
table(df_MemDash$PPL, df_MemDash$Copd, useNA = 'always')
dim(df_MemDash)
class(df_MemDash)
length(df_MemDash)    # Number of columns
table(df_MemDash$PPL, useNA = 'always')
str(df_MemDash)
dim(df_MemDash)
str(df_MemDash[70:152])

# * 4.2 Delete Fields -------

class(df_MemDash)
str(df_MemDash)
table(df_MemDash$PPL, useNA = 'always')
grep('MCO.CC', names(df_MemDash), value = T)
grep('ACT', names(df_MemDash), value = T)
grep('Lipid.Screen', names(df_MemDash), value = T)
grep('DTP', names(df_MemDash), value = T)
grep('Fills', names(df_MemDash), value = T)
grep('Fill.Date', names(df_MemDash), value = T)

group_of_fields_eliminate_util <- c("MCO.CC", "ACT.", "CST.", 
                                    "Lipid.Screen", "Glucose.Screen", 
                                    "Residential.Treatment", "Fills", "Fill.Date")
group_of_fields_eliminate_util

df_MemDash <- fx_eliminate_groups_fields(group_of_fields_eliminate_util,
                                         df_MemDash)
rm(group_of_fields_eliminate_util)

str(df_MemDash)
dim(df_MemDash)

df_MemDash <- df_MemDash %>% select(-c(#'Client.Phone', 
  'Deceased')) 
# 'Known.Medicare.Inpatient.Cost', 
# 'Known.Medicare.Outpatient.Cost',
# 'Known.Medicare.Professional.Cost'))


# * 4.3 Change Formats  ----

str(df_MemDash)
tablexy(df_MemDash$PPL, nchar(df_MemDash[,"Date.of.Birth"]))
df_MemDash$Client.Zip <- substr(df_MemDash$Client.Zip,1,5)
df_MemDash[,"Date.of.Birth"] <- sub(" 12:00:00 AM","",df_MemDash[,"Date.of.Birth"])
#df_MemDash[,"Date.of.Birth"][df_MemDash$PPL == '202002'] <- sub(" 0:00","",df_MemDash[,"Date.of.Birth"][df_MemDash$PPL == '202002'])
#df_MemDash[,"Date.of.Birth"][df_MemDash$PPL != '202002'] <- sub(" 12:00:00 AM","",df_MemDash[,"Date.of.Birth"][df_MemDash$PPL != '202002'])
tablexy(df_MemDash$PPL, nchar(df_MemDash[,"Date.of.Birth"]))

grep('Last', names(df_MemDash), value = T)
#df_MemDash[,"Date.of.Last.CM.Interaction_PL"][df_MemDash$PPL != '202002'] <- sub(" 12:00:00 AM","",df_MemDash[,"Date.of.Birth"][df_MemDash$PPL != '202002'])

# https://stackoverflow.com/questions/9319242/remove-everything-after-space-in-string
df_MemDash$Date.of.Last.CM.Interaction_PL <- gsub('([0-9]+) .*', '\\1', df_MemDash$Date.of.Last.CM.Interaction_PL)
tablex(nchar(df_MemDash[,"Date.of.Last.CM.Interaction_PL"]))

# FOLLOW UP
grep('Follow', names(df_MemDash), value = T)
df_MemDash$Outpatient.Follow.up.Recommended.w.in <- 
  paste(df_MemDash$Outpatient.Follow.up.Recommended.w.in, ' days', sep="")

# INPATIENT READMISSIONS ERROR - DIVIDE BY 100
df_MemDash %>% group_by(PPL) %>% summarise(Readm = sum(Medicaid.Readmissions, na.rm = T))
df_MemDash$Medicaid.Readmissions <- df_MemDash$Medicaid.Readmissions / 100
df_MemDash %>% group_by(PPL) %>% summarise(Readm = sum(Medicaid.Readmissions, na.rm = T))

# FILLS NULL TO ZERO
df_MemDash[,grep('.Fills', names(df_MemDash), value = T)] <- 
  lapply(df_MemDash[,grep('.Fills', names(df_MemDash), value = T)], fx_nulltozero)

# CHANGE RISK OF ADMISSION TO CHARACTER
df_MemDash$Risk.for.admission.within.the.next.12.months.1 <- as.character(df_MemDash$Risk.for.admission.within.the.next.12.months.1)
df_MemDash$Risk.for.admission.within.the.next.30.Days <- as.character(df_MemDash$Risk.for.admission.within.the.next.30.Days)

# COST FIX FORMAT COMMAS BLANKS

df_MemDash[,c('MCO.Capitation', 'Pace.Capitation')] <- 
  lapply(df_MemDash[,c('MCO.Capitation', 'Pace.Capitation')], fx_convmoney)

#df_COST_MD2[,Cost_Cols_convert_Num2] <- lapply(df_COST_MD2[,Cost_Cols_convert_Num2], fx_convmoney)
#df_COST_MD <- bind_rows(df_COST_MD, df_COST_MD2)
str(df_MemDash)

# FILL NULL TO ZERO 201903 - 202005
CostColIni <- match('Total.Medicaid.Cost', names(df_MemDash))
CostColFin <- match('Management.Fees', names(df_MemDash))
df_MemDash[CostColIni:CostColFin] <- lapply(df_MemDash[CostColIni:CostColFin],
                                            fx_nulltozero)
rm(CostColIni, CostColFin)

table(df_MemDash$PPL, is.na(df_MemDash$Total.Medicaid.Cost))

str(df_MemDash)


# * 4.4 CALCULATED COLUMNS  -----------------------------------------------
# CAPITATION COST
grep('.Capitation', names(df_MemDash), value = T)
df_MemDash <- df_MemDash %>%
  #rowwise will make sure the sum operation will occur on each row
  rowwise() %>%
  mutate(Calc_Capitation_Cost = sum(MCO.Capitation, Pace.Capitation, na.rm=TRUE))

df_MemDash %>% select(MCO.Capitation, Pace.Capitation, Calc_Capitation_Cost)
df_MemDash <- df_MemDash %>% select(-c(MCO.Capitation, Pace.Capitation))
class(df_MemDash)

str(df_MemDash)

dim(df_MemDash)

# * 4.5 RENAME TO MATCH NAMES PHD -----------------------------------------------

names(df_MemDash)
#df_MemDash$Copd
grep("Copd", names(df_MemDash), value = T)

str(df_MemDash)

df_MemDash <- df_MemDash %>% rename(Name = Client.Name,
                                    Medicaid.ID = Current.MID,
                                    Primary.Insurance.Provider = Health.Plan.Description,
                                    Phone = Client.Phone, 
                                    DOB = Date.of.Birth,
                                    Address = Client.Address,
                                    State = Client.State,
                                    City = Client.City,
                                    Zip = Client.Zip,
                                    #Practice_County = PCP_County,
                                    Practice_Name = CA.PCP.Name,
                                    Patient.County = Client.County,
                                    
                                    Dual = Dual.Eligibility,
                                    Personality = Personality.Disorders.2,
                                    #Asthma = Asthma,
                                    History.Myoc.Inf = History.of.Myocardial.Infarction,
                                    CVA.Stroke = Cerebrovascular.Disease,
                                    ADHD = Adhd,
                                    Bipolar.Disorder = Bipolar.Disease, 
                                    HIV = HIV.1,
                                    Cancer = Cancer.1,
                                    Eating.Dis = Eating.Disorders.1,
                                    # Eating.Disorder = 
                                    Diabetes = Diabetes.1,
                                    #Bipolar.Disorder = Bipolar.Disease,
                                    #Chronic_GI_Disease = Chronic.GI.Disease,
                                    Chr.Neurological.Dis = Chronic.Neurological.Disease, 
                                    #Chronic_Neurological_Disease = Chronic.Neurological.Disease,
                                    Musculoskeletal.Connective.Tissue.Dis = Musculoskeletal.Connective.Tissue.Disease,
                                    Sickle.Cell = Sickle.Cell.Disease,
                                    Dementia = Dementia.1,
                                    #HTN = Hypertension,
                                    Anxiety = Anxiety.1,
                                    Autism = Autism.1,
                                    Chr.Liver.Dis = Chronic.Liver.Disease, 
                                    #Chronic_Liver_Disease = Chronic.Liver.Disease,
                                    Chr.Kidney.Dis = Chronic.Kidney.Disease, 
                                    Chr.Liver.Dis = Chronic.Liver.Disease, 
                                    Chr.GastroInt.Dis = Chronic.GI.Disease, 
                                    #Chronic_Kidney_Disease = Chronic.Kidney.Disease,
                                    ABD = Aged..Blind.or.Disabled,
                                    Conduct.Disorder = Conduct.Disorder.and.Oppositional.Defiant.Disorder, 
                                    # Conduct = ,
                                    Schizophrenia.Schizoaffective = Schizophrenia.or.Schizoaffective.Disorder,
                                    Ischemic.Vascular.Dis = Ischemic.Vascular.Disease,
                                    OCD = Obbsessive.Compulsive.Disorder,
                                    #CHF = CHF,
                                    Substance.Abuse = Substance.Abuse..excludes.tobacco.use.,
                                    PTSD = Posttraumatic.Stress.Disorder,
                                    Any.MH = Mental.Health.Condition,
                                    Chronic.Pain = Chronic.Pain.1,
                                    SPMI = SPMI.Indicator,
                                    COPD = Copd,
                                    Dev.Disab = Developmental.Disability.1,
                                    Depression = Depression.1,
                                    Pressure_Ulcer_Or_Stasis_Ulcer = Pressure.Ulcer.Or.Stasis.Ulcer,
                                    
                                    TC_Impactability_Score = TC.Impactability.Score.1, 
                                    CM_Impactability_Score = CM.Impactability.Score, 
                                    Transitional_Care_Priority = Transitional.Care.Priority,
                                    CCNC_Priority = CCNC.Priority,
                                    New_CCNC_Priority_since_prior_month = New.CCNC.Priority.since.prior.month,
                                    Foster_Care_Indicator = Foster.Care.Indicator,
                                    Palliative_Care_Indicator = Palliative.Care.Indicator,
                                    
                                    Inpatient.Readmissions..Last.12.mos. = Medicaid.Readmissions,
                                    ED.Visits..Last.12.mos. = Total.ED.Visits.1,
                                    Inpatient.Admissions..Last.12.mos. = Total.IP.Visits.1,
                                    Date.of.Last.Known.Practice.Visit = Last.PCP.Visit.Date,
                                    
                                    Date.of.Last.CM.Interaction = Date.of.Last.CM.Interaction_PL, 
                                    Date.Most.Recent.Care.Needs.Screening.Completed = Date.of.Most.Recent.Screening, 
                                    Date.Most.Recent.Comprehensive.Needs.Assessment.Completed = Primary.CNA.Date.Last.Completed, 
                                    Date.Most.Recent.Care.Plan.Completed = Primary.CP.Date.Last.Completed, 
                                    
                                    
                                    
                                    Total.Healthcare.Cost..Last.12.mos..... = Total.Medicaid.Cost,
                                    Inpatient.Cost..Last.12.mos.... = Inpatient.Cost,
                                    Outpatient.Cost..Last.12.mos.... = Outpatient.Cost,
                                    Professional.Cost..Last.12.mos.... = Professional.Cost,
                                    RHC.FQHC.Health.Dept.Cost..Last.12.mos.... = RHC.FQHC..Health.Dept.Cost,
                                    Nursing.Home.Cost..Last.12.mos.... = Nursing.Home.Cost,
                                    Personal.Care.Services.Cost..Last.12.mos.... = Personal.Care.Services.Cost,
                                    Drug.Cost..Last.12.mos.... = Drug.Cost,
                                    Private.Duty.Nurse.Cost..Last.12.mos.... = Private.Duty.Nursing.Cost,
                                    Durable.Medical.Equipment.Cost..Last.12.mos.... = Durable.Medical.Equipment.Cost,
                                    Mental.Health.Cost..Last.12.mos.... = Mental.Health.Non.Capitated.Cost,
                                    Home.Health.Cost..Last.12.mos.... = Home.Health.Cost,
                                    Therapy.Services..PT.RT.ST..Cost..Last.12.mos.... = Therapy.Services..PT..RT..ST..Cost,
                                    Hospice.Cost..Last.12.mos.... = Hospice.Cost,
                                    Ambulance.Cost..Last.12.mos.... = Ambulance.Cost,
                                    Management.Fee.Cost..Last.12.mos.... = Management.Fees,
                                    Dental.Cost..Last.12.mos.... = Dental.Cost,
                                    Capitation.Cost..Last.12.mos.... = Calc_Capitation_Cost,
                                    Other.Institutional.Cost..Last.12.mos.... = Other.Medicaid.Cost)

# * 4.6 COMPARISON FIELDS COLUMN NAMES PPL_PHD PPL_MbD -------------------------------------

# table(ALL_PPL$PPL, ALL_PPL$Insurance.Provider, useNA = 'always')
# str(df_MemDash$Insurance.Provider)

fx_comparison_two_DFs_Fields(ALL_PPL,df_MemDash)

table(ALL_PPL$Insurance.Provider, useNA = 'always')

# * 4.7 BIND ROWS ALL PPL SINCE 201903 ------------------------------------

ALL_PPL <- bind_rows(ALL_PPL, df_MemDash)
rm(df_MemDash)
dim(ALL_PPL)
str(ALL_PPL)
str(ALL_PPL[50:141])

table(ALL_PPL$PPL)
table(ALL_PPL$PPL, is.na(ALL_PPL$Date.of.Last.CM.Interaction), useNA = 'always')
tablexy(ALL_PPL$PPL, nchar(ALL_PPL$Date.of.Last.CM.Interaction))


Sys.time() - st

head(ALL_PPL %>% group_by(PPL) %>% summarise(EDVs = sum(ED.Visits..Last.12.mos., na.rm = T)), 20)
tail(ALL_PPL %>% group_by(PPL) %>% summarise(EDVs = sum(ED.Visits..Last.12.mos., na.rm = T)), 20)

# 5 PPL 2018 - 201902 DF  -----------------------

# * 5.1 Read LIST OF CSV files --------------------------------------

#setwd("D:/Information Technology/DATA_Reports/REPORT_4C_MONTHLY_ENROLLMENT/DATA_PPL_2019_02_and_before")   # SET WORKING DIRECTORY
setwd("C:/Users/bbacareyes/")
setwd("./OneDrive - Carolina Collaborative Community Care/")
setwd("./DATA_Reports/")
setwd("./REPORT_4C_MONTHLY_ENROLLMENT/")
setwd("./DATA_PPL_2019_02_and_before") # SET WORKING DIRECTORY
getwd()

list_PREV <- list.files(path=getwd(), pattern="^PPL_Data", full.names=FALSE)
list_PREV <- sort(list_PREV,decreasing = TRUE,) 
list_PREV
list_PREV <- list_PREV[1:8] # Only since JUly 2018 # December 2017
list_PREV

substr("PPL_Data_201902.csv", 10, 15)
df_PREV <- fx_append_csvfiles(list_PREV, 10, 15)

rm(list_PREV)
dim(df_PREV)
str(df_PREV)
str(df_PREV[90:180])
df_PREV %>% group_by(PPL) %>% tally()

# # * 5.2 Delete Fields -------------------------

# DELETE Groups of Fields by String on Name 

grep('MCO', names(df_PREV), value = T)

# Eliminate All MCO_CC / Fill_Date / Fills fields
group_of_fields_eliminate <- c("MCO_CC", "Fill_Date",
                               "Fills", "DTP_", "ACT_",
                               "CST_", "Lipid_", "Glucose_", "Residential_Treatment_",
                               "Outpatient_MH", "Enhanced_MH")
group_of_fields_eliminate

df_PREV <- fx_eliminate_groups_fields(group_of_fields_eliminate,
                                      df_PREV)

rm(group_of_fields_eliminate)
dim(df_PREV)
str(df_PREV)
df_PREV$Visit_PCP_last_Year

# Delete PCP Data Other Not Needed Columns

df_PREV <- df_PREV %>%  select(-c("CA_PCP_SITE_ADDRESS", 
                                  "CA_PCP_SITE_CITY", 
                                  # "PCP_County", 
                                  #"Phone",
                                  "CA_PCP_SITE_STATE",
                                  "CA_PCP_SITE_ZIP_CODE", 
                                  "CPESN_Pharmacy",
                                  #                                  "Last_Non_MCO_ED_Visit_Date", 
                                  #                                  "Last_MCO_ED_Visit_Date",
                                  #                                  "Last_Non_MCO_IP_Visit_Date",
                                  #                                  "Last_MCO_IP_Visit_Date",
                                  #                                  "Util_IP_MCO", 
                                  #                                  "Util_ED_MCO", 
                                  #                                  "Util_ED_Mediciad",
                                  "PALDate",
                                  "Narcotic_Lock_In_Program", 
                                  "Psychiatric_Medications", 
                                  "No_of_Pharmacies", 
                                  "Psychiatric_Services", 
                                  "Composite_Pharmacy_Risk_Score",
                                  "CC4C_Care_Manager", 
                                  "CC4C_Care_Management_Status", 
                                  "CASE_MANAGEMENT_CURRENT_STATUS",
                                  "CCNC_Deferral_Reason",
                                  "OBCMName", 
                                  "OB_Care_Management_Status", 
                                  "Last_Time_Patient_Went_to_Hospital_per_ADT_feed",
                                  "Visit_Type_per_ADT_feed",
                                  "Known_Medicare_Inpatient",
                                  "Known_Medicare_Outpatient",
                                  "Known_Medicare_Professional",
                                  "CAP_PROGRAM",
                                  "CA_PCP_NUMBER",
                                  "NETWORK", 
                                  "NETWORK_NUMBER")) 

# * 5.3 CHANGE FORMATS  ------

class(df_PREV$Zip)
#df_PREV$Zip
df_PREV$Zip <- sub('(\\d{5}).*', '\\1', df_PREV$Zip) # Replace string
class(df_PREV$Zip)
#df_PREV$Zip

# CONVERT UTILIZATION DATA FROM NULL TO ZERO 
str(df_PREV)
table(df_PREV$PPL, df_PREV$Util_ED_All, useNA = 'always')
df_PREV %>% filter(is.na(Util_ED_All)) %>% group_by(PPL) %>% tally()
df_PREV %>% group_by(PPL) %>% summarise(EDVs = sum(Util_ED_All, na.rm = T))
df_PREV %>% group_by(PPL) %>% summarise(IPVs = sum(Util_IP_All, na.rm = T))
df_PREV %>% group_by(PPL) %>% summarise(Readmits = sum(Util_Readmits_Medicaid, na.rm = T))
#df_PREV$Util_ED_All[is.na(df_PREV$Util_ED_All)] <- 0
FirstUtil <- match('OUTPATIENT_BILLERS___MD__DO__NP__PA_', names(df_PREV)) # First Cost
FirstUtil   # First Column related to cost
LastUtil <- match('Util_ED_All', names(df_PREV)) # Last Cost
LastUtil    # Last Column related to cost
paste0("Range of Utilization info : from ",FirstUtil," TO: ", LastUtil)
names(df_PREV[,FirstUtil:LastUtil]) # Apply Fx Null to Zero
str(df_PREV[FirstUtil:LastUtil])
df_PREV[,FirstUtil:LastUtil] <- lapply(df_PREV[,FirstUtil:LastUtil], fx_nulltozero)
str(df_PREV[,FirstUtil:LastUtil])
rm(FirstUtil, LastUtil)

# CONVERT COST DATA FROM STRING TO NUMERIC VALUES 
FirstCost <- match('Cost_Total_Medicaid', names(df_PREV)) # First Cost
FirstCost   # First Column related to cost
LastCost <- match('Cost_MgmtFees_Medicaid', names(df_PREV)) # Last Cost
LastCost    # Last Column related to cost
paste0("Range of Cost info : from ",FirstCost," TO: ", LastCost)
str(df_PREV[FirstCost:LastCost])
df_PREV[,c(FirstCost:LastCost)][df_PREV[,c(FirstCost:LastCost)] == " "] <- 0
df_PREV[,c(FirstCost:LastCost)][df_PREV[,c(FirstCost:LastCost)] == ""] <- 0
df_PREV[,FirstCost:LastCost] <- lapply(df_PREV[,FirstCost:LastCost], fx_convmoney )
str(df_PREV[FirstCost:LastCost])
str(df_PREV)
rm(FirstCost, LastCost)

# CONVERT CONDITIONS DATA TO YES or NO 
FirstCond <- match('DIABETES_INDICATOR', names(df_PREV)) # First Cost
FirstCond   # First Column related to Conditions
LastCond <- match('Chronic_Pain_Indicator', names(df_PREV)) # Last Cost
LastCond    # Last Column related to Conditions
paste0("Range of Conditions info : from ",FirstCond," TO: ", LastCond)
str(df_PREV[FirstCond:LastCond])
df_PREV[,c(FirstCond:LastCond)][df_PREV[,c(FirstCond:LastCond)] == ""] <- "No"
df_PREV[,c('IsABD')][df_PREV[,c('IsABD')] == ""] <- "No"
df_PREV[,c('Foster_Care_Indicator')][df_PREV[,c('Foster_Care_Indicator')] == ""] <- "No"
df_PREV[,c('CCNC_Priority')][df_PREV[,c('CCNC_Priority')] == ""] <- "No"
df_PREV[,c('New_CCNC_Priority_since_prior_month')][df_PREV[,c('New_CCNC_Priority_since_prior_month')] == ""] <- "No"
df_PREV[,c('Transitional_Care_Priority')][df_PREV[,c('Transitional_Care_Priority')] == ""] <- "No"
str(df_PREV[FirstCond:LastCond])
rm(FirstCond, LastCond)

# Change Format
df_PREV$National_Prov_ID_and_Loc <- as.character(df_PREV$National_Prov_ID_and_Loc)

# Convert CM Score to Numeric
# Delete the comma in 1,000 score
df_PREV$CM_Impactability_Score <- as.integer(gsub("[\\,]", "", df_PREV$CM_Impactability_Score))
#table(df_PREV$CM_Impactability_Score, useNA = 'always')

class(df_PREV)
length(df_PREV)    # Number of columns
str(df_PREV)
grep('Visit_PCP', names(df_PREV), value = T)
names(df_PREV)[grep("Mental", names(df_PREV))]
dim(df_PREV)

# Date.of.Last.CM.Interaction
df_PREV[,"LAST_TASK_COMPLETED_DATE"]
nchar(df_PREV[,"LAST_TASK_COMPLETED_DATE"])
df_PREV[,"LAST_TASK_COMPLETED_DATE"] <- sub(" 12:00:00 AM","",df_PREV[,"LAST_TASK_COMPLETED_DATE"])

# df_PREV$Any.Mental.Health.Condition <- as.character(df_PREV$Any.Mental.Health.Condition)

names(df_PREV)
dim(df_PREV)
table(df_PREV$PPL, useNA = 'always')
table(df_PREV$Util_ED_All, useNA = 'always')

# table(df_PREV$PPL, useNA = 'always')

#paste(names(df_PREV), collapse = ", ") # Delete

df_PREV$Visit_PCP_last_Year

# * 5.5 CALCULATED COLUMNS  -----------------------------------------------
# CAPITATION COST
grep('Cost_Cap_', names(df_PREV), value = T)
df_PREV <- df_PREV %>%
  #rowwise will make sure the sum operation will occur on each row
  rowwise() %>%
  mutate(Calc_Capitation_Cost = sum(Cost_Cap_BH_MCO, Cost_Cap_PACE, na.rm=TRUE))

df_PREV <- data.frame(df_PREV)
#df_PREV %>% select(-c(Visit_PCP_last_Year)
df_PREV %>% select(Cost_Cap_BH_MCO, Cost_Cap_PACE, Calc_Capitation_Cost) %>% head()

df_PREV <- df_PREV %>% select(-c(Cost_Cap_BH_MCO, Cost_Cap_PACE))

class(df_PREV)

table(ALL_PPL$Program.Category.Description)
table(ALL_PPL$Pop.Group.Code)
table(df_PREV$PROGRAM_CATEGORY)


# for(i in 1:111){
#   print(i)
# }

str(df_PREV[1:10])
str(df_PREV[11:20])

names(df_PREV)
paste(names(df_PREV), collapse = "', '")
#paste(names(ALL_PPL[30:61]), collapse = "', '")

#dim(df_PREVBACKUP)

# * 5.9 RENAME TO MATCH NAMES PHD -----------------------------------------------

df_PREV <- df_PREV %>% rename(
  Name = NAME,
  Medicaid.ID = MID_Current,
  #Practice_County = PCP_County,
  Primary.Insurance.Provider = PrimaryPayer,
  
  Practice_Name = CA_PCP,
  Base.MID = Base_MID, 
  Practice.County = CA_PCP_SITE_COUNTY, 
  Race = RACE, 
  Ethnicity = ETHNICITY, 
  Program.Category.Description = PROGRAM_CATEGORY,
  Provider_NPI = National_Prov_ID_and_Loc,
  
  CRG.Code = CRGCode,
  CRG.Description = CRGDescription,
  ACRG3.Code = ACRG3Code,
  ACRG3.Description = ACRG3Description,
  
  New.TC.Priority.since.prior.month = New_TC_Priority_since_prior_month, 
  TC.Home.Visit.Priority = TC_Home_Visit_Priority, 
  Outpatient.Follow.up.Recommended.w.in = Outpatient_Follow_up_Recommended, 
  
  Visit.Type..per.ADT.feed. = CCNC_Physician_visit_in_Past_Year,
  
  Opioid.Safety.Indicator = Opioid_Safety_Indicator, 
  ABD = IsABD, 
  Primary.Care.Manager = CASE_MANAGER_LAST_NAME,
  Patient.County = CLIENT_COUNTY,
  Dual = DUAL_ELIGIBILITY_STATUS,
  Personality = Personality_Indicator, 
  Asthma = ASTHMA_INDICATOR,
  History.Myoc.Inf = POST_MI_INDICATOR,
  CVA.Stroke = CEREBROVASCULAR_DISEASE_CVA,
  ADHD = ADHD_INDICATOR,
  Eating.Dis = Eating_Indicator,
  Diabetes = DIABETES_INDICATOR,
  Bipolar.Disorder = BIPOLAR_INDICATOR,
  Hypertension = HTN_INDICATOR, 
  Anxiety = Anxiety_Indicator, 
  Autism = Autism_Indicator, 
  Conduct.Disorder = Conduct_Indicator, 
  Schizophrenia.Schizoaffective = SCHIZOPHRENIA_AND_SCHIZOAFFECTIVE_INDICATOR, 
  Ischemic.Vascular.Dis = ISCHEMIC_VASCULAR_DISEASE_INDICATOR, 
  OCD = OCD_Indicator,
  CHF = CHF_INDICATOR, 
  Musculoskeletal.Connective.Tissue.Dis = MUSCULOSKELETAL_CONNECTIVE_TISSURE_DISEASE, 
  Pressure_Ulcer_Or_Stasis_Ulcer = PRESSURE_ULCER_OR_STATIS_ULCER, 
  Dev.Disab = DD_INDICATOR, 
  Depression = DEPRESSION, 
  Chr.Kidney.Dis = CHRONIC_KIDNEY_DISEASE, 
  Sickle.Cell = SICKLE_CELL_INDICATOR, 
  Chr.GastroInt.Dis = CHRONIC_GI_DISEASE, 
  Chr.Liver.Dis = CHRONIC_LIVER_DISEASE, 
  Chr.Neurological.Dis = CHRONIC_NEUROLOGICAL_DISEASE, 
  Cancer = CANCER, 
  Substance.Abuse = SA_Indicator,
  PTSD = POSTTRAUMATIC_STRESS_DISORDER,
  Any.MH = MH_INDICATOR,
  Chronic.Pain = Chronic_Pain_Indicator,
  SPMI = SPMI_Indicator,
  #Insurance.Provider = Health_Plan_Description,
  COPD = COPD_INDICATOR, 
  ED.Visits..Last.12.mos. = Util_ED_All, 
  Medicaid.ED.Visits.1 = Util_ED_Mediciad, 
  Inpatient.Admissions..Last.12.mos. = Util_IP_All,
  # Observation.Stays..Last.12.mos. = Util_Readmits_Medicaid,
  Inpatient.Readmissions..Last.12.mos. = Util_Readmits_Medicaid,
  Behavioral.Health...MCO.IP.Visits = Util_IP_MCO, 
  Behavioral.Health...MCO.ED.Visit = Util_ED_MCO, 
  Last.Non.MCO.ED.Visit.Date = Last_Non_MCO_ED_Visit_Date,
  Last.Non.MCO.IP.Visit.Date = Last_Non_MCO_IP_Visit_Date, 
  Last.MCO.ED.Visit.Date = Last_MCO_ED_Visit_Date, 
  Last.MCO.IP.Visit.Date = Last_MCO_IP_Visit_Date, 
  Risk.for.admission.within.the.next.30.Days = Risk_for_admission_within_the_next_30_Days,
  Risk.for.admission.within.the.next.12.months.1 = Risk_for_admission_within_the_next_12_months,
  Inpatient.Cost..Last.12.mos.... = Cost_Inpatient_Medicaid, 
  Outpatient.Cost..Last.12.mos....= Cost_Outpatient_Medicaid, 
  Professional.Cost..Last.12.mos.... = Cost_Professional_Medicaid, 
  Drug.Cost..Last.12.mos.... = Cost_Drug_Medicaid, 
  Hospice.Cost..Last.12.mos.... = Cost_Hospice_Medicaid, 
  Durable.Medical.Equipment.Cost..Last.12.mos.... = Cost_DurMedEquip_Medicaid, 
  Private.Duty.Nurse.Cost..Last.12.mos.... = Cost_PrivateDutyNurse_Medicaid, 
  Home.Health.Cost..Last.12.mos.... = Cost_HomeHealth_Medicaid, 
  Total.Healthcare.Cost..Last.12.mos..... = Cost_Total_Medicaid, 
  Nursing.Home.Cost..Last.12.mos.... = Cost_NursHome_Medicaid, 
  Personal.Care.Services.Cost..Last.12.mos.... = Cost_PersonalCare_Medicaid, 
  Therapy.Services..PT.RT.ST..Cost..Last.12.mos.... = Cost_Therapy_Svcs_Medicaid, 
  Dental.Cost..Last.12.mos.... = Cost_Dental_Medicaid,
  Mental.Health.Cost..Last.12.mos.... = Cost_MH_NonCap_Medicaid,
  #MCO.Capitation = Cost_Cap_BH_MCO,
  #Pace.Capitation = Cost_Cap_PACE,
  Capitation.Cost..Last.12.mos.... = Calc_Capitation_Cost,
  Other.Institutional.Cost..Last.12.mos.... = Cost_Other_Medicaid,
  Ambulance.Cost..Last.12.mos.... = Cost_Ambulance_Medicaid, 
  Management.Fee.Cost..Last.12.mos.... = Cost_MgmtFees_Medicaid, 
  Date.of.Last.CM.Interaction = LAST_TASK_COMPLETED_DATE, 
  Date.of.Last.Known.Practice.Visit = Last_PCP_Visit_Date, 
  Date.Most.Recent.Comprehensive.Needs.Assessment.Completed = Last_CHA_Assessment_Date,
  Medicaid.Eligibility.Months = MosElig,
  CCNC.Member.Months.In.The.Past.12.Months = MosMem
)

# * 5.10 COMPARISON FIELDS COLUMN NAMES PPL_PHD PPL_MbD -------------------------------------
# tablex(ALL_PPL$Program.Category.Description)
# tablex(df_PREV$Program_Category)

fx_comparison_two_DFs_Fields(ALL_PPL,df_PREV)
tablex(ALL_PPL$Program.Category.Description)
tablex(df_PREV$Program_Category)
dim(ALL_PPL)

# table(df_PREV$Program_Category, useNA = 'always')
# grep('Program', names(ALL_PPL), value = T)
# grep('Program', names(df_PREV), value = T)
# table(ALL_PPL$Program.Category.Description, useNA = 'always')
# 

# * 5.11 BIND ROWS ALL PPL SINCE 2018 ------------------------------------
dim(ALL_PPL)
dim(df_PREV)
ALL_PPL <- bind_rows(ALL_PPL, df_PREV)
dim(ALL_PPL)
ls()
rm(df_PREV)

ALL_PPL <- ALL_PPL %>% relocate(Medicaid.ID, Base.MID)
head(ALL_PPL)

##### *****END OF THIS PROCEDURE ********************************* --------------

# 6 CHANGES TO ALL_PPL ALL DATA ---------------------------

# * 6.1 UPDATE RENAME PCP NAMES as Recent Data ---------------------------------

# Renaming some Providers (Special cases)

class(ALL_PPL)
ALL_PPL <- data.frame(ALL_PPL)
# df$var <- ifelse(df$var == " ?", " Private", df$var)

# PCP Change of Names
PCPchng <- data.frame(OriginalName= character(0), ChangedName= character(0))
PCPchng[nrow(PCPchng)+1,] <- c("CAPE FEAR FAMILY MEDICAL CARE 1340 WALTER REED",
                               "CAPE FEAR FAMILY MED CARE")
PCPchng[nrow(PCPchng)+1,] <- c("CAPE FEAR FAMILY MEDICAL CARE 413 OWEN DR", 
                               "CAPE FEAR FAMILY MED CARE")
PCPchng[nrow(PCPchng)+1,] <- c("CAPE FEAR FAMILY MEDICAL CARE 405 OWEN DR", 
                               "CAPE FEAR FAMILY MED CARE")
PCPchng[nrow(PCPchng)+1,] <- c("CAROLINA RHEUMATOLOGY AND INTERNAL MEDICINE", 
                               "CAPE FEAR VALLEY PRIMARY CARE - JOHN SMITH")
PCPchng[nrow(PCPchng)+1,] <- c("WOMEN'S HEALTH HAVEN", 
                               "CFVMC DIVISION OF DUKE OB/GYN")
PCPchng[nrow(PCPchng)+1,] <- c("FAYETTEVILLE GERIATRIC & INTERNAL MEDICINE",
                               "CAROLINA PRIMARY & INTERNAL MEDICINE")
PCPchng[nrow(PCPchng)+1,] <- c("WADE FAMILY MEDICAL CENTER",
                               "STEDMAN - WADE HEALTH SERVICES")
PCPchng[nrow(PCPchng)+1,] <- c("CAPE FEAR VALLEY PRIMARY CARE - SKIBO ROAD",
                               "CAPE FEAR VALLEY PRIMARY CARE - FAYETTEVILLE FAMILY")
PCPchng

ALL_PPL <- fx_change_Practice_Names("Practice_Name", ALL_PPL, PCPchng)

rm(PCPchng)

# * 6.1 ADD PRACTICES TYPE COLUMN ---------------------------------
#setwd("D:/Information Technology/DATA_Reports/@Tables_Data")   
setwd("C:/Users/bbacareyes/")
setwd("./OneDrive - Carolina Collaborative Community Care/")
setwd("./DATA_Reports/")
setwd("./@Tables_Data/")
getwd()

Practice_Type <- read_excel("Table_Practices_Type.xlsx", sheet = "CAP_PCP", range="A1:O1000") 
Practice_Type <- Practice_Type %>% select(`Practice Name`, `Practice Type`)
names(Practice_Type) <- c('Practice_Name', 'Practice_Type')
ALL_PPL <- ALL_PPL %>% left_join(Practice_Type, by = c('Practice_Name'))
ALL_PPL
tablexy(ALL_PPL$Practice_Name, ALL_PPL$Practice_Type)
#tablex(grep('SOUTHERN', ALL_PPL$Practice_Name, value = T))
#names(tablex(grep('SUNSHINE', ALL_PPL$Practice_Name, value = T)))
#ALL_PPL %>% filter(Practice_Name == 'POLYMEDIC PRIMARY CARE') %>% group_by(PPL, Practice_Type) %>% tally()

# * 6.2 COST FIELDS MOVE TO RIGHT  ---------

Cost_Cols <- grep('Cost', names(ALL_PPL), value = T)
Cost_Cols
ALL_PPL <- ALL_PPL %>% select(!Cost_Cols, Cost_Cols) 
rm(Cost_Cols)

# * 6.3 COLUMN AGE GROUP ------

ALL_PPL <- ALL_PPL %>% 
  mutate(Age_Group = if_else(Age >= 21, 'Adult 21+ y.o.', 
                             'Pediatric 0 - 20 y.o.'))


# * 6.4 RACE CHANGE NAMES OF CATEGORIES ---------

tail(tablexy(ALL_PPL$PPL, ALL_PPL$Race), 15)

tablex(ALL_PPL$Race)
ALL_PPL$Race[ALL_PPL$Race == 'BLACK'] <- 'BLACK OR AFRICAN AMERICAN'
ALL_PPL$Race[ALL_PPL$Race == 'PACIFIC ISLANDER OR NATIVE HAWAIIAN'] <- 'HAWAIIAN OR PACIFIC ISLANDER'
ALL_PPL$Race[ALL_PPL$Race == 'AM-INDIAN OR ALASKA NATIVE'] <- 'AMERICAN INDIAN'
tablexy(ALL_PPL$PPL, ALL_PPL$Race)

# UNREPORTED : REPLACING FOR OTHER RECORDED VALUE
# Filter all pts (except 'unreported', sorted by more to less records)
# number of records by patients and race
NoUnreported <- ALL_PPL %>% arrange(desc(PPL)) %>% filter(Race != 'UNREPORTED') %>% select(Medicaid.ID, Race) %>% group_by(Medicaid.ID, Race) %>% tally() %>% arrange(Medicaid.ID, desc(n))
#NoUnreported   
tablex(NoUnreported$Race)
# PATIENTS WITH 2+ DIFFERENT RECORDED RACES
NoUnreported2 <- NoUnreported %>% select(-c(n)) %>% group_by(Medicaid.ID) %>% tally() %>% arrange(desc(n)) %>% filter(n>1)  # Racerename(NURace = Race)
NoUnreported2  # With 2+ different races
# LIST OF UNIQUE RACE PER PATIENTS
NoUnreported3 <- NoUnreported %>% select(-c(n)) %>% distinct(Medicaid.ID, .keep_all = T) %>% rename(RaceX = Race)
head(NoUnreported3)
# LEFT JOIN TABLE OF UNIQUE RACE + COALESCE
ALL_PPL <- ALL_PPL %>% left_join(NoUnreported3, by = c('Medicaid.ID'))
ALL_PPL$Race <- coalesce(ALL_PPL$RaceX, ALL_PPL$Race)
ALL_PPL <- ALL_PPL %>% select(-c(RaceX))
ALL_PPL$Race[is.na(ALL_PPL$Race)] <- 'UNREPORTED'
# ALL_PPL %>% select(Race, RaceX)
tablexy(ALL_PPL$PPL, ALL_PPL$Race)
head(tablexy(ALL_PPL$PPL, ALL_PPL$ED.Visits..Last.12.mos.),5)
tail(tablexy(ALL_PPL$PPL, ALL_PPL$ED.Visits..Last.12.mos.),5)

# repID <- NoUnreported2$Medicaid.ID
# for(i in seq_along(repID)){
# print(ALL_PPL %>% filter(ALL_PPL$Race != 'UNREPORTED' & Medicaid.ID == repID[i]) %>% select(Medicaid.ID, Race) %>% group_by(Medicaid.ID, Race) %>% tally() %>% arrange(Medicaid.ID, desc(n))) # Racerename(NURace = Race))
# }

#str(ALL_PPL[80:118])

# BACKUP ----
# ALL_PPL_BACKUP <- ALL_PPL
# ALL_PPL        <- ALL_PPL_BACKUP

# * 6.5 MEMBER MONTHS CALCULATION -------------

#ALL_PPL$Mem
# dfPPL <- ALL_PPL %>% group_by(PPL) %>% arrange(PPL) %>% distinct(PPL) 
#ALL_PPL %>% group_by(PPL) %>% arrange(PPL) %>% distinct(PPL)
#ALL_PPL %>% distinct(PPL) %>% arrange(PPL)
dfPPL <- ALL_PPL %>% distinct(PPL) %>% arrange(PPL)
dfPPL$PPL_Id <- seq_along(1:nrow(dfPPL))
head(dfPPL)
tail(dfPPL)

# ## ADD PPL_Id to main data table
# ALL_PPL <- ALL_PPL %>% inner_join(dfPPL, by = 'PPL')
# head(ALL_PPL)

# * * 6.5.1 MELT DCAST MID PPL -----
castedMIDPPL <- dcast(melt(data.table(ALL_PPL), 
                           id.vars = c("Medicaid.ID", "PPL"), 
                           measure.vars = c("PPL")),  # = c("PPL_Id")),
                      Medicaid.ID ~ PPL, fun.agg = length)
castedMIDPPL   # 1 or 0 if the pt is enrolled in each PPL
castedMbMos <- castedMIDPPL

# * * 6.5.2 CALCULATION MEMBER MONTHS PER PATIENT ----
# https://stackoverflow.com/questions/50639903/paste-variable-name-in-mutate-dplyr
for(i in 12:nrow(dfPPL)){   
  #  i <- 12
  dfPPLfincol <- dfPPL$PPL[i]
  dfPPLfincol     # Name of Month example "201809"
  C3fincol <- match(dfPPLfincol, names(castedMbMos))
  C3fincol        # Index of Month example "13"
  dfPPLinicol <- dfPPL$PPL[i-11]
  dfPPLinicol
  C3inicol <- match(dfPPLinicol, names(castedMbMos))
  C3inicol
  castedMbMos[,C3inicol:C3fincol]
  castedMbMos <- castedMbMos %>% 
    mutate( !!paste0(dfPPLfincol, 'MbMo') := 
              rowSums(castedMbMos[,C3inicol:C3fincol]))
}

# * * 6.5.3 DELETE INDIVIDUAL MONTHS - ONLY FIELDS Member Months -----
castedMbMos
dim(castedMbMos)
castedMbMos <- castedMbMos %>% select(-names(castedMbMos)[1:nrow(dfPPL)+1])
castedMbMos
dim(castedMbMos)

MbMos <- melt(data.table(castedMbMos), 
              id.vars = c("Medicaid.ID"), 
              measure.vars = grep("MbMo", names(castedMbMos), value = T))
MbMos
MbMos <- MbMos %>% rename(MbMos = value, 
                          PPL = variable)
MbMos
MbMos$PPL <- substr(MbMos$PPL, 1, 6)
MbMos
ALL_PPL <- ALL_PPL %>% left_join(MbMos, by = c('Medicaid.ID', 'PPL'))
head(ALL_PPL,1)
# RECAP
tablex(ALL_PPL %>% filter(PPL == '202103') %>% select('MbMos'))
tablexy(ALL_PPL$PPL, ALL_PPL$MbMos)
tail(ALL_PPL %>% group_by(PPL) %>% summarise(AllMbMos = sum(MbMos)), 20)
#tablex(ALL_PPL$MbMos)

# * * 6.5.4 JOIN CALCULATE COLUMN TO DATA - VERIFY ------
sampleMID <- sample_n(ALL_PPL,1)[,'Medicaid.ID']
sampleMID
ALL_PPL %>% filter(ALL_PPL$Medicaid.ID == sampleMID) %>% 
  select(Medicaid.ID, PPL, MbMos) %>% arrange(desc(PPL))

# COMPARATION APROX CALCULATED RESULTS VS EXISTING
ALL_PPL %>% filter(PPL == '202005') %>% 
  select(CCNC.Member.Months.In.The.Past.12.Months, MbMos)
ALL_PPL %>% filter(PPL == '202005') %>% 
  select(CCNC.Member.Months.In.The.Past.12.Months, MbMos)
# ALL_PPL[ALL_PPL$PPL == '202005', 'CCNC.Member.Months.In.The.Past.12.Months'] - 
#   ALL_PPL[ALL_PPL$PPL == '202005', 'MbMos']
barplot(table(
  ALL_PPL[ALL_PPL$PPL == '202005', 'CCNC.Member.Months.In.The.Past.12.Months'] - 
    ALL_PPL[ALL_PPL$PPL == '202005', 'MbMos']
))


# * 6.5 NEW PATIENT NOT IN PREVIOUS MONTH -------------

#castedMIDPPLbackup <- castedMIDPPL
#castedMIDPPL <- castedMIDPPLbackup

castedMIDPPL
# * * 6.5.2 CALCULATION NEW PATIENT NOT IN PREVIOUS MONTH ----
# https://stackoverflow.com/questions/50639903/paste-variable-name-in-mutate-dplyr
for(ii in 2:nrow(dfPPL)){   
  #ii <- 40
  dfPPLfincol <- dfPPL$PPL[ii]
  dfPPLfincol     # Name of Month example "201809"
  C3fincol <- match(dfPPLfincol, names(castedMIDPPL))
  C3fincol        # Index of Month example "13"
  dfPPLinicol <- dfPPL$PPL[ii-1]
  dfPPLinicol
  C3inicol <- match(dfPPLinicol, names(castedMIDPPL))
  C3inicol
  castedMIDPPL[,C3inicol:C3fincol]
  castedMIDPPL[,C3inicol:C3inicol]
  castedMIDPPL <- castedMIDPPL %>% 
    mutate( !!paste0(dfPPLfincol, 'NewPt') := 
              if_else(castedMIDPPL[,C3fincol:C3fincol] == 1 & 
                        castedMIDPPL[,C3inicol:C3inicol] == 0, 1, 0))
}

# * * 6.5.3 MbMos - DELETE INDIVIDUAL MONTHS - ONLY FIELDS Member Months -----
castedMIDPPL
dim(castedMIDPPL)
castedMIDPPL <- castedMIDPPL %>% select(-names(castedMIDPPL)[1:nrow(dfPPL)+1])
castedMIDPPL
dim(castedMIDPPL)

NewPt <- melt(data.table(castedMIDPPL), 
              id.vars = c("Medicaid.ID"), 
              measure.vars = grep("NewPt", names(castedMIDPPL), value = T))
NewPt
NewPt <- NewPt %>% rename(NewPt = value, 
                          PPL = variable)
NewPt
NewPt$PPL <- substr(NewPt$PPL, 1, 6)
NewPt
ALL_PPL <- ALL_PPL %>% left_join(NewPt, by = c('Medicaid.ID', 'PPL'))
head(ALL_PPL,1)

tablexy(ALL_PPL$PPL, ALL_PPL$NewPt)
tablexy(ALL_PPL$PPL, ALL_PPL$History.Myoc.Inf)

# Pre-Last PPL
PreLastPPL <- as.character(head(tail(dfPPL[1],2),1))
# Last PPL
LastPPL <- as.character(tail(tail(dfPPL[1],2),1))
# PATIENTS UNENROLLED IN LAST MONTH
Unenrolled <- tablex(!ALL_PPL$Medicaid.ID[ALL_PPL$PPL == PreLastPPL] %in%   ALL_PPL$Medicaid.ID[ALL_PPL$PPL == LastPPL])['TRUE']
# NEW PATIENTS
NewPts <- tablex(!ALL_PPL$Medicaid.ID[ALL_PPL$PPL == LastPPL] %in%   ALL_PPL$Medicaid.ID[ALL_PPL$PPL == PreLastPPL])['TRUE']
# DIFFERENCE
tablex(ALL_PPL$PPL)
DifferenceLastTwoMonths <- length(ALL_PPL$Medicaid.ID[ALL_PPL$PPL == LastPPL]) - 
  length(ALL_PPL$Medicaid.ID[ALL_PPL$PPL == PreLastPPL])
DifferenceLastTwoMonths
recaprecap <- bind_cols(Unenrolled, NewPts, DifferenceLastTwoMonths)
names(recaprecap) <- c('Unenrolled', 'NewPts', 'Mo_Diff')

# ######################################################o
# # * 6.6 UTILIZATION HOSPITAL VISITS PER MONTH 
# 
# head(dfPPL)  # FROM OLDEST TO NEWEST
# 
# ALL_PPL %>% filter(Medicaid.ID == '101400502H') %>% arrange(PPL) %>% select(PPL, ED.Visits..Last.12.mos.)
# # onemid <- ALL_PPL %>% filter(Medicaid.ID == '101400502H') %>% arrange(PPL) %>% select(PPL, ED.Visits..Last.12.mos.)
# # tablexy(onemid$PPL, is.na(onemid$ED.Visits..Last.12.mos.))
# 
# # * * 6.6.1 MELT DCAST MID PPL 
# meltedEDV <- melt(data.table(ALL_PPL), id.vars = c("Medicaid.ID", "PPL"), 
#      measure.vars = c("ED.Visits..Last.12.mos."), value.var = "value")
# 
# #meltedEDV <- meltedEDV %>% filter(Medicaid.ID == '101400502H')
# #meltedEDV <- meltedEDV %>% filter(Medicaid.ID == '955645616N')
# #955645616N
# meltedEDV
# #meltedEDV$value[is.na(meltedEDV$value)] <- 0
# tablex(is.na(meltedEDV$value))
# meltedEDV %>% filter(is.na(value))
# 
# castEDV <- dcast(meltedEDV,  # = c("PPL_Id")),
#                  Medicaid.ID ~ PPL, fun.agg = function(x) sum(x))
# class(castEDV)
# castEDV %>% filter(Medicaid.ID == '101400502H')
# #castEDV <- data.frame(castEDV)
# getwd()
# #fwrite(castEDV, 'delete.csv')
# castEDV %>% filter(Medicaid.ID == '088649022R')
# #castEDV %>% filter(is.na(201912))
# #castEDV[is.na(castEDV)] <- 'None'
# #castEDV[,2:nrow(dfPPL)+1] <- lapply(castEDV[,2:nrow(dfPPL)+1], fx_nulltozero)
# castEDV %>% filter(castEDV[,'201912']>0)
# head(castEDV)
# 
# # casted3 <- castEDV %>% mutate( !!paste0(dfPPLfincol, 'MbMo') := 
# #                                  rowSums(casted3[,C3inicol:C3fincol]))
# 
# # * * 6.6.2 EDV VISITS PER MONTH PER PATIENT 
# # https://stackoverflow.com/questions/50639903/paste-variable-name-in-mutate-dplyr
# for(i in 3:nrow(dfPPL)){
#   ii <- 3
#   ii
#   dfPPLfincol <- dfPPL$PPL[ii]
#   dfPPLfincol # Name of Col 
#   #EDVfincol <- match(dfPPLfincol, names(castEDV))
#   EDVfincol <- match(dfPPL$PPL[ii], names(castEDV))
#   EDVfincol  # Number Index Col
#   dfPPLinicol <- dfPPL$PPL[ii-1]
#   dfPPLinicol # Name of Col 
#   EDVinicol <- match(dfPPLinicol, names(castEDV))
#   EDVinicol   # Number Index Col
#   castEDV[,EDVinicol:EDVfincol]
#   casted3 <- castEDV %>% mutate( !!paste0(dfPPLfincol, 'EDVs') := 
#                          if_else(castEDV[, ..EDVfincol] - castEDV[, ..EDVinicol] < 0, 0 ,
#                                  castEDV[, ..EDVfincol] - castEDV[, ..EDVinicol]))
# }
# 
# castEDV[,"201712"]

# 101400502H  example
# 101400502H  example



# # * 6.6 TRANSITIONAL CARE REVIEW  -
# 
# tablexy(ALL_PPL$PPL, ALL_PPL$Transitional_Care_Priority)
# 
# 
# # 10 YEAR ENDING YE DATA FRAME and PPL  --
# # Attempt to link a PPL (from enrollment) 
# # with the previous twelve months (from hospital)
# # Not used by Now. Manually changed in Power BI
# PPL_YEBlistf <- c() # Creation of empty list
# PPL_YEAlistf <- c() # Creation of empty list
# for(i in 12:nrow(dfPPL)){
#   #i <- 12
#   #dfPPL$PPL[1:12]
#   PPL_YEBlist <- dfPPL$PPL[(i-11):i]
#   PPL_YEAlist <- rep(dfPPL$PPL[i],12)
#   PPL_YEBlistf <- c(PPL_YEBlistf, PPL_YEBlist)
#   PPL_YEAlistf <- c(PPL_YEAlistf, PPL_YEAlist)
# }
# dfYearEnding <- data.frame("YE" = PPL_YEAlistf, "PPL" = PPL_YEBlistf)


# # FILL VALUES
# grep('Last', names(ALL_PPL), value = T)
# colLastInt <- match("Date.of.Last.CM.Interaction", names(ALL_PPL))
# LID202006 <- ALL_PPL %>% filter(PPL == '202006') %>% select(Medicaid.ID, Date.of.Last.CM.Interaction)
# LID202006
# ALL_PPL[ALL_PPL$PPL == '202006', c('Medicaid.ID', 'Date.of.Last.CM.Interaction')] <- 
#   ALL_PPL[ALL_PPL$PPL == '202006',c('Medicaid.ID', 'Date.of.Last.CM.Interaction')] %>% left_join(
#     ALL_PPL[ALL_PPL$PPL == '202007', 
#             c('Medicaid.ID', 'Date.of.Last.CM.Interaction')], 
#             by = c('Medicaid.ID'))#  , c('Medicaid.ID', 'Date.of.Last.CM.Interaction')]
#     [match(ALL_PPL$Medicaid.ID[ALL_PPL$PPL == '202006'],
#           ALL_PPL$Medicaid.ID[ALL_PPL$PPL == '202007']), 'Date.of.Last.CM.Interaction'] 
#     match("Date.of.Last.CM.Interaction", names(ALL_PPL))
#     ALL_PPL[ALL_PPL$PPL == '202007'][    
#     
# ALL_PPL[ALL_PPL$PPL == '202007']


# * 6.6 INDEX UNIQUE ROWS ENROLLMENT  R0000000 ---------
# For Power BI link of tables without repeated values
# ONE INDEX PER ROW
#head(Index)

ALL_PPL$Index <- paste0('E',stringr::str_pad(1:nrow(ALL_PPL), 7, side="left", pad="0"))
# To move Index field at the left of the data table
ALL_PPL <- ALL_PPL %>% select(Index, everything())
head(ALL_PPL)

# * 6.7 INDEX UNIQUE MID ENROLLMENT E0000000 ---------
# ONE INDEX PER PATIENT
#head(Index)

Unique_MIDs <- ALL_PPL %>% select(Medicaid.ID) %>% distinct(Medicaid.ID)
Unique_MIDs$MIDIndex <- paste0('ME',stringr::str_pad(1:nrow(Unique_MIDs), 6, side="left", pad="0"))
head(Unique_MIDs)
ALL_PPL <- ALL_PPL %>% inner_join(Unique_MIDs, by = c('Medicaid.ID'))
head(ALL_PPL)
str(ALL_PPL)

# * 6.8 PATIENTS COLUMN ---------

ALL_PPL$Patients <- 1L

rm(fx_append_excel_data, fx_eliminate_groups_fields, 
   fx_convmoney, fx_append_csvfiles, 
   fx_comparison_two_DFs_Fields, fx_change_Practice_Names)

ls()

# * 6.9 OTHER FIELDS ANALYZING ------

# * * 6.9.1 Date.of.Last.CM.Interaction  -------

# Measure Work Done
## Date.of.Last.CM.Interaction
## LAST_TASK_COMPLETED_DATE
str(ALL_PPL)
grep('Last', names(ALL_PPL), value = T)
tablexy(ALL_PPL$PPL, is.na(ALL_PPL$Date.of.Last.CM.Interaction))
tablexy(ALL_PPL$PPL, nchar(ALL_PPL$Date.of.Last.CM.Interaction))
grep('Inter', names(ALL_PPL), value = T)


# # 7 FILTER PPL SINCE 201901 ONLY FOR THE MONTHLY RECAP EXPORT
# # * 7.1 FILTER ONLY 2019 AND 2020 FOR POWERBI SPLIT BLOCKS GROUPS OF FIELDS
# dim(ALL_PPL)
# dim(ALL_PPL %>% group_by(PPL) %>% tally())
# #ALL_PPL <- ALL_PPL %>% filter(PPL >= '201901')
# dim(ALL_PPL %>% group_by(PPL) %>% tally())
# dim(ALL_PPL)

# 8 SPLIT TO EXPORT BLOCKS GROUPS OF FIELDS  ----------

str(ALL_PPL)

table(ALL_PPL$PPL, ALL_PPL$Pressure_Ulcer_Or_Stasis_Ulcer, useNA = 'always')
tablexy(ALL_PPL$PPL, ALL_PPL$Primary.Insurance.Provider)
dim(ALL_PPL[ALL_PPL$PPL == '202006',])

tablexy(ALL_PPL$PPL, ALL_PPL$Primary.Insurance.Provider)
tablex(ALL_PPL$PPL)

ALL_PPL$Primay.Insurance.Provider
names(ALL_PPL)

ALL_PPL[ALL_PPL$Primary.Insurance.Provider == 'NC MEDICAID',
        'Primary.Insurance.Provider'] <- 'Medicaid'
ALL_PPL <- ALL_PPL %>%
  mutate(Medicaid = if_else(Primary.Insurance.Provider == 'Medicaid' |
                              Primary.Insurance.Provider == 'NC MEDICAID', 1L, 0L))
ALL_PPL <- ALL_PPL %>%
  mutate(Health_Choice = if_else(Primary.Insurance.Provider == 'Health Choice', 1L, 0L))

grep('Hosp', names(ALL_PPL), value = T)
as.vector(t(names(ALL_PPL)))
paste(names(ALL_PPL[30:61]), collapse = "', '")
paste(names(ALL_PPL[118:139]), collapse = "', '")


# personalfields <- c("Medicaid.ID", "Base.MID", "Patient.Name", 
#                     "DOB", "Address")

patientfields <- c('Medicaid.ID', 'MIDIndex', 'Name', 'DOB', 
                   'Index', 'Age_Group', 'Age', 'Gender', 'Race',
                   'Patients', 'PPL', 'Practice_Name', 'Patient.County',
                   'Medicaid', 'Health_Choice', 'NewPt' )

patientfields[patientfields %in% names(ALL_PPL)]
patientfields[!patientfields %in% names(ALL_PPL)]
table(patientfields %in% names(ALL_PPL), useNA = 'always')
table(!patientfields %in% names(ALL_PPL), useNA = 'always')
dfPatient <- ALL_PPL %>% select(all_of(patientfields))
dim(dfPatient)

utilizationfields <- c('Index', 
                       'ED.Visits..Last.12.mos.',
                       'Inpatient.Admissions..Last.12.mos.', 
                       'Inpatient.Readmissions..Last.12.mos.', 
                       'Observation.Stays..Last.12.mos.', 
                       'Date.of.Last.CM.Interaction', 
                       'Date.of.Last.Known.Practice.Visit', 
                       'Date.of.Last.Known.Wellness.Visit')

utilizationfields[utilizationfields %in% names(ALL_PPL)]
utilizationfields[!utilizationfields %in% names(ALL_PPL)]

dfUtilization <- ALL_PPL %>% select(all_of(utilizationfields))
dim(dfUtilization)

conditionsfields <- c('Index', 'Dual',
                      'Any.MH', 'ADHD', 'Anxiety',
                      'Autism', 'Bipolar.Disorder', 
                      'Conduct.Disorder', 'Depression', 
                      'Eating.Dis', 'OCD', 'Personality', 
                      'PTSD', 'Schizophrenia.Schizoaffective', 
                      'Substance.Abuse', 'Dev.Disab', 
                      'Asthma', 'Cancer', 'CVA.Stroke', 'CHF', 
                      'Chr.GastroInt.Dis', 'Chr.Kidney.Dis', 
                      'Chr.Liver.Dis', 
                      'Chr.Neurological.Dis', 'Chronic.Pain', 
                      'COPD', 'Musculoskeletal.Connective.Tissue.Dis', 
                      'Dementia', 'Diabetes', 'HIV', 'Hypertension', 
                      'Ischemic.Vascular.Dis', 
                      'History.Myoc.Inf', 'Sickle.Cell', 
                      'ABD', 'Foster_Care_Indicator', 'SPMI', 
                      'Pressure_Ulcer_Or_Stasis_Ulcer',
                      'Palliative_Care_Indicator', 
                      'CCNC_Priority', 'Transitional_Care_Priority', 
                      'New_CCNC_Priority_since_prior_month')

conditionsfields[conditionsfields %in% names(ALL_PPL)]
conditionsfields[!conditionsfields %in% names(ALL_PPL)]
dfConditions <- ALL_PPL %>% select(all_of(conditionsfields))
dim(dfConditions)

costfields <- c('Index', 
                'Total.Healthcare.Cost..Last.12.mos.....',
                'Inpatient.Cost..Last.12.mos....', 
                'Outpatient.Cost..Last.12.mos....', 
                'Professional.Cost..Last.12.mos....', 
                'Emergency.Room.Cost..Last.12.mos....', 
                'Lab.Radiology.Cost..Last.12.mos....', 
                'Drug.Cost..Last.12.mos....', 
                'Specialty.Drug.Cost..Last.12.mos....', 
                'Mental.Health.Cost..Last.12.mos....', 
                'Nursing.Home.Cost..Last.12.mos....', 
                'Home.Health.Cost..Last.12.mos....', 
                'Hospice.Cost..Last.12.mos....', 
                'Personal.Care.Services.Cost..Last.12.mos....', 
                'Private.Duty.Nurse.Cost..Last.12.mos....', 
                'Durable.Medical.Equipment.Cost..Last.12.mos....', 
                'Therapy.Services..PT.RT.ST..Cost..Last.12.mos....', 
                'Dental.Cost..Last.12.mos....', 
                'Other.Institutional.Cost..Last.12.mos....', 
                'Management.Fee.Cost..Last.12.mos....', 
                'Capitation.Cost..Last.12.mos....', 
                'RHC.FQHC.Health.Dept.Cost..Last.12.mos....', 
                'Ambulance.Cost..Last.12.mos....', 
                'CCNC.Member.Months.In.The.Past.12.Months', 
                'Medicaid.Eligibility.Months', 
                'MbMos')

costfields[!costfields %in% names(ALL_PPL)]
dfCost <- ALL_PPL %>% select(all_of(costfields))
dim(dfCost)

pharmacyfields <- c('Index', 'Pharmacy.Fills..Last.12.mos.')
pharmacyfields[!pharmacyfields %in% names(ALL_PPL)]
dfPharmacy <- ALL_PPL %>% select(all_of(pharmacyfields))

NewPtMbMosLastPPL <- c('Medicaid.ID', 'NewPt', 'MbMos', 'PPL' )
dfNewPtMbMosLastPPL <- ALL_PPL %>% select(all_of(NewPtMbMosLastPPL))
dfNewPtMbMosLastPPL <- ALL_PPL %>% filter(PPL == max(dfPPL$PPL)) %>% select(all_of(NewPtMbMosLastPPL))
dim(dfNewPtMbMosLastPPL)

tablex(ALL_PPL$PPL)
# distinct(ALL_PPL$PPL)
head(ALL_PPL)

# %>% arrange(desc(PPL)) %>% pull(PPL)
PPL12 <- ALL_PPL %>% distinct(PPL, .keep_all = FALSE) %>% top_n(12) %>% pull(PPL)  
PPL12
ALL_PPL %>% filter(PPL %in% PPL12) %>% distinct(Medicaid.ID) %>% tally()

ALL_PPL %>% filter(PPL == '202106') %>% distinct(Medicaid.ID) %>% tally()

ALL_PPL %>% filter(PPL == '202105') %>% distinct(Medicaid.ID) %>% tally()


# 9  FWRITE BLOCKS OF DATA BY PERIOD -------------------

#setwd("D:/Information Technology/DATA_Reports/ACCREDITATION REPORTS/data_ENROLLMENT_HOSPITAL_VISITS_from_R")
setwd("C:/Users/bbacareyes/")
setwd("./OneDrive - Carolina Collaborative Community Care/")
setwd("./DATA_Reports/")
setwd("./DATAPROCESSED_FROM_R") # SET WORKING DIRECTORY

#fwrite(racetable,     "racetable_from_R.csv")
fwrite(dfPatient,     "dfPatient_from_R.csv")
fwrite(dfUtilization, "dfUtilization_from_R.csv")
fwrite(dfConditions,  "dfConditions_from_R.csv")
fwrite(dfCost,        "dfCost_from_R.csv")
fwrite(dfPharmacy,    "dfPharmacy_from_R.csv")
fwrite(dfNewPtMbMosLastPPL,  paste0("dfNewPtMbMosLastPPL_",max(dfPPL$PPL),".csv"))
fwrite(recaprecap,  paste0("recaprecap_",max(dfPPL$PPL),".csv"))

#fwrite(dfYearEnding,  "dfYEAR_ENDING_from_R.csv")
# fwrite(CheckEDV06,        "CheckEDV06.csv")

Sys.time() - st

# 10 POST CALC VERIFICATION - MATRIX OF RESULTS PPL, Pts, EDVs, IPVs, Readm. ----
dfPts <- ALL_PPL %>% group_by(PPL) %>% summarise(Pts = sum(Patients, na.rm = T)) 
dfEDVs <- ALL_PPL %>% group_by(PPL) %>% summarise(EDVs = sum(ED.Visits..Last.12.mos., na.rm = T))
dfIPVs <- ALL_PPL %>% group_by(PPL) %>% summarise(IPVs = sum(Inpatient.Admissions..Last.12.mos., na.rm = T))
dfReadm <- ALL_PPL %>% group_by(PPL) %>% summarise(Readm = sum(Inpatient.Readmissions..Last.12.mos., na.rm = T))
dfMbMos <- ALL_PPL %>% group_by(PPL) %>% summarise(MbMos = sum(MbMos, na.rm = T))

t(dfPts %>% inner_join(dfEDVs, by = 'PPL') %>% 
    inner_join(dfIPVs, by = 'PPL') %>% inner_join(dfReadm, by = 'PPL') %>% inner_join(dfMbMos, by = 'PPL'))  

head(ALL_PPL %>% group_by(PPL) %>% summarise(EDVs = sum(ED.Visits..Last.12.mos., na.rm = T)), 20)
tail(ALL_PPL %>% group_by(PPL) %>% summarise(EDVs = sum(ED.Visits..Last.12.mos., na.rm = T)),20)
tobarplot <- ALL_PPL %>% group_by(PPL) %>% summarise(EDVs = sum(ED.Visits..Last.12.mos., na.rm = T))

# Leave it with NAs to Auto Fill in Power BI
table(ALL_PPL$PPL, is.na(ALL_PPL$'ED.Visits..Last.12.mos.'))
utilizationfields %in% names(ALL_PPL)

# 10.1 POST BARPLOTS ----------
EDVbarplot <- ALL_PPL %>% group_by(PPL) %>% arrange(PPL) %>% 
  summarise(EDVs = sum(ED.Visits..Last.12.mos., na.rm = T))
EDVbarplot <- data.frame(EDVbarplot)
str(EDVbarplot)
barplot(EDVbarplot$EDVs, 
        main="ED Visits in the Last 12 months",
        xlab = "PPL",
        ylab = "ED Visits", col = 'lightblue', 
        las = 2, 
        cex.names = 0.7)
ggplot(EDVbarplot, aes(PPL, EDVs)) +    # ggplot2 plot with modified x-axis labels
  geom_bar(stat = "identity", fill = "#158000") +
  theme(axis.text.x = element_text(angle = 90, size = 10)) +
  ggtitle("ED Visits in the Last 12 months")

#######################################.
IPVplot <- ALL_PPL %>% group_by(PPL) %>% arrange(PPL) %>% 
  summarise(IPVs = sum(Inpatient.Admissions..Last.12.mos., na.rm = T))
barplot(IPVplot$IPVs, 
        main="IP Visits in the Last 12 months",
        xlab = "PPL",
        ylab = "IP Visits", col = 'lightgray')
ggplot(IPVplot, aes(PPL, IPVs)) +    # ggplot2 plot with modified x-axis labels
  geom_bar(stat = "identity", fill = "#990000") +
  theme(axis.text.x = element_text(angle = 90, size = 10)) + 
  ggtitle("IP Visits in the Last 12 months")

#######################################.
READMBARPLOT <- ALL_PPL %>% group_by(PPL) %>% arrange(PPL) %>% 
  summarise(ReadmIPVs = sum(Inpatient.Readmissions..Last.12.mos., na.rm = T))
tail(READMBARPLOT)
barplot(READMBARPLOT$ReadmIPVs, 
        main="IP Readmissions in the Last 12 months",
        xlab = "PPL",
        ylab = "IP Readmissions", col = 'lightgray')
ggplot(READMBARPLOT, aes(PPL, ReadmIPVs)) +    # ggplot2 plot with modified x-axis labels
  geom_bar(stat = "identity", fill = "#8000FF") +
  theme(axis.text.x = element_text(angle = 90, size = 10)) + 
  ggtitle("IP Readmissions in the Last 12 months")
#######################################.
tail(ALL_PPL %>% group_by(PPL) %>% arrange(PPL) %>% 
       summarise(MbMos = sum(MbMos, na.rm = T)))
tobarplot <- ALL_PPL %>% group_by(PPL) %>% arrange(PPL) %>% 
  summarise(MbMos = sum(MbMos, na.rm = T))
barplot(tobarplot$MbMos, 
        main="Monthly Total of Member Months",
        xlab = "PPL",
        ylab = "Member Months", col = 'brown')

# POST CALC CM2A  

dim(ALL_PPL %>% filter(PPL %in% c('201709', '201710', '201711')) %>% distinct(Medicaid.ID))
dim(ALL_PPL %>% filter(PPL %in% c('201907', '201908', '201909')) %>% distinct(Medicaid.ID))
dim(ALL_PPL %>% filter(PPL %in% c('202007', '202008', '202009')) %>% distinct(Medicaid.ID))
PPL_3mos_2019_uniquepts <- ALL_PPL %>% filter(PPL %in% c('201907', '201908', '201909')) %>% select(Medicaid.ID, ED.Visits..Last.12.mos.)

#######################################################.
# 11 RECAP ED VISITS MARCH APRIL 2021 ----------------------
#######################################################.
#######################################################.
#########################################################.
# ENROLLMENT RECAP LAST 18 MONTHS FOR POWER BI
#########################################################.
# NOTE: 
# EVALUATE IF IT IS CONVENIENT TO SUMMARISE DATA, AS WITH POWER BI IS POSSIBLE TO 
# FILL NULL VALUES. PENDING TO REVIEW FOR ENROLLMENT MARCH 2021
# BETTER TO CONVERT 'Yes' to 1 to reduce table to dcast

dtRECAP <- dfPatient %>% inner_join(dfConditions, by = c('Index'))
dtRECAP <- dtRECAP %>% inner_join(dfUtilization, by = c('Index'))
dtRECAP <- dtRECAP %>% inner_join(dfCost, by = c('Index'))
#dtRECAP <- dtRECAP %>% inner_join(dfCost, by = c('Index'))
#rm(dfPatient, dfConditions, dfUtilization)
str(dtRECAP)
dim(dtRECAP)
grep('MbMos', names(dtRECAP))

dtRECAP <- dtRECAP %>% filter(PPL %in% c('202103', '202104')) %>% 
  select(Medicaid.ID, MIDIndex, Name, DOB, PPL, ED.Visits..Last.12.mos., MbMos)

# ALLUVIAL CHECK --------
tttt <- data.table(dtRECAP %>% group_by(MIDIndex, Medicaid.ID, Name, DOB, PPL) %>% 
                     summarise(EDV = sum(ED.Visits..Last.12.mos.)))
tttt
ttt2 <- dcast(tttt, MIDIndex + Medicaid.ID + Name + DOB ~ PPL, value.var = 'EDV', fill = 0)
ttt2
ttt3 <- ttt2 %>% group_by(`202103`, `202104`) %>% tally()
ttt3
dim(ttt3)
ttt4 <- ttt3[!(ttt3$`202103` == 0 & ttt3$`202104` == 0),]

alluvial(ttt4[,1:2], 
         freq=ttt4$n)#, 
#         hide = recap2$x == 0)

ttt2$DIFF <- ttt2$`202104` - ttt2$`202103`
tablex(ttt2$DIFF)
ttt2[ttt2$DIFF == 10,]
hist(ttt2$DIFF, breaks = 100)

# RECAP EDVs 202103 and 202104 ----------

dtRECAPr <- dtRECAP %>% mutate(EDVs = 
                                 if_else(ED.Visits..Last.12.mos. >= 11, "11+", 
                                         sprintf("%02d", dtRECAP$ED.Visits..Last.12.mos.)))
tablexy(dtRECAPr$EDVs, dtRECAPr$PPL)

recap2 <- data.table(dtRECAP %>% group_by(PPL, ED.Visits..Last.12.mos.) %>% tally())
recap2

str(dtRECAPr)
dim(dtRECAPr)
dtRECAPr$ED.Visits..Last.12.mos.

#dtRECAP<- 
# text(dtRECAP$ED.Visits..Last.12.mos., "00")
# text(dtRECAP$ED.Visits..Last.12.mos., "00")
#dtRECAP$ED.Visits..Last.12.mos.[dtRECAP$ED.Visits..Last.12.mos. >= 15] <- '15+'

tablexy(dtRECAP$EDVs, dtRECAP$PPL)
dtRECAP
recap3 <- tablexy(dtRECAP$ED.Visits..Last.12.mos., dtRECAP$PPL)
barplot(tablexy(dtRECAP$PPL, dtRECAP$EDVs), beside = T)

dtRECAP
dtRECAP
library(alluvial)
dtRECAP$ED.Visits..Last.12.mos.[dtRECAP$ED.Visits..Last.12.mos. >= 11] <- 11
recap2
alluvial(recap3[,1:2], 
         freq=recap3[,1:2], 
         hide = recap2$x == 0)


dtRECAP %>% group_by(PPL) %>% summarise(EDVs = sum(ED.Visits..Last.12.mos.))




sum(ttt2$`202103`)
sum(ttt2$`202104`)

melt(dtRECAP, )

tablex(dtRECAP$PPL)
tablexy(dtRECAP$MIDIndex, dtRECAP$PPL)
dcast(data = dtRECAP, MIDIndex ~ PPL, var=ED.Visits..Last.12.mos.)


tablexy(dtRECAP$PPL, dtRECAP$NewPt)
tablexy(dtRECAP$PPL, dtRECAP$ABD)
#tablexy(dtRECAP$PPL, dtRECAP$New_CCNC_Priority_since_prior_month)
Sys.time() - st
ls()
list=ls()

#list[list != 'dtRECAP']
list[!list %in% c('dtRECAP', 'st', 'tablex', 'tablexy')]

rm(list=list[!list %in% c('dtRECAP', 'st', 'tablex', 'tablexy')])    # Delete all objects

