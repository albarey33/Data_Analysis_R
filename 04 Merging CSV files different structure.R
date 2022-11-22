
#######################################################################################.
##################### CARE MANAGEMENT - MONTHLY ENROLLMENT  ##########################.
# SCRIPT / FUNCTION TO HIGHLIGHT DISCREPANCIES SO THAT THEY CAN BE DELETE OR RENAMED
# USE CASE: ARRANGE AND RENAME FIELDS WITH DIFFERENT STRUCTURE BEFORE MERGE THEM
# FOR DATA ANALYSIS OF CARE MANAGEMENT RESULTS
# Data Source: PATIENT HEALTH DASHBOARD (PHD) - POPULATION SUMMARY
# FREQUENCY: Execute this script after enrollment ONCE A MONTH
######################################################################################.

rm(list=ls()) # Delete all objects
ls() # List variables in memory

# .libPaths("C:/R_Libraries")     # Defined as Environmental Variable
# .libPaths() ###  Check the default folder where packages are installed

# 0 PREPARE INSTALL CALL PACKAGES -----------------------------------------
required_packages <- c("dplyr")
lapply(required_packages, library, character.only = TRUE)

# 1 PARAMETERS CHANGE NAMES / UPDATE --------------------------------------------------

# Paths # Location of Source files
path            <- "PopulationSamples"       

# 2 READ THE DOWNLOADED DATA EXCEL FILES ------------------------------------

# Read sample files using full path and regex
filenames_list <- list.files(path= path, full.names=TRUE, 
                             pattern=paste0("^Different.*?.csv"))

# ######### FUNCTION READ ONE CSV FILE
fx_append_csvfile <- function(list_of_csv_file){
   df.list <- read.csv(list_of_csv_file)
   print(paste("Number of records in ",
               list_of_csv_file," = ",nrow(df.list[[1]]),
               " ; columns = ",length(df.list[[1]]),sep=""))
   df.list <- data.frame(df.list)
   df.list
 }

# Reading files individually - Apply defined function each CSV
tibble_1mo <- fx_append_csvfile(filenames_list[1])
tibble_2mo <- fx_append_csvfile(filenames_list[2])

head(tibble_1mo,2)
head(tibble_2mo,2)

# Example of Merging tables with different structure / field names
str(bind_rows(tibble_1mo, tibble_2mo))  

# 3 COMPARISON FIELDS TWO DATA FRAMES STRUCTURE AND COLUMN NAMES -----------------------
# The first file has 9 columns and the second file has 12 columns. Before 
# merge them all the columns have to match in names.

######### FUNCTION COMPARISON - ONLY SHOW DISCREPANCIES
fx_comparison_two_DFs_Fields <- function(df_x, df_y){
  LeftW <- data.frame(field=names(df_x),match=match(names(df_x), names(df_y)), "L in R")
  RightW <- data.frame(field=names(df_y),match=match(names(df_y), names(df_x)),"R in L")
  compardf <- LeftW %>% full_join(RightW, by= 'field') %>% filter(is.na(match.x) | is.na(match.x) )
  print(compardf)
}

fx_comparison_two_DFs_Fields(tibble_1mo, tibble_2mo)

# DELETE FIELDS TO MATCH BOTH DATA TABLES

tibble_2mo <- tibble_2mo %>% select(-c("Primary.Insurance.Provider",
                                       "Medicare.ID","Commercial.ID"))

fx_comparison_two_DFs_Fields(tibble_1mo, tibble_2mo)

# RENAME FIELDS TO MATCH BOTH DATA TABLES

tibble_1mo <- tibble_1mo %>% rename(Medicaid.ID = Current.MID,
                      Name = Patient.Name,
                      DOB = Patient.DOB,
                      Age = Patient.Age,
                      Gender = Patient.Gender,
                      Phone = Patient.Phone,
                      Address = Patient.Address,
                      City = Patient.City,
                      Zip = Patient.Zip
                      )

fx_comparison_two_DFs_Fields(tibble_1mo, tibble_2mo)

# Result zero fields with discrepancies. The fields are ready to merge correctly

bind_rows(tibble_1mo, tibble_2mo)

# Other option to rename fields using data.table
# Three_Enroll <- data.table::setnames(tibble_1mo, 
#                                      old=c("Current.MID","Patient.Name"),
#                                      new=c("Medicaid.ID","Name"))

# END -------
