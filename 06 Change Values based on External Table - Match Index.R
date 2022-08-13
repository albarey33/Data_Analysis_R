
###################################################################.
# SCRIPT: REPLACE VALUES BY TAKEN INFO FROM ANOTHER TABLE
# For this example, the table will be written in the same script but it 
# might be imported.
# Example: Change old names of Practices by new ones defined in table
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
PPL_df %>% group_by(Practice_Name) %>% tally() %>% arrange(desc(n))


# 3 RENAME PCP PROVIDERS  ------------------------------
# Renaming some Providers (Special cases) Change values

# * 3.1 PRELIMINAR VIEW ------

PPL_df[PPL_df$Practice %in% c("CAPE FEAR FAMILY MEDICAL CARE 1340 WALTER REED",
                            "CAPE FEAR FAMILY MEDICAL CARE 413 OWEN DR",
                            "CAPE FEAR FAMILY MEDICAL CARE 405 OWEN DR",
                            "CAPE FEAR FAMILY MED CARE",
                            "CAROLINA RHEUMATOLOGY AND INTERNAL MEDICINE", 
                            "CAPE FEAR VALLEY PRIMARY CARE - JOHN SMITH",
                            # "WOMEN'S HEALTH HAVEN", 
                            # "CFVMC DIVISION OF DUKE OB/GYN",
                            "FAYETTEVILLE GERIATRIC & INTERNAL MEDICINE",
                            "CAROLINA PRIMARY & INTERNAL MEDICINE",
                            "WADE FAMILY MEDICAL CENTER",
                            "STEDMAN - WADE HEALTH SERVICES"),] %>% 
                            # "CAPE FEAR VALLEY PRIMARY CARE - SKIBO ROAD",
                            # "CAPE FEAR VALLEY PRIMARY CARE - FAYETTEVILLE FAMILY"
                            group_by(Practice_Name) %>% tally() %>% arrange(desc(n))



# * 3.2 Create a table with name changes  -----
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
# PCPch[nrow(PCPch)+1,] <- c("WOMEN'S HEALTH HAVEN", 
#                            "CFVMC DIVISION OF DUKE OB/GYN")
PCPch[nrow(PCPch)+1,] <- c("FAYETTEVILLE GERIATRIC & INTERNAL MEDICINE",
                           "CAROLINA PRIMARY & INTERNAL MEDICINE")
PCPch[nrow(PCPch)+1,] <- c("WADE FAMILY MEDICAL CENTER",
                           "STEDMAN - WADE HEALTH SERVICES")
# PCPch[nrow(PCPch)+1,] <- c("CAPE FEAR VALLEY PRIMARY CARE - SKIBO ROAD",
#                            "CAPE FEAR VALLEY PRIMARY CARE - FAYETTEVILLE FAMILY")
PCPch
str(PCPch)

# * 3.3 FUNCTION CHANGE PRACTICE NAMES SHOWING CHANGED RECORDS -------
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

# * 3.4 Apply change of names ----

PPL_df <- fx_change_Practice_Names("Practice_Name", PPL_df, PCPch)


# * 3.5 RESULTING  VIEW ----------

PPL_df[PPL_df$Practice %in% c("CAPE FEAR FAMILY MEDICAL CARE 1340 WALTER REED",
                              "CAPE FEAR FAMILY MEDICAL CARE 413 OWEN DR",
                              "CAPE FEAR FAMILY MEDICAL CARE 405 OWEN DR",
                              "CAPE FEAR FAMILY MED CARE",
                              "CAROLINA RHEUMATOLOGY AND INTERNAL MEDICINE", 
                              "CAPE FEAR VALLEY PRIMARY CARE - JOHN SMITH",
                              # "WOMEN'S HEALTH HAVEN", 
                              # "CFVMC DIVISION OF DUKE OB/GYN",
                              "FAYETTEVILLE GERIATRIC & INTERNAL MEDICINE",
                              "CAROLINA PRIMARY & INTERNAL MEDICINE",
                              "WADE FAMILY MEDICAL CENTER",
                              "STEDMAN - WADE HEALTH SERVICES"),] %>% group_by(Practice_Name) %>% tally() %>% arrange(desc(n))
                              # "CAPE FEAR VALLEY PRIMARY CARE - SKIBO ROAD",
                              # "CAPE FEAR VALLEY PRIMARY CARE - FAYETTEVILLE FAMILY"

Sys.time() - st

########################### END ------ 

