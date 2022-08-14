
###################################################################.
# SCRIPT: DATA PRE-PROCESSING EXTRACTING MULTIPLE FILES, CHECKING RECORDS
# ADD COLUMNS WITH THE DATA FILE NAME
# Change Date formats. # Convert POSIXct, format to Date, format
# Interactions_WEEK_26_2021_20210630A_REVIEW_20210921.R
########################################################################.

# 0 PREPARE INSTALL CALL PACKAGES -----------------------------------------

rm(list=ls()) # Delete all objects
ls() # List variables in memory

options(header=T, sep=",", stringsAsFactors = FALSE,
        str = strOptions(strict.width = "cut"),
        readr.show_progress=FALSE)
options(str = strOptions(strict.width = "cut"))

st <- Sys.time()

# Load-multiple-packages-at-once
required_packages <- c("dplyr", "data.table", "lubridate", 
                       "tidyverse", "readxl")
lapply(required_packages, FUN = 'library', character.only = TRUE)

tibble::tibble(a = 1)

# 1 PARAMETERS CHANGE NAMES / UPDATE --------------------------------------------------

# Paths # Location of Source files
path            <- "REPORT_INTERACTIONS_VH//DATA_Interactions_by_CM//"

# path            <- "PopulationSamples"       
#resultingfile   <- "data_merging_files//result_table.csv" 

# Location of resulting file ----
# resultingfile   <- paste0("PopulationSamples//MergedFile", currPPL, ".csv")

# 2 READ THE DOWNLOADED DATA EXCEL FILES ------------------------------------
# Read sample files using full path and regex (known pattern)

list_files <- list.files(path= path, full.names=TRUE, 
                             pattern=paste0("^Interactions.*?.xlsx"))
list_files

##################################################################$
# 1 LAST WEEK DATA -----------------------------------------------
##################################################################$

# * 1.1 Update Names Last Week -------------------------------------------------
currentweek   <- "Week_2021_26" # Next to apply # UNTIL JUNE 30
##########previousweek <- "Week_2021_04" # Next to apply
mondaycurrentweek <- as.Date("2021-06-28")   # Monday
weekdays(mondaycurrentweek)                  # Verification
mondaycurrentweek + 6
lastdaycurrentweek <- mondaycurrentweek + 6  # Sunday
weekdays(lastdaycurrentweek)                 # Verification

# * 1.2 Reading data excel files per Staff Member -------------------------------


seq_along(list_files)
paste0("Elements in List (Care Managers): ",length(list_files))

# fx_append_excel_data(list_files)

##### Read Excel files - tibbles df.list

vectortypes <- c("date",rep(c("text"),3),"date",rep(c("text"),7))
#vectortypes <- rep(c("text"),12)
vectortypes

# * 2.2 Function: Read Excel files with equal structure --------

fx_readfiles <- function(filename){
  #print(paste("Merging",basename(filename),sep = " "))
  xlfile <- readxl::read_excel(filename, col_types = vectortypes)
  print(paste("Number of records in ",
              basename(filename)," = ",nrow(xlfile),
              " ; columns = ",length(xlfile),sep=""))
  dfXLfile <- data.frame(xlfile)
  dfXLfile
}

# Apply defined function to list / Create tibbles
df.list <- lapply(list_files,fx_readfiles)
df.list   # All tibbles

# * 1.3 Add as column the original data file name -------------
##### Add file name as column per list
for( i in seq_along(df.list)){
  df.list[[i]]$ExcelFile <- rep(basename(list_files[i]),nrow(df.list[[i]])) # list_files[i]
  #print(head(df.list[[i]]$ExcelFile,1))
}

# * 1.4 SKIP Tibbles with ZERO rows ----------

fx_append_non_zero_data <- function(ListCMs){
  options(message = TRUE)
  mytypelist <- list() # Creation of empty list
  for(i in seq_along(ListCMs)){   # Data in Three Groups or Files
    if(nrow(ListCMs[[i]]) != 0){
      mytypelist <- append(mytypelist, ListCMs[i])
    } 
    else {
      mytypelist <- mytypelist
    }
  }
  mytypelist
}

length(df.list)
df.list <- fx_append_non_zero_data(df.list)
length(df.list)

# * 1.5 Bind rows from list and convert to data.frame ### Convert  --------
dfWK <- data.frame(bind_rows(df.list))
str(dfWK)
dim(dfWK)
head(dfWK,1)

# 2 VERIFICATION OF CARE MANAGER NAMES BY SUBMITTED BY ----

nrow(data.frame(dfWK %>% group_by(ExcelFile, Submitted.By) %>% tally()))
data.frame(dfWK %>% group_by(ExcelFile, Submitted.By) %>% tally())

# 3 CONVERT POSIXct, format to Date, format  -----

dfWK[] <- lapply(dfWK, function(x) {
  if (inherits(x, "POSIXt")) lubridate::as_date(x) else x
})

# Convert POSIXct, format to Date, format
# class(base::as.Date(dfWK$Date.of.Interaction))
# class(lubridate::as_date(dfWK$Date.of.Interaction))
# dfWK$Date.of.Interaction <- lubridate::as_date(dfWK$Date.of.Interaction)
# dfWK$DOB <- lubridate::as_date(dfWK$DOB)

# In other cases, to convert date format to character
# df.list[[i]]$Date.of.Interaction <- format(df.list[[i]]$Date.of.Interaction, format="%m/%d/%Y")
# df.list[[i]]$DOB <- format(df.list[[i]]$DOB, format="%m/%d/%Y")
# dfWK$Date.of.Interaction <- lubridate::mdy(dfWK$Date.of.Interaction)
class(dfWK$Date.of.Interaction)
head(dfWK$Date.of.Interaction)
str(dfWK$Date.of.Interaction,1)
table(dfWK$Date.of.Interaction)

# 5 CHECK BARPLOT INTERACTIONS DATES ------

hist(dfWK$Date.of.Interaction, breaks = 10, col = 'lightblue')
#dfWK %>% filter(Date.of.Interaction == "2021-01-18") # MLK day


tobarplot <- dfWK %>% 
  group_by(Date.of.Interaction) %>% 
  summarise(PerDay = length(Date.of.Interaction))
tobarplot
barplot(tobarplot$PerDay, 
        xlab = 'Date.of.Interaction',
        names.arg = tobarplot$Date.of.Interaction, col = 'lightblue')
#dfWK <- dfWK %>% filter(Date.of.Interaction >= mondaycurrentweek &
#                 Date.of.Interaction <= lastdaycurrentweek)
dim(dfWK)

table(dfWK$Date.of.Interaction, useNA = 'always')


# 5 SAVE FWRITE LAST INTERACTIONS --------------------------

# paste("DATA_FROM_",currentweek,".csv", sep = "") # <<<< NEW DATA FILE
# data.table::fwrite(dfWK, file=paste("DATA_FROM_",currentweek,".csv", sep = ""))
# 
# str(dfWK)
# 
# rm(dfWK)

Sys.time() - st

