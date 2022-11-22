#####################################################################.
# SCRIPT TO COLLECT INTERACTIONS PER CARE MANAGER AND MERGE DATA
# Weekly Process
# Pre-Process to Interactions All Week
# This file is save in the repository without data
#####################################################################.

# 0. PREPARE INSTALL CALL PACKAGES ---------------------------------------------------

rm(list=ls())

ls()
options(str = strOptions(strict.width = "cut"))
st <- Sys.time()

#devtools::session_info()
# .libPaths("C:/R_Libraries")
.libPaths() ###  Check the default folder where packages are installed

# Load-multiple-packages-at-once
required_packages <- c("dplyr", "data.table", "lubridate", 
                       "tidyverse", "readxl")
lapply(required_packages, FUN = 'library', character.only = TRUE)

tibble::tibble(a = 1)

########## FUNCTION APPEND EXCEL FILES
# fx_append_excel_data <- function(DataXLSfiles){
#   options(message = TRUE)
#   mytypelist <- list() # Creation of empty list
#   for(i in seq_along(DataXLSfiles)){   # Data in Three Groups or Files
#     details <- file.info(DataXLSfiles)
#     #details <- details[with(details, order(as.POSIXct(mtime), decreasing = TRUE)),]
#     file.list <- rownames(details[1])  # Only name column[1]
#     df.list <- lapply(file.list[i], read_excel)  ######### Convert list to Data frame 
#     df.list[[1]]$ExcelFile <- file.list[i]               # R will recycle values
#     #    df.list[[1]]$AddedCol <- rep(vectorvh[i],nrow(df.list[[1]])) # Not needed # Not needed
#     print(paste(i," ","Number of records in file ",file.list[i]," = ",nrow(df.list[[1]])," ; columns = ",length(df.list[[1]]),sep=""))
#     print(nrow(df.list[[i]]))
#     if(nrow(df.list[[i]]) != 0){
#       mytypelist <- append(mytypelist, df.list)
#     } else {
#       mytypelist <- mytypelist
#     }
#   }
#   mytypelist
# }

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

#DATA_Reports\REPORT_INTERACTIONS_VH\DATA_Interactions_by_CM
setwd("./DATA_Reports/REPORT_INTERACTIONS_VH/DATA_Interactions_by_CM")
getwd()

list_files <- list.files(pattern='^Client_')
# fileinfolist_files <- file.info(list.files(pattern='^Client_'))
# fileinfolist_files
# fileinfolist_files <- fileinfolist_files[substr(rownames(fileinfolist_files),1,1) != 0,]   # Delete datafiles with 0 rows

head(list_files)
class(list_files)
list_files
head(file.info(list_files))   # Details of Files
length(list_files)
seq_along(list_files)
paste0("Elements in List (Care Managers): ",length(list_files))

# fx_append_excel_data(list_files)

##### Read Excel files - tibbles df.list
df.list <- lapply(list_files, read_excel)
seq_along(df.list)                    # Currently members in Staff
df.list   # All tibbles

# * 1.3 Number of Rows per Tibble  # Check out for Zeros -------------

#mynrowslist <- list()
# Method 1

length(df.list[[30]])  # Columns one Tibble
for( i in seq_along(df.list)){
  print(paste0("Interactions for ", list_files[i], " / Rows: ",
               nrow(df.list[[i]]), " / Cols: ", length(df.list[[i]])
               )
        )
}

##### Add file name as column per list
for( i in seq_along(df.list)){
  df.list[[i]]$ExcelFile <- rep(list_files[i],nrow(df.list[[i]])) # list_files[i]
}
str(df.list[[30]])

###### To know if a specific field have the same class in all the tibbles
for( i in seq_along(df.list)){
  print(class(df.list[[i]]$`Date of Interaction`))
}

# df.list2930 <-  append(df.list[[29]], df.list[[30]])

# * 1.4 EXCLUDE Tibbles with ZERO rows ----------

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

# for( i in seq_along(df.list)){
#   a <- 0L
#   nrow(df.list[[i]]) != 0
#   a = a + 1
# } else {
#   a = a
# }

length(df.list)
df.list <- fx_append_non_zero_data(df.list)
length(df.list)

# * 1.5 Bind rows from list and convert to data.frame ### Convert  --------
dfWK <- data.frame(bind_rows(df.list))
str(dfWK)
dim(dfWK)
head(dfWK,1)
dim(dfWK)

# 2 VERIFICATION OF CARE MANAGER NAMES BY SUBMITTED BY ----

#head(distinct(dfWK[,c('ExcelFile','Submitted.By')]),15)
nrow(data.frame(dfWK %>% group_by(ExcelFile, Submitted.By) %>% tally()))
head(data.frame(dfWK %>% group_by(ExcelFile, Submitted.By) %>% tally()),16)
tail(data.frame(dfWK %>% group_by(ExcelFile, Submitted.By) %>% tally()),16)

str(dfWK)

# 3 CHECK DATE OF INTERACTION DATE  -----
# CONVERT TO DATE: Date of Interaction
class(dfWK$Date.of.Interaction)
dfWK$Date.of.Interaction <- lubridate::mdy(dfWK$Date.of.Interaction)
class(dfWK$Date.of.Interaction)
head(dfWK$Date.of.Interaction)
str(dfWK$Date.of.Interaction,1)
dfWK$Date.of.Interaction <- as_date(dfWK$Date.of.Interaction)
class(dfWK$Date.of.Interaction)
head(dfWK$Date.of.Interaction)
table(dfWK$Date.of.Interaction)

par("mar")
par(mar=c(3,3,4,3))

hist(dfWK$Date.of.Interaction, breaks = 150, col = 'lightblue')
#dfWK %>% filter(Date.of.Interaction == "2021-01-18") # MLK day

# Change of class Member Number
dfWK$Member.Number <- as.double(dfWK$Member.Number)
class(dfWK$Member.Number)

str(dfWK)
dim(dfWK)


# 4 CHANGE DATE IF NECESSARY FOR FIX WRONG DATES (CHECK EXCEPTIONS) ----

# * 4.1 Check Barplot BEFORE CHANGE IN DATES FOR HOLIDAYS ------
 
tobarplot <- dfWK %>% 
  group_by(Date.of.Interaction) %>% 
  summarise(PerDay = length(Date.of.Interaction))
tobarplot
barplot(tobarplot$PerDay, 
        xlab = 'Date.of.Interaction',
        names.arg = tobarplot$Date.of.Interaction, col = 'gray')
#dfWK <- dfWK %>% filter(Date.of.Interaction >= mondaycurrentweek &
#                 Date.of.Interaction <= lastdaycurrentweek)
dim(dfWK)

table(dfWK$Date.of.Interaction, useNA = 'always')

# Check Saturday Interaction
dfWK[dfWK$Date.of.Interaction == '2021-06-12',]

# SATURDAY TO FRIDAY
dfWK$Date.of.Interaction[dfWK$Date.of.Interaction == '2021-06-12'] <- as_date('2021-06-11')

# * 4.2 Check Barplot AFTER CHANGE IN DATES FOR HOLIDAYS ------

table(dfWK$Date.of.Interaction, useNA = 'always')

tobarplot <- dfWK %>% 
  group_by(Date.of.Interaction) %>% 
  summarise(PerDay = length(Date.of.Interaction))
tobarplot
barplot(tobarplot$PerDay, 
        xlab = 'Date.of.Interaction',
        names.arg = tobarplot$Date.of.Interaction, col = 'lightblue')

# 5 SAVE FWRITE LAST INTERACTIONS --------------------------

#setwd("D:/Information Technology/DATA_Reports/DATASOURCE_INTERACTIONS_VH_CM/ALL_PERIODS")
setwd("./DATA_Reports/REPORT_INTERACTIONS_VH/DATA_ALL_PERIODS")
getwd()
paste("DATA_FROM_",currentweek,".csv", sep = "") # <<<< NEW DATA FILE
data.table::fwrite(dfWK, file=paste("DATA_FROM_",currentweek,".csv", sep = ""))

str(dfWK)

rm(dfWK)

Sys.time() - st
    

