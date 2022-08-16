
###################################################################.
# SCRIPT: Interactions by date with de-identified pt info
# Change Date formats. # Convert POSIXct, format to Date, format
# EXAMPLE: INTERACTIONS by date
# Algorithms in this script:
# * 
########################################################################.

# 0. PREPARE INSTALL CALL PACKAGES ---------------------------------------------------

rm(list=ls())
ls()
options(str = strOptions(strict.width = "cut"))
st <- Sys.time()

# Load-multiple-packages-at-once
required_packages <- c("dplyr", "data.table", 
                       "lubridate", "tidyverse", "readxl", "tidyr")
lapply(required_packages, library, character.only = TRUE)

# Functions ----- 
# TABLES USENA = 'always'
tablex <- function(x){print(table(x, useNA = 'always'))}
tablexy <- function(x,y){print(table(x,y, useNA = 'always'))}

#####################################################################.
# 1 READ DATA nteractions data - all periods -------
#####################################################################.

# Paths: Location of Source files
path            <- "PopulationSamples//"

filenames_list <- list.files(path= path, full.names=TRUE, 
                             pattern=paste0("^interactions.*?.xlsx"))
filenames_list

dfINT <- data.frame(readxl::read_excel(filenames_list))

dim(dfINT)
str(dfINT)

# 2 CHANGE OF DATE FORMATS MDY YMD ------

# * 2.1 CONVERT multiple fields POSIXct, format to Date, format  -----

dfINT[] <- lapply(dfINT, function(x) {
  if (inherits(x, "POSIXt")) lubridate::as_date(x) else x
})

str(dfINT)

# * 2.2 Other options to convert dates
class(base::as.Date(dfINT$Date.of.Interaction))
class(lubridate::as_date(dfINT$Date.of.Interaction))
str(lubridate::as_date(dfINT$Date.of.Interaction))
# dfWK$Date.of.Interaction <- lubridate::as_date(dfWK$Date.of.Interaction)

# * 2.3 In other cases, to convert date format to character ----
format(dfINT$Date.of.Interaction, format="%m/%d/%Y") # as character
format(dfINT$DOB, format="%m/%d/%Y")                 # as character
lubridate::ymd(dfINT$Date.of.Interaction)   # Date to date, no change

# * 2.4 Convert character format mdy to date
lubridate::mdy(format(dfINT$DOB, format="%m/%d/%Y"))                 # as character

# 3 RECAP PER YEAR - MONTH with format.Date ---------

tablexy(lubridate::year(dfINT$Date.of.Interaction),
        lubridate::month(dfINT$Date.of.Interaction))

table(paste(format.Date(dfINT$Date.of.Interaction, "%Y"),
            format.Date(dfINT$Date.of.Interaction, "%m"), sep="/"))

# Check for Interactions in Specific Months
head(dfINT %>% filter(paste(format.Date(Date.of.Interaction, "%Y"),
                              format.Date(Date.of.Interaction, "%m"), 
                              sep="/") == "2021/04"),5)


# 4 CHANGE SPECIFIC VALUES (example a date) ----

dfINT$Date.of.Interaction[dfINT$Date.of.Interaction == '2021-01-18'] <- as_date('2021-01-19')

# 5 FILTERING DATA ONLY AFTER A SPECIFIC DATE OR PERIOD --------------

class(dfINT$Date.of.Interaction)

dfINT %>% 
  filter(Date.of.Interaction >= as.Date("2021-02-01") & 
         Date.of.Interaction <= as.Date("2021-02-28")) %>% tally()

# 5 Add fields Weekdays (names) and wdays (numbers)  ---------------

# Weekdays 
dfINT$WeekDay <- weekdays(dfINT$Date.of.Interaction)  # Wednesday
tablex(dfINT$WeekDay)

# * 5.1 Fix Dates saved by error as Saturday dates --------------------

# Pinpoint these records
dfINT %>% arrange(desc(Date.of.Interaction)) %>% 
  filter(WeekDay == "Saturday")

# FIX DATES SATURDAY TO FRIDAY - MINUS ONE DAY
dfINT <- dfINT %>% mutate(Date.of.Interaction = 
              if_else(weekdays(Date.of.Interaction) == 'Saturday',  
                  Date.of.Interaction -1, Date.of.Interaction))

# Run again weekdays
dfINT$WeekDay <- weekdays(dfINT$Date.of.Interaction)  # Wednesday

tablex(dfINT$WeekDay)      # without Saturday

# * 5.2 Add field of related Monday - wday() --------------------
# wday() function - # of week day beginning Sunday=1,Monday=2
# fix w/ -1 so Monday will be wday #1 

dfINT$WDay <- lubridate::wday(dfINT$Date.of.Interaction) - 1          # 4
distinct(dfINT[,c("WeekDay","WDay")])

dfINT$Mondays <- dfINT$Date.of.Interaction - dfINT$WDay + 1

head(dfINT,1)

# * 5.3 Year2 and Week Number (Based on Mondays) ------------------------------------------

dfINT$Year2 <- year(dfINT$Mondays)
tablex(dfINT$Year2)  # ALL Interactions (w UTR)

# Column Week YYYY-WW (2020-01, 2020-02, ... )
# Sprintf gives 2 digits format 
dfINT$Week <- paste(dfINT$Year2, 
                    sprintf("%02d", week(dfINT$Mondays)),sep="-")

### 1- Monday   2- Tuesday 3- Wednesday  4- Thursday    5- Friday
dfINT$WDay2 <- paste(dfINT$WDay, dfINT$WeekDay, sep="- ")

tail(table(paste(dfINT$Week,dfINT$Mondays,sep=" - "),dfINT$WDay2),10) # ALL Interactions (w UTR)
table(dfINT$WDay2)

# * 5.3 Filter data Last 16 weeks - pull() %in% --------------------

# Get Last 16 Weeks vector
tail(table(dfINT$Week),16)
L16W <- dfINT %>% distinct(Week) %>% arrange(desc(Week)) %>% 
                                       top_n(16) %>% pull()
L16W

# Count records with filter: "Last16Weeks"
dfINT %>% filter(Week %in% L16W) %>% group_by(Mondays) %>% tally()


# 6 PLOT INTERACTIONS by WEEK ------------------------------

# All Contact Successful Interactions are not UTR 
dfsuccessint <- dfINT %>% 
  filter(Outgoing.Contact.Result != 'Unable to Reach (UTR)') %>% 
  group_by(Week) %>% tally()
  
# par: Set or Query Graphical Parameters
par("mar")
par(mar=c(4,6,2,2))

barplot(dfsuccessint$n, xlab = 'Week', 
        ylab = 'No UTR Int', col='lightblue')


# 7 UNIQUE PATIENTS ON EACH WEEK --------------------------

# NOTE: GROUP BY NAME/DOB

# * 7.1 EXCLUDING OUTGOING CONTACT RESULTS IN BLANK --------
unique_wo_incoming <- dfINT %>% 
  filter(Outgoing.Contact.Result == 'Contact Successful') %>% 
  select(Week, Mondays, Client, DOB) %>% 
  group_by(Week, Mondays) %>% 
  summarise(Unique_Patients = n_distinct(Client, DOB))  #   
unique_wo_incoming <- data.frame(unique_wo_incoming)
tail(unique_wo_incoming,5)

# 8 COUNT OF WORKING DAYS PER WEEK - MELT DCAST --------------

# Days with zero minutes are counted as days off 

days_per_week <- dfINT %>% 
#  filter(Outgoing.Contact.Result == 'Contact Successful') %>% 
  group_by(Week, Mondays,WDay2) %>% 
  summarise(sum_mins = sum(Duration))
days_per_week <- data.table(days_per_week) # Convert to dt before melt dcast
days_per_week
days_per_week <- dcast(melt(days_per_week, id.vars = c("Week", "Mondays","WDay2"), 
                            measure.vars = c("sum_mins")),
                       variable + Week ~ WDay2, fun.agg = function(x) sum(x), value.var = "value")
days_per_week[,3:7] <- lapply(days_per_week[,3:7], function(x) ifelse(x > 0, 1, 0))
days_per_week$daysweek <- rowSums(days_per_week[,3:7])
days_per_week <- days_per_week[,c('Week',"daysweek")]
tail(days_per_week,10)

# Adding Column Days per Week to Main Data 

dfINT <- dplyr::inner_join(dfINT,days_per_week,by="Week") # ??? Not Sure


# 9 TABLE UNIQUE PATIENTS AND TOTAL INTERACTIONS --------------------

# unique_w_incoming
unique_pts <- unique_wo_incoming %>% 
              dplyr::inner_join(days_per_week,by="Week")
tail(unique_pts)

# 10 WITH ALL CONTACT INTERACTIONS dfsuccessint (FROM 7)
recapweeks <- unique_pts %>% 
  dplyr::inner_join(dfsuccessint,by="Week") %>% rename(interactions = n)

tail(recapweeks)

# 11 DELETE NOT NEEDED COLUMNS ---------------------------------------
# 11 DELETE NOT NEEDED COLUMNS ---------------------------------------
# 11 DELETE NOT NEEDED COLUMNS ---------------------------------------
# 11 DELETE NOT NEEDED COLUMNS ---------------------------------------

str(dfINT)
############ Delete ExcelFile column
dfINT <- dfINT %>% select(-c("ExcelFile", "ExcelFile2"))
#dfINT <- dfINT %>% select(-c("Notes", "ExcelFile", "ExcelFile2"))
# dfINT <- dfINT %>% filter(Week != max(dfINT$Week)) 

#####################################################################$
#####################################################################$
#####################################################################$
#####################################################################$
# 12 REASON FOR SERVICE ONLY INTERACTIONS WITHOUT UTR ----------------
# Split a string of values into multiple columns per value
#####################################################################$

### Data WITHOUT UTR
dfRecapNoUTR <- dfINT %>% filter(Outgoing.Contact.Result != "Unable to Reach (UTR)")
dfRecapNoUTR$ID <- seq.int(nrow(dfRecapNoUTR))   # Add an ID Index column
str(dfRecapNoUTR)
dim(dfRecapNoUTR)

#table(dfINT$Reason.s..for.Service)

dat <- with(dfRecapNoUTR, strsplit(Reason.s..for.Service, ','))
dat  # Lists within lists
df2 <- data.frame(ID = factor(rep(dfRecapNoUTR$ID, times = lengths(dat)),
                              levels = dfRecapNoUTR$ID),
                  Reason.s..for.Service = unlist(dat))
head(df2) # ALL ID ~ Reasons
df3 <- as.data.frame(cbind(ID = dfRecapNoUTR$ID,
                           table(df2$ID, df2$Reason.s..for.Service)))  
head(df3)
str(df3)
dfRecapReasons <- dfRecapNoUTR %>% select("Week", "ID") %>% 
  inner_join(df3, by = 'ID')

str(dfRecapReasons)   # WEEK + ID + 17 REASONS
head(dfRecapReasons)
dim(dfRecapReasons)

# match('Interdisciplinary Care Team Meeting',dfRecapNoUTR$Reason.s..for.Service) PENDING TO CONTINUE 

dfRecapReasons <- data.table(dfRecapReasons %>% 
                               group_by(Week) %>% 
                               select(-c("ID")) %>% 
                               summarise_all(list(sum)))
dim(dfRecapReasons)
head(dfRecapReasons)
str(dfRecapReasons)   # WEEK + 17 REASONS

unique_pts
unique_pts <- unique_pts %>% inner_join(dfRecapReasons, by="Week")
unique_pts <- unique_pts %>% mutate(Last16Weeks = if_else(Week %in% L16W, "Last16Weeks", NULL))

head(unique_pts,3)
str(unique_pts)
head(unique_pts) # Week, Mondays, Unique_Patients, daysweek, Unique_Incoming_Patients, Successful_Inter + 17 REASONS + LAST16WEEKS
dim(unique_pts)  # 6 COLS from UNIQUE + 17 REASONS + LAST16WEEK

dim(unique_pts)
unique_pts %>% filter(Week == '2021-14')
# Ratio reasons per day
str(unique_pts[7])
str(unique_pts[,c(7:ncol(unique_pts)-1)])
head(unique_pts[,c(7:23)],3)
dim(unique_pts[,c(7:23)]) # 17 REASONS
head(unique_pts[,c(7:23)],3)
reasonsweek <- round(unique_pts[,c(7:23)]/unique_pts$daysweek,1)
dim(reasonsweek) # 17 REASONS
head(reasonsweek) # 17 REASONS
names(reasonsweek) # 17 REASONS
names(unique_pts[,c(7:23)])  # 17 REASONS
names(reasonsweek) <- paste0(names(reasonsweek)," per day")
#names(reasonsweek) <- paste0(names(unique_pts[,c(6:23)])," per day")
names(reasonsweek)
dim(reasonsweek)

unique_pts <- unique_pts %>% dplyr::bind_cols(reasonsweek)
tail(unique_pts,3)


#####################################################################o
# 13 MID - MEMBER NUMBER - ADD COLUMN FROM  -----------------
#####################################################################o
# IMPORT IMPORT IMPORT
#setwd("D:/Information Technology/DATA_Reports/@Tables_Data")
setwd("C:/Users/..../DATA_Reports/@Tables_Data")
tMbMID <- read.csv("table_dataVH.csv")
head(tMbMID)
#dfINT %>% filter(Member.Number == 'B01dsadf' & Date.of.Interaction == '2021-04-07')
# DISTINCT :> IMPORTANT TO AVOID DUPLICATED RECORDS
tMbMID <- tMbMID %>% arrange(desc(ADDED)) %>% 
  distinct(Member.Number, .keep_all = TRUE)
#tMbMID <- tMbMID %>% arrange(Valid_Phone, desc(ADDED)) %>% distinct(Member.Number, .keep_all = TRUE)
#tMbMID <- tMbMID %>% select(Member.Number, Medicaid.ID)

str(tMbMID)
dim(tMbMID)


#tMbMID %>% filter(Member.Number == 'Basdasdf')

# !!!! COMPLETE LATER MISSING WITH ANOTHER PROCESS
# R SCRIPT: MATCH_MEMBER_NUMBER_MEDICAID_ID_20210412A
#tablex(!distinct(dfINT,Member.Number) %in% tMbMID$Member.Number)
#  Member.Number       Last.Name First.Name        DOB Medicaid.ID

# COMPLETE DATA WITH ANOTHER PROCESS
# MISSING_MID_from_Interactions <-  MATCH BY NAME DOB
tablex(dfINT$Member.Number %in% tMbMID$Member.Number)
distinct(dfINT[!dfINT$Member.Number %in% tMbMID$Member.Number,
               c('Member.Number', 'Client', 'DOB')], .keep_all = T)
# MISSING MEMBER NUMBERS

woMID <- dfINT[!dfINT$Member.Number %in% tMbMID$Member.Number,
               c('Client', 'Member.Number', 'DOB')]
woMID <- woMID %>% distinct(.keep_all = TRUE)
dim(woMID)

# MISSING IN tMbMID table
# dim(MISSING_MID_from_Interactions)
head(tMbMID)
tail(tMbMID)

##########.

dfINT <- dfINT %>% left_join(tMbMID[,c("Member.Number", 
                                       "Medicaid.ID", 
                                       "Do.Not.Call", 
                                       "Unable.to.Reach")], 
                             by = c("Member.Number"))
dim(dfINT)
str(dfINT)
dfINT %>% filter(is.na(Client))

# PATIENTS "NO_MID_N/A"
tablex(is.na(dfINT$Medicaid.ID))  # MISSING
#tail(tablex(dfINT$Medicaid.ID))
tablex(dfINT$Medicaid.ID == "NO_MID_N/A")
NOMIDNA <- dfINT[dfINT$Medicaid.ID == "NO_MID_N/A" & !is.na(dfINT$Medicaid.ID),]
NOMIDNA <- NOMIDNA %>% select(Client, DOB, Member.Number, Medicaid.ID) %>% distinct()
dim(NOMIDNA) # COMPLETE
NOMIDNA

#split(NOMIDNA$Client, ", ")
# str_split_fixed(NOMIDNA$Client, ", ", n = 2)[,1]
# str_split_fixed(NOMIDNA$Client, ", ", n = 2)[,2]

# * 13.1 MISSING NA_NEWS ----
# * 13.1 MISSING NA_NEWS ----
# * 13.1 MISSING NA_NEWS ----

# PATIENTS "N/A" (NEW)
tablex(is.na(dfINT$Medicaid.ID))  # MISSING
#tail(tablex(dfINT$Medicaid.ID))
NA_NEWS <- dfINT[is.na(dfINT$Medicaid.ID),]
NA_NEWS
NA_NEWS <- NA_NEWS %>% select(Client, DOB, Member.Number) %>% distinct()
dim(NA_NEWS) # COMPLETE "NO_MID_N/A" IN DATA TABLE tMbMID
#NOMIDNA %>% arrange(Client)[1:40]
#NOMIDNA %>% filter(Client == "Benjamin, Keidrick")
NA_NEWS %>% arrange(Client) %>% select(Client) %>% pull()

# TO CHECK COMPLETE MISSING MIDs
setwd("C:/Users/")
setwd("./DATA_Reports/@Tables_Data")
fwrite(NA_NEWS,   "NA_NEWS.csv")

# #split(NOMIDNA$Client, ", ")
# str_split_fixed(NA_NEWS$Client, ", ", n = 2)[,1]
# str_split_fixed(NA_NEWS$Client, ", ", n = 2)[,2]
# NA_NEWS[NA_NEWS$Member.Number == 'B6asdf58',]
# #dfINT %>% filter(Member.Number == 'Basdasfd' & Date.of.Interaction == '2021-04-07')
# 
# dfINT %>% filter(Member.Number == 'Basdf2')
# tMbMID %>% filter(Member.Number == 'B6asdf4')

###################################################################.
# 14 RECAP LAST 52 WEEKS - TO CROSS WITH OTHER REPORTS ----
###################################################################.

head(dfINT)
tablex(dfINT$Last52Weeks)
#dfINT[!is.na(dfINT$Last52Weeks),]
meltedPCInt <- melt(data.table(dfINT[!is.na(dfINT$Last52Weeks),]),
                    id.vars = c("Medicaid.ID", "Member.Number", 'Do.Not.Call', 'Unable.to.Reach'),
                    measure.vars = c("Interaction_Classification"))
recap_Last_52_weeks <- dcast(meltedPCInt, Medicaid.ID + Member.Number + Do.Not.Call + Unable.to.Reach ~ value, fun.agg = length, value.var = "value")
recap_Last_52_weeks
names(recap_Last_52_weeks) <- c('MID', 'Mb.Nmb', 'Do.Not.Call', 'UTR', 'PCI', 'OtherPt', 'OtherInt', 'UTR_Int')
head(recap_Last_52_weeks)
recap_Last_52_weeks$Pt_Interactions <- rowSums(recap_Last_52_weeks[,5:6])
recap_Last_52_weeks$Total <- rowSums(recap_Last_52_weeks[,5:8])

# NUMERATOR PENETRATION RATE
nrow(recap_Last_52_weeks %>% filter(Pt_Interactions > 0) %>% distinct(MID))
head(recap_Last_52_weeks)
dim(recap_Last_52_weeks)
recap_Last_52_weeks$Int_Group <- ifelse(recap_Last_52_weeks$Pt_Interactions > 0,
                                        'Completed','Other/UTR')

#7557/7438 # CHECK WITH THE NUMBER IN SLA PENETRATION RATE CARE IMPACT SYSTEM !
tablex(recap_Last_52_weeks$Int_Group)

# ONLY OTHER (MAIL / FAX)
dim(recap_Last_52_weeks %>% filter(OtherInt > 0 & PCI == 0 & 
                                     OtherPt == 0 & UTR_Int == 0)) # Other Int

# OTHER UNIQUE PATIENTS

# plotCompleted <- recap_Last_52_weeks %>% group_by(Pt_Interactions) %>% summarise(Pts = length(MID)) 
# plotTotalInt <- recap_Last_52_weeks %>% group_by(totalInt) %>% summarise(Pts = length(MID)) 
#tablex(recap_Last_52_weeks$Pt_Interactions)
barplot(tablex(recap_Last_52_weeks$Pt_Interactions), 
        xlab = 'Patient Interactions per Patient', 
        ylab = 'Patients', 
        col = 'pink')
#tablex(recap_Last_52_weeks$totalInt)
barplot(tablex(recap_Last_52_weeks$Total), 
        xlab = 'Total Interactions per Patient', 
        ylab = 'Patients', 
        col = 'lightblue')
#barplot(plotTotalInt$Pts)

head(recap_Last_52_weeks)

###################################################################.
# 14B RECAP LAST YEAR - TO CROSS WITH OTHER REPORTS ----
###################################################################.

head(dfINT)
tablex(dfINT$Last52Weeks)
class(dfINT$Date.of.Interaction)

df2021 <- dfINT %>% filter(Date.of.Interaction >= "2020-07-01")

#dfINT[!is.na(dfINT$Last52Weeks),]
meltedPCInt <- melt(data.table(df2021),
                    id.vars = c("Medicaid.ID", "Member.Number", 'Do.Not.Call', 'Unable.to.Reach'),
                    measure.vars = c("Interaction_Classification"))
recap_2021 <- dcast(meltedPCInt, Medicaid.ID + Member.Number + Do.Not.Call + Unable.to.Reach ~ value, fun.agg = length, value.var = "value")
recap_2021
names(recap_2021) <- c('MID', 'Mb.Nmb', 'Do.Not.Call', 'UTR', 'PCI', 'OtherPt', 'OtherInt', 'UTR_Int')
head(recap_2021)
recap_2021$Pt_Interactions <- rowSums(recap_2021[,5:6])
recap_2021$Total <- rowSums(recap_2021[,5:8])

# NUMERATOR PENETRATION RATE
nrow(recap_2021 %>% filter(Pt_Interactions > 0) %>% distinct(MID))
head(recap_2021)
dim(recap_2021)
recap_2021$Int_Group <- ifelse(recap_2021$Pt_Interactions > 0,
                               'Completed','Other/UTR')

#7557/7438 # CHECK WITH THE NUMBER IN SLA PENETRATION RATE CARE IMPACT SYSTEM !
tablex(recap_2021$Int_Group) # CONSIDER THIS NUMBER FOR THE REPORT 2021-09-21
# 7237

# ONLY OTHER (MAIL / FAX)
dim(recap_2021 %>% filter(OtherInt > 0 & PCI == 0 & 
                            OtherPt == 0 & UTR_Int == 0)) # Other Int

# OTHER UNIQUE PATIENTS

# plotCompleted <- recap_Last_52_weeks %>% group_by(Pt_Interactions) %>% summarise(Pts = length(MID)) 
# plotTotalInt <- recap_Last_52_weeks %>% group_by(totalInt) %>% summarise(Pts = length(MID)) 
#tablex(recap_Last_52_weeks$Pt_Interactions)
barplot(tablex(recap_2021$Pt_Interactions), 
        xlab = 'Patient Interactions per Patient', 
        ylab = 'Patients', 
        col = 'pink')
#tablex(recap_Last_52_weeks$totalInt)
barplot(tablex(recap_2021$Total), 
        xlab = 'Total Interactions per Patient', 
        ylab = 'Patients', 
        col = 'lightblue')
#barplot(plotTotalInt$Pts)

head(recap_Last_52_weeks)




#####################################################################.
# 14 RECAP FOR POWER BI ------
#####################################################################.

dim(dfINT)

c('Submitted.By', 'Week', 'Mondays', 
  'Interaction_Classification', 'Last40Weeks')
str(dfINT)
tablex(dfINT$Last40Weeks)
dfIntPowerBI <- dfINT %>% 
  select('Department', 'Submitted.By', 'Week', 'Mondays', 
         'Interaction_Classification', 
         'Last40Weeks', 'Last16Weeks', 'Mode', 'WDay2', 
         'daysweek', 'Duration') %>% 
  filter(Last40Weeks == 'Last40Weeks')
dfIntPowerBI$Int <- 1L
str(dfIntPowerBI)
tablexy(dfIntPowerBI$Submitted.By, dfIntPowerBI$Department)

# * 14.1 Recap Interaction Classification ---- 

# meltedPCInt <- melt(data.table(dfIntPowerBI), 
#                     id.vars = c("Week", 
#                     "Interaction_Classification"),
#                    measure.vars = c("Int"))
# casted3 <- dcast(meltedPCInt, Week ~ Interaction_Classification + value, fun.agg = length, value.var = "value")
# casted3



# meltedPCInt <- melt(data.table(dfIntPowerBI),
#                     id.vars = c("Submitted.By", "Week",
#                                 "Mondays", "Interaction_Classification"),
#                     measure.vars = c("Int"))
# casted3 <- dcast(meltedPCInt,Submitted.By + Week + Mondays ~ Interaction_Classification + value, fun.agg = length, value.var = "value")
# casted3
casted3 <- dfIntPowerBI
dim(casted3)
#                                               "EpisodeType"),
#                     measure.vars = c("Episode.Date.Created"))


# 
# # ERROR FIX ----------
# 
# dfIntPowerBI %>% group_by(Submitted.By, Week)
# IDvars <- c('Department', 'Submitted.By', 
#             'Week', 'Mondays', 
#          'Interaction_Classification', 
#          'Last40Weeks', 'Last16Weeks', 'Mode', 'WDay2', 
#          'daysweek')
# dfINT %>% group_by(Department)
# , 
#            'Week', 'Mondays', 
#            'Interaction_Classification', 
#            'Last40Weeks', 'Last16Weeks', 'Mode', 'WDay2', 
#            'daysweek') %>% summarise(Durat = sum(Duration))
# 
# dfIntPowerBI <- dfINT %>% 
#   select(c('Department', 'Submitted.By', 'Week', 'Mondays', 
#            'Interaction_Classification', 
#            'Last40Weeks', 'Last16Weeks', 'Mode', 'WDay2', 
#            'daysweek', 'Duration')) %>% 
#   filter(Last40Weeks == 'Last40Weeks')
# dfIntPowerBI 
# 
# 
# melt(days_per_week, id.vars = c("Week", "Mondays","WDay2"), 
#      measure.vars = c("sum_mins")
# 
# dim(dfIntPowerBI)
# str(dfIntPowerBI)
# 

dim(dfINT)
str(dfINT)
tablex(dfINT$PCP)
tablex(dfINT$Mode.PCI)
tail(dfINT %>% group_by(PCP) %>% select(Week) %>% tally())
#hist(dfINT[is.na(dfINT$PCP),] %>% select(Date.of.Interaction))


# LAST WEEK RECAP --------

LastWeek <- dfINT %>% distinct(Week) %>% arrange(desc(Week)) %>% top_n(1) %>% pull()
meltLastWk <- melt(data.table(dfINT[dfINT$Week == LastWeek,]),
                   id.vars = c("Submitted.By"),
                   measure.vars = c("Interaction_Classification"))
recaplastweek <- dcast(meltLastWk, Submitted.By ~ value, fun.agg = length, value.var = "value")
recaplastweek <- recaplastweek %>% 
  rename('Completed PCI' = 'Completed Pt. Centered Interaction', 
         'UTR' = 'Unable to Reach (UTR)')
recaplastweek$CompletedPCI <- rowSums(recaplastweek[,c(2:3)])
recaplastweek$Total_INT <- rowSums(recaplastweek[,c(2:5)])
recaplastweek <- recaplastweek %>% arrange(desc(CompletedPCI), desc(Total_INT))

#####################################################################$
# 13 WRITE DATA TABLES -----------------------------------------------
#####################################################################$

# DATA.TABLE::FWRITE
#setwd("../")
#setwd("D:/Information Technology/DATA_Reports/REPORT_INTERACTIONS_VH/FILES_FROM_R")
setwd("C:/Users/")
setwd("./DATA_Reports/REPORT_INTERACTIONS_VH/INT_FILES_FROM_R")

getwd()

#setwd("../DATA_ALL_PERIODS")
#getwd()

#setwd("D:/Information Technology/DATA_Reports/DATASOURCE_INTERACTIONS_VH_CM")
format.Date(today(), '%Y%m%d')
data.table::fwrite(unique_pts, file=paste0("unique_recap_",format.Date(today(),'%Y%m%d'),".csv"))  # Imported in Excel
data.table::fwrite(dfINT, file=paste0("All_Interactions_",format.Date(today(),'%Y%m%d'),".csv")) # Imported in Excel
data.table::fwrite(dfIntPowerBI, file="dfIntPowerBI_from_R.csv")
data.table::fwrite(casted3, file="dfIntPowerBICasted.csv")
data.table::fwrite(recap_Last_52_weeks, file=paste0("MID_Interactions_last52w_",format.Date(today(),'%Y%m%d'),".csv"))
data.table::fwrite(recaplastweek, file=paste0("recaplastweek_",format.Date(today(),'%Y%m%d'),".csv"))
data.table::fwrite(recap_2021, file=paste0("recap2021_",format.Date(today(),'%Y%m%d'),".csv"))

#data.table::fwrite(MISSING_MID_from_Interactions, file="dfCompleteMissing.csv")
# See resulting graph in jupyter notebook: Interactions from VH-toPowerBI
Sys.time() - st

##############################.

