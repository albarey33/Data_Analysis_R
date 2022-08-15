
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
# 1 read data - all periods -------
#####################################################################.

# Paths: Location of Source files
path            <- "PopulationSamples"       

filenames_list <- list.files(path= path, full.names=TRUE, 
                             pattern=paste0("^interactions.*?.xlsx"))
filenames_list

dfINT <- data.frame(readxl::read_excel(filenames_list))

dim(dfINT)

# * 1.1 Recap per Year - Month with format.Date --------------------

table(dfINT$Outgoing.Contact.Result, useNA = 'ifany') # MOVE

#table(paste(lubridate::year(dfINT$Date.of.Interaction),
#            text(lubridate::month(dfINT$Date.of.Interaction),"00"), sep="/"))

#mdy(dfINT$Date.of.Interaction)
#tablex(nchar(dfINT$Date.of.Interaction))
#dfINT %>% filter(nchar(Date.of.Interaction) == 8) %>% select(Date.of.Interaction)

table(paste(format.Date(dfINT$Date.of.Interaction, "%Y"),
            format.Date(dfINT$Date.of.Interaction, "%m"), sep="/"))

#####################################################################o
# 2 CHECK OF DUPLICATES ---------------------------------------------
#####################################################################o

# Verification List of duplicated interactions in different Excel Files

dfDuplicates <- dfINT %>% group_by(Client,DOB,Member.Number, Date.of.Interaction, 
                                   Submitted.By,Primary.Participant,Reason.s..for.Service, 
                                   Duration, Outgoing.Contact.Result,Notes) %>% 
  filter(Outgoing.Contact.Result != 'Unable to Reach (UTR)') %>% 
  count(Member.Number) %>% filter(n>1)

dfDuplicates <- data.frame(dfDuplicates)
dfDuplicates <- dfDuplicates %>% arrange(desc(Date.of.Interaction))
str(dfDuplicates)
dim(dfDuplicates)
tablex(dfDuplicates$Submitted.By)
tablex(dfDuplicates$Date.of.Interaction)
tablex(dfDuplicates$Duration)

################ Inner join sub table to check in detail the Duplicates
dfDuplicates

tablexy(format.Date(dfDuplicates$Date.of.Interaction, "%Y"),
        format.Date(dfDuplicates$Date.of.Interaction, "%m"))

head(dfDuplicates,20)

# Check for dfDuplicates in Specific Months
dfDuplicates %>% filter(paste(format.Date(dfDuplicates$Date.of.Interaction, "%Y"),
                              format.Date(dfDuplicates$Date.of.Interaction, "%m"), 
                              sep="/") == "2020/12")

dfDuplicates %>% filter(paste(format.Date(dfDuplicates$Date.of.Interaction, "%Y"),
                              format.Date(dfDuplicates$Date.of.Interaction, "%m"), 
                              sep="/") == "2021/04")

#dfDuplicates %>% filter(Submitted.By == 'Stanhope, Melissa') %>% 
#  select(Client,Date.of.Interaction, Duration)

#dfDuplicates %>% filter(Submitted.By == 'McLeod, Carol') %>% 
#  select(Client,Date.of.Interaction, Duration)

###########################################################################s
# Number of Times Medicaid is repeated in df
#n_occur <- data.frame(table(dfRecentInteractions$Medicaid.Number, Primary.Participant,Notes ))    
#str(n_occur)
#n_occur[n_occur$Freq >1,]
#nrow(dfRecentInteractions)   # If FALSE there are duplicates
#dfdistinct <- dplyr::distinct(dfRecentInteractions,Medicaid.Number,.keep_all = TRUE)
# dim(dfRecentInteractions)             # Table dimensions for Comparison
# help(lapply) 

dfINT <- dfINT

#  Recap
dim(dfINT)
# table(dfINT$Week)
head(dfINT,1)
str(dfINT) 

dfINT[is.na(dfINT$PCP),] %>% select(Date.of.Interaction) # DF
dfINT[is.na(dfINT$PCP),"Date.of.Interaction"] # VECTOR

# * 2.4 Replace NA or Blank values in "Outgoing.Contact.Result" --------------------

tablex(dfINT$Outgoing.Contact.Result)
dfINT$Outgoing.Contact.Result <- dfINT$Outgoing.Contact.Result %>% 
  replace_na("Contact result in Blank")
table(dfINT$Outgoing.Contact.Result, useNA = 'ifany')


#####################################################################$
# 3 TABLES: PCI, Prim. Part, Mode ------------------------------------
#####################################################################$

#PENETRATION RATE:
#Denominator: All pts with CA-II at any point during the year
#Numerator: All patients with a completed interaction within the year.
# This includes calls, emails, video conferences, texts, 
# ICT meetings and other.

# * 3.1 Mode Classification Mode.PCI, Mode2, ModewoFax -------------

dfINT <- dfINT %>% mutate(Mode.PCI = if_else(Mode %in% 
                                               c("Call", "Visit", "Videoconference"), "Yes", ""))

# Mode2   # Used in Excel reports
dfINT <- dfINT %>% mutate(Mode2     = if_else(Mode %in% 
                                                c("Fax", "Email", "ICT Meeting", "Videoconference", 
                                                  "Text", "Other"), "Other", Mode))

# # Mode without Fax or Mail (not in penetration rate numerator)
# dfINT <- dfINT %>% mutate(ModewoFaxMail = if_else(Mode %in% 
#       c("Call", "Visit", "Email", "ICT Meeting", "Videoconference", 
#         "Text", "Other"), "Yes", ""))
# 
# data.frame(dfINT %>% group_by(Mode, Mode.PCI, Mode2, ModewoFaxMail) %>% 
#                         tally() %>% arrange(desc(Mode.PCI)))

# * 3.2 Table Prim.Part.PCI Primary Participant --------------------------------

dfINT <- dfINT %>% 
  mutate(Prim.Part.PCI = if_else(Primary.Participant %in% c("Member",
                                                            "Parent","Caregiver","Guardian","Power of Attorney"),"Yes", "")) # 

data.frame(dfINT %>% group_by(Primary.Participant, Prim.Part.PCI) %>% 
             tally() %>% arrange(desc(Prim.Part.PCI), desc(n)))

table("PrimPartPCI / Prim Part" = paste(dfINT$Prim.Part.PCI, dfINT$Primary.Participant, sep="/"),
      dfINT$Outgoing.Contact.Result, useNA = "ifany")

# Int_Classification -> Three Groups: PCI, Other, UTR
#dfINT <- dfINT %>% 
#  mutate(Interaction_Classification = if_else(Mode.PCI == "Yes" &
#                       Prim.Part.PCI == "Yes" &
#                       Outgoing.Contact.Result == "Contact Successful", 
#               "Completed Pt. Centered Interaction", 
#               if_else(Outgoing.Contact.Result == "Contact Successful", 
#              "Other Completed Interactions", 
#            if_else(Outgoing.Contact.Result == "Contact result in Blank",
#           "Contact Result in Blank", "Unable to Reach (UTR)"))))


# * 3.4 Interaction Classification : PCI, Other Pt, UTR ------------------------

############### NEW CLASSIFICATION Int_Classification -> 
############### Three Groups: PCI, OtherPatient, UTR
### Change: Contact result in Blank also considered as Pt. Ctrd. Interaction
### as long as it is Mode.PCI and Prim.Part.PCI

# Equivalent with Less Code
dfINT <- dfINT %>% 
  mutate(Interaction_Classification = 
           if_else(Outgoing.Contact.Result == "Unable to Reach (UTR)", 
                   "Unable to Reach (UTR)",    # Gray color
                   if_else(
                     Prim.Part.PCI == "Yes" &
                       Mode.PCI == "Yes",
                     "Completed Pt. Centered Interaction",  # Dark green color
                     if_else(
                       Prim.Part.PCI == "Yes" &    # CHANGE 20210426
                         !Mode %in% c('Fax', 'Mail'),        
                       "Other Completed Interactions w/Pt.",  # Light green color
                       "Other Interactions"))))           # Peach color

table(paste(dfINT$Prim.Part.PCI, dfINT$Mode.PCI, dfINT$Primary.Participant,dfINT$Mode,  sep="/"),
      paste(dfINT$Interaction_Classification, dfINT$Outgoing.Contact.Result), useNA = "ifany")

table(paste(dfINT$Interaction_Classification, dfINT$Outgoing.Contact.Result,  sep="/"), 
      paste(dfINT$Prim.Part.PCI, dfINT$Mode.PCI,  sep="/"),
      useNA = "ifany")
dfINT %>% group_by(Interaction_Classification, 
                   Prim.Part.PCI, Mode, Mode.PCI, Mode2) %>% tally()

# * 3.5 4C STAFF VS DEPARTMENTS  --------
#setwd(ROOT_4C_ONEDRIVE)
#setwd("D:/Information Technology/DATA_Reports/@Tables_Data")
setwd("C:/Users//OneDrts/@Tables_Data")
StaffDept <- read.csv("t_4C_STAFF_vs_DEPARTMENTS.csv")
StaffDept %>% arrange(Department)
dfINT <- dfINT %>% left_join(StaffDept, by=c('Submitted.By'))
str(dfINT)

#####################################################################$
# 4 FIX DATES --------------------------------------------------------
#####################################################################$

# * 4.1 Fix Dates Saturday and Sunday to Friday --------------------------------------------

dfINT$Date.of.Interaction <- as.Date(dfINT$Date.of.Interaction)

# Weekdays
dfINT$WeekDay <- weekdays(dfINT$Date.of.Interaction)  # Wednesday
table(dfINT$WeekDay)

# FIX DATES SATURDAY TO FRIDAY 

checkSatdm <- dfINT %>% filter(WeekDay == "Saturday") %>% 
  select(ExcelFile, Outgoing.Contact.Result, Date.of.Interaction)

tablexy(format.Date(checkSatdm$Date.of.Interaction, '%Y'),
        format.Date(checkSatdm$Date.of.Interaction, '%m'))

dfINT %>% arrange(desc(Date.of.Interaction)) %>% 
  filter(WeekDay == "Saturday") %>% head()

dfINT %>% filter(WeekDay == "Saturday") %>% 
  select(ExcelFile, Outgoing.Contact.Result, Date.of.Interaction)

dfINT <- dfINT %>% mutate(Date.of.Interaction = 
                            if_else(weekdays(Date.of.Interaction) == 'Saturday',  
                                    Date.of.Interaction -1, Date.of.Interaction))
dfINT$Date.of.Interaction <- as_date(dfINT$Date.of.Interaction)
dfINT$WeekDay <- weekdays(dfINT$Date.of.Interaction)  # Wednesday
table(dfINT$WeekDay)
str(dfINT)

table(dfINT$WeekDay)
str(dfINT)

# FIX DATES SUNDAY TO FRIDAY 

dfINT %>% filter(WeekDay == "Sunday") %>% 
  select(ExcelFile, Outgoing.Contact.Result, Date.of.Interaction)

dfINT <- dfINT %>% mutate(Date.of.Interaction = 
                            if_else(weekdays(Date.of.Interaction) == 'Sunday',  
                                    Date.of.Interaction -2, Date.of.Interaction))
dfINT$Date.of.Interaction <- as_date(dfINT$Date.of.Interaction)
dfINT$WeekDay <- weekdays(dfINT$Date.of.Interaction)  # Again WeekDay
table(dfINT$WeekDay)

### EXCEPTION EXCEPTION EXCEPTION EXCEPTION Benefits Day  2020-05-27
dfINT$Date.of.Interaction[dfINT$Date.of.Interaction == '2020-05-27'] <- as_date('2020-05-28')
### EXCEPTION MLK Day  2021-01-18 # Syreeta Interactions
dfINT$Date.of.Interaction[dfINT$Date.of.Interaction == '2021-01-18'] <- as_date('2021-01-19')
dfINT$WeekDay <- weekdays(dfINT$Date.of.Interaction)  # Wednesday
table(dfINT$WeekDay)

# * 4.2 Days of Week (wday) when Monday -----------------------------------
# wday() function
dfINT$WDay <- wday(dfINT$Date.of.Interaction) - 1          # 4
table(dfINT$WDay, useNA = 'always')
#head(dfINT,5)
dfINT$Mondays <- dfINT$Date.of.Interaction - dfINT$WDay + 1

tail(table(dfINT$Mondays,wday(dfINT$Date.of.Interaction)-1),10)
tail(table(dfINT$Mondays,dfINT$WDay),10)

str(dfINT)

table(dfINT$WeekDay)
dfINT %>% filter(WeekDay == "Saturday")  # Check FIX!!!!!!!! Records on Saturday
dfINT %>% filter(WeekDay == "Sunday")    # Check 

# * 4.3 Year2 and Week Number (Based on Mondays) ------------------------------------------

dfINT$Year2 <- year(dfINT$Mondays)
table(dfINT$Year2, useNA = 'always')  # ALL Interactions (w UTR)
# Column Week YYYY-WW (2020-01, 2020-02, ... )
dfINT$Week <- paste(dfINT$Year2, 
                    sprintf("%02d", week(dfINT$Mondays)),sep="-")
table(week(dfINT$Mondays))  # ALL Interactions (w UTR)
table(sprintf("%02d", week(dfINT$Mondays))) # Sprintf gives format 2 digits

table(dfINT$Week)  # ALL Interactions (w UTR)

#WeeksMondays <- distinct(dfINT %>% 
#                  select(Week, Mondays) %>% arrange(Week)) # To join later

### 1- Monday   2- Tuesday 3- Wednesday  4- Thursday    5- Friday
dfINT$WDay2 <- paste(dfINT$WDay, dfINT$WeekDay, sep="- ")
table(dfINT$WDay2)
tail(table(dfINT$Week,dfINT$WDay2),10) # ALL Interactions (w UTR)

#dfINT <- dfINT %>% filter(Date.of.Interaction >= '2020-03-01')

# filter(dfINT,Week=='2020-03' & WDay2 == '6- Saturday')

dfINT %>% filter(Year2 == '2021' & WeekDay == 'Saturday') # Again Verification
dfINT %>% filter(Year2 == '2021' & WeekDay == 'Sunday')   # Again Verification

dfINT %>% select(Week, Mondays) %>% distinct()
dfINT %>% distinct(Week)
# dfINT %>% distinct(Week) %>% filter(Week <= '2021-15')
# dim(dfINT)
# dfINT <- dfINT %>% filter(Week <= '2021-15') # FILTER FOR 4CQA-2021Q1

# * 4.3 Last 16 / 40 / 52 Weeks --------------------------

# Last 16 Weeks
tail(table(dfINT$Week),16)
L16W <- dfINT %>% distinct(Week) %>% arrange(desc(Week)) %>% top_n(16) %>% pull()
L16W
dfINT <- dfINT %>% mutate(Last16Weeks = if_else(Week %in% L16W, "Last16Weeks", NULL)) 

tail(tablexy(dfINT$Week, dfINT$Last16Weeks),19)

# Last 40 Weeks

tail(table(dfINT$Week),40)
L40W <- dfINT %>% distinct(Week) %>% arrange(desc(Week)) %>% top_n(40) %>% pull() # filter(Week[c(1:40),])
L40W
dfINT <- dfINT %>% mutate(Last40Weeks = if_else(Week %in% L40W, "Last40Weeks", NULL)) 
tail(table(dfINT$Week, dfINT$Last40Weeks, useNA = 'always'),43)

# Last 52 Weeks

tail(table(dfINT$Week),52)
L52W <- dfINT %>% distinct(Week) %>% arrange(desc(Week)) %>% top_n(52) %>% pull() # filter(Week[c(1:40),])
L52W
dfINT <- dfINT %>% mutate(Last52Weeks = if_else(Week %in% L52W, "Last52Weeks", NULL)) 
tail(table(dfINT$Week, dfINT$Last52Weeks, useNA = 'always'))

# Last YEAR

# tail(table(dfINT$Week),52)
# L52W <- dfINT %>% distinct(Week) %>% arrange(desc(Week)) %>% top_n(52) %>% pull() # filter(Week[c(1:40),])
# L52W
# dfINT <- dfINT %>% mutate(Last52Weeks = if_else(Week %in% L52W, "Last52Weeks", NULL)) 
# tail(table(dfINT$Week, dfINT$Last52Weeks, useNA = 'always'))


#####################################################################$
# 6 FILTERING DATA ONLY AFTER MARCH 02, 2020  ------------------------
#####################################################################$
class(dfINT$Date.of.Interaction)
dfINT %>% filter(Date.of.Interaction >= as.Date("2020-03-02")) %>% # WEEK 9
  group_by(Week) %>% tally()

table(dfINT$Outgoing.Contact.Result) # Outgoing.Contact.Result

# Patients with same name (????) - Added DOB
table(dfINT %>% group_by(Client, DOB) %>% tally() %>% count(Client)> 1)

dfINT %>% group_by(Client, DOB) %>% 
  count(Client) %>%  #, DOB
  filter(n> 1)
nrow(dfINT %>% group_by(Client, DOB) %>% count(Client, DOB) %>% filter(n> 1))
head(dfINT,5)

#####################################################################$
# 7 ALL CONTACT SUCCESSFUL INTERACTIONS ------------------------------
#####################################################################$

# All Contact Successful Interactions are not UTR 

dfsuccessint <- dfINT %>% 
  filter(Outgoing.Contact.Result != 'Unable to Reach (UTR)') %>% 
  #       Mode.PCI == 'Yes' &             # Only Patient Centered Interactions
  #       Prim.Part.PCI == 'Yes') %>%     # Only Patient Centered Interactions
  group_by(Week) %>% tally()  # 
dfsuccessint <- data.frame(dfsuccessint)
dfsuccessint <- dfsuccessint %>% rename(Successful_Inter=n)
par("mar")
par(mar=c(4,6,2,2))
barplot(dfsuccessint$Successful_Inter, xlab = 'Week', 
        ylab = 'No UTR Int', col='lightblue')

#####################################################################$
# 8 UNIQUE PATIENTS --------------------------------------------------
#####################################################################$

# NOTE: BETTER DON'T GROUP BY AGE CAUSING EXTRA UNIQUE PATIENTS
# BEFORE AUGUST 17, 2020 (WEEK 32)
# * 8.1 EXCLUDING OUTGOING CONTACT RESULTS IN BLANK --------
unique_wo_incoming <- dfINT %>% 
  #  filter(Date.of.Interaction < "2020-08-17") %>%  # BEFORE 2020-08-17 !!!!!
  filter(Outgoing.Contact.Result == 'Contact Successful' & 
           Mode.PCI == 'Yes' &                             # Only Patient Centered Interactions
           Prim.Part.PCI == 'Yes') %>%                     # Only Patient Centered Interactions
  select(Week, Mondays, Client, DOB) %>% 
  group_by(Week, Mondays) %>% 
  summarise(Unique_Patients = n_distinct(Client, DOB))  #   
unique_wo_incoming <- data.frame(unique_wo_incoming)
tail(unique_wo_incoming,5)
barplot(unique_wo_incoming$Unique_Patients, xlab = 'Week', 
        ylab = 'No Incoming', col='brown')

#select(Week, Mondays, Unique_Patients)
dim(unique_wo_incoming)
tail(unique_wo_incoming, 8)

# * 8.2 UNIQUE PTS WITH INCOMING CONTACT SUCCESSFUL OPTION 7 --------

unique_w_incoming_OLD <- dfINT %>% 
  filter(Date.of.Interaction >= "2020-01-06") %>%                # "2020-01-06"
  filter(Outgoing.Contact.Result %in% 
           c('Contact Successful', 'Contact result in Blank') & # |        ### ) %>% #  & #) %>% # | # )  # | 
           Mode != 'Fax'  &       # Mode.PCI %in% c('Yes', 'Other_PtInt') &   # %>%   # & # #&                             # Only Patient Centered Interactions
           Prim.Part.PCI == 'Yes') %>%                      # Only Patient Centered Interactions
  select(Week, Mondays, Client, DOB) %>% group_by(Week, Mondays) %>% 
  summarise(Unique_Incoming_Patients = n_distinct(Client, DOB))  #   
unique_w_incoming_OLD <- data.frame(unique_w_incoming_OLD)
tail(unique_w_incoming_OLD, 5)
barplot(unique_w_incoming_OLD$Unique_Incoming_Patients, xlab = 'Week', 
        ylab = 'w Incoming OLD', col='gray')

# * 8.3 UNIQUE PTS BY PENETRATION RATE --------

unique_w_incoming <- dfINT %>% 
  filter(Date.of.Interaction >= "2020-01-06") %>%                # "2020-01-06"
  filter(Outgoing.Contact.Result %in% 
           c('Contact Successful', 'Contact result in Blank') & # |        ### ) %>% #  & #) %>% # | # )  # | 
           !Mode %in% c('Fax','Mail')  &       # Mode.PCI %in% c('Yes', 'Other_PtInt') &   # %>%   # & # #&                             # Only Patient Centered Interactions
           Prim.Part.PCI == 'Yes') %>%                      # Only Patient Centered Interactions
  select(Week, Mondays, Client, DOB) %>% group_by(Week, Mondays) %>% 
  summarise(Unique_Incoming_Patients = n_distinct(Client, DOB))  #   
unique_w_incoming <- data.frame(unique_w_incoming)
tail(unique_w_incoming, 5)
barplot(unique_w_incoming$Unique_Incoming_Patients, xlab = 'Week', 
        ylab = 'w Incoming CURRENT', col='lightgreen')

# ADDED 5/4/2021 - VERIFICATION USING INTERACTION CLASSIFICATION
meltedPCInt <- melt(data.table(dfINT[!is.na(dfINT$Last52Weeks),]),
                    id.vars = c("Week", "Member.Number"),
                    measure.vars = c("Interaction_Classification"))
unqver2 <- dcast(meltedPCInt, Week + Member.Number ~ value, fun.agg = length, value.var = "value")
names(unqver2) <- c('Week', 'MbNb', 'PCI', 'OtherPt', 'OtherInt', 'UTR')
unqver2 <- unqver2[rowSums(unqver2[,3:4]) >0,] # Total Pt Int > 0
tail(unqver2[,1:2] %>% group_by(Week) %>% summarise(Unq = n_distinct(MbNb)))

#####################################################################$
# 9 COUNT OF WORKING DAYS PER WEEK (FIVE DAYS MINUS HOLIDAYS)  ---------------
#####################################################################$

days_per_week <- dfINT %>% 
  filter(Outgoing.Contact.Result == 'Contact Successful' & 
           Mode.PCI == 'Yes' &                             # Only Patient Centered Interactions
           Prim.Part.PCI == 'Yes') %>%                     # Only Patient Centered Interactions
  group_by(Week, Mondays,WDay2) %>% 
  summarise(sum_mins = sum(Duration))
days_per_week <- data.table(days_per_week)
days_per_week
days_per_week <- dcast(melt(days_per_week, id.vars = c("Week", "Mondays","WDay2"), 
                            measure.vars = c("sum_mins")),
                       variable + Week ~ WDay2, fun.agg = function(x) sum(x), value.var = "value")
days_per_week[,3:7] <- lapply(days_per_week[,3:7], function(x) ifelse(x > 0, 1, 0))
days_per_week
days_per_week$daysweek <- rowSums(days_per_week[,3:7])
days_per_week <- days_per_week[,c('Week',"daysweek")]
days_per_week

table(dfINT$Week)
table(dfINT$daysweek )
# Adding Column Days per Week to Main Data 
dfINT <- dplyr::inner_join(dfINT,days_per_week,by="Week") # ??? Not Sure
dfINT %>% group_by(daysweek) %>% tally()
#tablexy(dfINT$Week, dfINT$daysweek)

#####################################################################$
# 10 TABLE UNIQUE PATIENTS AND TOTAL INTERACTIONS --------------------
#####################################################################$

# unique_w_incoming
unique_pts <- dplyr::inner_join(unique_wo_incoming,days_per_week,by="Week")
#unique_pts
unique_pts <- dplyr::inner_join(unique_pts,unique_w_incoming,by=c("Mondays", "Week"))
unique_pts
dim(unique_pts)
str(unique_pts)

# FROM 7 ALL CONTACT INTERACTIONS dfsuccessint
unique_pts <- dplyr::inner_join(unique_pts,dfsuccessint,by="Week") 
# unique_pts <- unique_pts %>% filter(Week != max(unique_pts$Week)) # Delete the current incomplete week
unique_pts
dim(unique_pts)

#####################################################################$
# 11 DELETE NOT NEEDED COLUMNS ---------------------------------------
#####################################################################$
str(dfINT)
############ Delete ExcelFile column
dfINT <- dfINT %>% select(-c("Notes", "ExcelFile", "ExcelFile2"))
# dfINT <- dfINT %>% filter(Week != max(dfINT$Week)) 

#####################################################################$
# 12 REASON FOR SERVICE ONLY INTERACTIONS WITHOUT UTR ----------------
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


