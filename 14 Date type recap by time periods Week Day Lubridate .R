
###################################################################.
# SCRIPT: Interactions by date with de-identified pt info
# Change Date formats. # Convert POSIXct, format to Date, format
# EXAMPLE: INTERACTIONS by date
# Recap by different Periods: Year / Month / Week. Unique pts
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

# * 2.1 CONVERT multiple date fields POSIXct, format to Date, format  -----

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

# ALL Interactions - Convert table to dataframe
table_weekdays <- table(dfINT$Week,dfINT$WDay2)

df_weekdays <- as.matrix.data.frame(table_weekdays, 
                                       rownames.force = T)

colnames(df_weekdays) <- colnames(table_weekdays)

df_weekdays <- dplyr::bind_cols(Week = rownames(table_weekdays),df_weekdays)

tail(df_weekdays,10)

# * 5.3 Filter data Last 16 weeks - pull() %in% --------------------

# Get Last 16 Weeks vector
tail(table(dfINT$Week),16)
L16W <- dfINT %>% distinct(Week) %>% arrange(desc(Week)) %>% 
                                       top_n(16) %>% pull()
L16W

# Count records with filter: "Last16Weeks"
dfINT %>% filter(Week %in% L16W) %>% group_by(Mondays) %>% tally()


# 6 PLOT INTERACTIONS by WEEK ------------------------------

# All Interactions
dfsuccessint <- dfINT %>% group_by(Week) %>% tally()
  
# par: Set or Query Graphical Parameters
par("mar")
par(mar=c(4,6,2,2))

barplot(dfsuccessint$n, xlab = 'Week', 
        ylab = 'No UTR Int', col='lightblue')


# 7 UNIQUE PATIENTS ON EACH WEEK --------------------------

# NOTE: GROUP BY NAME/DOB
unique_pts <- dfINT %>% 
  #filter(Outgoing.Contact.Result == 'Contact Successful') %>% 
  select(Week, Mondays, Client, DOB) %>% 
  group_by(Week, Mondays) %>% 
  summarise(Unique_Patients = n_distinct(Client, DOB))  #   
unique_pts <- data.frame(unique_pts)
tail(unique_pts,10)

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

# # Adding Column Days per Week to Main Data 
# 
# dfINT <- dplyr::inner_join(dfINT,days_per_week,by="Week") # ??? Not Sure


# 9 TABLE UNIQUE PATIENTS AND TOTAL INTERACTIONS --------------------

# unique_w_incoming
recapweeks <- unique_pts %>% 
              dplyr::inner_join(days_per_week,by="Week")
tail(recapweeks)

# 10 WITH ALL CONTACT INTERACTIONS (FROM 7) -------------
recapweeks <- recapweeks %>% 
  dplyr::inner_join(dfsuccessint,by="Week") %>% rename(interactions = n)

tail(recapweeks,10)

# 11 WITH WEEKDAYS --------------

#recapweeks <- cbind(recapweeks, df_weekdays)
recapweeks <- recapweeks %>% 
  dplyr::inner_join(df_weekdays,by="Week")

tail(recapweeks,10)

Sys.time() - st

# END ---------
