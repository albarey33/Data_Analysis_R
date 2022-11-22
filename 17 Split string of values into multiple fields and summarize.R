
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

# 1 READ DATA - all periods -------

# Paths: Location of Source files
path            <- "PopulationSamples//"

filenames_list <- list.files(path= path, full.names=TRUE, 
                             pattern=paste0("^interactions.*?.xlsx"))
filenames_list

dfINT <- data.frame(readxl::read_excel(filenames_list))

dim(dfINT)
str(dfINT)

# Add Year-Month to group values
dfINT$YearMonth <- paste(format.Date(dfINT$Date.of.Interaction, "%Y"),
                         format.Date(dfINT$Date.of.Interaction, "%m"),sep = "-")


# 2 REASON FOR SERVICE ONLY INTERACTIONS WITHOUT UTR ----------------

tail(dfINT %>% group_by(Reason.s..for.Service) %>% tally,10)

# Split a string of values into multiple columns per value

### Data WITHOUT UTR
dfRecapNoUTR <- dfINT %>% filter(Outgoing.Contact.Result != "Unable to Reach (UTR)")
dfRecapNoUTR$ID <- seq.int(nrow(dfRecapNoUTR))   # Add an ID Index column
str(dfRecapNoUTR)
dim(dfRecapNoUTR)

dat <- with(dfRecapNoUTR, strsplit(Reason.s..for.Service, ','))
dat  # Lists within lists
df2 <- data.frame(ID = factor(rep(dfRecapNoUTR$ID, times = lengths(dat)),
                              levels = dfRecapNoUTR$ID),
                  Reason.s..for.Service = unlist(dat))
head(df2) # ALL ID ~ Reasons

# Sparse Matrix. Convert table to data frame
df3 <- as.data.frame(cbind(ID = dfRecapNoUTR$ID,
                           table(df2$ID, df2$Reason.s..for.Service)))  
head(df3)
str(df3)

dfRecapReasons <- dfRecapNoUTR %>% select("YearMonth", "ID") %>% 
  inner_join(df3, by = 'ID')

str(dfRecapReasons)   # MONTHS + ID + 17 REASONS
head(dfRecapReasons)

# 3 RECAP PER YEAR - MONTH ---------

dfRecapReasons <- data.table(dfRecapReasons %>% 
                               group_by(YearMonth) %>% 
                               select(-c("ID")) %>% 
                               summarise_all(list(sum)))
dim(dfRecapReasons)
str(dfRecapReasons)   # YEARMONTH + 17 REASONS
tail(dfRecapReasons,5)


Sys.time() - st

# END ------ 

