
###################################################################.
# SCRIPT: RECAP OF SPARSE MATRIX 
# USE CASE: To export and visualize Power BI
###################################################################.

# 0 PREPARE INSTALL CALL PACKAGES -----------------------------------------

rm(list=ls()) # Delete all objects
ls() # List variables in memory

options(header=T, sep=",", stringsAsFactors = FALSE,
        str = strOptions(strict.width = "cut"),
        readr.show_progress=FALSE)

st <- Sys.time()

# Load-multiple-packages-at-once
required_packages <- c("dplyr", "lubridate", "data.table", "ggplot2")
lapply(required_packages, library, character.only = TRUE)

tablex       <- function(x){print(table(x, useNA = 'always'))}
tablexy      <- function(x,y){print(table(x,y, useNA = 'always'))}

# 1 PARAMETERS CHANGE NAMES / UPDATE --------------------------------------------------

# Paths # Location of Source files
path            <- "PopulationSamples"

# 2 READ THE DATA FILE - SAMPLE 1500 patients -----------------------------------

dtRECAP <- read.csv(list.files(path= path, full.names=TRUE, pattern="Recap_6mo.csv"))
dim(dtRECAP)

str(dtRECAP)

# 3 Group of Medical Conditions - Values 0 / 1  ------------

Yes1Ini <- match("Any.MH",names(dtRECAP))
Yes1End <- match("Ischemic.Vascular.Dis",names(dtRECAP))
vmedconds <- c(names(dtRECAP)[Yes1Ini:Yes1End])

dtRECAP[,vmedconds] <- lapply(dtRECAP[,vmedconds],function(x){ifelse(is.na(x),0,x)})

str(dtRECAP)

# 4 DATE OF LAST INTERACTION, PRACTICE AND WELLNESS VISITS ------ 

# * 4.1 Change character fields to lubridate::mdy ----

str(dtRECAP)

vdatesfields <- c("Date.of.Last.CM.Interaction", "Date.of.Last.Known.Practice.Visit", 
           "Date.of.Last.Known.Wellness.Visit")

dtRECAP[,vdatesfields] <- lapply(dtRECAP[,vdatesfields],function(x){lubridate::mdy(x)})

str(dtRECAP)

# * 4.2 Plot Last Visits values ----

for(ii in seq_along(vdatesfields)){
  hist(dtRECAP[!is.na(vdatesfields[ii]),vdatesfields[ii]], 
       freq = TRUE, breaks = 150, col = "lightblue", 
       main=vdatesfields[ii])
       Sys.sleep(0.1)
}

# * 4.3 LAST DATE PER MONTH BY PPL (Day 16 of each month - new enrollment) ------
dtRECAP$lastDate <- as.Date(paste(substr(dtRECAP$PPL,1,4),
                                  substr(dtRECAP$PPL,5,6),
                                  16,sep="-"),"%Y-%m-%d")
dtRECAP %>% group_by(PPL, lastDate) %>% tally() %>% arrange(desc(PPL)) 

# * 4.4 Calculation Days between Last visit and day of enrollment (last day per month) ----
# Time Intervals / Differences with difftime {base}
dtRECAP$Days.Since.Last.CM.Interaction <- difftime(dtRECAP$lastDate, dtRECAP$Date.of.Last.CM.Interaction, units = 'days')
dtRECAP$Days.Since.Last.Known.Practice.Visit <- difftime(dtRECAP$lastDate, dtRECAP$Date.of.Last.Known.Practice.Visit, units = 'days')
dtRECAP$Days.Since.Last.Known.Wellness.Visit <- difftime(dtRECAP$lastDate, dtRECAP$Date.of.Last.Known.Wellness.Visit, units = 'days')

# * 4.5 Visit in last 365 days (last year) -----
dtRECAP$CM.Interaction.in.Last.Year <- ifelse(is.na(dtRECAP$Days.Since.Last.CM.Interaction)|
                                      dtRECAP$Days.Since.Last.CM.Interaction > 365, 0L, 1L)
dtRECAP$Practice.Visit.in.Last.Year <- ifelse(is.na(dtRECAP$Days.Since.Last.Known.Practice.Visit) |
                                  dtRECAP$Days.Since.Last.Known.Practice.Visit > 365, 0L, 1L)
dtRECAP$Wellness.Visit.in.Last.Year <- ifelse(is.na(dtRECAP$Days.Since.Last.Known.Wellness.Visit) |
                                  dtRECAP$Days.Since.Last.Known.Wellness.Visit > 365, 0L, 1L)

str(dtRECAP)

# 5 Data All Variables ~ Months ~ Practices using MELT DCAST ----

dtRECAP <- data.table(dtRECAP)

vectorcond <- c('Patients', vmedconds,  
                "CM.Interaction.in.Last.Year",
                "Practice.Visit.in.Last.Year", 
                "Wellness.Visit.in.Last.Year")
vectorcond

length(vectorcond)

# * 5.1 Melt Each Group of Conditions by PPL and Practices ----
# Function Melt Each Group of Conditions by PPL and Practices

fx_melttables <- function(vectorconditions, dfdata){
  dffinal <- data.frame()
  dfdata <- data.table(dfdata)
  for(i in seq_along(vectorconditions)){
    print(vectorconditions[i])
    melted1 <- melt(dfdata, id.vars = c("PPL", "Practice_Name"), measure.vars = vectorconditions[i]) 
    #print(head(melted1,1))
    dffinal <- bind_rows(dffinal, melted1)
  }
  print(dffinal)
}

results <- fx_melttables(vectorcond[1:18], dtRECAP)
dim(results)

# * 5.2 Group by PPL and Practice. Summarise all variables (Conditions) ----

totaltable <- results %>% group_by(variable, PPL, Practice_Name) %>% 
  summarise(Enrollees = sum(value), .groups = 'drop')
totaltable

str(totaltable)

# 6 Example LinePlot (ggplot2) for one Condition (Hypertension) -------

dfHypertension <- data.frame(totaltable %>% filter(variable== "Hypertension") %>% 
  group_by(PPL) %>% summarise(ptsHTN = sum(Enrollees, na.rm = T)))

# Basic Line
ln <- ggplot(data=dfHypertension, aes(x=PPL, y=ptsHTN, group=1, label=ptsHTN)) +
  geom_line(color="blue",size=1) + geom_point()+ geom_text(nudge_y = 1)

# y-axis limits
ln +  #ylim(0,100)
theme_light() + labs(x="Enrollment Month",y="Patients with Hypertension")

# 5 WRITE MATRIX RECAP -----------------------------------------------------

fwrite(totaltable,"PopulationSamples//all_months_variables_for_PowerBI.csv")

Sys.time() - st

# END ------

