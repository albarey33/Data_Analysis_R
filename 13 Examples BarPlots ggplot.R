
###################################################################.
# SCRIPT: ADD INDEX COLUMN TO DATA FRAME
# USE CASE: To de-identify patients and not showing PHI
###################################################################.

# 0 PREPARE INSTALL CALL PACKAGES -----------------------------------------

rm(list=ls()) # Delete all objects
ls() # List variables in memory

# options(header=T, sep=",", stringsAsFactors = FALSE, 
#         str = strOptions(strict.width = "cut"), 
#         readr.show_progress=FALSE)

st <- Sys.time()

# Load-multiple-packages-at-once
required_packages <- c("dplyr", "ggplot2")
lapply(required_packages, library, character.only = TRUE)

# 1 PARAMETERS CHANGE NAMES / UPDATE --------------------------------------------------

# Paths # Location of Source files
path            <- "PopulationSamples"       

# 2 READ THE DOWNLOADED DATA EXCEL FILES ------------------------------------
# Read sample files using full path and regex (known pattern)

filenames_list <- list.files(path= path, full.names=TRUE, 
                             pattern=paste0("^recaptable.*?.csv"))
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
recaptable <- fx_readfiles(filenames_list)

dim(recaptable)
recaptable$PPL <- as.character(recaptable$PPL)
head(recaptable)
 
# 3 POST CALC VERIFICATION - MATRIX OF RESULTS PPL, Pts, EDVs, IPVs, Readm. ----
# dfPts <- ALL_PPL %>% group_by(PPL) %>% summarise(Pts = sum(Patients, na.rm = T)) 
# dfEDVs <- ALL_PPL %>% group_by(PPL) %>% summarise(EDVs = sum(ED.Visits..Last.12.mos., na.rm = T))
# dfIPVs <- ALL_PPL %>% group_by(PPL) %>% summarise(IPVs = sum(Inpatient.Admissions..Last.12.mos., na.rm = T))
# dfReadm <- ALL_PPL %>% group_by(PPL) %>% summarise(Readm = sum(Inpatient.Readmissions..Last.12.mos., na.rm = T))
# dfMbMos <- ALL_PPL %>% group_by(PPL) %>% summarise(MbMos = sum(MbMos, na.rm = T))
# recaptable <- dfPts %>% inner_join(dfEDVs, by = 'PPL') %>% 
#   inner_join(dfIPVs, by = 'PPL') %>% inner_join(dfReadm, by = 'PPL') %>% inner_join(dfMbMos, by = 'PPL')
# recaptable
# fwrite(recaptable,"recaptable.csv")

str(recaptable)

t(recaptable[,c("PPL","MbMos")])

# tobarplot <- ALL_PPL %>% group_by(PPL) %>% summarise(EDVs = sum(ED.Visits..Last.12.mos., na.rm = T))
# tobarplot

# 3 BARPLOTS ----------

# * 3.1 Enrolled Patients ------------

# # EDVbarplot <- ALL_PPL %>% group_by(PPL) %>% arrange(PPL) %>% 
# #   summarise(EDVs = sum(ED.Visits..Last.12.mos., na.rm = T))
# # EDVbarplot <- data.frame(EDVbarplot)
# str(EDVbarplot)
barplot(recaptable$Pts, 
        main="Enrolled Patients",
        xlab = "PPL",
        ylab = "Pts", col = 'lightblue', 
        las = 2, 
        cex.names = 0.7)

# ggplot2 plot with modified x-axis labels
ggplot(recaptable, aes(PPL, Pts)) +    
  geom_bar(stat = "identity", fill = "#160000") +
  theme(axis.text.x = element_text(angle = 90, size = 10)) +
  ggtitle("Enrolled Patients")

# * 3.2 Emergency Department Visits (ED Visits) -----

# # EDVbarplot <- ALL_PPL %>% group_by(PPL) %>% arrange(PPL) %>% 
# #   summarise(EDVs = sum(ED.Visits..Last.12.mos., na.rm = T))
# # EDVbarplot <- data.frame(EDVbarplot)
# str(EDVbarplot)
barplot(recaptable$EDVs, 
        main="ED Visits in the Last 12 months",
        xlab = "PPL",
        ylab = "ED Visits", col = 'lightblue', 
        las = 2, 
        cex.names = 0.7)

# ggplot2 plot with modified x-axis labels
ggplot(recaptable, aes(PPL, EDVs)) +    
  geom_bar(stat = "identity", fill = "#158000") +
  theme(axis.text.x = element_text(angle = 90, size = 10)) +
  ggtitle("ED Visits in the Last 12 months")

# * 3.3 Inpatient Visits (IPVs) -----

# IPVplot <- ALL_PPL %>% group_by(PPL) %>% arrange(PPL) %>% 
#   summarise(IPVs = sum(Inpatient.Admissions..Last.12.mos., na.rm = T))
barplot(recaptable$IPVs, 
        main="IP Visits in the Last 12 months",
        xlab = "PPL",
        ylab = "IP Visits", col = 'lightgray')

# ggplot2 plot with modified x-axis labels
# ggplot: With x-axis labels
ggplot(recaptable, aes(PPL, IPVs)) +    
  geom_bar(stat = "identity", fill = "#990000") +
  theme(axis.text.x = element_text(angle = 90, size = 10)) + 
  ggtitle("IP Visits in the Last 12 months")

# * 3.4 Inpatient Readmissions -----
# READMBARPLOT <- ALL_PPL %>% group_by(PPL) %>% arrange(PPL) %>% 
#   summarise(ReadmIPVs = sum(Inpatient.Readmissions..Last.12.mos., na.rm = T))
tail(recaptable$Readm)
barplot(recaptable$Readm, 
        main="IP Readmissions in the Last 12 months",
        xlab = "PPL",
        ylab = "IP Readmissions", col = 'lightgray')
ggplot(recaptable, aes(PPL, Readm)) +    # ggplot2 plot with modified x-axis labels
  geom_bar(stat = "identity", fill = "#8000FF") +
  theme(axis.text.x = element_text(angle = 90, size = 10)) + 
  ggtitle("IP Readmissions in the Last 12 months")

# * 3.5 Member Months ------
# tail(ALL_PPL %>% group_by(PPL) %>% arrange(PPL) %>% 
#        summarise(MbMos = sum(MbMos, na.rm = T)))
# tobarplot <- ALL_PPL %>% group_by(PPL) %>% arrange(PPL) %>% 
#   summarise(MbMos = sum(MbMos, na.rm = T))
barplot(recaptable$MbMos, 
        main="Monthly Total of Member Months",
        xlab = "PPL",
        ylab = "Member Months", col = 'brown')

