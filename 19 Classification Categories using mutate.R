
###################################################################.
# SCRIPT: Classify records by Categories created with mutate
# Recap using tables and group_by
# Similar to Pivot tables
# Example: Interactions classification
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

# 1 read data - all periods -------

# Paths: Location of Source files
path            <- "PopulationSamples"       

filenames_list <- list.files(path= path, full.names=TRUE, 
                             pattern=paste0("^interactions.*?.xlsx"))
filenames_list

dfINT <- data.frame(readxl::read_excel(filenames_list))

dim(dfINT)

table(paste(format.Date(dfINT$Date.of.Interaction, "%Y"),
            format.Date(dfINT$Date.of.Interaction, "%m"), sep="/"))


# 2 Create classification Mode: TABLES: PCI, Prim. Part, Mode ----------------

# * 2.1 Replace NA replace_na  --------------------

tablex(dfINT$Outgoing.Contact.Result) # 609 NA values

dfINT$Outgoing.Contact.Result <- dfINT$Outgoing.Contact.Result %>% 
  replace_na("Contact result in Blank")
tablex(dfINT$Outgoing.Contact.Result)

#Numerator: All patients with a completed interaction within the year.
# This includes calls, emails, video conferences, texts, 
# ICT meetings and other.

# * 2.2 Classification Mode.PCI (Pt Centered Interaction) -------------

dfINT <- dfINT %>% mutate(Mode.PCI = if_else(Mode %in% 
                         c("Call", "Visit", "Videoconference"), "Yes", ""))
tablex(dfINT$Mode.PCI)
# Mode2   # Used in Excel reports

# * 2.3 Classification Mode - Group into "Other" -------------

tablex(dfINT$Mode)

dfINT <- dfINT %>% mutate(Mode2     = if_else(Mode %in% 
          c("Fax", "Email", "ICT Meeting", "Videoconference", 
            "Text", "Other"), "Other", Mode))

table(Mode.PCI = dfINT$Mode.PCI,Mode2 = dfINT$Mode2)

# data.frame(dfINT %>% group_by(Mode, Mode.PCI, Mode2, ModewoFaxMail) %>% 
#                         tally() %>% arrange(desc(Mode.PCI)))

# * 3.2 Table Prim.Part.PCI Primary Participant --------------------------------

tablex(dfINT$Primary.Participant)

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

head(dfINT,1)

# 4 Patients that received more Completed Interactions -----

ptsmoreint <- dfINT %>% 
  filter(Interaction_Classification == "Completed Pt. Centered Interaction") %>% 
  group_by(Client) %>% tally() %>% arrange(desc(n)) %>% filter(n >= 6)
ptsmoreint

barplot(ptsmoreint$n, 
        xlab = 'Patient Interactions per Patient', 
        ylab = 'Patients', 
        col = 'lightblue')

Sys.time() - st

##############################.


