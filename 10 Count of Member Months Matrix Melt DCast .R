
###################################################################.
# SCRIPT: COUNT OF ENROLLMENT MONTHS PER PATIENT - MEMBER MONTHS
# Example: Count of enrollment months on current and previous month. In real application: last 12 months
# USE CASE: Practical application of summarization, using melt - dast, similar to Excel pivot tables
###################################################################.

rm(list=ls())

library("data.table")

# Example of Missing values that can be filled using values from other periods (Inputation)

df <- data.frame(Period = c(rep("202101",4),rep("202102",4),rep("202103",4),rep("202104",4)),
                 MID = c(c("7004R",NA     ,"9265T",NA),
                         c("7004R","7443N","9265T","2051R"),
                         c("7004R","7443N",NA,"2051R"),
                         c(NA,     NA ,    NA,"2051R")) )
df <- df %>% arrange(MID) %>% filter(!is.na(MID))
df                 

# * 1 RECAP OF PERIODS  -------------

#ALL_PPL$Mem
# dfPPL <- ALL_PPL %>% group_by(PPL) %>% arrange(PPL) %>% distinct(PPL) 
#ALL_PPL %>% group_by(PPL) %>% arrange(PPL) %>% distinct(PPL)
#ALL_PPL %>% distinct(PPL) %>% arrange(PPL)
dfPeriod <- df %>% distinct(Period) %>% arrange(Period)
dfPeriod$PPL_Id <- seq_along(1:nrow(dfPeriod))
head(dfPeriod)
tail(dfPeriod)

# ## ADD PPL_Id to main data table
# ALL_PPL <- ALL_PPL %>% inner_join(dfPPL, by = 'PPL')
# head(ALL_PPL)

# * * 6.5.1 MELT DCAST MID Period -----
castedMIDPeriod <- data.table::dcast(melt(data.table(df), 
                           id.vars = c("MID", "Period"), 
                           measure.vars = c("Period")),  # = c("PPL_Id")),
                      MID ~ Period, fun.agg = length)

castedMIDPeriod   # 1 or 0 if the pt is enrolled in each Period
castedMbMos <- castedMIDPeriod

# * * 6.5.2 CALCULATION MEMBER MONTHS PER PATIENT ----
# https://stackoverflow.com/questions/50639903/paste-variable-name-in-mutate-dplyr
for(i in 2:nrow(dfPeriod)){   
  #  i <- 12
  dfPeriodfincol <- dfPeriod$Period[i]
  dfPeriodfincol     # Name of Month example "201809"
  C3fincol <- match(dfPeriodfincol, names(castedMbMos))
  C3fincol        # Index of Month example "13"
  dfPeriodinicol <- dfPeriod$Period[i-1]
  dfPeriodinicol
  C3inicol <- match(dfPeriodinicol, names(castedMbMos))
  C3inicol
  castedMbMos[,C3inicol:C3fincol]
  castedMbMos <- castedMbMos %>% 
    mutate( !!paste0(dfPeriodfincol, 'MbMo') := 
              rowSums(castedMbMos[,C3inicol:C3fincol]))
}

# * * 6.5.3 DELETE INDIVIDUAL MONTHS - ONLY FIELDS Member Months -----
castedMbMos

dim(castedMbMos)
castedMbMos <- castedMbMos %>% select(-names(castedMbMos)[1:nrow(dfPeriod)+1])
castedMbMos
dim(castedMbMos)

MbMos <- melt(data.table(castedMbMos), 
              id.vars = c("MID"), 
              measure.vars = grep("MbMo", names(castedMbMos), value = T))

MbMos
MbMos <- MbMos %>% rename(MbMos = value, Period = variable)
MbMos
MbMos$Period <- substr(MbMos$Period, 1, 6)
MbMos
df2df <- df %>% left_join(MbMos, by = c('MID', 'Period'))
df2df


