
#########################################################################.
# SCRIPT: REGULAR EXPRESSIONS UTILIZED IN SCRIPTS
# Example: Type values in third period are missing.
# Fill them up using values from other periods
# USE CASE: One Step to arrange the data frames
####################################################################.

rm(list=ls())

library("dplyr")

# 1 Example of Missing values -----
# that can be filled using values from other periods (Inputation)

df <- data.frame(MID = rep(c("7004R","7443N","9265T","2051R"),4),
                 Period = c(rep("202101",4),rep("202102",4),rep("202103",4),rep("202104",4)),
                 Type = c(rep(c("A","B","C","D"),2),rep(NA,4),rep(c("A","B","C","D"),1))
                 )

df

table(df$Period, df$Type)

# 2 Data without Excluding NA values ----
df_noNA <- df %>% dplyr::arrange(Period) %>% 
  filter(!is.na(Type)) %>% select(MID, Type) %>% 
  group_by(MID, Type) %>% tally() %>% 
  arrange(MID, desc(n)) %>% rename(Type_wo_NA = Type)

df_noNA

# 3 Replace missing (NA) values with values from other periods ----

# LEFT JOIN TABLE OF UNIQUE TYPE + Coalesce
df <- df %>% dplyr::left_join(df_noNA[,c(1:2)], by = c('MID'))

df$Type2 <- dplyr::coalesce(df$Type, df$Type_wo_NA)
df

# ALL_PPL %>% select(Race, RaceX)
# table(df$Period, df$Type2)

