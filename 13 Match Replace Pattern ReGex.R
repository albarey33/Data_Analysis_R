
###################################################################.
# SCRIPT: REGULAR EXPRESSIONS UTILIZED IN SCRIPTS
# Example: Change old names of Practices by new ones defined in table
# USE CASE: One Step to arrange the data frames
###################################################################.

# Substrings of a Character Vector
# Extract or replace substrings in a character vector.

df <- data.frame(Zip = c("28377-0000","28306-0000","28314-0000",
                         "28342-0000","28303-0000","27332-0000"))
df

substr(df$Zip,1,5)


# grep - Pattern Matching and Replacement 
x <- "$12,543.43"
as.numeric(gsub("[\\$,]", "", x))   # replace "$" and "," for ""

df <- data.frame(Int_Date = c("12/9/2019 12:00:00 AM",
                              "2/20/2020 12:00:00 AM",
                              "5/4/2020 12:00:00 AM"))
gsub('([0-9]+) .*', '\\1', df$Int_Date)
sub(" 12:00:00 AM","",df$Int_Date)

# Values with a string in pattern
grep("2020",df$Int_Date, value = T )

# Remove comma in 1000 value
df <- data.frame(Score = c("1,000","702","785","843","757"))
as.integer(gsub("[\\,]", "", df$Score))

########################### END ------ 
