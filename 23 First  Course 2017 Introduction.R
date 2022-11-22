
######################################################. 
# FIRST COURSE - R INTRODUCTION - 2017 -----
######################################################. 

############################################.
# 01 VARIABLES - ARITHMETICS ---------

height <- 6
width <- 3

# area
height <- 2
width <- 3
area <- height * width
area

#comment calculate the area
area <-  height * width
area
width

# checking all the variables in the workspace
ls()

# remove the area variable
rm(area)
ls()

# arithmetics

a <- 654
b <- 345

# addition
a+b

#substraction
a-b

# multiplication
a * b

# division
a/b

# exponentiation
a^2

# Modulo (remainder of division)
a%%b

ls()

# Clear the entire workspace
rm(list = ls())
ls()

# donut
r <- 2
R <- 6

# calculate volume
vol_donut <- 2*(pi^2)*(r^2)*R
vol_donut
pi

# remove intermediary variables
rm(r)
rm(R)

#list again elements in workspace
ls()

############################################.
# 02 DATA TYPES - CLASS() -------
# 
# Data Types or Atomic Vector types 

TRUE
class(TRUE)
FALSE
class(FALSE)
class(NA)

a <- 2   # Numeric atomic vector
b <- 3L  # Integer atomic vector

is.numeric(a) # is.functions ask type
is.numeric(b) # is.functions ask type
is.integer(a) # is.functions ask type
is.integer(b) # is.functions ask type

# Data Type Character
class("Work")

text1 <- "I love data science" # Atomic string vector
text1
class(text1)

# * Coercion --------

as.character(4)
as.numeric("4.34")
as.integer(5.36)
as.numeric(TRUE) # COERCION change the type of a variable
as.numeric(FALSE) # COERCION change the type of a variable
as.character(5) # COERCION change the type of a variable
as.numeric("4.3") # COERCION change the type of a variable
as.integer("4.3") # COERCION change the type of a variable
as.numeric("Hello") # COERCION change the type of a variable

# logical or boolean values

class(TRUE) # boolean
class(2) # numeric
class(2L) # integer


############################################.
# 03 CREATE AND NAME VECTORS --------------

#               vectors FUNCTION vector c()
drawn_suits <- c("hearts","spades","diamonds","diamonds","spades")
drawn_suits

# FUNCTION TABLE
table(drawn_suits)
is.vector(drawn_suits)

# name a vector fx names() 
remain <- c(11,12,11,13)
suits <- c("spades","hearts","diamonds","clubs")
names(remain) <- suits
remain

# FUNCTION STR()
str(remain) # Check named vector

length(remain)

############################################.
# 04 VECTOR ARITMETHIC --------------

earnings <- c(50,100,30) + c(50,30,10)
earnings

expenses <- c(80L,150L,60L)

names(earnings) <- c("Monday","Tuesday","Wednesday")

remain <- earnings - expenses
remain

earnings > expenses

str(expenses)
class(expenses)

########################################################.
# 05 VECTOR SUBSETTING -----

# * Vector subsetting with square brackets
earnings[3]
earnings
str(earnings)

# subsetting using an Index
earnings[2]

# subsetting using a field_name
earnings["Monday"]

remain
remain_two_days <- remain[c(1,2)]
remain_two_days 
only_third_day <- remain[-c(1,2)]
only_third_day

#     subsetting with a logical vector
remain
red <- remain[c(TRUE,FALSE,FALSE)]
red
red13 <- remain[c(2:4)]
red13
remain
remain2 <- c(A = 11,B=10,C=13,D=14)
remain2
compare <- c(remain != remain2)
compare

# Exercises ----------
# Vector
results <- c(4,6,5,8,6,2)
results
is.vector(results)
# Labels
days <- c("M", "TU", "W", "TH", "F", "S")
names(results) <- days
results
class(results)
# str Attributes
str(results)
# Length
length(results)
results_weeks <- c(w1=45,w2=24,w3=76,w4=87,w5=63)
results_weeks
length(results_weeks)
results_months <- c("m1"=98,"m2"=76,"m3"=87,"m4"=96,"m5"=63,"m6"=52)
results_months
length(results_months)
str(results_months)

#sum
sum(results)
sum(results_weeks)
sum(results_months)

#subsetting: The result is again a vectorresults
results[1]
results_weeks[2]
results_months[3]
results["TH"]
results
res12 <- results[c(1,2)]
res12
rw34 <- results_weeks[c(3,4)]
rw34
results_months[-5]
results_months[-c(3,4)]
halfdays <- results[c(FALSE,TRUE,TRUE)] # RECYCLING
halfdays
counter_asc <- c(1:101)
counter_dsc <- c(101:1)
counter_asc[c(FALSE,TRUE)]
#mean : AVERAGE
mean(counter_asc)
selection1 <- c(counter_asc > counter_dsc)
selection1
selection2 <- c(counter_asc < counter_dsc)
selection2
selection3 <- c(counter_asc >= counter_dsc)
selection3
selection4 <- c(counter_asc <= counter_dsc)
selection4
selection5 <- c(counter_asc == counter_dsc)
selection5
selection6 <- c(counter_asc != counter_dsc)
selection6


# vector sequence of data elements 
# of the same type

drawn_suits <- c("h","s","d","c","d")

drawn_suits

is.vector(drawn_suits)

table(drawn_suits)

##################################################################.
# 06 MATRIX - MATRICES -------------------------- 

#       Create matrix
mx1 <- matrix(1:600,nrow=2)
mx1

#       Create matrix
mx2 <- matrix(1:200,ncol=10, byrow = TRUE)
mx2
mx2[c(TRUE,TRUE,FALSE),c(TRUE,TRUE,FALSE)]
#       Create matrix sort by rows
mx3 <- matrix(1:15,ncol=5, byrow=TRUE)
mx3

#     paste vectors together   CBIND()  RBIND()
cbind(1:3,5:7)

rbind(5:2,9:6)

#    paste rows or columns to existing matrices
m <- matrix(1:6, byrow = TRUE, nrow = 2)
m
rbind(m,7:9)
cbind(m,c(10,11))

# NAMES to matrices rows and columns
m1 <- matrix(1:30,nrow=5,byrow=TRUE)
m1
# FUNCTION ROWNAMES()
rownames(m1) <- c("row1", "row2", "row3", "row4", "row5")
m1
# FUNCTION COLNAMES()
colnames(m1)<- c("col1","col2","col3","col4","col5","col6")
m1

# COERCION TWO MATRICES
num <- matrix(1:16,ncol=2)
num
char <- matrix(LETTERS[1:24],nrow=8,ncol=3)
char
n2 <- cbind(num,char)
rownames(n2) <- c("row1", "row2", "row3", "row4","row5", "row6","row7", "row8")
colnames(n2)<- c("col1","col2","col3","col4","col5")
n2

# Exercises - Star Wars Box office in millions 
new_hope <- c(460,314)
empire_Strikes <- c(290,247)
return_jedi <- c(309,165)
star_wars_matrix <- rbind(new_hope,empire_Strikes,return_jedi)
star_wars_matrix

# Name Columns and Rows
colnames(star_wars_matrix) <- c("US", "NonUS")
rownames(star_wars_matrix) <- c("new_hope","empire_Strikes","return_jedi")
star_wars_matrix
str(star_wars_matrix)

# 07 SUBSETTING MATRICES ----------
# With Double Brackets and Vectors 

star_wars_matrix  
star_wars_matrix[4] # without comma, fourth element column wise
star_wars_matrix[1,2]   # Single value
star_wars_matrix[,1]    #  Only column 1 as Vector
star_wars_matrix[,"NonUS"] #  Only column "NonUS"
star_wars_matrix[3:6]   # from 3 to 6 values, begginning w/first column
star_wars_matrix[c(2,3),"NonUS"] # values 2nd and 3rd column "NonUS"
star_wars_matrix

#   Logical vectors, recycling
star_wars_matrix[c(TRUE,FALSE),c(TRUE,FALSE)]
star_wars_matrix[c(TRUE,FALSE,TRUE),c(TRUE,FALSE)]

#                  Matrix 6 x 2 Vectors
phantom <- c(474,552)
attack <- c(310,338)
revenge <- c(380,468)
last_w <- rbind(phantom,attack,revenge)
last_w
rownames(last_w) <- c("phantom","attack","revenge")
last_w
all_w <- rbind(star_wars_matrix,last_w)
all_w

#        Selecting alternating
total <- rowSums(all_w)
totalall_w <- cbind(all_w,total)
totalall_w
#  Option A RIGHT TO SELECT TOTALS
#Instead of a vector(sequences of values
# FUNCTION seq() returns alternating values simmilar to recycle
totalall_w[seq(2, 6, by = 2), "total"]  
# Option B equal result
totalall_w[c(F,T,F,T,F,T),c(F,F,T)]
# Option C ERROR
totalall_w[c("new_hope",3,6),c(T,T,F)]
# Option C OK
totalall_w[c(2,3,6),c(T,T,F)]
# Option D RIGHT TO SELECT TOTALS
totalall_w[c(F,T),"total"]

# 08 MATRIX ARITMETHIC ------------------
# Arithmetic Functions for Matrices > Element Wise and Recycling
ls()
totalall_w
totalall_w/1.12
totalall_w - 50
# Discount a vector. R aplies recycling
tax <- c(50,80,100)
totalall_w - tax

ls()
rm("A_vector")
ls()
rm(list = ls())
ls()
ls()

#   FUNCTION  rowSums()  colSums()  sum()
total_profit_movies <- rowSums(star_wars_matrix)
total_profit_region <- colSums(star_wars_matrix)
total_profit_movies
is.vector(total_profit_movies)
total_profit_region 
# total
sum(star_wars_matrix)

#################################################.
# 09 FACTORS ---------------------------

#            Nominal Categorical Levels
blood <-  c("B", "AB", "O", "A", "O", "O", "A", "B")
blood
str(blood)

#           FUNCTION FACTOR() By default in Alphabetical Order
blood_factor <- factor(blood)
blood_factor
str(blood_factor)

#      With an Specific Order; Adding levels = 
blood_factor_order_2 <- factor(blood, levels = c("O", "A", "B", "AB"))
blood_factor_order_2
str(blood_factor_order_2)
blood_factor_order_2[1] < blood_factor_order_2[2] # Error

#    Ordinal Categorical ; Adding ordered = TRUE to levels
tshirt <- c("M", "L", "S", "S", "L", "M", "L", "M")
tshirt_factor <- factor(tshirt, ordered = TRUE, levels = c("S", "M", "L"))
tshirt_factor

tshirt_factor[1] < tshirt_factor[2]  # No Error because ordinal

#     Summary of categorical vector FUNCTION summary()
summary(tshirt)
#     Summary of categorical vector of factors
summary(tshirt_factor)

# Specification of Level and Labels
fly_class <- c("eco","fir","bus","fir","eco","bus",
               "fir","eco","fir","bus","fir","eco","bus")
lvls <- c("eco", "bus", "fir")
lbls <- c("economy", "business", "first")

# Encode fly_class as a factor, with the appropiate names and ordering
fly_class_factor <- factor(fly_class, levels=lvls, ordered=TRUE, labels=lbls)
fly_class
fly_class_factor
summary(fly_class)    # Vector
summary(fly_class_factor)         # Factor


#######################################################.
# 10 LISTS ------------------------------

rm(list=ls())
ls()

################################################################.
#       FUNCTION LIST() - Alternative 1 to name with function names()
song <- list("Rsome Times", 190, 5)
song
str(song)
is.list(song)
is.vector(song)
#         FUNCTION NAMES()
names(song) <- c("title","duration","track")
song
str(song)

# LIST IN LIST and Alternative 2 to name lists, using equal sign ( = )
similar_song <- list(title = "R you on time?", duration = 230)
similar_song

song <- list(title="R some times",
             duration=190,
             track=5,
             similar=similar_song)   # Other list
song
str(song)

#######################################################################.
# 11 SUBSETTING LISTS -------------
# Using single brackets return a list

f <- song[1]     # Return the first column "title" as another list
f
class(f)

# Using double brackets return a single element type: Character
g <- song[[1]]
g
class(g)

#   SUBSET USING A VECTOR and single brackets
song[c(1,3)]  # Returns the 1 and 3 columns of the list
str(song[c(1,3)])  # Returns the 1 and 3 columns of the list

# Error for double brackets
song[[c(1,3)]]  # subscript out of bounds equivalent to song[[1]][[3]]

#   Subset one element
song[[3]][[1]]      # Element 3 subelement 1
song[[4]][[1]]	  # Element 4 subelement 1
song[[c(4,1)]]	  # Element 4 subelement 1 - equivalent to previous
song[[4]][[2]]      # Element 4 subelement 2  # Works when a list in the 4 field

#         SUBSETTING BY NAME 
song["duration"]              # Returns a list 
class(song["duration"])       # Returns a list: verification
str(song["duration"])         # Returns a list: str
str(song[c("duration","similar")])      # Returns a list two fields

str(song[["duration"]])       # Returns a numeric
class(song[["duration"]])     # Returns a numeric verification

#         SUBSETTING BY LOGICALS (only with single bracket)
song[c(FALSE,TRUE,TRUE,FALSE)]
str(song[c(FALSE,TRUE,TRUE,FALSE)])

#      SUBSETTING EXTENDING WITH SIGN $. Returns a FIELDS like single brackets
song$duration
class(song$duration)
is.vector(song$duration)

# EXTEND LISTS - ADDING a FIELD to list w/vector and list$field - METHOD 1
friends <- c("Kurt","Florence","Rosa","Dave")
song$sent <- friends
song
# Verification
song$sent
class(song$sent)
is.list(song$sent)
is.vector(song$sent)

#      Double bracket to extend list (similar as previous ) - METHOD 2
song[["sent"]] <- friends
song

#  ????? ERROR ????? With vector function(similar as previous ) - METHOD 3
song <- c(song,sent<-friends)
song
song[8]

#      ADDING Elements(Fields) to an embedded list
song$similar$reason <- "too long"
song
str(song)

song$release <- 1983
song$director <- "Stanley Kubrick"
song
song["sent"]
song[6]
str(song)
song[[1]][1]

####################################################.
# 12 DATA.FRAMES -----------------

ls()
rm(list=ls())

#         First Data Frame   FUNCTION  data.frame()
name <- c("Anne","Pete","Ann","Julia","Frank") # characters
age <- c(28,30,21,39,35)                 # numeric
child <- c(FALSE,TRUE,TRUE,FALSE,TRUE)   # logical

# Create a data.frame

#   Name Data Frame
# Naming the Data Frame. Alternative 1
df <- data.frame(name,age,child)
names(df) <- c("Name","Age","Child")
str(df)

# Naming the Data Frame. Alternative 2
rm(df)
df <- data.frame(Name=name, Age=age,Child=child)
df
str(df)

class(df)

# To prevent the fields be created as Factors 
df <- data.frame(name,age,child, stringsAsFactors=FALSE)
df
str(df)  # Structure: Lists with three elements with equal (5) elements

# Subset Head Tail
head(df,2)
tail(df,3)

#          FUNCTION Dimensions
dim(df)

#           Structure
str(df)

# Exercises: Create the data frame EMPLOYEES
emp.data <- data.frame(
  emp_id = c (1:5), 
  emp_name = c("Rick","Dan","Michelle","Ryan","Gary"),
  salary = c(623.3,515.2,611.0,729.0,843.25), 
  start_date = as.Date(c("2012-01-01", "2013-09-23", "2014-11-15", "2014-05-11",
                         "2015-03-27")),
  stringsAsFactors = FALSE
)
# Print the data frame.			
print(emp.data) 
str(emp.data)

# Exercise: Create Data frame PLANETS
planets_df <- data.frame(
  planets = c("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune"),
  type = c("Terrestrial planet", "Terrestrial planet", "Terrestrial planet", "Terrestrial planet", "Gas giant", "Gas giant", "Gas giant", "Gas giant"),
  diameter = c(0.382, 0.949, 1, 0.532, 11.209, 9.449, 4.007, 3.883),
  rotation = c(58.64, -243.02, 1, 1.03, 0.41, 0.43, -0.72, 0.67),
  rings = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE))
planets_df
str(planets_df)
dim(planets_df)

# Create Data frame PLANETS FIXING FACTORS
planets_df <- data.frame(
  planets = c("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune"),
  type = factor(c("Terrestrial planet", "Terrestrial planet", "Terrestrial planet", "Terrestrial planet", "Gas giant", "Gas giant", "Gas giant", "Gas giant")),
  diameter = c(0.382, 0.949, 1, 0.532, 11.209, 9.449, 4.007, 3.883),
  rotation = c(58.64, -243.02, 1, 1.03, 0.41, 0.43, -0.72, 0.67),
  rings = c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE),
  stringsAsFactors = FALSE)
planets_df
str(planets_df)
names(planets_df) <- c("Name", "Type", "Diameter", "Rotation","Has Rings")
planets_df
print(planets_df)


# 13 SUBSET AND EXTEND DATA.FRAMES ----------------

rm(list=ls())
ls()

#                     First Data Frame
name <- c("Anne","Pete","Frank","Julia","Cath")
age <- c(28,30,21,39,35)
child <- c(FALSE,TRUE,TRUE,FALSE,TRUE)
people <- data.frame(name,age,child,stringsAsFactors=FALSE)
people
str(people)

# * Subsetting Data Frames --------

# * * SUBSETTING Single Elements
people[3,2]
class(people[3,2])
people[[3,2]]
class(people[[3,2]])
people[3,"age"]
class(people[3,"age"])

# * SUBSETTING FIELDS OR Columns
people[c(3,2)] # By default the vector without comma refers to fields or columns
class(people[c(3,2)])
people[,c(3,2)]  # With comma before the vector refers to fields or columns
class(people[,c(3,2)])
people[,"age"]   # all field info
people["child"]
class(people["child"])
str(people$child)
people[1] # Unlike matrices which return a vector(row), here results in data frame
people[2]
class(people[1])

# SUBSETTING OBSERVATIONS OR ROWS 
people[3,]   # all instance info
people[3,]       # Simmilar to matrices
people[c(3,2),]  # With comma AFTER the vector refers to observations or rows

# SUBSETTING with two vectors .... subset data frame
people[c(3,2),c("child","age")]
str(people[c(3,2),c("child","age")])
people
str(people)
#                  
data.frame(people$child,people$age)
data.frame(child,age)

#   SUBSET DATA FRAME - RESULTS ARE VECTORS
people$age    # Result vector numeric
people["age"] # Single bracket results in data frame
people[["age"]] # Double square brackets in data frame results a vector
people[,"age"]
people[[2]]     # Double square brackets in data frame results a vector
class(people$age)

#   SUBSET DATA FRAME - RESULTS ARE LISTS (DATA FRAMES w/one ELEMENT)
people["age"]
class(people["age"])
people[2]

# * Extend Data Frame -------
#      ADD A COLUMN ( A NEW VARIABLE) TWO OPTIONS WITH $ OR DOUBLE BRACKETS
#      SAME AS LISTS

heightvector <- c(163,177,163,162,157)
people$height <- heightvector
people

people[["height"]] <- heightvector
people

# ADD A COLUMN WITH CBIND
weight <- c(74,63,68,55,56)
people <- cbind(people,weight)
people

# ADD ROWS - NEW OBSERVATIONS DATA FRAME WITH ONE OBSERVATION (NO WEIGHT)
tom <- data.frame("Tom",37,FALSE,183)   # Results in Error names do not match
rbind(people,tom)                       # Results in Error names do not match

tom <- data.frame(name="Tom",age=37,child=FALSE,height=183, weight=60)
tom
rbind(people,tom)

# * Sorting Data Frames -----------

# SORTING - SORT ONLY RETURN ONE COLUMN - FUNCTION SORT()
sort_vector <- sort(people$height)
sort_vector
class(sort_vector)
sort(people$age)

# SORTING - ORDER RETURN INDEX (RANKS) FOR SORTED AGE - FUNCTION ORDER()
people$age
ranks <- order(people$age)
ranks               # Return a vector
people[ranks,]      # reorder the data.frame
people[order(people$age),]    # Replacing ranks
people[order(people$age, decreasing=TRUE),]  # same but decreasing 

# * Exercises: Subset, extend, order Data.Frames ----------
planets <- c("Mercury", "Venus", "Earth", "Mars", "Jupiter", "Saturn", "Uranus", "Neptune")
type <- c("Terrestrial planet", "Terrestrial planet", "Terrestrial planet", "Terrestrial planet", "Gas giant", "Gas giant", "Gas giant", "Gas giant")
diameter <- c(0.382, 0.949, 1, 0.532, 11.209, 9.449, 4.007, 3.883)
rotation <- c(58.64, -243.02, 1, 1.03, 0.41, 0.43, -0.72, 0.67)
rings <- c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, TRUE, TRUE)
planets_df <- data.frame(planets,type,diameter,rotation,rings)
str(planets_df)

# FIXING FACTOR Encode type as a factor: type_factor
type_factor <- factor(type)
planets_df <- data.frame(planets,type_factor,diameter,rotation,rings,
                         stringsAsFactors = FALSE)
planets_df
str(planets_df)

# SUBSETTING DATA FRAMES .
mars_type <- planets_df[4,3]   # Subset one element
mars_type                      # Subset one element
planets_df$rotation    # Results a vector
planets_df[[4]]        # Results a vector

# HEAD TAIL
closest_planets <- head(planets_df,4)   # FUNCTION head() 
closest_planets
furthest_planets <- tail(planets_df,4)  # FUNCTION tail() 
furthest_planets

# Planets with Rings  DIFFERENT WAYS TO SUBSET FILTERING A VALUE
rings_vector <- planets_df[["rings"]]
rings_vector           # Double square brackets in data frame results a vector
planets_df[rings_vector==TRUE,]           # Using logical vector rings_vector
planets_df[rings_vector,]                 # Using logical vector rings_vector
planets_df[rings_vector,TRUE]             # Using logical vector rings_vector
planets_df[planets_df$rings == TRUE,]
planets_df[planets_df[["rings"]] == TRUE,]

planets_without_rings <- planets_df[rings_vector==FALSE,]
planets_without_rings

# USING function subset function
subset(planets_df, subset = rings == TRUE)

# Planets with Rings    FILTERING WITH > 
planets_df
planets_df[planets_df$diameter >= 9.449,]
small_planets_df <- planets_df[planets_df$diameter <1,]
small_planets_df
slow_planets <- planets_df[abs(planets_df$rotation) > 1,]   #abs ABSOLUTE VALUE
slow_planets

# ADDING COLUMNS ###################	.
#   Adding columns > planets_df is already pre-loaded in your workspace
planets_df

# Definition of moons and masses
moons <- c(0, 0, 1, 2, 67, 62, 27, 14)
masses <- c(0.06, 0.82, 1.00, 0.11, 317.8, 95.2, 14.6, 17.2)

# Add moons to planets_df under the name "moon"
planets_df$moon <- moons
planets_df

# Add masses to planets_df under the name "mass"
planets_df$mass <- masses
planets_df
# planets_df <- cbind(planets_df,erase=c(1,2,3,4,5,6,7,8))
# planets_df <- planets_df[,-8]

# ADDING A NEW OBSERVATION # EXAMPLE Pluto 
# Name pluto correctly

pluto <- data.frame(
  planets = "Pluto", 
  type_factor = "Terrestrial planet",
  diameter = 0.18, 
  rotation = -6.38, 
  rings = FALSE,
  moon=0,
  mass=0.01)

# Bind planets_df and pluto together: planets_df_ext
planets_df_ext <- rbind(planets_df, pluto)

# Print out planets_df_ext
planets_df_ext

#   SORTING WITH ORDER
planets_df
planets_df[order(planets_df$diameter,decreasing=TRUE),]

####################################################.
# 14 BASIC GRAPHICS ------------------
rm(list=ls())	
ls()

# * PLOTS EXERCISES --------

str(Salaries)

#       PLOT NUMERICAL VALUES
plot(Salaries$yrs.service)

#       PLOT CATEGORICAL VALUES OR FACTORS Bar Plot
plot(Salaries$rank)

#       PLOT TWO CONTINUOUS VARIABLES -> Scatter Plot
plot(Salaries$yrs.service,Salaries$salary)

#       PLOT TWO CATEGORICAL FACTORS
plot(Salaries$rank,Salaries$discipline)
plot(Salaries$rank,Salaries$sex)

#       PLOT CATEGORICAL VS NUMERICAL - BOXPLOT
plot(Salaries$sex,Salaries$salary)


# * HISTOGRAMS --------

hist(Salaries$salary, breaks = 20)

####################################################.
# 15 CUSTOMIZING PLOTS ---------------------

plot(Salaries$yrs.service,Salaries$salary, 
     xlab = "Years of Service", 
     ylab = "Salaries", 
     main = "Years vs Salaries", 
     type = 'p', 
     col = "blue")

# * par() - Parameters for all graphs

par(col = "red")
plot(Salaries$yrs.service,Salaries$salary, 
     xlab = "Years of Service", 
     ylab = "Salaries", 
     main = "Years vs Salaries", 
     type = 'p')

plot(Salaries$discipline,Salaries$salary, 
     xlab = "discipline", 
     ylab = "Salaries", 
     main = "Discipline vs Salaries", 
     type = 'p')

par()$col

plot(Salaries$discipline,Salaries$salary, 
     xlab = "discipline", 
     ylab = "Salaries", 
     main = "Discipline vs Salaries", 
     type = 'p', 
     col.main = "purple", 
     cex.axis = 2.2,     # ratio of font size
     lty =  5,      # line type : dashed
     pch = 4)         # plot symbol - 4 for x
  

####################################################.
# 16 MULTIPLE PLOTS ---------------------

# * List all the graphical parameters - par() --------
str(par())

# GRID of graphs

par(mfrow = c(2,2))              # graphs added in a row wise fashion
plot(Salaries$yrs.service,Salaries$salary)
plot(Salaries$yrs.since.phd, Salaries$salary)
plot(Salaries$sex,Salaries$salary)
plot(Salaries$rank,Salaries$discipline)


par(mfcol = c(2,2))              # graphs added in a column wise fashion
plot(Salaries$yrs.service,Salaries$salary)
plot(Salaries$yrs.since.phd, Salaries$salary)
plot(Salaries$sex,Salaries$salary)
plot(Salaries$rank,Salaries$discipline)

# To reset as usual
par(mfcol = c(1,1)) 
plot(Salaries$yrs.service,Salaries$salary)

# * layout() -----------------

grid <- matrix( c(1,1,2,3), 
                nrow = 2, ncol = 2, byrow = TRUE)
layout(grid)
plot(Salaries$yrs.service,Salaries$salary)
plot(Salaries$yrs.since.phd, Salaries$salary)
plot(Salaries$sex,Salaries$salary)

# to reset to default 1 graph
layout(1)
par(mfcol = c(1,1)) 


# * Reset the parameters  ----------

old_par <- par()

par(col="red")
plot(Salaries$yrs.service,Salaries$salary)

par(old_par) # restore all the graphical parameters
plot(Salaries$yrs.service,Salaries$salary)

# * Stacking Graphical Elements -------------
# Adding layers on the same plot

plot(Salaries$yrs.service,Salaries$salary,     # plot(x,y)
     pch = 16, col = 2, 
     xlab = "Years of Service",
     ylab = "Salaries")

# In lm() the variables are inverted.            lm(y ~ x)
lm_salaries <- lm(Salaries$salary ~ Salaries$yrs.service)
abline(coef(lm_salaries), lwd = 2)   # line width

# * Add lines --------
ranks <- order(Salaries$yrs.service)
plot(Salaries$yrs.service,Salaries$salary,     # plot(x,y)
     pch = 16, col = 2, 
     xlab = "Years of Service",
     ylab = "Salaries")
abline(coef(lm_salaries), lwd = 2)   # line width
#plot(ranks)
lines(Salaries$yrs.service[ranks], Salaries$salary[ranks])


# END -------
