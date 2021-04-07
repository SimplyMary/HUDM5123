install.packages("data.table")
library(data.table)
?fread

# Put your computer path location for the csv file below.
ecls <- fread(file = "/Users/mazixuan/Desktop/childK4p.csv")

# The fread function imports the data very quickly. The other
# alternative is to use read.csv() but it takes forever, so we
# use fread() instead.

dim(ecls)

# Convert back to a data frame
ecls <- data.frame(ecls)
ecls[1:10,1:10]

# Let's find the variables we want to work with.
names(ecls) # Too many column names to search through visually.

# Will use the grep() function to find out where the names 
# are that we are looking for.

?grep

# Start with child's sex
grep(pattern = "CHSEX", 
     x = names(ecls))

# So we see that names of variables 529 2205 3056 4168 4244 5326 5442
# contain the string "CHSEX". Now let's see what the actual names
# are by subsetting the names with those indexes.

names(ecls)[grep(pattern = "CHSEX", x = names(ecls))]

# What is the difference between "X_CHSEX_R", "P1CHSEX", "P2CHSEX", etc.?
# Check the naming convention table on page 7-2.

# Could do this for each variable, one at a time, but there is a better
# way using the %in% operator.

a <- c(1, 2, 3, 8, 9, 10)
a
b <- c(3, 4, 5, 6, 7, 8)
b
a %in% b # Returns TRUE if that element of a is in b
b %in% a # Returns TRUE if that element of b is in a

which(a %in% b)
which(b %in% a)

# Since we know the names we want exactly, let's put them in a vector.
nms <- c("X_CHSEX_R", "X1BMI", "X8BMI", "X8SPECS", "X1MTHETK4", "X8MTHETK4")

# Now we can use the %in% operator to locate the variables we want.
(inds <- which(names(ecls) %in% nms))

names(ecls)[inds]

# Now subset the data, keeping only those variables.
ecls2 <- ecls[, inds,with=FALSE]
dim(ecls2)

head(ecls2)
# Lots of missing data. Let's do listwise deletion to get rid of it.
ecls3 <- na.omit(ecls2)
?
dim(ecls3) # dropped about 8,000 cases

# Now save the data set as an R file so we can work with it later.
save(ecls3, file = "/Users/bryankeller/Desktop/ECLS/ecls3.Rdata")
#can also save as csv

# And you're done. You now have a streamlined data set with only the 
# variables you want and with no missing data. There is no lab assignment
# due this week. Instead, spend time working with data to figure out what
# you want to do for your final project. Think about the data you have access
# to and formulate your research questions in such a way that they may be
# addressed with these data. 

# Remember to look at the syllabus for details about what elements are 
# required for the project. You need an outcome variable, a categorical 
# predictor for ANOVA, another categorical predicor for two-way ANOVA, 
# some covariates for ANCOVA, and then follow up with pairwise comparisons.

# Pick data and research questions that will allow you to work within the 
# parameters of the assignment. If you want to do something else, talk
# with Keller about it.