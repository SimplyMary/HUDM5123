# Welcome to R and RStudio. To begin, go to the RStudio menu 
# select 'Preferences.' There are three check boxes that offer
# to restore projects, source documents, and workspaces. Uncheck
# these boxes. You may also want to customize the appearance.

# As you may have noticed, '#' is the comment character.
# Anything on a line following a '#' will be ignored by R

# There are four window panes in RStudio. The one you are reading
# this text in is called the script viewer. It allows you to read
# and edit R scripts that are saved on your computer. You can 
# create a new script and save it using the "File" menu above.

# The pane directly below this one is the console. You can type 
# code directly into the console, but it will not be saved. That's
# why I suggest you always use a script file and save frequently.
# Try clicking on the console below so you see the cursor start
# blinking. Then type 5 + 5 and press "Enter" and R should return
# the answer 10 in the console. 

# The pane in the upper right quadrant is the environment/history
# pane. The environment is empty right now because we haven't saved
# anything in R. We will soon. The history tab shows the history
# of code you have run. You should see 10 + 10 there.

# The pane in the bottom right quadrant is where you will see
# help, plots, and interact with files on your computer. 

# To run code from a script file (such as this one), you can select 
# code and then push the "run" button above. Another option is to
# select some code and push command (on Mac) or control (on Windows)
# and enter. Try both methods with the following line:

5 + 5

# If it ran correctly, you should see the code and the answer in the
# console below. My favorite way to run code in a script is line by
# line by simply putting the blinking cursor on a line and then pushing
# command/control and enter. The code on the current line will run, and 
# the cursor will move to the next line, where you can do it again. Try
# it:

5 + 4
10 - 8
14/4
22*4
7^2
sqrt(49)
exp(1)
log(10)

# To get help on a function, use the ? Run the following line to get
# help on the exponential function. You should see the help page pop up
# in the help tab of the bottom right-hand pane.

?exp

# You can assign values to names using the <- operator.

?'<-'

# For example, 
x <- 5
x
x + 7
x*3
x^2
y <- 2000
x + y
z <- y/x
z

# The colon can be used to generate sequences of numbers
# one unit apart. The seq() function is more general.
1:20 # sequence from 1 to 20 by 1s
20:1
2.3:24.3
2.3:3.5 # Note 3.5 is ignored, only goes to 3.3
?seq
seq(from = 2, to = 8, by = 1)
seq(from = 2, to = 8, by = 0.1)
seq(from = 2, to = 8, by = 0.01)
x <- 1:10
x # x is now a vector with ten elements

# There are several data types in R
x <- 10
x # x is a numeric scalar (single number)

y <- seq(from = 2, to = 12, by = 2)
y # y is a vector
# What is the length of y?
length(y) # y has six elements
# What is the value of the third element?
y[3]

# Let's reassign the third element of y to be 999
y[3] <- 999
y

# Multiply the entire vector by 2
2*y

# Reassign y to be 2*y
y <- 2*y
y

# What is the value of the 6th element in y?
y[6]

# Use the c() function to create a vector
z <- c(4, 2, 6, 5, 3, 8, 12, 4, 225)
z
length(z)

# Create a matrix using the vector z
M <- matrix(data = z, nrow = 3, ncol = 3, byrow = TRUE)
M
M <- matrix(data = z, nrow = 3, ncol = 3, byrow = FALSE) 
M
# If you specify byrow = TRUE, the matrix is filled up
# across the rows. If you specify FALSE, it is filled up
# down the columns.

# What is the value of the element in the first row, second
# column?
M[1,2]

# Third row, third column?
M[3,3]

# Display the first column.
M[,1]

# Display the first row.
M[1,]

# Replace the first row with c(1,2,3)
M[1,] <- c(1,2,3)
M

# What are the dimensions of the matrix
dim(M) # 3 rows, 3 columns

# Download the ECLSK data file called eclsk.Rdata and open it
# with RStudio. You can do that by browsing for it in the "Files"
# tab in the bottom-right pane. You can also do it by right-clicking
# the file and opening with RStudio. 

# How many rows/columns
dim(eclsk)

# Variable names?
names(eclsk)

# Look at the first 6 rows of data
head(eclsk)

# eclsk is a data frame
class(eclsk)

# This means that it is a data matrix where columns represent 
# variables and have names. They can be accessed using the $.
# Also, RStudio shows relevant suggestions when you push the tab
# key. For example, write eclsk$ on a line below and put your 
# cursor after the $ and push tab. Note that you can pick from 
# the list of variables in the eclsk data.

# Get a table for gender.
table(eclsk$GENDER)

# Cross tabulate gender by special ed services.
table(eclsk$GENDER, eclsk$F5SPECS) # 289 male spec ed; 140 female spec ed

# Take the mean of the 5th grade math outcome score.
mean(eclsk$C6R4MSCL)

# Variance/SD
var(eclsk$C6R4MSCL)
sd(eclsk$C6R4MSCL)

# Plot histogram
hist(eclsk$C6R4MSCL)

# Adjust the x label and the main title
hist(eclsk$C6R4MSCL, 
     xlab = "5th Grade Math Score",
     main = "Histogram of ECLSK 5th Grade Math Scores")

# Boxplot
boxplot(eclsk$C6R4MSCL)

# Turn horizontal and label axis
?boxplot
boxplot(eclsk$C6R4MSCL, 
        horizontal = TRUE,
        xlab = "5th Grade Math Score",
        main = "Boxplot of ECLSK 5th Grade Math Scores")

# Bivariate display of 5th grade math score on math score in kindergarten
plot(x = eclsk$MIRT, y = eclsk$C6R4MSCL)

# Pearson correlation between MIRT and outcome.
cor(eclsk$MIRT, eclsk$C6R4MSCL)

# Add meaningful labels
plot(x = eclsk$MIRT, y = eclsk$C6R4MSCL,
     xlab =  "Kindergarten Math Score",
     ylab = "5th Grade Math Score",
     main = "ECLSK")

# Run the linear regression of math5 on mathK.
lm1 <- lm(formula = C6R4MSCL ~ MIRT, data = eclsk)
summary(lm1)

# Add the regression line to the plot
abline(lm1, col = "red")

# Widen the line
abline(lm1, col = "red", lwd = 3)

# Examine diagnostic residual plots found in package "car"
install.packages("car") 

# Use a quantile-quantile plot to examine the tenability of 
# the normaliy assumption.

library(car)
qqPlot(lm1) 

# Examine residual plots using the plot function applied to 
# output from fitting an lm model
plot(lm1)

# Look at the residuals vs fitted plot. It clearly shows a 
# curved pattern in the residuals.

# Regression in R, as we have seen above, is done within the 
# lm() function.
?lm

# The two essential arguments to the lm function are the 
# formula argument and the data argument.

# The formula should be of the form outcome ~ pred1 + pred2 + ... + predp
# For example, to run the multiple regression of 5th grade math 
# on kindergarten math score and socioeconomic status and student gender:

lm2 <- lm(formula = C6R4MSCL ~ RIRT + P1FIRKDG + P1FSTAMP, data = eclsk)

# Use the summary function on the saved output called lm2
summary(lm2)

# Can also access predicted values and residuals directly.
hist(lm2$residuals)

# plot residuals against predicted values
plot(x = lm2$fitted.values, y = lm2$residuals)

# Add a nonparametric fitted curve
scatter.smooth(x = lm2$fitted.values, y = lm2$residuals, col = "red")

# Non-constant trend in the residuals suggests linear functional form
# is misspecified.

# Add labels
plot(x = lm2$fitted.values, y = lm2$residuals,
     xlab = "Fitted Values", ylab = "Residuals")

# Use plot function to get residual plots automatically.
# You will need to go to the console and hit "enter" to move
# through the plots. Note the first plot (residuals vs fitted) 
# and second plot (QQ plot for normality).
plot(lm2)

# Prefer to have 95% confidence bands around the QQ line. Use package
# car.
library(car)
qqPlot(lm2)
