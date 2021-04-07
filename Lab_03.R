
# Load the ECLSK data set.
# Examine the first 6 rows.
head(eclsk)

# What is the structure?
str(eclsk)

# "num" means that the variable is numeric. Without further
# clarification, R will treat all those variables as 
# numeric varibles. If we want R to treat some as categorical,
# we need to specify which ones using the factor() function.

# The dollar sign $ may be used to access variables in a data
# frame.
mean(eclsk$MIRT)
hist(eclsk$MIRT)

mean(eclsk$GENDER)
table(eclsk$GENDER) 
table(eclsk$WKWHITE)

# To let R know that one of the variables (e.g., GENDER) is 
# categorical, we need to replace the GENDER variable with a
# factored version of itself.
eclsk$GENDER <- factor(eclsk$GENDER)
factor(eclsk$GENDER)

# Now look at the structure
str(eclsk)
mean(eclsk$GENDER)
# Notice that the GENDER variable is now a factor; it is no
# longer numeric. R won't let us take the mean of a categorical
# variable because it assumes the categories are not inerpretable
# as numbers. Sometimes, though, we would like to be able to work 
# with the numeric version of a factor (0s and 1s in this case). 
# To get back to that, we can apply two functions.
eclsk$GENDER <- as.numeric(as.character(eclsk$GENDER))

# Now the GENDER variable is back to being numeric 0s and 1s.
str(eclsk)
mean(eclsk$GENDER)

# Ok, back to factors.
eclsk$GENDER <- factor(eclsk$GENDER)
str(eclsk)

# Factors have levels
levels(eclsk$GENDER)
table(eclsk$GENDER)

# The levels can be changed
levels(eclsk$GENDER) <- c("female", "male")
str(eclsk)
table(eclsk$GENDER)

# By default, the first named level of a factor is treated as
# the reference group when R codes categorical factors. Levels are 
# ordered alphabetically by default.

lm1 <- lm(C6R4MSCL ~ GENDER, data = eclsk)
summary(lm1)
levels(eclsk$GENDER)

# Note that the GENDER predictor is called GENDERmale in the 
# summary of the output. This is because R automatically dummy
# codes factors and assigns the first level (in this case, "female")
# as the reference group. This can have implications for polytomous
# predictors.

# You can change the reference category by using the relevel function.
eclsk$GENDER <- relevel(eclsk$GENDER, ref = "male")
levels(eclsk$GENDER)
lm2 <- lm(C6R4MSCL ~ GENDER, data = eclsk)
summary(lm2)

# Notice the sign on the GENDER variable coefficient has changed 
# now that we changed the reference category. Can change it back.
eclsk$GENDER <- relevel(eclsk$GENDER, ref = "female")
levels(eclsk$GENDER)

# There are no polytomous variables in the ECLSK data frame, only
# continuous and dichotomous. We will need to make one up to have
# one to work with, so let's create a parent status variable called 
# PAR_STATUS that has three categories based on the two variables 
# "ONEPARENT" and "STEPPARENT." 
PAR_STATUS <- eclsk$ONEPARENT

# Reassign the values of PAR_STATUS
PAR_STATUS[eclsk$ONEPARENT == 1] <- "one_parent"
PAR_STATUS[eclsk$STEPPARENT == 1] <- "step_parent"
table(PAR_STATUS)

# Assign the rest to "other"
PAR_STATUS[PAR_STATUS == "0"] <- "other"

# Now it's just a character variable. Let's make it a factor and
# add to the data frame.
str(PAR_STATUS)
eclsk$PAR_STATUS <- factor(PAR_STATUS)

# But wait! We would like to have the "other" category to serve as the
# reference group.
levels(eclsk$PAR_STATUS)

eclsk$PAR_STATUS <- relevel(eclsk$PAR_STATUS, ref = "other")
levels(eclsk$PAR_STATUS)

#########################################################
### Common-slopes model

# When a factor is included in a regression, R will now automatically
# create dummies with the "other" group held out as reference.
lm3 <- lm(C6R4MSCL ~ MIRT + PAR_STATUS, data = eclsk)
summary(lm3)

# If you prefer to create your own dummies from a factor, you can  
# use the model.matrix function.
dummies <- model.matrix(~ PAR_STATUS - 1, data = eclsk)
head(dummies)
colnames(dummies)
colnames(dummies) <- c("D1", "D2", "D3")
# Add them to eclsk data
eclsk <- cbind(eclsk, dummies)

# Regression on dummies holding out D1 as reference
lm4 <- lm(C6R4MSCL ~ MIRT + D2 + D3, data = eclsk)
summary(lm4)

# Write out the model and discusss interpretation of 
# coefficients

# Plotting!
plot(x = eclsk$MIRT, 
     y = eclsk$C6R4MSCL,
     xlab = "Kindergarten Math Score",
     ylab = "Fifth Grade Math Score")

# Let's try again, this time breaking out point color by PAR_STATUS
# category. To do that, we need to make a variable that replaces
# category names with the colors we want plotted. Let's use blue, 
# red, and green.

# Create an empty character vector of the right length
cols <- character(dim(eclsk)[1])

# Fill it with the right color labels based on PAR_STATUS
cols[which(eclsk$PAR_STATUS == "other")] <- "blue"
cols[which(eclsk$PAR_STATUS == "one_parent")] <- "red"
cols[which(eclsk$PAR_STATUS == "step_parent")] <- "green"
table(cols)

# Now back to the plot
plot(x = eclsk$MIRT, 
     y = eclsk$C6R4MSCL,
     xlab = "Kindergarten Math Score",
     ylab = "Fifth Grade Math Score",
     col = cols)

# Let's use filled circles for step_parent and one_parent cases.
# The point character for filled circle is number 19. Google R pch
pnt_chars <- numeric(dim(eclsk)[1])
pnt_chars[which(eclsk$PAR_STATUS == "other")] <- 1
pnt_chars[which(eclsk$PAR_STATUS != "other")] <- 19

# Back to the plot again
plot(x = eclsk$MIRT, 
     y = eclsk$C6R4MSCL,
     xlab = "Kindergarten Math Score",
     ylab = "Fifth Grade Math Score",
     col = cols,
     pch = pnt_chars)

# Now add lines for the groups in the same colors as the points.
summary(lm4)
abline(a = 78.65, b = 1.49, col = "blue", lwd = 3)
abline(a = 78.65 - 6.78, b = 1.49, col = "green", lwd = 3)
abline(a = 78.65 - 3.72, b = 1.49, col = "red", lwd = 3)

# Regression on the categorical variable PAR_STATUS should produce
# identical output as "manual" dummy variable regression 
lm5 <- lm(C6R4MSCL ~ PAR_STATUS, data = eclsk)
summary(lm5)

# Add a legend.
?legend
legend(x = "bottomright",
       legend = c("Single Parent", "Step Parent", "Other"),
       col = c("red", "green", "blue"),
       pch = c(19, 19, 1), 
       lwd = 3,
       seg.len = 4)

# Use an incremental F test to test the overall effect of the
# PAR_STATUS variable.
lmR <- lm(C6R4MSCL ~ 1, data = eclsk)
lmF <- lm(C6R4MSCL ~ D2 + D3, data = eclsk)
anova(lmR, lmF)

#########################################################
### Common-slopes model with polynomial terms
lmp1 <- lm(C6R4MSCL ~ D2 + D3 + MIRT, data = eclsk)
lmp2 <- lm(C6R4MSCL ~ D2 + D3 + MIRT + I(MIRT^2), data = eclsk)
lmp3 <- lm(C6R4MSCL ~ D2 + D3 + MIRT + I(MIRT^2) + I(MIRT^3), data = eclsk)
lmp4 <- lm(C6R4MSCL ~ D2 + D3 + MIRT + I(MIRT^2) + I(MIRT^3) + I(MIRT^4), data = eclsk)
lmp5 <- lm(C6R4MSCL ~ D2 + D3 + MIRT + I(MIRT^2) + I(MIRT^3) + I(MIRT^4) + I(MIRT^5), data = eclsk)
anova(lmp1, lmp2, lmp3, lmp4, lmp5)

# Evidence of improvement until 3rd degree. After that, improvement
# is not significant.
summary(lmp3)

# To graph this, we need to write functions for plotting.
fun_1 <- function(x) {2.23 + 0.00 + 6.56*x - .0958*x^2 + 0.000481*x^3}
fun_2 <- function(x) {2.23 - 5.11 + 6.56*x - .0958*x^2 + 0.000481*x^3}
fun_3 <- function(x) {2.23 - 3.26 + 6.56*x - .0958*x^2 + 0.000481*x^3}

# Back to the plot again
plot(x = eclsk$MIRT, 
     y = eclsk$C6R4MSCL,
     xlab = "Kindergarten Math Score",
     ylab = "Fifth Grade Math Score",
     col = cols,
     pch = pnt_chars)

# Add the curve for the "other" group
points(x = seq(0, 100, .2),
       y = fun_1(seq(0, 100, .2)),
       type = "l", 
       col = "blue", 
       lwd = 3)
# And for the "one parent" group
points(x = seq(0, 100, .2),
       y = fun_2(seq(0, 100, .2)),
       type = "l", 
       col = "red", 
       lwd = 3)
# And for the "step parent" group
points(x = seq(0, 100, .2),
       y = fun_3(seq(0, 100, .2)),
       type = "l", 
       col = "green", 
       lwd = 3)

# What about an interaction with MIRT, kindergarten math score?
# Again, we can do this two ways: one involves manually creating 
# interaction variables and adding them to the data frame; the other
# involves using the named factor.
lm6 <- lm(C6R4MSCL ~ D2 + D3 + MIRT + D2:MIRT + D3:MIRT, data = eclsk)
summary(lm6)

# The model implies three different linear equations (one per group).
# Results may be graphed using the same method as above, but now the
# slopes will vary as well as the intercepts.

# Write out the model and discuss the meaning of coefficients by creating
# three formulas (one per group)
lm7 <- lm(C6R4MSCL ~ PAR_STATUS + MIRT + PAR_STATUS:MIRT, data = eclsk)
summary(lm7)

# Use an incremental F test to test if the interaction (in aggregate)
# is significant.
lmF2 <- lm(C6R4MSCL ~ D2 + D3 + MIRT + D2:MIRT + D3:MIRT, data = eclsk)
lmR2 <- lm(C6R4MSCL ~ D2 + D3 + MIRT, data = eclsk)
anova(lmR2, lmF2)

# With polynomial terms up to cubic:
lmip3 <- lm(C6R4MSCL ~ D2 + D3 + MIRT + D2:MIRT + D3:MIRT +
              I(MIRT^2) + D2:I(MIRT^2) + D3:I(MIRT^2) + 
              I(MIRT^3) + D2:I(MIRT^3) + D3:I(MIRT^3), 
            data = eclsk)
summary(lmip3)


########################
# Lab 03 Assignment ####
########################
# Your task for lab will be to analyze the categorical parental status 
# variable along with RIRT, reading kindergarten score, for predicting
# 5th grade *reading* score. Recall this reading outcome variable was 
# provided to you last lab in a file called "READ5".

# To add the READ5 variable to your eclsk data set, load it. Then
# add it as follows.
eclsk$READ5 <- READ5

# Your write up should include results from the
#   - common-slopes model (with/without polynomial terms)
#in investigating the relationship between ... , i ran...and find out that, sth is signi
#handbook of regression reporting
#such and such: (f=, q=) 
#non-interatsction: yi
#   - interaction model (with/without polynomail terms)
#   - graphs of the common-slopes model with and without polynomial terms
#   - incremental F tests for the main effect of parental status
#   - intremental F tests for interaction of paren stat with RIRT

# If you are feeling up to it, also do graphs for the interaction model
# with and without polynomial terms.
#####################################################################################################
#Feb 21
library(car)
install.packages("carData")
?Prestige
head(Prestige)

