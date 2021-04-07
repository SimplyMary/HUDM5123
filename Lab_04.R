#################################################
# Prestige data 
# install.packages("carData")
library(carData)
?Prestige
head(Prestige)
dim(Prestige)
str(Prestige)
Prestige # Note missing data for type variable

# Delete cases with missing values for type
Prestige <- na.omit(Prestige)
dim(Prestige) # dropped 4 cases

table(Prestige$type)
levels(Prestige$type)

# Prefer bc, wc, prof ordering, so let's reorder the levels 
# of the type factor in the Prestige data set.
Prestige$type <- factor(x = Prestige$type, levels = c("bc", "wc", "prof"))
levels(Prestige$type)

# Boxplots by group
boxplot(prestige ~ type, data = Prestige)


# R will automatically create dummy variables for factors
# that are entered as predictors in regression models. The
# reference category is the first level of the factor, so 
# if you want a different reference category, you need to 
# change the factor level orderings.

# Regression with dummy coding.
levels(Prestige$type) # "bc" will be reference
lm1 <- lm(prestige ~ type, data = Prestige)
summary(lm1)

# Let's use "wc" as reference instead.
Prestige$type <- relevel(Prestige$type, ref = "wc")
levels(Prestige$type) # "wc" will be reference
lm2 <- lm(prestige ~ type, data = Prestige)
summary(lm2)

# Change it back.
Prestige$type <- relevel(Prestige$type, ref = "bc")
levels(Prestige$type)

# To do ANOVA in R, we will use the Anova() function 
# from package car. Install the package if you haven't 
# already. If you have, you can skip the install.
# install.packages("car")
library(car)
?Anova

# To use the Anova function, we first have to fit a linear
# model with factor variables as predictors. Then, we will
# pass that fitted model on to the Anova function. There are 
# two options for how to have Anova() calculate sums of squares
# referred to as type III and type II. We will use Type III 
# going forward, so be sure to specify type = 3 when you use the
# function.

# To optimize our output, we need to ensure that the method
# R uses for factor coding aligns with the method typically
# used in ANOVA. In ANOVA, instead of dummy coding, deviation
# coding is typically used because it makes the intercept
# interpretable as the overall (grand) mean, instead of the 
# mean of the reference group (as in dummy coding). So, to 
# make sure that our model output gives estimates that are
# consistent with what we expect from ANOVA, we need to set the
# default behavior for unordered factors to helmert coding, 
# which is another way of specifying deviation coding.

# By default the coding is set to "contr.treatment", 
# which is equivalent to dummy coding.
options()$contrasts 

# For anova, we need to change it to deviation (helmert) 
# coding. Note that we only need to change the behavior for
# unordered factors. We leave the default coding scheme for
# ordered factors as "contr.poly"

# Dummy coding
options(contrasts = c("contr.treatment", "contr.poly"))
summary(lm(prestige ~ type, data = Prestige))

# Deviation (also called effect) coding
options(contrasts = c("contr.helmert", "contr.poly"))
summary(lm(prestige ~ type, data = Prestige))

# Now the intercept represents the grand mean, and the 
# other two slopes represent *deviations* from the grand
# mean (which is why it's called "deviation" coding).

# So now the option is set to do deviation coding. Fit
# and name the full model.
lm3 <- lm(prestige ~ type, data = Prestige)
summary(lm3)

# Check assumptions. 
# 1. Independence of errors (logical argument)
# 2. Normally distributed errors. - Examine a QQ plot
library(car)
qqPlot(lm3) 
# 3. Linearity (0 conditional mean). - Examine resid plot
residualPlot(lm3)
# 4. Homoskedasticity (constant conditional variance).  
#    Estimate group variances and compare ratio of 
#    largest to smallest. Want the ratio to be less than
#    four or so.
by(data = Prestige$prestige, INDICES = Prestige$type, FUN = var)
#    100.47/75.29 = 1.33
#    Can also examine boxplot IQRs.
boxplot(prestige ~ type, data = Prestige)
#    Can also run a test of H_0: var1 = var2 = var3
bartlett.test(Prestige$prestige ~ Prestige$type)

# Now run ANOVA using the Anova function.
# Here we are testing H_0: \mu_1 = \mu_2 = \mu_3
Anova(lm3, type = 3) # use type = 3

# The null hypothesis of H_0: \mu_1 = \mu_2 = \mu_3
# is rejected. Thus, we know that there is at least
# one pairwise difference among means. Next, we will
# follow up with pairwise comparisons. For that, we 
# use the emmeans package.

# Install and load the package.
# install.packages("emmeans")
library(emmeans)
?emmeans
install.packages("emmeans")
# emmeans stands for estimated marginal means. The
# package is useful for calculating group means for
# factor levels or combinations of factors levels and
# doing contrast comparisons among them.

# Three arguments passed to function emmeans:
#  object is the fitted model
#  specs is typically a right-hand side of a formula
#  Set adjust to "none" for now

emm1 <- emmeans(object = lm3,
                specs = ~ type,
                adjust = "none")
summary(emm1)
install.packages("ggplot2")
# Can plot an emmeans object to visualize the EMMs.
# The plot will include 95% confidence intervals around
# each marginal (group) mean.
plot(emm1) # default is horizontal plot
plot(emm1, horizontal = FALSE)

# Pairwise comparisons can be done with the function
# pairs()
prs1 <- pairs(emm1, adjust = "none")
prs1
# Results of pairwise comparisons can be visualized with 
# arrows in the plot.
plot(emm1, horizontal = FALSE, adjust = "none", comparisons = TRUE)

# We can see the contrast coefficients by extracting the
# model coefficients with coef()
coef(prs1)

# Specify complex contrasts with the contrast function
psis <- contrast(emm1, 
                 list(psi1 = c(.5, .5, -1), psi2 = c(0, 1, -1)))

# Generate confidence 95% confidence intervals for the custom contrasts
confint(psis, adjust = "none", level = .95)

### Lab 04 ###
# For this lab you will create a categorical factor
# by binning the continuous income variable. First 
# get the cut points for quantiles on income.
qtls <- quantile(x = Prestige$income, probs = c(.25, .5, .75))
qtls

# Now create a factor variable with four factors based
# on the cutpoints.
incomeF <- numeric(dim(Prestige)[1])
incomeF[which(Prestige$income < 4250.5)] <- "lowest"
incomeF[which(Prestige$income >= 4250.5 & Prestige$income < 6035.5)] <- "low"  
incomeF[which(Prestige$income >= 6035.5 & Prestige$income < 8226.25)] <- "high"  
incomeF[which(Prestige$income >= 8226.25)] <- "highest"  
Prestige$incomeF <- factor(incomeF)
head(Prestige)
str(Prestige)

# Your task for lab is to run one-way ANOVA to assess the 
# effect of income on prestige. If you find an overall effect,
# follow up with pairwise comparisons and describe the results.
# Finally, check for a significant difference in the average of 
# prestige of lowest and low vs the averge prestige of high and 
# highest. Be sure to check assumptions.
#assigment done as follow:
#1 relevel the incomeF
Prestige$incomeF<-factor(x=Prestige$incomeF, levels =c("lowest","low", "high", "highest"))
#2 visualize the data
boxplot(prestige ~ incomeF, data = Prestige)
# use relevel to change the reference group,
Prestige$incomeF<-relevel(Prestige$incomeF, ref = "lowest")
#Regression with dummy coding
lm1 <- lm(prestige ~ incomeF, data = Prestige)
summary(lm1)
#set devidation coding for the anova
lm2 <- lm(prestige ~ incomeF, data = Prestige)
summary(lm2)

# Check assumptions. 
# 1. Independence of errors (logical argument)
# 2. Normally distributed errors. - Examine a QQ plot
library(car)
qqPlot(lm2) 
# 3. Linearity (0 conditional mean). - Examine resid plot
residualPlot(lm2)
# 4. Homoskedasticity (constant conditional variance).  
#    Estimate group variances and compare ratio of 
#    largest to smallest. Want the ratio to be less than
#    four or so.
by(data = Prestige$prestige, INDICES = Prestige$incomeF, FUN = var)
#    232.58/81.4725 = 1.33
232.58/81.4725
#    Can also examine boxplot IQRs.
boxplot(prestige ~ type, data = Prestige)
#    Can also run a test of H_0: var1 = var2 = var3
bartlett.test(Prestige$prestige ~ Prestige$incomeF)
#run ANOVA using anova function
Anova(lm2, type = 3)
#marginal means comparisons
emm1 <- emmeans(object = lm2,
                specs = ~ incomeF,
                adjust = "none")
summary(emm1)
#plot emmeans
plot(emm1) # default is horizontal plot
plot(emm1, horizontal = FALSE)

# Pairwise comparisons can be done with the function
# pairs()
prs1 <- pairs(emm1, adjust = "none")
prs1
# Results of pairwise comparisons can be visualized with 
# arrows in the plot.
plot(emm1, horizontal = FALSE, adjust = "none", comparisons = TRUE)

# Specify complex contrasts with the contrast function
psis <- contrast(emm1, 
                 list(psi1 = c(.5, .5, -0.5,-0.5)))

# Generate confidence 95% confidence intervals for the custom contrasts
confint(psis, adjust = "none", level = .95)
psis
