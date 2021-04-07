# Lab 10: Repeated Measures ANOVA

# Start by loading the Rdata file called rmd, which contains
# three dataframes named rmd1, rmd2, and rmd3.

###
# rmd1 is the example from lecture that involves measuring ten
# participants' response times under four levels of noise distraction.
rmd1

# Variance/covariance matrix 
round(var(rmd1), 2)

# Correlation matrix
round(cor(rmd1), 2)

# First step in running repeated measures ANOVA in R is to 
# run a multivariate regression. The four columns of rmd1
# are treated as the outcome variable and are fit with an
# intercept-only model.
mv1 <- lm(cbind(No, Mil, Mod, Sev) ~ 1, data = rmd1)

# Next, we need to specify the factor names and levels. There is 
# only one factor, which we will call noise. The gl() function
# generates levels for a factor variable.
?gl
noise <- gl(n = 4, k = 1, labels = c("No", "Mil", "Mod", "Sev"))
noise <- ordered(noise) # it is an ordered factor
noise
fac <- as.data.frame(noise)

# Run ANOVA with the Anova() function by specifying three things.
# First, the multivariate model, mv1. Second, for repeated-
# measures data, the idata and idesign arguments are needed.
# From the car package documentation:

# idata:	an optional data frame giving a factor or factors defining 
# the intra-subject model for multivariate repeated-measures data. 

# idesign: a one-sided model formula using the “data” in idata and 
# specifying the intra-subject design.

rm_anova1 <- Anova(mod = mv1, idata = fac, idesign = ~ noise)
summary(rm_anova1)

# Note the output for Mauchly's test for sphericity along with the 
# sphericity-assumed output (p = 1.6e-12) and the GG and HF corrected
# output (ps = 1.046e-06 and 2.868e-07).

###
# A perceptual psychologist brought subjects into the laboratory 
# and seated them in front of a tachistoscope. Subjects were told 
# they would see either the letter ‘T’ or the letter ‘I’ on the screen.
# The factor varied by the experimenter is where in the display the letter
# appeared. This factor, which is called angle, has three levels. The 
# target letter was either shown at the center of the screen, i.e.,
# 0°off-center, where the subject was instructed to fixate, 4°off- center,
# or 8°off-center. In the latter two cases, the deviation from the center 
# varies randomly between left and right. Mean reaction times for each 
# level of the angle factor for 20 subjects separated by age (10 younger 
# and 10 older) are given, along with age group, in the rmd2 dataframe.

# rmd2 is what is referred to as a "mixed" or "split-plot" design 
# because one factor is repeated and the other is not. That is, 
# one factor (age) is a between-subjects factor (i.e., not repeatedly
# measured) and the other (angle) is a within-subjects (i.e., 
# repeatedly measured) factor.
rmd2
str(rmd2)

# Convert age to a factor
rmd2$age <- factor(rmd2$age)

# An additional assumption required for the split-plot case is that
# the var/cov matrixes for the repeatedly measured data are the same
# across levels of the between-subjects factors. That is, the var/cov 
# for age = 1 should be the same as that for age = 2.
# install the heplots package to get 

# Variance/covariance matrix for age == 1 
round(var(rmd2[rmd2$age == 1, 2:4]), 2)
round(cor(rmd2[rmd2$age == 1, 2:4]), 2)

# Variance/covariance matrix for age == 2 
round(var(rmd2[rmd2$age == 2, 2:4]), 2)
round(cor(rmd2[rmd2$age == 2, 2:4]), 2)

# There is a statistical test for equality of var/cov matrixes called
# Box's M test, which can be found in package heplots.
install.packages("heplots")
library(heplots)

# First fit the multivariate regression
mv2 <- lm(cbind(angle0, angle4, angle8) ~ age, data = rmd2)

# Then run Box's M test.
boxM(mv2)

# The null hypothesis is that the covariance matrices are equal.
# Here, we fail to reject (p = .56) and, thus, do not have
# conclusive evidence that the matrices differ. Carry on.

# Generate names for repeated-measures factors
angle <- rep(c("angle0", "angle4", "angle8"), 1)
angle <- ordered(angle)
angle
df2 <- data.frame(angle)
df2

# Run ANOVA
rm_anova2<- Anova(mod = mv2, idata = df2, idesign = ~ angle)
summary(rm_anova2)

# Here we get separate sphericity tests for each term involving
# a repeatedly-measured factor. The only repeatedly-measured factor
# in this case is angle, so both the main effect for angle and the
# age by angle interaction are tested for spherecity and have 
# GG and HF adjusted p-values reported.

# Thus, we find that the two-way interaction between age and 
# angle is significant. To follow-up the two-way interaction, 
# break the data up based on the levels of the within-subjects
# factor angle and analyze each subset as if it came from a simpler
# design.

# Simple omnibus tests of age at each level of angle and use a 
# Holm-Bonferroni correction:
Anova(lm(angle0 ~ age, data = rmd2), type = 3) # ns
Anova(lm(angle4 ~ age, data = rmd2), type = 3) # ns
Anova(lm(angle8 ~ age, data = rmd2), type = 3) # sig (p = .001)

# Pairwise comparisons here are identical to the omnibus test
# because there are only two groups (age = 1 or 2). Nevertheless,
# can do it with emmeans:

library(emmeans)
emm1 <- emmeans(object = lm(angle8 ~ age, data = rmd2), 
                specs = ~ age, adjust = "none")
pairs(emm1)

# What if we had decided to split the data based on the age 
# factor instead? Then we would need to do two simple omnibus
# tests: one for angle at age = 1 and the other for angle at age = 2.
# But recall that angle is a within-subject factor, so the simple 
# omnibus tests should be analyzed using repeated-measures methods.

mv2_1 <- lm(cbind(angle0, angle4, angle8) ~ 1, data = rmd2[rmd2$age == 1,])
rm_anova2_1 <- Anova(mod = mv2_1, idata = df2, idesign = ~ angle)
summary(rm_anova2_1) # sig

mv2_2 <- lm(cbind(angle0, angle4, angle8) ~ 1, data = rmd2[rmd2$age == 2,])
rm_anova2_2 <- Anova(mod = mv2_2, idata = df2, idesign = ~ angle)
summary(rm_anova2_2) # sig

# Define a contrast as usual by ψ = c1μ1 + c2μ2 + c3μ3. Each contrast 
# should be tested against a measure of its own variability. 
# This is accomplished by first applying the contrast to the data.
# Let's begin with the simple pairwise comparison of angle0 with angle4
# at the age = 1 level. That is, ψ = 1μ1 - 1μ2 + 0μ3. Then, apply the
# contrast to the data.
psi_vals <- 1 * rmd2$angle0[rmd2$age == 1] - 1 * rmd2$angle4[rmd2$age == 2]
psi_vals
mean(psi_vals)

# The standard error used as the denominator in the t ratio is the 
# square root of the variance of psi_vals/sample size.
(se <- sqrt(var(psi_vals) / 10))

# Calculate the t ratio
(t_ratio <- mean(psi_vals) / se)

# The t_ratio has N - 1 or 10 - 1 = 9 degrees of freedom. The p-value:
(p_val1 <- 2 * pt(q = t_ratio, df = 9, lower.tail = TRUE))

# Now let's do it again for angle0 vs angle4 at age = 1. Then again for 
# angle4 vs angle 8 at age = 1. Then, move to age = 2 and do the same
# procedure. Shaffer's planned post-omnibus works well here for familywise
# Type I error rate control.