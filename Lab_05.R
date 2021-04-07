# Get the prestige data set from Canvas and load it.
head(prestige)

# The variable incomeF is a categorical factor with two 
#  levels: low and high.

# Check the order of levels.
levels(prestige$type) # reorder to bc wc prof
levels(prestige$incomeF) # looks good (low)

# Reorder the levels
prestige$type <- factor(x = prestige$type, 
                        levels = c("bc", "wc", "prof"))
levels(prestige$type) # looks good

# Cross tabulation of job type and income category
table(prestige$incomeF, prestige$type)

# Load the emmeans package and the car package
library(emmeans)
library(car)

# Data visualization is almost always a good first step.
?emmip

# Need to fit a linear model to pass to the emmip() function.
# Dummy coding
options(contrasts = c("contr.treatment", "contr.poly"))
lm1 <- lm(prestige ~ type + incomeF + type:incomeF, data = prestige)
summary(lm1)

# Deviation coding (note: use "contr.sum", not "contr.helmert")
options(contrasts = c("contr.sum", "contr.poly"))
lm2 <- lm(prestige ~ type + incomeF + type:incomeF, data = prestige)
summary(lm2)

# Relate the estimated coefficients from lm1 to lm2. For example,
# can you see that the predicted value (estimated marginal mean)
# for low income white collar professions

# The emmip() function creates an interaction plot for 
#  estimated marginal means. The first argument is an lm
#  or emmeans object; here we will use lm2. The second
#  argument is a formula of the form
#   trace.factors ~ x.factors | by.factors
#  Since we want type on the horizontal axis, it will be
#  the x factor. That leaves incomeF to be the trace factor.
#  We only have two factors, so we don't need to specify
#  by.factors
emmip(lm2, formula = incomeF ~ type)
emmip(lm2, formula=type~incomeF)
# The plot appears to suggest an interaction such
# that the income associated with a profession has a 
# larger impact on prestige for blue collar jobs than for 
# white collar or professional work. Does it make sense?

# When working with two-way ANOVA designs, the first
# step in the analysis is to test the interaction.
# If the interaction is significant, then we follow
# up by testing simple main effects instead of overall
# (averaged) main effects.
Anova(lm2, type = 3)

# Create an emmeans object for the two way ANOVA.
emm1 <- emmeans(object = lm2,
                specs = ~ type | incomeF,
                adjust = "none")
emm1

# Remember that simple effects look at the effects of a 
# factor conditional on the levels of the other factor.  
# There are always two ways to construct simple effects 
# in a two-way design. Here, we could test for an effect
# of income at the three levels of job type:
#  income effect at bc?
#  income effect at wc?
#  income effect at prof?

# or, we could test for an effect of job type at the two
# levels of income:
#  job type effect at income = low?
#  job type effect at income = high?

# Substantively, which set of simple effects is more 
# interesting? Would you prefer to look at the effect
# of income on prestige at each job type or the effect 
# of job type on prestige at both levels of income?

###
# Suppose you prefer to look at the effect of job type on  
# prestige at each level of income:
joint_tests(emm1, by = "incomeF")

# The first test (F = 43.317) tests the null hypothesis
#  H0: mu_{bc at low} = mu_{wc at low} = mu_{prof at low}

# The second test (F = 47.783) tests the null hypothesis
#  H0: mu_{bc at high} = mu_{wc at high} = mu_{prof at high}

# Both tests are significant, so we will follow up with
# simple pairwise tests for each.

pairs(emm1, simple = "type")
coef(pairs(emm1, simple = "type"))

# The first contrast here tests for a difference between 
# bc and wc at the low level of income:
#  H0: mu_{bc at low} = mu_{wc at low}

# The only null hypothesis here that is not rejected is for
#  H0: mu_{bc at high} = mu_{wc at high}

# Thus we do not find evidence of a statistially significant
# mean difference in prestige for blue collar and white 
# collar jobs that are associated with high incomes. All other
# simple pairwise comparison tests find significant differences.

# Interpretation of results: the prestige gaps between blue
# collar and white collar jobs and also between white collar
# and professional-type jobs are significant and real with
# one exception: higher-paying blue collar do not differ in
# average prestige from higher-paying white collar jobs.

###
# Suppose instead that you prefer to look at the effect of 
# income on prestige at each level of job type:
joint_tests(emm1, by = "type")

# The first test (F = 33.561) tests the omnibus null hypothesis
#  H0: mu_{low at bc} = mu_{high at bc}

# Note that only this first test is significant, so we will only
# follow up on the first test. Because the income factor only has
# two levels, there is no follow-up needed; the pairwise test is 
# the same as the omnibus test because there are two groups. 
# Nevertheless, here, for the sake of completeness, is the output:
pairs(emm1, simple = "incomeF")
coef(pairs(emm1, simple = "incomeF"))
# added a mean function in class: mean(prestige$prestige[which(presitige$income)])

# Thus, we find only find evidence of an income effect on prestige
# for the blue collar professions. 

# Interpretation of results: It seems as though making a lot of
# money in a blue collar profession can provide a big boost to 
# prestige; whereas, for white collar and professional jobs, the
# prestige associated with the work is not dependent on the salary.


###
# What happens if you analyze a two-way ANOVA design that does
# not have a significant interaction?

# Download and then open 05eclsk.Rdata. I added a categorical
# factor to the data called WKSESL_factor. This variable takes on
# values low, middle, and high for students in the lowest, middle, 
# and highest thirds of the continuous SES variable WKSESL.

head(eclsk)
str(eclsk)
options(contrasts = c("contr.sum", "contr.poly"))
lm3 <- lm(C6R4MSCL ~ F5SPECS + WKSESL_factor + F5SPECS:WKSESL_factor, 
          data = eclsk)
summary(lm3)

# Plot the group means.
emmip(object = lm3, 
      formula = F5SPECS ~ WKSESL_factor,
      xlab = "Socioeconoic Status Level")

# Run the two-way ANOVA
Anova(lm3, type = 3)

# Note that the interaction is not significant and appears to be
# inconsequential from the plot. Thus, it is safe to interpet 
# average main effects.

# The test of the overall main effect of special education (F5SPECS),
# averaging over the levels of the SES factor is given in the ANOVA
# output:
Anova(lm3, type = 3)

# Because the main effect of F5SPECS is significant, we will follow 
# up. But note that F5SPECS only has two levels, so the follow up
# main effect pairwise contrast is identical to the omniubs main
# effect test. Nevertheless, we might consider running the pairwise
# test anyhow just to get estimated means for the two groups, averaging
# over the levels of SES.
emm2 <- emmeans(object = lm3,
                specs = ~ WKSESL_factor | F5SPECS)
emm2 

# Don't want cell means because there is no interaction. Instead,
# want to average over the levels of the other factor.

emm3 <- emmeans(object = lm3, 
                specs = ~ F5SPECS,
                adjust = "none")

# The emmeans package valiantly warns us that we may be making
# a mistake by averaging over the levels of the SES factor. We
# have done this intentionally here because of lack of evidence 
# for a two-way interaction. Thus, we proceed and ignore the warning.

emm3
pairs(emm3, adjust = "none")

  # Interpretation: We find no evidence for a two-way interaction 
# between SES and special education factors. The main effect of special 
# education, averaged over the levels of SES, is estimated to be 15.7
# points and is significant (t = 10.30; p < .0001).

# Now let's follow-up on the main effect for SES.

emm4 <- emmeans(object = lm3, 
                specs = ~ WKSESL_factor)
emm4
pairs(emm4, adjust = "none")
coef(pairs(pairs(emm4, adjust = "none")))
# Interpretation: The main effect of SES is significant. Follow-up
# pairwise comparisons, averaged over the levels of special education,
# reveal that the mean 5th grade math scores differ for all three
# SES groups.


### Lab assignment
# Examine the impact of S2KPUPRI (public school = 1; private school = 0) 
# and SES on 5th grade math score. Create and report an interaction plot. 
# Follow up the plot by running a two-way ANOVA. Interpret results. Then 
# follow up the ANOVA with either simple tests or main effects tests as 
# appropriate and, finally, pairwise comparison. Interpret the results 
# in context.
#1. create interaction plot and linear regression
options(contrasts = c("contr.treatment", "contr.poly"))
lm1 <- lm(C6R4MSCL ~ S2KPUPRI + WKSESL_factor + S2KPUPRI:WKSESL_factor, 
          data = eclsk)
summary(lm1)
#plot the group mean 
emmip(object = lm1, 
      formula = S2KPUPRI ~ WKSESL_factor,
      xlab = "Socioeconoic Status Level")
emmip(object = lm1, 
      formula = WKSESL_factor ~S2KPUPRI ,
      xlab = "Socioeconoic Status Level")

Anova(lm1, type = 3)

emm1 <- emmeans(object = lm1,
                specs = ~S2KPUPRI | WKSESL_factor,
                adjust = "none")
emm1

emm2 <- emmeans(object = lm1,
                specs = ~ WKSESL_factor| S2KPUPRI,
                adjust = "none")
emm2

joint_tests(emm1, by = "WKSESL_factor")

joint_tests(emm1, by = "S2KPUPRI")

pairs(emm2, simple = "WKSESL_factor")
coef(pairs(emm1, simple = "S2KPUPRI"))
















