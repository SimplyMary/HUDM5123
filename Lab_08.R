############### 
# Count Regression
###############
# Data for the count data part of this lab are related to an
# online drug abuse prevention program for teen girls. This 
# was a randomized experiment. Read the abstract here:
# https://link.springer.com/article/10.1007%2Fs10964-017-0714-4

# Load relevant packages. Install them first if needed.
install.packages("AER")
install.packages("lmtest")
library(car) # for qqPlot()
library(MASS) # for negative binomial model fitting
library(pscl) # for zero-inflated model fitting
library(lmtest) # for Likelihood ratio tests of glm models
library(AER) # for overdispersion test

# Load the data called Lab_09.Rdata
dim(dat)
head(dat)
names(dat)
str(dat) #1st level, corresponding levels
#dat$STUDY_ARM<- factor(dat$STUDY_ARM)

# How many control and treated cases?
table(dat$STUDY_ARM)

# Distribution of ages at enrollment.
table(dat$AGE_YRS.1)

# Parent have college degree?
table(dat$EDUC2.1)

# Child's grades in school.
table(dat$GRADES.1)

# Pretest cigarette usage. Lots of zeros!
hist(dat$THIRTYDAYCIG.1, breaks = 100)
table(dat$THIRTYDAYCIG.1)

# Control group was using a bit more at pretest
table(dat$STUDY_ARM, dat$THIRTYDAYCIG.1)
by(dat$THIRTYDAYCIG.1, dat$STUDY_ARM, mean, na.rm = TRUE)

# delayed-Posttest cigarette usage
table(dat$THIRTYDAYCIG.3)
table(dat$STUDY_ARM, dat$THIRTYDAYCIG.3)
by(dat$THIRTYDAYCIG.3, dat$STUDY_ARM, mean, na.rm = TRUE)
# Control group was using a lot more at posttest. But significant?

# Examine missing data patterns
install.packages("mice")
library(mice)
?md.pattern
####could not found md.pattern 
md.pattern(x = dat)
# Reading the left margin:
#  680 cases with no missingness
#  42 cases missing only EDUC2.1
#  15 cases missing only THIRTYDAYCIG.3, etc.
# Reading the bottom margin:
#  47 total cases missing EDUC2.1
#  25 total cases missing THIRTYDAYCIG.3, etc.

# Check balance at baseline (i.e., was randomization successful?)
# Use chi-square test of independence for categorical 
# baseline variables (and Fisher's exact test if 
# expected counts are too low).
chisq.test(table(dat$STUDY_ARM, dat$AGE_YRS.1))
fisher.test(table(dat$STUDY_ARM, dat$AGE_YRS.1))

chisq.test(table(dat$STUDY_ARM, dat$EDUC2.1))

chisq.test(table(dat$STUDY_ARM, dat$GRADES.1))
fisher.test(table(dat$STUDY_ARM, dat$GRADES.1))

# Use t-test (Welch's version, not assuming equal
# group variances) for quantitative baseline variables.
t.test(x = dat$THIRTYDAYCIG.1[dat$STUDY_ARM == 0],
       y = dat$THIRTYDAYCIG.1[dat$STUDY_ARM == 1],
       alternative = "two.sided",
       var.equal = FALSE)

# No significant differences found at baseline.
# Conclude randomization was successful in balancing
# groups.
#################################################
# Estimate the treatment effect with ANOVA
lm1 <- lm(THIRTYDAYCIG.3 ~ STUDY_ARM, 
          data = dat)
options(contrasts = c("contr.sum", "contr.poly"))
Anova(lm1, type = 3)
summary(lm1)
qqPlot(lm1) # Really bad!

# Is baseline cigarette use linearly related to posttest use?
cor(dat$THIRTYDAYCIG.1, dat$THIRTYDAYCIG.3, use = "complete.obs")

# Estimate the treatment effect with ANCOVA,
# controlling for pretest
lm2 <- lm(THIRTYDAYCIG.3 ~ STUDY_ARM + THIRTYDAYCIG.1, data = dat)
Anova(lm2, type = 3)
summary(lm2)
qqPlot(lm2) # Really bad!
residualPlot(lm2) # Also bad!

# Now turn to count models. Recall that the Poisson regression
# model requires conditional mean and variance to be the same.
mean(dat$THIRTYDAYCIG.3, na.rm = TRUE)
var(dat$THIRTYDAYCIG.3, na.rm = TRUE) # Yikes, big difference.
plot(dat$STUDY_ARM,log(dat$THIRTYDAYCIG.2))
# Poisson model with no covariates.

glm1 <- glm(THIRTYDAYCIG.3 ~ STUDY_ARM, 
            family = "poisson", 
            na.action = na.omit, data = dat)
summary(glm1)

# Poisson model controlling for baseline cig usage.
glm2 <- glm(THIRTYDAYCIG.3 ~ STUDY_ARM + THIRTYDAYCIG.1, 
            family = "poisson", 
            na.action = na.omit, data = dat)
summary(glm2)

exp()
# Test for overdispersion. Null hypothesis is that Poisson 
# assumption of equal mean and variance is true.
dispersiontest(glm1)
dispersiontest(glm2)
# Null hypothesis is rejected (p = .0023). So we should use
# negative binomial.

# Negative binomial model with no covariates.
nb1 <- glm.nb(THIRTYDAYCIG.3 ~ STUDY_ARM,  
               na.action = na.omit, data = dat)
summary(nb1)

# Does the negative binomial model fit significantly better
# as measured by likelihood-ratio test? NOTE: this test will
# not run unless you delete missing data from the start as follows:
dat <- na.omit(dat)
library(lmtest)
lrtest(glm1, nb1)
lrtest(glm(THIRTYDAYCIG.2~STUDY_ARM, family = "poisson",data = na.omit,dat)),
       glm.nb()#accroding to what is uploaded in the Canvas

# Significant p-val means that the neg bin model fits significantly

# better than the Poisson model.

# Negative binomial model with pretest
nb2 <- glm.nb(THIRTYDAYCIG.3 ~ STUDY_ARM + THIRTYDAYCIG.1,  
               control = glm.control(maxit = 1000),
               na.action = na.omit, data = dat)
summary(nb2)

# Zero-inflated negative binomial #to see if the zero in the data have some kind of pattern
zinb2 <- zeroinfl(THIRTYDAYCIG.3 ~ STUDY_ARM + THIRTYDAYCIG.1 | 
                   EDUC2.1 + GRADES.1, 
                  dist = "negbin", na.action = na.omit, data = dat)
#(the formula above is fitting two models, the model on the fisrt line is a possion model
#the second line is a 0/1 logistic model)
#for the assignment we just need to report that there is zero inflation, but don't need to go
#to details
summary(zinb2)

### Define aic & bic functions for use with ZINB models
bic <- function(fit){
  pars <- fit$df.null - fit$df.residual + 2
  n <- fit$n
  lgLik <- logLik(fit)[1]
  out <- -2 * lgLik + log(n) * pars
  return (out)
}

aic <- function(fit){
  pars <- fit$df.null - fit$df.residual + 2
  n <- fit$n
  lgLik <- logLik(fit)[1]
  out <- -2 * lgLik + 2 * pars
  return (out)
}

c(AIC(lm2), AIC(glm2), AIC(nb2), aic(zinb2))
c(BIC(lm2), BIC(glm2), BIC(nb2), bic(zinb2))
# According to AIC and BIC, zero-inflated model is preferred.
#how good your model is fitting your data, and the lower the score,
#the better the model is fitiing your data
#bic usually gives your bigger model 


summary(zinb2)
# Following the zero-inflated baseline-adjusted model,
# note that the intervention was effective in reducing
# predicted cigarette usage (p = .01). In particular, 
# exp(-.80745) = 0.45. Thus, being in the treatment group
# was associated with a 55% decrease in smoking, on average,
# at posttest.

# Lab 09 Task 1:
# Do your own analysis for THIRTYDAYCIG.3 and 
# summarize your results.




############### 
# Logistic and Multinomial Logistic Regression
###############
# Data for the logistic and multinomial logistic analyses
# come from the BEPS that we looked at in class.
install.packages("EffectStars")
library(EffectStars)
data(BEPS) # load the data
?BEPS # Data description 
head(BEPS)
str(BEPS)

# Vote is a three-category variable we will use as
# the outcome variable. The three parties are Conservative,
# Labour, and Liberal Democrat.

table(BEPS$Vote, useNA = "ifany")

# We will begin with logistic regression so we need 
# a dichotomous outcome variable. To get one, we will
# drop the Labour category from the Vote variable by
# making a copy of the vote variable called vote_d and
# changing all the "Labour" observations to missing.

BEPS$Vote_d <- BEPS$Vote
BEPS$Vote_d[which(BEPS$Vote_d == "Labour")] <- NA
table(BEPS$Vote_d, useNA = "ifany")
str(BEPS)

# Run factor with Vote_d to drop the Labour level
BEPS$Vote_d <- factor(BEPS$Vote_d)
str(BEPS)
levels(BEPS$Vote_d) # Conservative is the reference category

# The missing Labour values will be dropped from
# any analyses that use Vote_d

# Logistic regression of Vote_d on Europe, 3 leader vars, Age, 
# Gender, Political Knowledge, National Economy, and
# Household. The research question centers on the role of
# Euroskepticism in predicting the outcome, controlling for the
# other variables in the model, and whether the Europe variable 
# interacts with political knowledge.

# Logistic regression uses the glm function, which stands 
# for "generalized linear model" and, since the outcome 
# is dichotomous and we wish to fit a logistic model,
# we must specify family = "binomial".
glm1 <- glm(formula = Vote_d ~ Europe + Leader_Cons + Leader_Labour + 
              Leader_Liberals + Age + Gender + Political_Knowledge + 
              National_Economy + Household + Europe:Political_Knowledge,
            data = BEPS, family = "binomial")
summary(glm1)

# Coefficients:
coef(glm1)

# Exponentiated (and rounded) coefficients:
round(exp(coef(glm1)), 2)

# The coefficient on Europe:Political_Knowledge is significant,
# so there is evidence of an interaction. Since both variables
# involved in the interaction are continuous, one way to follow 
# up is by conditioning on the values of one variable to see how
# the impact of the other variable changes.

table(BEPS$Political_Knowledge)

# The model for the odds is as follows:
# Pr(Y = Dem | Covs)/[1 - Pr(Y = Dem | Covs)] = .49 * 3.17^PK * 1.04^E * .84^(PK*E) * other stuff...

# When PK = 0,
# Pr(Y = Dem | Covs)/[1 - Pr(Y = Dem | Covs)] = .49 * 3.17^0 * 1.04^E * .84^(0*E) * other stuff...
# Pr(Y = Dem | Covs)/[1 - Pr(Y = Dem | Covs)] = .49 * 1.04^E * other stuff...
# A one-unit incrase in Euroskepticism leads to a 4% increase in odds of
# identifying as a Liberal Democrat relative to identifying as a Conservative

# When PK = 2,
# Pr(Y = Dem | Covs)/[1 - Pr(Y = Dem | Covs)] = .49 * 3.17^2 * 1.04^E * .84^(2*E) * other stuff...
# Pr(Y = Dem | Covs)/[1 - Pr(Y = Dem | Covs)] = .49 * 10.05 * 1.04^E * .71^(2*E) * other stuff...
# Pr(Y = Dem | Covs)/[1 - Pr(Y = Dem | Covs)] = .49 * 10.05 * 1.04^E * .71^*E * other stuff...
# Pr(Y = Dem | Covs)/[1 - Pr(Y = Dem | Covs)] = .49 * 10.05 * (1.04 * .71)^E * other stuff...
# Pr(Y = Dem | Covs)/[1 - Pr(Y = Dem | Covs)] = .4.92 * (.74)^E * other stuff...
# A one-unit incrase in Euroskepticism leads to a 26% decrease in odds of
# identifying as a Liberal Democrat relative to identifying as a Conservative

# For multinomial logistic regression we will use package nnet.
install.packages("nnet")
library(nnet)

# Now we go back to the original three-category Vote variable.
# The function for fitting multinomial logistic regression is called 
# multinom.
?multinom

levels(BEPS$Vote)
# Note that conservative is the reference category. Could change
# reference category as follows:
# BEPS$Vote <- relevel(BEPS$Vote, ref = "Liberal Democrat")
# But will leave it as conservative here.

mn1 <- multinom(formula = Vote ~ Europe + Leader_Cons + Leader_Labour + 
                  Leader_Liberals + Age + Gender + Political_Knowledge + 
                  National_Economy + Household + Europe:Political_Knowledge,
                data = BEPS)
summary(mn1)

# Wald tests are used to test coefficients for significance
# based on the standard normal distribution. The Wald test
# statistic is the estimate divided by its SE.
(coeffs <- summary(mn1)$coefficients)
(SEs <- summary(mn1)$standard.errors)
(walds <- coeffs/SEs)
(pvals <- 2*pnorm(abs(walds), lower.tail = FALSE))
round(pvals, 2)

# Lab 09 Task 2:
# Write out the multinomial model in both logit and odds forms 
# based on the estimated coefficients from the mn1 fit. Then, 
# describe the relationship between Euroskepticism and voting
# when knowledge = 0 and again when knowledge = 2.
