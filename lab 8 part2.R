# Logistic and Multinomial Logistic Regression
###############
# Data for the logistic and multinomial logistic analyses
# come from the BEPS that we looked at in class.
#install.packages("EffectStars")
#library(EffectStars)
#data(BEPS) # load the data
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
#BEPS$Vote <- relevel(BEPS$Vote, ref = "Liberal Democrat")
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
(SEs <- summary(mn1)$standard.errors) #all the coefficients
(walds <- coeffs/SEs)
colMeans(BEPS, na.rm = FALSE, dims = 1)
mean(BEPS, na.rm = TRUE)

(pvals <- 2*pnorm(abs(walds), lower.tail = FALSE))
round(pvals, 2)
exp(coeffs)
round(exp(coeffs),2)
# Lab 09 Task 2:
# Write out the multinomial model in both logit and odds forms 
# based on the estimated coefficients from the mn1 fit. Then, 
# describe the relationship between Euroskepticism and voting
# when knowledge = 0 and again when knowledge = 2.