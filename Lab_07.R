###
# The first part of the lab has to do with ANCOVA.
# The second part has to do with error rate and multiple
# comparisons.
###

# For this lab we will use data from a 
# randomized trial of acupuncture for treating headache.
# Results of the study were published in the BMJ in 2004.
# Look over the paper at
#     http://www.bmj.com/content/328/7442/744.full

# Read in the acupuncture data (called 'acupuncture.csv')
dat <- read.csv(file = file.choose())
head(dat)
dim(dat)
table(dat$group)
str(dat)

# Convert categorical variables to factors
dat$group <- factor(dat$group)
dat$sex <- factor(dat$sex)
dat$migraine <- factor(dat$migraine)

str(dat)

# Variable name and description: 
# Age (yrs)
# sex (0 = male; 1 = female)
# migraine (diagnosis of migraine =1 or tension-type =0)
# chronicity (number of years of headache disorder at baseline)
# acupuncturist (id for acupuncture provider)
# group (0 = comparison; 1 = acupuncture)
# pk1 (headache severity at baseline)
# pk5 (headache severity at 1 year) - outcome variable

# Check for baseline balance across treatment conditions on 
# age, sex, migraine, chronicity, and baseline severity to 
# see if randomization was successfully implemented.

# Calculate covariate means by treatment status
by(data = dat$age, INDICES = dat$group, FUN = mean)
by(data = as.numeric(as.character(dat$sex)), INDICES = dat$group, FUN = mean)
by(data = as.numeric(as.character(dat$migraine)), INDICES = dat$group, FUN = mean)
by(data = dat$chronicity, INDICES = dat$group, FUN = mean)
by(data = dat$pk1, INDICES = dat$group, FUN = mean)

# Convert to standardized mean differences
diff(by(data = dat$age, INDICES = dat$group, FUN = mean)/
     mean(by(data = dat$age, INDICES = dat$group, FUN = sd)))
diff(by(data = as.numeric(as.character(dat$sex)), INDICES = dat$group, FUN = mean)/
       mean(by(data = as.numeric(as.character(dat$sex)), INDICES = dat$group, FUN = sd)))
diff(by(data = as.numeric(as.character(dat$migraine)), INDICES = dat$group, FUN = mean)/
       mean(by(data = as.numeric(as.character(dat$migraine)), INDICES = dat$group, FUN = sd)))
diff(by(data = dat$chronicity, INDICES = dat$group, FUN = mean)/
       mean(by(data = dat$chronicity, INDICES = dat$group, FUN = sd)))
diff(by(data = dat$pk1, INDICES = dat$group, FUN = mean)/
       mean(by(data = dat$pk1, INDICES = dat$group, FUN = sd)))

# Run statistical tests for group differences
t.test(x = dat$age[dat$group == 0], y = dat$age[dat$group == 1])
chisq.test(x = table(dat$sex, dat$group))
chisq.test(x = table(dat$migraine, dat$group))
t.test(x = dat$chronicity[dat$group == 0], y = dat$chronicity[dat$group == 1])
t.test(x = dat$pk1[dat$group == 0], y = dat$pk1[dat$group == 1])

# Set deviation contrasts as the default
options(contrasts = c("contr.sum", "contr.poly"))  

# Run one-way ANOVA to see if group had an effect
lm1 <- lm(pk5 ~ group, data = dat)
library(car)
Anova(lm1, type = 3)

library(emmeans)
emm1 <- emmeans(object = lm1,     
                specs = ~ group,  
                adjust = "none")  
emm1

plot(emm1, 
     horizontal = FALSE,
     ylab = "Severity", 
     xlab = "Group")

pairs(emm1, adjust = "none")

# Consider using baseline level of severity (pk1) as
# a covariate. First check for a strong linear 
# relationship.
cor(dat$pk1, dat$pk5)

# Examine baseline vs one-year severity relationship
# graphically
plot(pk5 ~ pk1,
     data = dat, 
     xlab = "Baseline Severity Rating",
     ylab = "1 Year Severity Rating")

# The ANCOVA model traditionally assumes no interaction
# between treatment and covariate
lm2 <- lm(pk5 ~ group + pk1, data = dat)

# Test that the assumption is reasonable. The assumption
# is called "homogeneity of regression slopes"
lm3 <- lm(pk5 ~ group + pk1 + pk1:group, data = dat)
anova(lm2, lm3)

# In this case the assumption is violated. There is
# evidence that the pk1 slope differs across treatment
# conditions.

# Different colors for treated vs control
plot(pk5 ~ pk1,
     data = dat, 
     xlab = "Baseline Severity Rating (pk1)",
     ylab = "1 Year Severity Rating (pk5)",
     pch = c(19, 0)[as.numeric(as.character(dat$group)) + 1],
     col = as.numeric(as.character(dat$group)) + 1,
     main = "ANCOVA Model with Interaction")
legend(x = "bottomright", 
       lwd = 2, col = 1:2, pch = c(19, 0), lty = 1:2,
       legend = c("Control", "Treatment"), seg.len = 4)

summary(lm3)
abline(a = 1.35, b = .69, col = 1, lwd = 2, lty = 1)
# Treated: pk5 = 1.35 + .96 + (.69 - .13)*pk1
#              = 2.31 + .56*pk1
abline(a = 2.31, b = .56, col = 2, lwd = 2, lty = 2)

# For the purposes of the lab, we will pretend that
# the interaction is not significant so we can proceed
# with traditional ANCOVA (assuming no interaction).
# In your work, if you find evidence of a significant
# interaction between treatment and a covariate, it 
# means the efficacy of the treatment varies based
# on the value of the covariate. In that case, you 
# would want to make marginal mean comparisons at a
# variety of covariate values to describe how the 
# relationship varies.

# Here, we move forward assuming no interaction (i.e.,
# a constant treatment effect).
# Here, we move forward assuming no interaction (i.e.,
# a constant treatment effect).
plot(pk5 ~ pk1,
     data = dat, 
     xlab = "Baseline Severity Rating (pk1)",
     ylab = "1 Year Severity Rating (pk5)",
     pch = c(19, 0)[as.numeric(as.character(dat$group)) + 1],
     col = as.numeric(as.character(dat$group)) + 1,
     main = "ANCOVA Model Assuming No Interaction")
legend(x = "bottomright", 
       lwd = 2, col = 1:2, pch = c(19, 0), lty = 1:2,
       legend = c("Control", "Treatment"), seg.len = 4)

# Two models (by group) based on the ANCOVA model
summary(lm2)
# Control: pk5 = 1.16 + .71*pk1
abline(a = 1.16, b = .71, col = 1, lwd = 2)
# Treated: pk5 = 1.16 - 2.29 + .71*pk1 = -1.13 + .71*pk1
abline(a = -1.13, b = .71, col = 2, lwd = 2, lty = 2)

# ANCOVA overall results
Anova(lm2, type = 3)

# Compare with ANOVA
Anova(lm1, type = 3)

# Pairwise comparisons with ANCOVA are based on 
# "adjusted means," which are predicted values
# for treated and control cases at the mean
# value of the covariate.

mean(dat$pk1)

# The reference grid consists of combinations of 
# independent variables over which predictions are made. 
emm2 <- emmeans(object = lm2,
                specs = ~ group,
                ref_grid(object = lm2, 
                         at = list(pk1 = mean(dat$pk1))),
                adjust = "none")
emm2

pairs(emm2, adjust = "none")


###
# The second part of the lab has to do with 
# multiple comparisons.
###

# An example with three groups.
# The research report is here:
# https://pdf.sciencedirectassets.com/280666/1-s2.0-S2211335514X00033/1-s2.0-S2211335515001072/main.pdf?x-amz-security-token=AgoJb3JpZ2luX2VjEBQaCXVzLWVhc3QtMSJIMEYCIQDACFleEdZrJX1vWYEl89RNoJdrSZTsGKQCOWBGkOduJAIhAKGM6Ez5JyrbbKQ22YpElNk8449YE1b9Ilr5phj%2FtWYSKuMDCM3%2F%2F%2F%2F%2F%2F%2F%2F%2F%2FwEQAhoMMDU5MDAzNTQ2ODY1Igycjv1xHPFzOxQcUgYqtwPKl4aQAtDzQDWR8aurOAkj5ty4kceIhYuQxmGzPZ2DTKAjKz3oWSQUtMIq2pdBQZgk%2BxtugLP0thlQbsOI90qttW3ebzRCWdEq7wd%2BYnpZZ0yCUjHwfss%2B9XbAm0dEg6vVJA5bIu6KJP7KHWD3O%2BLvpE2jvjzk7oS5JP3MvpoFTt%2BrbDLWDqaOulkFRJT1WPoWfbrCfRx5rmRh26hKiryIP2012tYV%2BXZWRdvZVqewqF5Ix8JyFn7yVTTIefMXkBiSF85M735SkZI1Luc5PPmMjnWrdek3FteX4s7fJvl%2FvhV0agHa%2BOkzKRm07lPcs%2FgY4W2bmeTKBDBAnI49Pf3CuRTl3w8uYSk9IAPPm0OuynFQs3503TTqCBx%2BCZ1Y2wMNFCghdcqjDKc4l75FrlKX%2F%2FGFpP4VfZmKqI%2Bo6U8v24VCw%2BI%2FF23%2Bc9wbTNcjhKWwMo9A9gfbw6Ud10SFgkqrm3uVhx%2FSUpWRmnkv8jBrtDMPVC1YmebJfB4OG4gB%2FNPyF2HfIQGZZqK4qMpzynwdK6vOw6lE89ctEhatF2ZOJxf4i9QlfRjBfR%2BcXYYn3rDboBXQCq5zMJHxleUFOrMBL88FYwb%2BEspbnqSDFOr2ZzAlBV4hms0dKY720EKGfCEEUCImSvzSYwBqQXlYNJAS05RzECToVZF%2BNRMBdkCSdM0BBp1Tjw1ULZh%2B0HRpZGPTuy4hK7fElKmGvcPoGXas6ZPFpDvJdex1SE7idwovKt9esRd9I%2B%2BSa8rlx9ahxzgyYKxPMwq6A61C5gebY0KYxxeeiSeEBYIkomJtUw2KNCz7XNtMb6NqvEP3wpCajAPakns%3D&AWSAccessKeyId=ASIAQ3PHCVTYRTSRUCGG&Expires=1554350406&Signature=6eATyPNJ%2FYXAQXtEUyUx2cgzF5s%3D&hash=bd81859a4b34accb124cd39209b81431318af637ee106fc27f3405b60f05f383&host=68042c943591013ac2b2430a89b270f6af2c76d8dfd086a07176afe7c76c2c61&pii=S2211335515001072&tid=spdf-31060cde-79fb-4993-959d-85d193871a16&sid=27e6db9a85f7244fc35b80e-cd4c8eac82bdgxrqa&type=client
# Look over the pdf, paying particular attention to the methods section. 

# The data set is described and made available here:
# https://www.sciencedirect.com/science/article/pii/S2352340915002437?via%3Dihub

dat2 <- read.csv(file = file.choose())
head(dat2)
str(dat2)

# Convert condition to a factor
dat2$condition <- factor(dat2$condition)

# Visually examine data.
boxplot(enrolled_count ~ condition, data = dat2)
by(data = dat2$enrolled_count, INDICES = dat2$condition, FUN = mean)

# Fit the full model and examine residuals.
lm1F <- lm(enrolled_count ~ condition, data = dat2)
library(car)
residualPlot(lm1F) # linearity looks ok
qqPlot(lm1F) # Ack! This looks bad!

# Histogram of data
hist(dat2$enrolled_count)

# Data are heavily skewed right. Why? Because they are 
# frequency counts. One way to model these data would be
# with a method that handles count outcome data, but since
# we haven't covered that yet, let's explore other options.

# log transformation is one option.
hist(log(dat2$enrolled_count + 1))
hist(x = log(dat2$enrolled_count[dat2$condition == 1] + 1, 25))
hist(x = log(dat2$enrolled_count[dat2$condition == 2] + 1, 25))
hist(x = log(dat2$enrolled_count[dat2$condition == 3] + 1, 25))

lm1F_log <- lm(log(enrolled_count + 1) ~ condition, data = dat2)
residualPlot(lm1F_log) # linearity still looks ok
qqPlot(lm1F_log) # Much better, though still a little misfit for lowest vals

# Proceed with ANOVA using log transformed outcome
Anova(lm1F_log, type = 3) # Condition is not significant (p = .079).

# Another approach would be to use nonparametric methods that do 
# not assume normality. The Kruskal-Wallis test is a non-parametric
# analogue to one-way ANOVA that assumes identical distributions
# (not necessarily normal) that are possibly shifted across groups.

# Let's look at histograms of the data within each group.
hist(x = dat2$enrolled_count[dat2$condition == 1], 25)
hist(x = dat2$enrolled_count[dat2$condition == 2], 25)
hist(x = dat2$enrolled_count[dat2$condition == 3], 25)

by(data = dat2$enrolled_count, INDICES = dat2$condition, FUN = var)

# Note that the assumption of identical distribution shape and
# variance are tenable based on histograms and sample variances.

# Run the Kruskal-Wallis test. Under these assumptions (of 
# identical distribution shape and variance across groups) the
# null hypothesis of the K-W test is that the MEDIANS (as opposed
# to the means) of each group are identical.

# Run the test.
?kruskal.test
kruskal.test(enrolled_count ~ condition, data = dat2)

# The group effect is significant (p = .043). Follow up with pairwise
# comparisons. To follow up a significant K-W omnibus test, we use the
# Wilcoxon-Mann-Whitney (WMW) non-parametric test, which is the paired 
# version of the K-W test. That is, WMW is to K-W as t-test is to ANOVA.

# Test group 1 (social) vs group 2 (control)
?wilcox.test
wilcox.test(x = dat2$enrolled_count[dat2$condition == 1], 
            y = dat2$enrolled_count[dat2$condition == 2], 
            alternative = "two.sided")
# p-val = .0176

# Test group 3 (media) vs group 2 (control)
wilcox.test(x = dat2$enrolled_count[dat2$condition == 3], 
            y = dat2$enrolled_count[dat2$condition == 2], 
            alternative = "two.sided")
# p-val = .0826

# Test group 3 (media) vs group 1 (social)
wilcox.test(x = dat2$enrolled_count[dat2$condition == 3], 
            y = dat2$enrolled_count[dat2$condition == 1], 
            alternative = "two.sided")
# p-val = .4035

# Type I error rate adjustment. I will illustrate several methods here, but
# note that you should pick your method BEFORE running analyses in practice. 
# Then, once you are ready to analyze the data, only run the single correction
# method that you selected.
# 1. Bonferroni: test each at .05/3.
#    Conclusion: Fail to reject all.
# 2. Holm-Bonferroni: rank p-vals smallest to largest; test first at .05/3.
#    Conclusion: Fail to reject. Stop.
# 3. Tukey is not applicable here because it assumes normal distribution.
# 4. Shaffer: test omnibus at .05. Reject (p = .043); rank p-vals and test 
#    remaining at .05.
#    Conclusion: Reject the social vs control comparison (p = .0176).


# Load the prestige data from the carData package
library(carData)
Prestige$type <- factor(Prestige$type, levels = c("prof", "wc", "bc"))
Prestige$incomeF <- factor(c("low", "high")[(Prestige$income > median(Prestige$income)) + 1])
Prestige$incomeF <- factor(Prestige$incomeF, levels = c("low", "high"))
interaction.plot(x.factor = Prestige$type,
                 trace.factor = Prestige$incomeF,
                 response = Prestige$prestige,
                 type = "o",
                 xlab = "Type of Profession",
                 ylab = "Prestige",
                 col = c("black", "green"),
                 pch = c(16,17),
                 legend = FALSE)
legend(x = "topright", lty = 1, pch = 16:17,
       col = c("black", "green"), 
       legend = c("Low Income", "High Income"))

lm2 <- lm(prestige ~ type + incomeF + type:incomeF, data = Prestige)
summary(lm2)

library(car)
Anova(lm2, type = "III")

# Two-way interaction is significant, so must
# look at simple effects.
library(emmeans)
joint_tests(object = lm2, by = "incomeF")

# Both simple omnibus tests are significant, so follow both up.

emm1 <- emmeans(object = lm2,
                specs = ~ type | incomeF,
                adjust = "none")
plot(emm1, 
     horizontal = FALSE,
     ylab = "Prestige", 
     xlab = "Type")
pairs(emm1, adjust = "none")
pairs(emm1, adjust = "bonferroni")
pairs(emm1, adjust = "holm")
pairs(emm1, adjust = "scheffe")
pairs(emm1, adjust = "tukey")
pairs(emm1, adjust = "fdr")
