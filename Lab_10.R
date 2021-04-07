# The Riesby data are described in Hedeker & Gibbons (p. 52)
# The study focused on the longitudinal relationship between
# imiprimine (IMI) and desipramine (DMI) plasma levels and 
# clinical response in 66 depressed inpatients. IMI is a 
# tricyclic antidepressant that biotransofrms into DMI. In 
# this study, 29 patients were classified as nonendogenous
# and the remaining 37 as endogenous, where nonendogenous
# depression is associated with some tragic life event and
# endogenous depression is not. Following a placebo period
# of 1 week, patients received 225 mg/day doses of IMI for
# four weeks. Subjects were rated with the Hamilton Depression
# Rating Scale (HDRS) at the beginning of the study and 
# again at the end of every week. Plasma levels of IMI and
# DMI were also recorded every week, and the sex and age
# of each patient was recorded, along with their diagnosis
# as either nonendogenous or endogenous.

# Open the .Rdata file containing the Riesby data.
head(riesby)
dim(riesby)
riesby # data are in long format

# install.packages("tidyr")
library(tidyr)
# convert to wide format with spread()
riesby_wide <- spread(data = riesby, key = wk, value = hdrs)
head(riesby_wide)
# change names from 0:5 to wk0:wk5
names(riesby_wide)[3:8] <- paste0("wk", 0:5)
head(riesby_wide)

# see ?gather for going from wide to long

# Means and SDs by time point
round(apply(riesby_wide[,3:8], 2, mean, na.rm = TRUE), 1)
round(apply(riesby_wide[,3:8], 2, sd, na.rm = TRUE), 1)

# correlation matrix across time points
(cor1 <- round(cor(riesby_wide[,3:8], use = "pairwise"), 2))

# var/cov matrix 
(cov1 <- round(cov(riesby_wide[,3:8], use = "pairwise"), 2))

# Fit multivariate regression model to run Mauchly's test
# of sphericity. Note: 20 observations deleted due to missingness!
mlm1 <- lm(cbind(wk0, wk1, wk2, wk3, wk4, wk5) ~ 1, data = riesby_wide, na.action = na.omit)
summary(mlm1)

# Compound symmetry does not appear tenable here (p < .001)
mauchly.test(object = mlm1, X = ~1)

# boxplots at each time point
boxplot(riesby$hdrs ~ riesby$wk,
        xlab = "Week Number",
        ylab = "HDRS")

# plot individual change over time
plot(x = riesby$wk, y = riesby$hdrs, 
     xlab = "Week Number",
     ylab = "HDRS", pch = ".")

for (i in unique(riesby$id)) {
  lines(x = riesby$wk[riesby$id == i], 
        y = riesby$hdrs[riesby$id == i],
        type = "l", lty = 1) }
# Add nonparametric regression curve
points(loess.smooth(x = riesby$wk, y = riesby$hdrs),
       type = "l", lwd = 3, col = 2)

# linear regression modeling (not appropriate
# because independence of errors assumption
# is violated)
lm1 <- lm(hdrs ~ wk, data = riesby)
summary(lm1)

# Random intercept model
library(lme4) # for estimating mixed-effects models
library(lmerTest) # for getting p-values for fixed effects

# Random intercept model with time as the only predictor
lmem1 <- lmer(formula = hdrs ~ wk + (1|id),
              data = riesby, REML = FALSE)
summary(lmem1)

Z1 <- cbind(c(1,1,1,1,1,1))
Sigma_nu1 <- cbind(16.16)
sigma_sq_eps1 <- 19.04
I1 <- diag(6)
(varcov1 <- Z1 %*% Sigma_nu1 %*% t(Z1) + sigma_sq_eps1 * I1)

# Random intercept and slope with time as the only predictor
lmem2 <- lmer(formula = hdrs ~ wk + (wk|id),
              data = riesby, REML = FALSE)
summary(lmem2)

# covariance of RE terms is correlation times sqrt of prod of vars
(cov12 <- -.28*sqrt(12.63*2.08))
(Z2 <- cbind(c(1,1,1,1,1,1), 0:5))
(Sigma_nu2 <- cbind(c(12.63, -1.44), c(-1.44, 2.08)))
sigma_sq_eps2 <- 12.22
(I2 <- diag(6))
(varcov2 <- Z2 %*% Sigma_nu2 %*% t(Z2) + sigma_sq_eps2 * I2)

# Does diagnosis make a difference?
by(data = riesby$hdrs, INDICES = riesby$endog, FUN = mean, na.rm = TRUE)
(mnsbygp <- by(data = riesby$hdrs, INDICES = list(riesby$wk,riesby$endog), FUN = mean, na.rm = TRUE))
plot(x = rep(0:5, times = 2), y = mnsbygp, type = "n",
     xlab = "Week Number", ylab = "HDRS")
points(x = 0:5, y = mnsbygp[1:6], type = "o", lwd = 2)
points(x = 0:5, y = mnsbygp[7:12], type = "o", lwd = 2, lty = 2)
legend(x = "topright", lty = 1:2, lwd = 2, pch = 1, legend = c("Nonendog", "Endog"), seg.len = 4)

lmem3 <- lmer(formula = hdrs ~ wk + endog + wk:endog + (wk|id),
              data = riesby, REML = FALSE)
summary(lmem3)


# separate plots by type of depression (endog vs nonendog)
par(mfrow = c(1,2), mar = c(3,2,2,1))
# Endogenous
plot(x = riesby$wk[riesby$endog == 1], y = riesby$hdrs[riesby$endog == 1], 
     xlab = "Week Number", main = "Endogenous",
     ylab = "HDRS", pch = ".", ylim = c(0, 40))
for (i in unique(riesby$id[riesby$endog == 1])) {
  lines(x = riesby$wk[riesby$endog == 1][riesby$id[riesby$endog == 1] == i], 
        y = riesby$hdrs[riesby$endog == 1][riesby$id[riesby$endog == 1] == i],
        type = "l", lty = 1) }
# Add nonparametric regression curve
points(loess.smooth(x = riesby$wk[riesby$endog == 1], y = riesby$hdrs[riesby$endog == 1]),
       type = "l", lwd = 3, col = 2)

# Nonendogenous
plot(x = riesby$wk[riesby$endog == 0], y = riesby$hdrs[riesby$endog == 0], 
     xlab = "Week Number", main = "Nonendogenous",
     ylab = "HDRS", pch = ".", ylim = c(0, 40))
for (i in unique(riesby$id[riesby$endog == 0])) {
  lines(x = riesby$wk[riesby$endog == 0][riesby$id[riesby$endog == 0] == i], 
        y = riesby$hdrs[riesby$endog == 0][riesby$id[riesby$endog == 0] == i],
        type = "l", lty = 1) }
# Add nonparametric regression curve
points(loess.smooth(x = riesby$wk[riesby$endog == 0], y = riesby$hdrs[riesby$endog == 0]),
       type = "l", lwd = 3, col = 2)

