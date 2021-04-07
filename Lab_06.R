
# Lab 06
# Load the cogData

# Set contrast to deviation coding via "contr.sum"
options(contrasts = c("contr.sum", "contr.poly"))

# Fit the interaction model so we can graph it
lm1 <- lm(mem ~ task + disease + task:disease, data = cogData)

# Load the emmeans package
library(emmeans)

# Create an interaction plot.
emmip(object = lm1, 
      formula = disease ~ task,
      xlab = "Task",
      ylab = "Estimated Marginal Means")

# Create a custom interaction plot with different line and
# point styles denoting different disease groups.
interaction.plot(x.factor = cogData$task,
                 trace.factor = cogData$dis,
                 response = cogData$mem,
                 xlab = "Task",
                 ylab = "Estimated Marginal Means",
                 col = 1:3, lwd = 3, type = "o",
                 pch = 1:3, legend = FALSE, 
                 lty = 1:3, cex = 1.5)
legend(x = "bottomright",
       legend = c("Amnesic", "Control", "Huntington's"),
       col = 1:3, lwd = 3, pch = 1:3, 
       seg.len = 4, lty = 1:3, cex = 1)

# Check assumptions for two-way ANOVA

# Linearity
library(car)
residualPlot(lm1)

# Normality
qqPlot(lm1)

# Constant variance
# Widen the bottom margin in the plotting window to account for 
# long names
op <- par(mar = c(11, 4, 2, 2) + 0.1)
boxplot(mem ~ disease:task, data = cogData, las = 2 )
op <- par(mar = c(5, 4, 2, 2) + 0.1)

# Estimate group variances & examine ratio
by(data = cogData$mem, 
   INDICES = cogData$disease:cogData$task,
   FUN = var)

# bartlett.test(mem ~ factor(I(task:disease)), data = cogData)

# Now run two-way ANOVA and examine the interaction.
library(car)
Anova(lm1, type = 3)

# We find evidence of a significant interaction. Plot
# also suggests crossing type interaction. Follow up the 
# sig interaction with simple omnibus tests.

# Create an emmeans object with task broken out by level 
# of disease
emm1 <- emmeans(object = lm1,
                specs = ~ task | disease,
                adjust = "none")
emm1

# Run the three simple omnibus tests by conditioning on 
# disease.
joint_tests(emm1, by = "disease")

# These simple omnibus tests test for a main effect of 
# task at the three levels of disease. We find a significant 
# omnibus test for Huntington's. Follow up with simple pairwise 
# comparisons.

p1 <- pairs(emm1, adjust = "none")
p1

# Examine the coefficients to see which contrasts were used to 
# test pairwise differences.
coef(p1)

# Next, use emmeans to fit custom contrasts. To do so, have
# to run the emmeans function with the interaction instead of 
# the `pipe` character (|).
emm2 <- emmeans(object = lm1,
                specs = ~ task * disease,
                adjust = "none")
emm2

# Create the contrasts you are interested in and test them.
psis <- contrast(emm2, 
                 list(psi1 = c(.5, .5, -1, 0, 0, 0, -.5, -.5, 1),
                      psi2 = c(1, -1, 0, 0, 0, 0, 0, 0, 0)))
psis
