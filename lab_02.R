# Data transformations (Fox ch. 4)

### Start by loading the eclsk data set.
hist(eclsk$C6R4MSCL)
hist(eclsk$C6R4MSCL - 50, xlab = "")
hist((eclsk$C6R4MSCL - 50)^1.5, xlab = "")
hist((eclsk$C6R4MSCL - 50)^2, xlab = "")
hist((eclsk$C6R4MSCL - 50)^3, xlab = "")
hist(READ5^2)
hist(eclsk$RIRT)
hist(eclsk$RIRT - 22, xlab = "")
hist((eclsk$RIRT - 22)^.7, xlab = "")
hist((eclsk$RIRT - 22)^.3, xlab = "")
hist(log(eclsk$RIRT - 22), xlab = "")


lmFull <- lm(C6R4MSCL ~ ., data = eclsk)
summary(lmFull)
library(car)
plot(x = eclsk$MIRT, 
     y = eclsk$C6R4MSCL, 
     xlab = "Kindergarten Math Score", 
     ylab = "Fifth Grade Math Score", 
     col = "red")
#?loess
loess1 <- loess.smooth(x = eclsk$MIRT, 
                       y = eclsk$C6R4MSCL, 
                       span = 2/3, 
                       degree = 2)
points(x = loess1$x, y = loess1$y, type = "l", col = "green", lwd = 3)
lm1 <- lm(C6R4MSCL ~ MIRT, data = eclsk)
abline(lm1, col = "blue", lwd = 3)
residualPlot(lm1, col = "red")

# Transform using powers
xx = (eclsk$MIRT - 11)^.5
yy = (eclsk$C6R4MSCL - 50)^2
plot(x = xx, 
     y = yy, 
     xlab = expression("Kindergarten Math Score"^{1/2}), 
     ylab = expression("Fifth Grade Math Score"^2), 
     col = "red")
loess2 <- loess.smooth(x = xx, 
                       y = yy, 
                       span = 2/3, 
                       degree = 2)
points(x = loess2$x, 
       y = loess2$y, 
       type = "l", 
       col = "green", 
       lwd = 3)
lm2 <- lm(yy ~ xx, data = eclsk)
abline(lm2, col = "blue", lwd = 3)
residualPlot(lm2, col = "red")


#homework assignment Q2
lm1<-lm(READ5 ~ RIRT, data=eclsk)
plot(lm1)
lm2<-lm(READ5 ~ RIRT+I(RIRT^2), data=eclsk)
lm3<-lm(READ5 ~ RIRT+I(RIRT^2) +I(RIRT^3), data=eclsk)
lm4<-lm(READ5 ~ RIRT+I(RIRT^2) +I(RIRT^3)+I(RIRT^4), data=eclsk)
lm5<-lm(READ5 ~ RIRT+I(RIRT^2) +I(RIRT^3)+I(RIRT^4)+I(RIRT^5), data=eclsk)
anova(lm1,lm2)
anova(lm2,lm3)
anova(lm3,lm4)
anova(lm4,lm5)
plot(lm3)
plot(lm4)

#teachers note
plot(x = eclsk$MIRT, 
     y = eclsk$C6R4MSCL, 
     xlab = "Kindergarten Math Score", 
     ylab = "Fifth Grade Math Score", 
     col = "red")

# Get the range of values for the MIRT variable
range(eclsk$MIRT)
# Create a new data frame with only the MIRT variable
newd <- data.frame("MIRT" = matrix(seq(10, 100, .1), ncol = 1))
# Get predicted values based on lm3 model for the MIRT values in newd
ys <- predict(object = lm3, newdata = newd)
# plot the lm3 fitted curve 
points(x = newd[,1], y = ys, col = "blue", type = "l", lwd = 3)

residualPlot(lm3, col = "red")

####################################################################
# If you haven't already done so, install the car package
# install.packages("car")
# Then load the package
library(car)
# Collinearity detection via variance inflation factors
# Fit the big model with all 34 covariates
lmFull <- lm(READ5 ~ ., data = eclsk)

summary(lmFull)
READ5 <- cbind(eclsk,READ5)
names(READ5)[length(names(READ5))] <- "RIRT5"
# Use the vif() function from package car
vif(lmFull)

# Note that the vif for avg_MIRT is largest at 5.21

# Check the vif for avg_MIRT by running the regresion 
# of avg_MIRT on all other predictors and getting R^2.
lmCheck <- lm(avg_MIRT ~ . - READ5, data = eclsk)
summary(lmCheck)
# R^2 = .8082
1/(1-.8082)
eclsk <- cbind(eclsk,READ5)
READ5 <- cbind(eclsk,READ5)
names(READ5)[length(names(READ5))] <- "RIRT5"

# Incremental F Testing
# Is there evidence that the variables P1HSCALE, P1SADLON, and P1IMPULS,
# as a set, contribute to predictive power of the model?
lmRed <- lm(READ5 ~ . - P1HSCALE - P1SADLON - P1IMPULS , data = eclsk)
anova(lmRed, lmFull)
#assingment 2
install.packages("car")
length(READ5)
lmFull<-(READ5)
library(car)
install.packages("car")
vif(lmFull)