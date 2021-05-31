
setwd("C:/Users/Lenovo/Desktop/Spring 2021/ECO 487 459/Data")

library(tidyverse)
library(AER)
library(sandwich)
library(lmtest)
library(car)
library(MASS)
library(stargazer)

detach(data)
data = read.csv("data.csv", sep = ",", header = T)
attach(data)

summary(data)

cor(na.omit(data))

par(mfrow = c(3,3))

hist(CASES)
hist(HCI)
hist(MEAT)
hist(PD)
hist(HYGIENE)
hist(SANITATION)
hist(WATER)
hist(SL)
hist(HAQI)

boxplot(CASES, main = "Boxplot of CASES")
boxplot(HCI,  main = "Boxplot of HCI")
boxplot(MEAT, main = "Boxplot of MEAT")
boxplot(PD, main = "Boxplot of PD")
boxplot(HYGIENE, main = "Boxplot of HYGIENE")
boxplot(SANITATION, main = "Boxplot of SANITATION")
boxplot(WATER, main = "Boxplot of WATER")
boxplot(SL, main = "Boxplot of SL")
boxplot(HAQI, main = "Boxplot of HAQI")

par(mfrow = c(1,1))

pairs(data)

# Equation 1 Final Model

Model1 = lm(CASES ~ ., data = data)
summary(Model1)$adj.r.squared

Model1 = lm(I(log(CASES)) ~ ., data = data)
summary(Model1)$adj.r.squared

Model1 = lm(CASES ~ (.)^2, data = data)
summary(Model1)$adj.r.squared

Model1 = lm(I(log(CASES)) ~ (.)^2, data = data)
summary(Model1)$adj.r.squared

Model1 = lm(CASES ~ (.)^2 + I(SANITATION^2)+ I(SANITATION^3) + I(log(SL)) + I(WATER^2) + I(WATER^3), data = data)
summary(Model1)$adj.r.squared

Model1 = lm(I(log(CASES)) ~ (.)^2 + I(SANITATION^2)+ I(SANITATION^3) + I(log(SL)) + I(WATER^2) + I(WATER^3), data = data)
summary(Model1)$adj.r.squared

# This One
Model1 = lm(CASES ~ (HCI + HAQI + HYGIENE + MEAT + PD + SANITATION + I(SANITATION^2) + I(SANITATION^3) + I(log(SL)) + WATER + I(WATER^2) + I(WATER^3) + MEAT*PD + MEAT*HAQI + PD*HYGIENE + PD*SANITATION + PD*HAQI + WATER*SL), data = data)
summary(Model1)$adj.r.squared

Model1 = lm(I(log(CASES)) ~ (HCI + HAQI + HYGIENE + MEAT + PD + SANITATION + I(SANITATION^2) + I(SANITATION^3) + I(log(SL)) + WATER + I(WATER^2) + I(WATER^3) + MEAT*PD + MEAT*HAQI + PD*HYGIENE + PD*SANITATION + PD*HAQI + WATER*SL), data = data)
summary(Model1)$adj.r.squared

Model1 = lm(CASES ~ HCI + HAQI + HYGIENE + MEAT + PD + SANITATION + I(SANITATION^2) + I(SANITATION^3) + I(log(SL)) + WATER + I(WATER^2) + I(WATER^3), data = data)
summary(Model1)$adj.r.squared

Model1 = lm(I(log(CASES)) ~ HCI + HAQI + HYGIENE + MEAT + PD + SANITATION + I(SANITATION^2) + I(SANITATION^3) + I(log(SL)) + WATER + I(WATER^2) + I(WATER^3), data = data)
summary(Model1)$adj.r.squared

# --------------------------

Model = lm(CASES ~ (HCI + HAQI + HYGIENE + MEAT + PD + SANITATION + I(SANITATION^2) + I(SANITATION^3) + I(log(SL)) + WATER + I(WATER^2) + I(WATER^3) + MEAT*PD + MEAT*HAQI + PD*HYGIENE + PD*SANITATION + PD*HAQI + WATER*SL), data = data)

bc = boxcox(Model, lambda = seq(-3,3))
lam = bc$x[which(bc$y==max(bc$y))]
lam

Model_bc = lm(I(CASES^lam) ~ (HCI + HAQI + HYGIENE + MEAT + PD + SANITATION + I(SANITATION^2) + I(SANITATION^3) + I(log(SL)) + WATER + I(WATER^2) + I(WATER^3) + MEAT*PD + MEAT*HAQI + PD*HYGIENE + PD*SANITATION + PD*HAQI + WATER*SL), data = data)
summary(Model_bc)$adj.r.squared
# The boxcox transformation worsens our model's fit. Hence, we must not use it.

FinalModel1 = lm(CASES ~ (HCI + HAQI + HYGIENE + MEAT + PD + SANITATION + I(SANITATION^2) + I(SANITATION^3) + I(log(SL)) + WATER + I(WATER^2) + I(WATER^3) + MEAT*PD + MEAT*HAQI + PD*HYGIENE + PD*SANITATION + PD*HAQI + WATER*SL), data = data)
summary(FinalModel1)
hetero_FinalModel1 = coeftest(FinalModel1, vcov = vcovHC(FinalModel1, "HC1"))
stargazer(hetero_FinalModel1, type = "text")

#FinalModel1_iv = ivreg(CASES ~ (HCI + HAQI + HYGIENE + MEAT + PD + SANITATION + I(SANITATION^2) + I(SANITATION^3) + I(log(SL)) + WATER + I(WATER^2) + I(WATER^3) + MEAT*PD + MEAT*HAQI + PD*HYGIENE + PD*SANITATION + PD*HAQI + WATER*SL), data = data)

# Equation 2 Models

Model2 = lm(SL ~ ., data = data)
summary(Model2)$adj.r.squared

Model2 = lm(I(log(SL)) ~ ., data = data)
summary(Model2)$adj.r.squared

Model2 = lm(SL ~ (.)^2, data = data)
summary(Model2)$adj.r.squared

Model2 = lm(I(log(SL)) ~ (.)^2, data = data)
summary(Model2)$adj.r.squared

Model2 = lm(SL ~ (.)^2 + I(HAQI^2) + I(HAQI^3) + I(MEAT^2)+ I(MEAT^3) + I(PD^2) + I(PD^3) + I(SANITATION^2) + I(SANITATION^3) + I(WATER^2) + I(WATER^3), data = data)
summary(Model2)$adj.r.squared

Model2 = lm(I(log(SL)) ~ (.)^2 + I(HAQI^2) + I(HAQI^3) + I(MEAT^2)+ I(MEAT^3) + I(PD^2) + I(PD^3) + I(SANITATION^2) + I(SANITATION^3) + I(WATER^2) + I(WATER^3), data = data)
summary(Model2)$adj.r.squared

# This One
Model2 = lm(SL ~ HCI + HAQI + I(HAQI^2) + I(HAQI^3) + HYGIENE + MEAT + I(MEAT^2) + I(MEAT^3) + PD + I(PD^2) + I(PD^3) + SANITATION + I(SANITATION^2) + I(SANITATION^3) + WATER + I(WATER^2) + I(WATER^3) + CASES*HYGIENE + MEAT*SANITATION + HYGIENE*SANITATION, data = data)
summary(Model2)$adj.r.squared

Model2 = lm(I(log(SL)) ~ HCI + HAQI + I(HAQI^2) + I(HAQI^3) + HYGIENE + MEAT + I(MEAT^2) + I(MEAT^3) + PD + I(PD^2) + I(PD^3) + SANITATION + I(SANITATION^2) + I(SANITATION^3) + WATER + I(WATER^2) + I(WATER^3) + CASES*HYGIENE + MEAT*SANITATION + HYGIENE*SANITATION, data = data)
summary(Model2)$adj.r.squared

Model2 = lm(SL ~ (HCI + HAQI + I(HAQI^2) + I(HAQI^3) + HYGIENE + MEAT + I(MEAT^2) + I(MEAT^3) + PD + I(PD^2) + I(PD^3) + SANITATION + I(SANITATION^2) + I(SANITATION^3) + WATER + I(WATER^2) + I(WATER^3)), data = data)
summary(Model2)$adj.r.squared

Model2 = lm(I(log(SL)) ~ (HCI + HAQI + I(HAQI^2) + I(HAQI^3) + HYGIENE + MEAT + I(MEAT^2) + I(MEAT^3) + PD + I(PD^2) + I(PD^3) + SANITATION + I(SANITATION^2) + I(SANITATION^3) + WATER + I(WATER^2) + I(WATER^3)), data = data)
summary(Model2)$adj.r.squared

# --------------------------

Model = lm(SL ~ HCI + HAQI + I(HAQI^2) + I(HAQI^3) + HYGIENE + MEAT + I(MEAT^2) + I(MEAT^3) + PD + I(PD^2) + I(PD^3) + SANITATION + I(SANITATION^2) + I(SANITATION^3) + WATER + I(WATER^2) + I(WATER^3) + CASES*HYGIENE + MEAT*SANITATION + HYGIENE*SANITATION, data = data)
bc = boxcox(Model, lambda = seq(-3,3))
lam = bc$x[which(bc$y==max(bc$y))]
lam

Model_bc = lm(I(SL^lam) ~ HCI + HAQI + I(HAQI^2) + I(HAQI^3) + HYGIENE + MEAT + I(MEAT^2) + I(MEAT^3) + PD + I(PD^2) + I(PD^3) + SANITATION + I(SANITATION^2) + I(SANITATION^3) + WATER + I(WATER^2) + I(WATER^3) + CASES*HYGIENE + MEAT*SANITATION + HYGIENE*SANITATION, data = data)
summary(Model_bc)$adj.r.squared
# The boxcox transformation worsens our model's fit. Hence, we must not use it.

FinalModel2_1 = lm(SL ~ HCI + HAQI + I(HAQI^2) + I(HAQI^3) + HYGIENE + MEAT + I(MEAT^2) + I(MEAT^3) + PD + I(PD^2) + I(PD^3) + SANITATION + I(SANITATION^2) + I(SANITATION^3) + WATER + I(WATER^2) + I(WATER^3) + CASES*HYGIENE + MEAT*SANITATION + HYGIENE*SANITATION, data = data)
summary(FinalModel2_1)
hetero_FinalModel2_1 = coeftest(FinalModel2_1, vcov = vcovHC(FinalModel2_1, "HC1"))
hetero_FinalModel2_1
stargazer(hetero_FinalModel2_1, type = "text")



# IV Regression

iv_1 = ivreg(CASES ~ HYGIENE + SANITATION + I(SANITATION^2) +
               I(SANITATION^3) + WATER + I(WATER^2) + I(WATER^3) +
               PD + MEAT*PD + MEAT*HAQI + PD*HYGIENE +
               PD*SANITATION + PD*HAQI + WATER*SL | HCI + HAQI + MEAT +
               PD + I(log(SL)) + SANITATION + I(SANITATION^2) + MEAT*PD + 
               MEAT*HAQI + PD*HYGIENE + WATER + I(WATER^2) + PD*SANITATION +
               PD*HAQI + WATER*SL, data = data)
summary(iv_1, vcov = sandwich, df = Inf)

iv_2 = ivreg(SL ~ HAQI + I(HAQI^2) + I(HAQI^3) + HYGIENE + SANITATION + I(SANITATION^2) +
               I(SANITATION^3) + WATER + I(WATER^2) + I(WATER^3) + CASES*HYGIENE +
               MEAT*SANITATION + HYGIENE*SANITATION | MEAT + I(MEAT^2) + I(MEAT^3) +
               I(PD^2) + I(PD^3) + SANITATION + I(SANITATION^2) + HAQI +
               I(HAQI^2) + I(HAQI^3) + WATER + I(WATER^2) + + CASES*HYGIENE +
               MEAT*SANITATION + HYGIENE*SANITATION , data = data)
summary(iv_2, vcov = sandwich, df = Inf)

summary(lm(HYGIENE ~ HCI + HAQI + MEAT +
             PD + I(log(SL)) + SANITATION + I(SANITATION^2) + MEAT*PD + 
             MEAT*HAQI + PD*HYGIENE + WATER + I(WATER^2) + PD*SANITATION +
             PD*HAQI + WATER*SL, data = data))
summary(lm(HYGIENE ~ MEAT + I(MEAT^2) + I(MEAT^3) +
             I(PD^2) + I(PD^3) + SANITATION + I(SANITATION^2) + HAQI +
             I(HAQI^2) + I(HAQI^3) + WATER + I(WATER^2) + + CASES*HYGIENE +
             MEAT*SANITATION + HYGIENE*SANITATION + PD + HCI, data = data))
summary(lm(I(SANITATION^3) ~ HCI + HAQI + MEAT +
             PD + I(log(SL)) + SANITATION + I(SANITATION^2) + MEAT*PD + 
             MEAT*HAQI + PD*HYGIENE + WATER + I(WATER^2) + PD*SANITATION +
             PD*HAQI + WATER*SL, data = data))
summary(lm(I(SANITATION^3) ~ MEAT + I(MEAT^2) + I(MEAT^3) +
             I(PD^2) + I(PD^3) + SANITATION + I(SANITATION^2) + HAQI +
             I(HAQI^2) + I(HAQI^3) + WATER + I(WATER^2) + + CASES*HYGIENE +
             MEAT*SANITATION + HYGIENE*SANITATION + PD + HCI, data = data))
summary(lm(I(WATER^3) ~ HCI + HAQI + MEAT +
             PD + I(log(SL)) + SANITATION + I(SANITATION^2) + MEAT*PD + 
             MEAT*HAQI + PD*HYGIENE + WATER + I(WATER^2) + PD*SANITATION +
             PD*HAQI + WATER*SL, data = data))
summary(lm(I(WATER^3) ~ MEAT + I(MEAT^2) + I(MEAT^3) +
             I(PD^2) + I(PD^3) + SANITATION + I(SANITATION^2) + HAQI +
             I(HAQI^2) + I(HAQI^3) + WATER + I(WATER^2) + + CASES*HYGIENE +
             MEAT*SANITATION + HYGIENE*SANITATION + PD + HCI, data = data))
