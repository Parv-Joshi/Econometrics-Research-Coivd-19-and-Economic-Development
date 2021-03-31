
setwd("C:/Users/Lenovo/Desktop/Spring 2021/ECO 487 459/Data")

library(tidyverse)
library(AER)
library(sandwich)
library(lmtest)
library(car)
library(MASS)

data = read.csv("data.csv", sep = ",", header = T)
attach(data)

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

# Equation 2 Model

raw = lm(CASES ~ (HCI + HAQI + HYGIENE + MEAT + PD + SANITATION + I(SANITATION^2) + I(SANITATION^3) + I(log(SL)) + WATER + I(WATER^2) + I(WATER^3)), data = data)
summary(raw)

# Boxcox Transformation
bc=boxcox(raw)
lam = bc$x[which(bc$y==max(bc$y))]
lam

trans = lm(I(CASES^lam) ~ (HCI + HAQI + HYGIENE + MEAT + PD + SANITATION + I(SANITATION^2) + I(SANITATION^3) + I(log(SL)) + WATER + I(WATER^2) + I(WATER^3)), data = data)
summary(trans)
hetero_trans = coeftest(trans, vcov = vcovHC(trans, "HC1"))
hetero_trans

# Stepwise Regression
step = stepAIC(trans, direction = "both", trace = F)
step

# Final Model (Interaction terms have NA and NaN values)
Model1 = lm(I(CASES^lam) ~ MEAT + SANITATION + I(SANITATION^2) + I(SANITATION^3) + I(log(SL)) + I(WATER^2) + I(WATER^3), data = data)
summary(Model1)
hetero_Model1 = coeftest(Model1, vcov = vcovHC(Model1, "HC1"))
hetero_Model1


# Equation 2 Model

raw = lm(SL ~ (HCI + HAQI + I(HAQI^2) + I(HAQI^3) + HYGIENE + MEAT + I(MEAT^2) + I(MEAT^3) + PD + I(PD^2) + I(PD^3) + SANITATION + I(SANITATION^2) + I(SANITATION^3) + CASES + I(CASES^2) + I(CASES^3) + WATER + I(WATER^2) + I(WATER^3)), data = data)
summary(raw)

# Boxcox Transformation
bc=boxcox(raw)
lam = bc$x[which(bc$y==max(bc$y))]
lam

trans = lm(I(CASES^lam) ~ (HCI + HAQI + I(HAQI^2) + I(HAQI^3) + HYGIENE + MEAT + I(MEAT^2) + I(MEAT^3) + PD + I(PD^2) + I(PD^3) + SANITATION + I(SANITATION^2) + I(SANITATION^3) + CASES + I(CASES^2) + I(CASES^3) + WATER + I(WATER^2) + I(WATER^3)), data = data)
summary(trans)
hetero_trans = coeftest(trans, vcov = vcovHC(trans, "HC1"))
hetero_trans

# Stepwise Regression
step = stepAIC(trans, direction = "both", trace = F)
step

# Final Model (Interaction terms have NA and NaN values)
Model2 = lm(I(CASES^lam) ~ I(HAQI^2) + I(SANITATION^3) + CASES + I(CASES^2) + I(CASES^3), data = data)
summary(Model2)
hetero_Model2 = coeftest(Model2, vcov = vcovHC(Model2, "HC1"))
hetero_Model2

# Equation 3 Models

# We saw from our hetero_Model2 that HCI doesn't affect SL but I(HAQI^2) does. Hence, Our equation 3.1 becomes void since it does not satisfy the second stage regression. However, our equation 3.2 is still relevant.
# Also, we know that WATER, SANITATION and HYGIENE were possibly correlated to only the variables we selected in Model 2. Hence, we can cay that the correlation of WATER, SANITATION and HYGIENE with the error term is negligible.

# Model 3.2

# First, find transformation for HAQI
tmodel = lm(HAQI ~ SANITATION + I(SANITATION^2) + I(SANITATION^3) + WATER + I(WATER^2) + I(WATER^3) + HYGIENE, data = data)
summary(tmodel)
hetero_tmodel = coeftest(Model2, vcov = vcovHC(tmodel, "HC1"))
hetero_tmodel

# Boxcox Transformation
bc=boxcox(tmodel)
lam_1 = bc$x[which(bc$y==max(bc$y))]
lam_1

# Let us now perform first stage regressions for WATER, SANITATION and HYGIENE
model_iv_s = lm(I(HAQI^lam_1) ~ SANITATION + I(SANITATION^2) + I(SANITATION^3), data = data)
summary(model_iv_s)
hetero_model_iv_s = coeftest(model_iv_s, vcov = vcovHC(model_iv_s, "HC1"))
hetero_model_iv_s

model_iv_w = lm(I(HAQI^lam_1) ~  WATER + I(WATER^2) + I(WATER^3), data = data)
summary(model_iv_w)
hetero_model_iv_w = coeftest(model_iv_w, vcov = vcovHC(model_iv_w, "HC1"))
hetero_model_iv_w

model_iv_h = lm(I(HAQI^lam_1) ~ HYGIENE, data = data)
summary(model_iv_h)
hetero_model_iv_h = coeftest(model_iv_h, vcov = vcovHC(model_iv_h, "HC1"))
hetero_model_iv_h

# Since WATER, SANITATION and HYGIENE are significant to I(HAQI^2) in first stage regression, we can now perform instrumental variable regression

# IV Regression
ivreg_model_1 = ivreg(I(SL^lam) ~ I(HAQI^lam_1)|I(WATER^3) +  HCI + MEAT + I(MEAT^2) + I(MEAT^3) + PD + I(PD^2) + I(PD^3) + CASES + I(CASES^2) + I(CASES^3), data = data)
summary(ivreg_model_1, vcov = sandwich, df = Inf)

ivreg_model_2 = ivreg(I(SL^lam) ~ I(HAQI^lam_1)|HYGIENE +  HCI + MEAT + I(MEAT^2) + I(MEAT^3) + PD + I(PD^2) + I(PD^3) + CASES + I(CASES^2) + I(CASES^3), data = data)
summary(ivreg_model_2, vcov = sandwich, df = Inf)
