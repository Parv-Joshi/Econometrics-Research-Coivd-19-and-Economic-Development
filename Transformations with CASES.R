
setwd("C:/Users/Lenovo/Desktop/Spring 2021/ECO 487 459/Data")
library(AER)
library(tidyverse)

detach(data)
data = read.csv("data.csv", sep = ",", header = T)
attach(data)

y = CASES

# ---------------------------------------------------------------------------------------------------------------------------------
x = HCI

linear = lm(y ~ x, data = data)
squared = lm(y ~ x + I(x^2), data = data)
cubed = lm(y ~ x + I(x^2) + I(x^3), data = data)


waldtest(squared, c("I(x^2)"), vcov = vcovHC(cubed, "HC1"))
# Since p-value = 0.8092 > 0.05, Linear model is better

waldtest(cubed, c("I(x^2)", "I(x^3)"), vcov = vcovHC(cubed, "HC1"))
# Since p-value = 0.2042 > 0.05, Linear model is better

p = ggplot(data, aes(x = x, y = y)) + geom_point()
p + stat_smooth(method = "lm", formula = y ~ x, color = "red") + ggtitle("CASES vs. Linear Transformation of HCI")
p + stat_smooth(method = "lm", formula = y ~ log(x), color = "green") + ggtitle("CASES vs. Log Transformation of HCI")

# From graphs, we can conclude that the linear transformation is the best
# ---------------------------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------------------------
x = HAQI

linear = lm(y ~ x, data = data)
squared = lm(y ~ x + I(x^2), data = data)
cubed = lm(y ~ x + I(x^2) + I(x^3), data = data)


waldtest(squared, c("I(x^2)"), vcov = vcovHC(cubed, "HC1"))
# Since p-value = 0.951 > 0.05, Linear model is better

waldtest(cubed, c("I(x^2)", "I(x^3)"), vcov = vcovHC(cubed, "HC1"))
# Since p-value = 0.2249 > 0.05, Linear model is better

p = ggplot(data, aes(x = x, y = y)) + geom_point()
p + stat_smooth(method = "lm", formula = y ~ x, color = "red") + ggtitle("CASES vs. Linear Transformation of HAQI")
p + stat_smooth(method = "lm", formula = y ~ log(x), color = "green") + ggtitle("CASES vs. Log Transformation of HAQI")

# From graphs, we can conclude that the linear transformation is the best
# ---------------------------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------------------------
x = HYGIENE

linear = lm(y ~ x, data = data)
squared = lm(y ~ x + I(x^2), data = data)
cubed = lm(y ~ x + I(x^2) + I(x^3), data = data)


waldtest(squared, c("I(x^2)"), vcov = vcovHC(cubed, "HC1"))
# Since p-value = 0.8377 > 0.05, Linear model is better

waldtest(cubed, c("I(x^2)", "I(x^3)"), vcov = vcovHC(cubed, "HC1"))
# Since p-value = 0.2885 > 0.05, Linear model is better

p = ggplot(data, aes(x = x, y = y)) + geom_point()
p + stat_smooth(method = "lm", formula = y ~ x, color = "red") + ggtitle("CASES vs. Linear Transformation of HYGIENE")
p + stat_smooth(method = "lm", formula = y ~ log(x), color = "green") + ggtitle("CASES vs. Log Transformation of HYGIENE")

# From graphs, we can conclude that the linear transformation is the best
# ---------------------------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------------------------
x = MEAT

linear = lm(y ~ x, data = data)
squared = lm(y ~ x + I(x^2), data = data)
cubed = lm(y ~ x + I(x^2) + I(x^3), data = data)


waldtest(squared, c("I(x^2)"), vcov = vcovHC(cubed, "HC1"))
# Since p-value = 0.8658 > 0.05, Linear model is better

waldtest(cubed, c("I(x^2)", "I(x^3)"), vcov = vcovHC(cubed, "HC1"))
# Since p-value = 0.1886 > 0.05, Linear model is better

p = ggplot(data, aes(x = x, y = y)) + geom_point()
p + stat_smooth(method = "lm", formula = y ~ x, color = "red") + ggtitle("CASES vs. Linear Transformation of MEAT")
p + stat_smooth(method = "lm", formula = y ~ log(x), color = "green") + ggtitle("CASES vs. Log Transformation of MEAT")

# From graphs, we can conclude that the linear transformation is the best
# ---------------------------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------------------------
x = PD

linear = lm(y ~ x, data = data)
squared = lm(y ~ x + I(x^2), data = data)
cubed = lm(y ~ x + I(x^2) + I(x^3), data = data)


waldtest(squared, c("I(x^2)"), vcov = vcovHC(cubed, "HC1"))
# Since p-value = 0.7751 > 0.05, Linear model is better

waldtest(cubed, c("I(x^2)", "I(x^3)"), vcov = vcovHC(cubed, "HC1"))
# Since p-value = 0.4489 > 0.05, Linear model is better

p = ggplot(data, aes(x = x, y = y)) + geom_point()
p + stat_smooth(method = "lm", formula = y ~ x, color = "red") + ggtitle("CASES vs. Linear Transformation of PD")
p + stat_smooth(method = "lm", formula = y ~ log(x), color = "green") + ggtitle("CASES vs. Log Transformation of PD")

# From graphs, we can conclude that the linear transformation is the best
# ---------------------------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------------------------
x = SANITATION

linear = lm(y ~ x, data = data)
squared = lm(y ~ x + I(x^2), data = data)
cubed = lm(y ~ x + I(x^2) + I(x^3), data = data)


waldtest(squared, c("I(x^2)"), vcov = vcovHC(cubed, "HC1"))
# Since p-value = 0.8325 > 0.05, Linear model is better

waldtest(cubed, c("I(x^2)", "I(x^3)"), vcov = vcovHC(cubed, "HC1"))
# Since p-value = 0.02524 < 0.05, Cubic model is better

p = ggplot(data, aes(x = x, y = y)) + geom_point()
p + stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), color = "red") + ggtitle("CASES vs. Cubic Transformation of SANITATION")
p + stat_smooth(method = "lm", formula = y ~ log(x), color = "green") + ggtitle("CASES vs. Log Transformation of SANITATION")

# From graphs, we can conclude that the cubic transformation is the best
# ---------------------------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------------------------
x = SL

linear = lm(y ~ x, data = data)
squared = lm(y ~ x + I(x^2), data = data)
cubed = lm(y ~ x + I(x^2) + I(x^3), data = data)


waldtest(squared, c("I(x^2)"), vcov = vcovHC(cubed, "HC1"))
# Since p-value = 0.9164 > 0.05, Linear model is better

waldtest(cubed, c("I(x^2)", "I(x^3)"), vcov = vcovHC(cubed, "HC1"))
# Since p-value = 0.5494 > 0.05, Linear model is better

p = ggplot(data, aes(x = x, y = y)) + geom_point()
p + stat_smooth(method = "lm", formula = y ~ x, color = "red") + ggtitle("CASES vs. Linear Transformation of SL")
p + stat_smooth(method = "lm", formula = y ~ log(x), color = "green") + ggtitle("CASES vs. Log Transformation of SL")

# From graphs, we can conclude that the log transformation is the best
# ---------------------------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------------------------
x = WATER

linear = lm(y ~ x, data = data)
squared = lm(y ~ x + I(x^2), data = data)
cubed = lm(y ~ x + I(x^2) + I(x^3), data = data)


waldtest(squared, c("I(x^2)"), vcov = vcovHC(cubed, "HC1"))
# Since p-value = 0.9194 > 0.05, Linear model is better

waldtest(cubed, c("I(x^2)", "I(x^3)"), vcov = vcovHC(cubed, "HC1"))
# Since p-value = 0.03671 < 0.05, Cubic model is better

p = ggplot(data, aes(x = x, y = y)) + geom_point()
p + stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), color = "red") + ggtitle("CASES vs. Cubic Transformation of WATER")
p + stat_smooth(method = "lm", formula = y ~ log(x), color = "green") + ggtitle("CASES vs. Log Transformation of WATER")

# From graphs, we can conclude that the cubic transformation is the best
# ---------------------------------------------------------------------------------------------------------------------------------

detach(data)
