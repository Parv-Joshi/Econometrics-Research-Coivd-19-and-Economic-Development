
setwd("C:/Users/Lenovo/Desktop/Spring 2021/ECO 487 459/Data")
library(AER)
library(tidyverse)

detach(data)
data = read.csv("data.csv", sep = ",", header = T)
attach(data)

y = HAQI

# ---------------------------------------------------------------------------------------------------------------------------------
x = WATER

linear = lm(y ~ x, data = data)
squared = lm(y ~ x + I(x^2), data = data)
cubed = lm(y ~ x + I(x^2) + I(x^3), data = data)


waldtest(squared, c("I(x^2)"), vcov = vcovHC(cubed, "HC1"))
# Since p-value = 0.7349 > 0.05, Linear model is better

waldtest(cubed, c("I(x^2)", "I(x^3)"), vcov = vcovHC(cubed, "HC1"))
# Since p-value = 1.154e-05 < 0.05, Cubic model is better

p = ggplot(data, aes(x = x, y = y)) + geom_point()
p + stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), color = "red") + ggtitle("HAQI vs. Cubic Transformation of WATER")
p + stat_smooth(method = "lm", formula = y ~ log(x), color = "green") + ggtitle("HAQI vs. Log Transformation of WATER")

# From graphs, we can conclude that the cubic transformation is the best
# ---------------------------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------------------------
x = SANITATION

linear = lm(y ~ x, data = data)
squared = lm(y ~ x + I(x^2), data = data)
cubed = lm(y ~ x + I(x^2) + I(x^3), data = data)


waldtest(squared, c("I(x^2)"), vcov = vcovHC(cubed, "HC1"))
# Since p-value = 0.6015 > 0.05, Linear model is better

waldtest(cubed, c("I(x^2)", "I(x^3)"), vcov = vcovHC(cubed, "HC1"))
# Since p-value = 0.002077 < 0.05, Cubic model is better

p = ggplot(data, aes(x = x, y = y)) + geom_point()
p + stat_smooth(method = "lm", formula = y ~ x + I(x^2) + I(x^3), color = "red") + ggtitle("HAQI vs. Cubic Transformation of SANITATION")
p + stat_smooth(method = "lm", formula = y ~ log(x), color = "green") + ggtitle("HAQI vs. Log Transformation of SANITATION")

# From graphs, we can conclude that the cubic transformation is the best
# ---------------------------------------------------------------------------------------------------------------------------------

# ---------------------------------------------------------------------------------------------------------------------------------
x = HYGIENE

linear = lm(y ~ x, data = data)
squared = lm(y ~ x + I(x^2), data = data)
cubed = lm(y ~ x + I(x^2) + I(x^3), data = data)


waldtest(squared, c("I(x^2)"), vcov = vcovHC(cubed, "HC1"))
# Since p-value = 0.7561 > 0.05, Linear model is better

waldtest(cubed, c("I(x^2)", "I(x^3)"), vcov = vcovHC(cubed, "HC1"))
# Since p-value = 0.1287 > 0.05, Linear model is better

p = ggplot(data, aes(x = x, y = y)) + geom_point()
p + stat_smooth(method = "lm", formula = y ~ x, color = "red") + ggtitle("HAQI vs. Linear Transformation of HYGIENE")
p + stat_smooth(method = "lm", formula = y ~ log(x), color = "green") + ggtitle("HAQI vs. Log Transformation of HYGIENE")

# From graphs, we can conclude that the linear transformation is the best
# ---------------------------------------------------------------------------------------------------------------------------------
