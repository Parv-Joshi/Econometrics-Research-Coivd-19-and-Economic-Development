setwd("D:/Spring 2021/ECO 487 459/Data/Individual Variables Seperated Excel")
messy_data = read.csv("Health Access and Quality Index.csv", sep = ",", header = TRUE)
purified = na.omit(messy_data)
new_p = purified[order(purified$Country),]
setwd("D:/Spring 2021/ECO 487 459/Data/Individual Var NA OMIT CSV")
write.table(new_p, file="Health.csv", sep = ",")
