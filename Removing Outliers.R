remove_outliers <- function(x, na.rm = TRUE) {
  qnt = quantile(x, probs=c(.25, .75), na.rm = na.rm)
  H = 1.5 * IQR(x, na.rm = na.rm)
  y = x
  y[x < (qnt[1] - H)] = NA
  y[x > (qnt[2] + H)] = NA
  y
}

x = CASES
CASES = remove_outliers(x)
x = HCI
HCI = remove_outliers(x)
x = MEAT
MEAT = remove_outliers(x)
x = PD
PD = remove_outliers(x)
x = HYGIENE
HYGIENE = remove_outliers(x)
x = SANITATION
SANITATION = remove_outliers(x)
x = WATER
WATER = remove_outliers(x)
x = SL
SL = remove_outliers(x)
x = HAQI
HAQI = remove_outliers(x)

data = data.frame(CASES, HCI, MEAT, PD, HYGIENE, SANITATION, WATER, SL, HAQI)

data = na.omit(data)