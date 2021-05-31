# Econometrics Research - Coivd 19 and Economic Development [Undergraduate Thesis]

Research Supervisor: Dr. Maria Perez-Urdiales

This was an independent research project I undertook at Stony Brook University as part of two courses -- ECO 487: Independent Research in Economics, and ECO 459: Write Effectively in Economics. I have uploaded my research paper (in pdf), the dataset (data.csv), and my R code (.R and .Rmd files) here.

My Research focused on two research questions:
1. To find the effect of the hygiene service industry on the spread of the coronavirus.
1. To find the direct dependence of the hygiene service industry on the standard of living of a country.

The dataset contains nine variables with 70 observations (country wise). The variables are:
CASES:		Average number of new coronavirus positive cases for a country, on a seven-day rolling basis as of March 1, 2021
HCI:		Human Capital Index: Average levels of education and health of an individual in a country
MEAT:		Meat consumption per capita of a country
PD:		Population Density: Number of people per square kilometer
HYGIENE:	Percentage of the population who has access to basic hygiene services
SANITATION:	Percentage of the population who has access to at least basic sanitation services
WATER:		Percentage of the population who has access to safe water for drinking
SL:		Standard of Living: GDP per capita, PPP adjusted, for current U.S. Dollars
HAQI:		Health Access and Quality Index: It measures the quality and accessibility of the healthcare institutions

The data was complied from the databases of four sources: *The World Bank*, *UNICEF*, *Master Covid-19 Dataset*, and *Our World in Data*.

I used pooled-data econometrics to carry out this this project. I used model specifications such as including interaction terms, using variable transformations, and correcting for heteroskedasticity robust standard errors. I used linear, quadratic, cubic, and logarithmic transformations for independent variables, while for response variables, I used the logarithmic and the Box-Cox power transformations. Moreover, I used Instrumental Variable (IV) Regression, also know as the Two-Stage Least Squares (TSLS or 2SLS) Regression to correct for endogeneity bias.

In this research, I concluded that access to basic sanitation significantly predicts economic development, access to basic hygiene significantly predicts the spread of the virus, while access to safe water does not predict any of those. The policy implications I found were that to reduce the spread of Covid-19, economic policies must focus on increasing access to hygiene services, improving the quality and accessibility of healthcare, and reducing population density. Moreover, to improve a country’s standard of living, policies must attempt to increase access to sanitation services, improve the quality and accessibility of healthcare, and limit the combined spending on hygiene and sanitation services.
