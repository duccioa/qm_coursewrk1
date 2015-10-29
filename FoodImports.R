library(dplyr)
countries <- read.csv("countries.csv", sep = ",", stringsAsFactors = FALSE)
names(countries) <- c("X", "Year", "CountryCode", "Country", "Population", "gdp", "FoodImports", "FuelImports")

      
attach(countries)
#Population - not good match
plot(log(Population), log(FoodImports))
plot(Population, FoodImports, xlim = c(0,max(Population)*1.2))
text(countries$Population, countries$FoodImports, labels = countries$CountryCode, pos = 4)
summary(lm(FoodImports ~ Population, data = countries))
detach(countries)

attach(countries)
summary(lm(FoodImports ~ gdp + I(gdp^2) + gdp:Population, data = countries))
