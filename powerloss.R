# C. McClintock
# Electricity Theft
# Feb. 21st, 2018

# ......................................................................................................

# DATA IMPORT & CLEANING

# set up
getwd()
setwd("/Users/charmed33/R/HultPrize2018")

# libraries
library(tidyverse)
library(dplyr)

# read in the data, all from the world bank
powerloss <- read_csv("electric_loss.csv", skip = 4) 
# Electric power transmission and distribution losses (% of output)
access <- read_csv("access.csv", skip = 4) 
# Access to electricity (% of population)
population <- read_csv("population.csv", skip = 4)
# Population, total
kwh <- read_csv("kwhpercap.csv", skip = 4)
# Electric power consumption (kWh per capita)
gdp <- read_csv("gdp.csv", skip = 4)
# GDP (current US$)
countries <- read_csv("wb_countrydata.csv")
# Children out of school, female (% of female primary school age)
girlsed <- read_csv("girlsed.csv", skip = 4)

# change some names, use data from 2014, last available
colnames(powerloss)[colnames(powerloss)=="Country Name"] <- "country"
colnames(powerloss)[colnames(powerloss)=="Country Code"] <- "country_code"
colnames(powerloss)[colnames(powerloss)=="2014"] <- "percentloss"

colnames(population)[colnames(population)=="Country Name"] <- "country"
colnames(population)[colnames(population)=="Country Code"] <- "country_code"
colnames(population)[colnames(population)=="2014"] <- "population"

colnames(gdp)[colnames(gdp)=="Country Name"] <- "country"
colnames(gdp)[colnames(gdp)=="Country Code"] <- "country_code"
colnames(gdp)[colnames(gdp)=="2014"] <- "gdp"

colnames(kwh)[colnames(kwh)=="Country Name"] <- "country"
colnames(kwh)[colnames(kwh)=="Country Code"] <- "country_code"
colnames(kwh)[colnames(kwh)=="2014"] <- "kwh"

colnames(access)[colnames(access)=="Country Name"] <- "country"
colnames(access)[colnames(access)=="Country Code"] <- "country_code"
colnames(access)[colnames(access)=="2014"] <- "access"

colnames(girlsed)[colnames(girlsed)=="Country Name"] <- "country"
colnames(girlsed)[colnames(girlsed)=="Country Code"] <- "country_code"
colnames(girlsed)[colnames(girlsed)=="2014"] <- "girlsed"

names(countries) <- c("country_code","region","income_group","country")


# select the 2014 data
powerloss <- select(powerloss, country, country_code, percentloss)
population <- select(population, country, country_code, population)
gdp <- select(gdp, country, country_code, gdp)
kwh <- select(kwh, country, country_code, kwh)
access <- select(access, country, country_code, access)
girlsed <- select(girlsed, country, country_code, girlsed)

# merge it all together
power <- left_join(powerloss, countries, by = 
           c("country_code" = "country_code","country" = "country"))
power <- left_join(power, population, by = 
           c("country_code" = "country_code", "country" = "country"))
power <- left_join(power, gdp, by = 
           c("country_code" = "country_code", "country" = "country"))
power <- left_join(power, kwh, by = 
           c("country_code" = "country_code", "country" = "country"))
power <- left_join(power, access, by = 
           c("country_code" = "country_code", "country" = "country"))
power <- left_join(power, girlsed, by = 
           c("country_code" = "country_code", "country" = "country"))

# create gdp per capita by dividing gdp by population
power <- mutate(power, gdp_percap = gdp / population)
power <- select(power, country, country_code, region, 
                income_group, gdp, gdp_percap, population, everything())
power <- power[complete.cases(power[ , 3]),]
power$income_group <- as.factor(power$income_group)
power <- mutate(power,
                income_group=fct_relevel(income_group,
                  "Low income",
                  "Lower middle income",
                  "Upper middle income",
                  "High income"))

# .......................................................................................................

# EXPLORATORY DATA ANALYSIS

# how much electricity is being lost?
summary(power$percentloss) # average of 13 percent loss, much higher in poor countries
summary(power$gdp_percap)

# how does that vary by income group?

## average use by income group
kwhbyincome <- as.data.frame(power %>% 
                                   group_by(income_group) %>% 
                                   summarise(mean(kwh, na.rm = TRUE)))

## average percent loss by income group
percentbyincome <- as.data.frame(power %>% 
             group_by(income_group) %>% 
              summarise(mean(percentloss, na.rm = TRUE)))

## number of countries in each income group
numbyincome <- as.data.frame(power %>% 
             group_by(income_group) %>% 
             summarise(byincome = n()))

## number of countries in each income group
percentbyregion <- as.data.frame(power %>% 
                               group_by(region) %>% 
                               summarise(mean(percentloss, na.rm = TRUE)))


# generalized linear models 

# gdp per capita vs. percent loss
gdp_percentloss <- glm(gdp_percap ~ percentloss, data = power)
summary(gdp_percentloss) # intercept = 28953.6, slope = -851.6, p < .001

# kwh per capita vs. percent loss
percentloss_kwh <-  glm(kwh ~ percentloss, data = power)
summary(percentloss_kwh) # intercept = 6866.62, slope =-184.9, p < .001

# kwh per capita vs. gdp per capita
gdp_kwh <-  glm(kwh ~ gdp_percap, data = power)
summary(gdp_kwh) # intercept = 1134, slope = .1845, p < .001

# access vs. loss
access_loss <- glm(access ~ kwh, data = power)
summary(access_loss) # intercept = 82.16, slope = .00132, p < .001

# ......................................................................................................

# VISUALIZATION

# gdp per capita vs. percent loss
ggplot(data = power, mapping = aes(gdp_percap, percentloss, na.rm = TRUE)) + 
  geom_point(aes(colour = factor(income_group)), size = 2) + geom_smooth(se = FALSE) +
  coord_cartesian(xlim = c(0, 125000)) + labs(colour = "Income Level", 
       title = "GDP per Capita vs. Transmission & Distribution Loss (% of output)", 
       caption = "based on most recent available data from the World Bank (2014)",
       x = "GDP per Capita (US$)", 
       y = "Electric power transmission and distribution losses (% of output)")

# kwh per capita vs. percent loss
ggplot(data = power, mapping = aes(kwh, percentloss, na.rm = TRUE)) + 
  geom_point(aes(colour = factor(income_group)), size = 2) + geom_smooth(se = FALSE) +
  coord_cartesian(xlim = c(0, 25000)) + labs(colour = "Income Level", 
       title = "kWh per Capita vs. Transmission & Distribution Loss (% of output)", 
       caption = "based on most recent available data from the World Bank (2014)",
       x = "Electric power consumption (kWh per capita)", 
       y = "Electric power losses (% of output)")

# kwh per capita vs. gdp per capita
ggplot(data = power, mapping = aes(kwh, gdp_percap), na.rm = TRUE) + 
  geom_point(aes(colour = factor(income_group)), size = 2) + coord_cartesian(ylim = c(0,125000),
  xlim=c(0,25000)) + labs(colour = "Income Level", 
       title = "Electric power consumption vs. GDP per Capita", 
       caption = "based on most recent available data from the World Bank (2014)",
       y = "GDP per Capita", 
       x = "Electric power consumption (kWh per capita)")

# kwh per capita vs. percent access
ggplot(data = power, mapping = aes(access, kwh), na.rm = TRUE) + 
  geom_point(aes(colour = factor(income_group)), size = 2) + 
  coord_cartesian(ylim = c(0,25000)) + 
  labs(colour = "Income Level", 
       title = "Percent Access to Electricity vs. kWh per Capita", 
       caption = "based on most recent available data from the World Bank (2014)",
       x = "Access to Electricity(%)", 
       y = "Electric power consumption (kWh per capita)")


