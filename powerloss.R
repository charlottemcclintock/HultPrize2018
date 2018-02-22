# C. McClintock
# Electric Power Losses


library(tidyverse)
library(dplyr)

# set up
getwd()
setwd("/Users/charmed33/R/HultPrize2018")

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

names(countries) <- c("country_code","region","income_group","country")


# select the 2014 data
powerloss <- select(powerloss, country, country_code, percentloss)
population <- select(population, country, country_code, population)
gdp <- select(gdp, country, country_code, gdp)
kwh <- select(kwh, country, country_code, kwh)
access <- select(access, country, country_code, access)

# merge it all together
power <- left_join(powerloss, countries, by = c("country_code" = "country_code","country" = "country"))
power <- left_join(power, population, by = c("country_code" = "country_code", "country" = "country"))
power <- left_join(power, gdp, by = c("country_code" = "country_code", "country" = "country"))
power <- left_join(power, kwh, by = c("country_code" = "country_code", "country" = "country"))
power <- left_join(power, access, by = c("country_code" = "country_code", "country" = "country"))

power <- select(power, country, country_code, region, income_group, gdp, population, everything())
power <- mutate(power, gdp_percap = gdp / population)
power <- power[complete.cases(power[ , 3]),]
power$income_group <- as.factor(power$income_group)
power <- mutate(power,
                income_group=fct_relevel(income_group,
                  "Low income",
                  "Lower middle income",
                  "Upper middle income",
                  "High income"))

# exploratory data analysis

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

# generalized linear models #### ADD COMMENTED SUMMARIES

# gdp per capita vs. percent loss
gdp_percentloss <- glm(gdp_percap ~ percentloss, data = power)
summary(gdp_percentloss) 

# kwh per capita vs. percent loss
percentloss_kwh <-  glm(kwh ~ percentloss, data = power)
summary(percentloss_kwh)

# kwh per capita vs. gdp per capita
gdp_kwh <-  glm(gdp_percap ~ kwh, data = power)
summary(gdp_kwh)

# access vs. loss
access_loss <- glm(access ~ kwh, data = power)
summary(access_loss)


# some visualization

# gdp per capita vs. percent loss
ggplot(data = power, mapping = aes(gdp_percap, percentloss, na.rm = TRUE)) + 
  geom_point(aes(colour = factor(income_group)), size = 2) + geom_smooth(se = FALSE) +
  labs(colour = "Income Level", 
       title = "GDP per Capita vs. Transmission & Distribution Loss (% of output)", 
       caption = "based on most recent available data from the World Bank (2014)",
       x = "GDP per Capita (US$)", 
       y = "Electric power transmission and distribution losses (% of output)")

# kwh per capita vs. percent loss
ggplot(data = power, mapping = aes(kwh, percentloss, na.rm = TRUE)) + 
  geom_point(aes(colour = factor(income_group)), size = 2) + geom_smooth(se = FALSE) +
  labs(colour = "Income Level", 
       title = "GDP per Capita vs. kWh per Capita", 
       caption = "based on most recent available data from the World Bank (2014)",
       x = "kWh per Capita", 
       y = "Electric power transmission and distribution losses (% of output)")

# kwh per capita vs. gdp per capita
ggplot(data = power, mapping = aes(gdp_percap, kwh), na.rm = TRUE) + 
  geom_point(aes(colour = factor(income_group)), size = 2) + 
  geom_smooth(method = "lm", se = FALSE) + coord_cartesian(xlim = c(0,125000), ylim=c(0,25000)) +
  labs(colour = "Income Level", 
       title = "GDP per Capita vs. kWh per Capita", 
       caption = "based on most recent available data from the World Bank (2014)",
       x = "GDP per Capita", 
       y = "kWh per Capita")

# kwh per capita vs. percent access
ggplot(data = power, mapping = aes(access, kwh), na.rm = TRUE) + 
  geom_point(aes(colour = factor(income_group)), size = 2) + 
  coord_cartesian(ylim = c(0,25000)) + 
  labs(colour = "Income Level", 
       title = "Percent Access to Electricity vs. kWh per Capita", 
       caption = "based on most recent available data from the World Bank (2014)",
       x = "Access to Electricity(%)", 
       y = "kWh per Capita")






