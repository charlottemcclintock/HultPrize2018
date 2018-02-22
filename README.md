# HultPrize2018
Data analysis of energy consumption, theft, and covariates for pitch at the Boston Regional Finals.

### Goal: 
Explore trends related to global electricity theft to better understand energy consumption and correlated variables to pitch a scalable, sustainable social enterprise to bring energy to 10 million people currently living in energy poverty. 

Data accessed through the [World Bank Open Data Portal](https://data.worldbank.org)
* Data Sets:
  + [Population, total](https://data.worldbank.org/indicator/SP.POP.TOTL)
  + [GDP (current US$)](https://data.worldbank.org/indicator/NY.GDP.MKTP.CD)
  + [Access to electricity (% of population)](https://data.worldbank.org/indicator/EG.ELC.ACCS.ZS)
  + [Electric power consumption (kWh per capita)](https://data.worldbank.org/indicator/EG.USE.ELEC.KH.PC)
  + [Electric power transmission and distribution losses (% of output)](https://data.worldbank.org/indicator/EG.ELC.LOSS.ZS)
  
  
  
```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, message=FALSE, warning = FALSE)
setwd("/Users/charmed33/R/HultPrize2018")
library(tidyverse)
library(dplyr)
```

```{r set, echo = FALSE}
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
```


## Goal
> Explore trends related to global electricity theft to better understand energy consumption and correlated variables to pitch a scalable, sustainable social enterprise to bring energy to 10 million people currently living in energy poverty. 

## Data

Data accessed through the [World Bank Open Data Portal](https://data.worldbank.org)

* Data Sets:
    + [Population, total](https://data.worldbank.org/indicator/SP.POP.TOTL)
     + [GDP (current US$)](https://data.worldbank.org/indicator/NY.GDP.MKTP.CD)
      + [Access to electricity (% of population)](https://data.worldbank.org/indicator/EG.ELC.ACCS.ZS)
    + [Electric power consumption (kWh per capita)](https://data.worldbank.org/indicator/EG.USE.ELEC.KH.PC)
     + [Electric power transmission and distribution losses (% of output)](https://data.worldbank.org/indicator/EG.ELC.LOSS.ZS)
     + [Children out of school, female (% of female primary school age)](https://data.worldbank.org/indicator/SE.PRM.UNER.FE.ZS?view=chart)
     
## Exploratory Data Analysis

We are interested in what variables affect and are correlated with electric power and distribution losses. The variables included in our analysis are electric power transmission and distribution losses (by % of output), access to electricity (% of population), total population, GDP (total US$), and development indicators in GDP per capita, and education of girls by % of female primary school age children out of school.

### GDP per Capita vs. kWh per Capita

 First, economic development, measured by GDP per capita and energy use, measured by kWh per capita, are highly correlated in a basic linear model. Interestingly, Iceland has the highest kWh per capita, twice as high as the next country, and will be excluded from the models for clarity.
  
```{r plot 1, echo = FALSE}

ggplot(data = power, mapping = aes(gdp_percap, kwh), na.rm = TRUE) + 
  geom_point(aes(colour = factor(income_group)), size = 2) + 
  geom_smooth(method = "lm", se = FALSE) + coord_cartesian(xlim = c(0,125000), 
  ylim=c(0,25000)) + labs(colour = "Income Level", 
       title = "GDP per Capita vs. kWh per Capita", 
       caption = "based on most recent available data from the World Bank (2014)",
       x = "GDP per Capita", 
       y = "kWh per Capita")
```








