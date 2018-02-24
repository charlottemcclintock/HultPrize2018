# HultPrize2018
Data analysis of energy consumption, theft, and covariates for pitch at the Boston Regional Finals.

### Objective
Explore trends related to global electricity theft to better understand energy consumption and correlated variables to pitch a scalable, sustainable social enterprise to bring energy to 10 million people currently living in energy poverty. 

Data accessed through the [World Bank Open Data Portal](https://data.worldbank.org)
* Data Sets:
  + [Population, total](https://data.worldbank.org/indicator/SP.POP.TOTL)
  + [GDP (current US$)](https://data.worldbank.org/indicator/NY.GDP.MKTP.CD)
  + [Access to electricity (% of population)](https://data.worldbank.org/indicator/EG.ELC.ACCS.ZS)
  + [Electric power consumption (kWh per capita)](https://data.worldbank.org/indicator/EG.USE.ELEC.KH.PC)
  + [Electric power transmission and distribution losses (% of output)](https://data.worldbank.org/indicator/EG.ELC.LOSS.ZS)
  
## Exploratory Data Analysis

We are interested in what variables affect and are correlated with theft of electricity, studied through electric power and distribution losses. The variables included in our analysis are electric power transmission and distribution losses (by % of output), access to electricity (% of population), total population, GDP (total US$), and development indicators in GDP per capita, and education of girls by % of female primary school age children out of school.


## Results

To view the results, download `power_explained.html`. 

First, economic development, measured by GDP per capita and energy use, measured by kWh per capita, are highly correlated in a basic linear model. Interestingly, Iceland has the highest kWh per capita, twice as high as the next country, and will be excluded from the models for clarity. Next, looking at electric power distribution and transmission loss as a function of GDP per capita. As expected, the highest output losses occur in the lowest income countries.  Low electric power consumption (kWh per capita) is associated with high rates of loss (theft), and high electric power consumer means almost no theft. This is the most interesting trend discovered. Power consumption in kWh per capita does not rise substantially until a country has 100% electrification, and upper middle or high income levels. 
  

## Conclusion

Causation cannot be drawn from correlation, but the results are informative. Total electric power consumption only substantially increases when 100% of the population can be connected to the grid. There is substantial space in the market for investment in theft-resistant technologies.










