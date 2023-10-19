## load your libraries

library(dplyr)
library(ggplot2)
library(gapminder)

# for esquisse

library(esquisse)
library(shiny)

# for mapping with shapefiles

library(tidycensus)
library(tigris)
library(sf)


## gapminder data EDA

head(gapminder)

str(gapminder)

colnames(gapminder)

table(gapminder$year)

table(gapminder$continent)

table(gapminder$country)

range(gapminder$lifeExp)

range(gapminder$gdpPercap)

range(gapminder$pop)


## gapminder data for visualization

WorldPop1 <- gapminder %>%
  group_by(year) %>%
  summarise(total = sum(pop))


WorldPop <- gapminder %>%
  group_by(as.factor(year)) %>%
  summarise(total = sum(pop))


USGapminder <- gapminder %>%
  filter(country == "United States") %>%
  mutate(across(c(year),factor))


## visualize with esquisse- gapminder

# viz 1 gapminder
esquisser(WorldPop1)

ggplot(WorldPop1) +
 aes(x = year, y = total) +
 geom_line(colour = "#112446") +
 labs(y = "Population", 
 title = "World Population 1952:2007", 
 caption = "Source: Gapminder") +
 theme_minimal() +
 theme(plot.title = element_text(size = 16L, 
 hjust = 0.5))


# viz 2 gapminder
esquisser(WorldPop)  #with year as factor

ggplot(WorldPop) +
  aes(x = `as.factor(year)`, y = total, fill = total) +
  geom_col() +
  scale_fill_gradient(low = "#FFF5F0", 
                      high = "#67000D") +
  labs(title = "World Population 1952:2007") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16L, 
                                  hjust = 0.5))


# viz 3, open in a browser
esquisser(USGapminder)

#esquisser(USGapminder, viewer = "browser")

# viz 3
esquisser(gapminder)

gapminder %>%
  filter(lifeExp >= 33.5 & lifeExp <= 73.5) %>%
  filter(pop >= 50000000L & pop <= 1000000000L) %>%
  group_by(continent) %>%
  arrange(desc(lifeExp)) %>%
  ggplot() +
  aes(x = country, fill = continent, weight = lifeExp) +
  geom_bar() +
  scale_fill_manual(values = c(Africa = "#000004", 
                               Americas = "#501379", Asia = "#B63778", 
                               Europe = "#FA8764", Oceania = "#FCFDBF")) +
  labs(x = "Country", 
       y = "Life Expectancy", 
       title = "Life Expectancy for Countries", 
       subtitle = "Population > 50M and < 1B ", 
       fill = "Continent") +
  coord_flip() +
  ggthemes::theme_tufte() +
  theme(plot.title = element_text(size = 15L, 
                                  hjust = 0.5), 
        plot.subtitle = element_text(hjust = 0.5))


## tidycensus data

#census_api_key("YOUR KEY GOES HERE", install = TRUE)

## decennial variables

decennial_vars <- load_variables(2020, "pl")

View(decennial_vars)


## tidycensus function 

decennial_2020 <- function(defstate, defvar, defgeo) {
  get_decennial(
    geography = defgeo,
    variables = defvar,
    #geometry = TRUE,
    state = defstate,
    year = 2020)
}


## datasets for visualization

RegionalData <- decennial_2020(defstate = NULL, "P1_001N", "region")

StateData <- decennial_2020(defstate = NULL, "P1_001N", "state")

DMV <- c("MD", "VA", "DC")

PopGQ <- c("P1_001N", "P5_001N")

USPopGroupQtr <- decennial_2020(defstate = NULL, PopGQ, "state")

PopOccupied <- c("P1_001N", "H1_002N", "H1_003N")

USPlace <- decennial_2020(defstate = NULL, PopOccupied, "place")

WVcounty <- decennial_2020("WV", PopOccupied, "county")

TXtract <- decennial_2020("TX", CollegePop, "tract")




## visualize it with esquisse

options(scipen = 999)

esquisser(RegionalData)

esquisser(StateData)

esquisser(USPopGroupQtr)

esquisser(USPlace)

esquisser(WVcounty)

esquisser(TXtract)

