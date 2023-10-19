library(tidycensus)
library(tigris)
library(tidyverse)
library(sf)


tigris_cache_dir('YOUR DIRECTORY HERE')

options(tigris_use_cache = TRUE)

#census_api_key("YOUR KEY GOES HERE", install = TRUE)

## search recent variables, decennial and acs

decennial_vars <- load_variables(2020, "pl")

View(decennial_vars)


acs_detailed5 <- load_variables(2021, "acs5")

acs_detailed1 <- load_variables(2021, "acs1")

View(acs_detailed5)

View(acs_detailed1)


## get your data with tidycensus

# total population of all states

pop20 <- get_decennial(
  geography = "state",
  variables = "P1_001N",
  geometry = TRUE,
  year = 2020
) %>% 
  shift_geometry()  # shift and rescale AK, HI, and PR in a US-wide sf object

ggplot(pop20) +
  geom_sf()


# without shifting the geometry, not an ideal visual

pop20_noshift <- get_decennial(
  geography = "state",
  variables = "P1_001N",
  geometry = TRUE,
  year = 2020)

ggplot(pop20_noshift) +
  geom_sf()


# function to get specific state

state_pop_2020 <- function(defstate, defvar) {
  get_decennial(
    geography = "state",
    variables = defvar,
    #geometry = TRUE,
    state = defstate,
    year = 2020)
}

state_pop_2020(defstate = NULL, defvar = "P1_001N")

state_pop_2020("MD", "P1_001N")

state_pop_2020("MI", "P1_001N")


DMV <- c("MD", "VA", "DC")

PopGQ <- c("P1_001N", "P5_001N")

DMVpopGQ <- state_pop_2020(DMV, PopGQ)


# function to get county by state

county_pop_2020 <- function(defstate, defvar) {
  get_decennial(
    geography = "county",
    variables = defvar,
    #geometry = TRUE,
    state = defstate,
    year = 2020)
}


# count of occupied houses in WV, by county

WVcounty <- county_pop_2020("WV", "H1_002N")


# function to get tract by state

tract_pop_2020 <- function(defstate, defvar) {
  get_decennial(
  geography = "tract",
  variables = defvar,
  #geometry = TRUE,
  state = defstate,
  year = 2020) 
}


# count of Hispanic population in TX, by tract

TXtract <- tract_pop_2020("TX", "P4_001N")


# function to get block level by state

block_pop_2020  <- function(defstate, defvar) {
  get_decennial(
  geography = "block",
  variables = defvar,
  #geometry = TRUE,
  state = defstate,
  year = 2020)
}


# count of college population in UT, by block

CollegePop <- c("P1_001N", "P5_008N")

UTblock <- block_pop_2020("UT", CollegePop)


# one function 

decennial_2020 <- function(defstate, defvar, defgeo) {
  get_decennial(
    geography = defgeo,
    variables = defvar,
    #geometry = TRUE,
    state = defstate,
    year = 2020)
}


# recreate the previous objects with the new function

RegionalData <- decennial_2020(defstate = NULL, "P1_001N", "region")

DMVpopGQ_v2 <- decennial_2020(DMV, PopGQ, "state")

USPlace <- decennial_2020(defstate = NULL, "P1_001N", "place")

WVcounty_v2 <- decennial_2020("WV", "H1_002N", "county")

TXtract_v2 <- decennial_2020("TX", "P4_001N", "tract")

UTblock_v2 <- decennial_2020("UT", CollegePop, "block")


## get your shapefiles with tigris
## explore your shapefile options: https://github.com/walkerke/tigris


## United States Population

options(scipen = 999)  #disable scientific notation

statesAll <- states(cb = FALSE, resolution = "500k", year = 2021) %>%
  sf::st_transform(crs = "ESRI:102003")

StatesMap <- st_join(pop20, statesAll, by = GEOID)

ggplot() +
  geom_sf(data = StatesMap, aes(fill = value)) +
  labs(title = "Population in United States, 2020") + 
  theme(plot.title = element_text (hjust = 0.5))
  

## turn our ggplot into a function

mapping_Census <- function(defdata, deffill, deftitle) {
ggplot() +
  geom_sf(data = defdata, aes(fill = deffill)) +
  labs(title = deftitle) + 
  theme(plot.title = element_text (hjust = 0.5))
}


## regions

USregions_2021 <- regions() %>%
  sf::st_transform(crs = "ESRI:102003")

RegionalPop <- left_join(RegionalData, USregions_2021, by = "GEOID") %>%
  st_as_sf()

mapping_Census(defdata = RegionalPop, deffill = RegionalPop$value, 
               deftitle = "Regional Population in 2020")


## places

DMVplaces_2021 <- places(DMV) %>%
  sf::st_transform(crs = "ESRI:102003")

DMVPlacesPop <- left_join(USPlace, DMVplaces_2021, by = "GEOID") %>%
  st_as_sf()

places1 <- mapping_Census(defdata = DMVPlacesPop, deffill = DMVPlacesPop$value, 
               deftitle = "US Places Population in 2020") 

DMV_outline <- states(cb = TRUE) %>%
  filter(NAME %in% c("District of Columbia", "Maryland", "Virginia"))

ggplot() +
  geom_sf(data = DMV_outline) +
  geom_sf(data = DMVPlacesPop, aes(fill = DMVPlacesPop$value)) +
  labs(title = "DMV Place Population in 2020") + 
  theme(plot.title = element_text (hjust = 0.5)) 


## county: occupied houses in WV

WVCounty_2021 <- counties("WV") %>%
  sf::st_transform(crs = "ESRI:102003")

WVCountyPop <- left_join(WVcounty_v2, WVCounty_2021, by = "GEOID") %>%
  st_as_sf()

WVOutline <-  states(cb = TRUE) %>%
  filter(NAME %in% c("West Virginia"))
  
ggplot() +
  geom_sf(data = WVOutline) +
  geom_sf(data = WVCountyPop, aes(fill = WVCountyPop$value)) +
  labs(title = "WV Occupied Housing in 2020") + 
  theme(plot.title = element_text (hjust = 0.5))+
  scale_fill_gradient(low = '#FFD700', high = '#27408B', na.value = '#FFFAFA' )


## tracts

TXtracts_2021 <- tracts("TX") %>%
  sf::st_transform(crs = "ESRI:102003")

TXTractPop <- left_join(TXtract_v2, TXtracts_2021, by = "GEOID") %>%
  st_as_sf()

TXOutline <-  states(cb = TRUE) %>%
  filter(NAME %in% c("Texas"))

ggplot() +
  geom_sf(data = TXOutline) +
  geom_sf(data = TXTractPop, aes(fill = TXTractPop$value)) +
  labs(title = "TX Hispanic Population in 2020") + 
  theme(plot.title = element_text (hjust = 0.5))+
  scale_fill_gradient(low = '#FFD700', high = '#B03060', na.value = '#FFFAFA' )


## blocks

UTblocks_2021 <- blocks("UT") %>%
  sf::st_transform(crs = "ESRI:102003")

UTBlockPop <- left_join(UTblock_v2, UTblocks_2021, join_by("GEOID" == "GEOID20")) %>%
  st_as_sf()

UTOutline <-  states(cb = TRUE) %>%
  filter(NAME %in% c("Utah"))

ggplot() +
  geom_sf(data = UTOutline) +
  geom_sf(data = UTBlockPop, aes(fill = UTBlockPop$value)) +
  labs(title = "UT College Population in 2020") + 
  theme(plot.title = element_text (hjust = 0.5))+
  scale_fill_gradient(low = '#8A8D8F', high = '#00263A', na.value = '#FFFAFA')


## write to a shapefile you can use in ArcGIS, Tableau, or elsewhere:

st_write(UTBlockPop, "UTBlockPop.shp")


## other helpful functions in tigris

list_counties("MD")

places("Maryland") %>% list_places()

block_groups("Maryland")


## pumas: public use microdata areas

pumas_2021 <- function(defStatePuma){
  pumas(defStatePuma)
}

pumas_2021("MD")

ggplot(pumas_2021("MI")) +
  geom_sf()


## school districts

ggplot(school_districts("MD")) +
  geom_sf()


## landmarks

ggplot(landmarks("MD")) +
  geom_sf()






