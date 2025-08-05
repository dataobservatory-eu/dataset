library(eurostat)
library(tidyverse)
gdp_raw <- get_eurostat("naida_10_gdp")
gdp <- gdp_raw %>%
  filter(geo %in% c("AD", "LI", "SM")) %>%
  filter(TIME_PERIOD >= as.Date("2020-01-01")) %>%
  filter(unit == "CP_MEUR", na_item == "B1G") %>%
  mutate(year = as.integer(as.character(substr(TIME_PERIOD, 1, 4)))) %>%
  select(geo, year, gdp = values, unit, freq)

gdp_fo <- gdp_raw %>%
  filter(geo %in% c("FO")) %>%
  filter(TIME_PERIOD >= as.Date("2020-01-01")) %>%
  filter(unit == "CP_MEUR", na_item == "B1G") %>%
  mutate(year = as.integer(as.character(substr(TIME_PERIOD, 1, 4)))) %>%
  select(geo, year, gdp = values, unit, freq)

gdp_fo$gdp
usethis::use_data(gdp)

defined(
  x = gdp_dataset$gdp,
  label = "Gross Domestic Product",
  unit = gdp_dataset$unit[1],
  definition = "http://data.europa.eu/83i/aa/GDP"
)


defined(
  x = gdp_dataset$geo,
  label = "Country",
  definition = "http://dd.eionet.europa.eu/vocabulary/eurostat/geo/"
)

defined(
  x = gdp_dataset$freq,
  definition = "http://purl.org/linked-data/sdmx/2009/code"
)
defined(
  x = gdp_dataset$year,
  definition = "http://purl.org/linked-data/sdmx/2009/dimension#refPeriod"
)
