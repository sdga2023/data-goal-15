library(tidyr)
library(dplyr)
library(WDI)
library(readxl)
library(countrycode)
library(wbstats)
library(ggplot2)
library(jsonlite)

countries <- wbstats::wb_countries()

# Country surface areas
areas.raw <- WDI(indicator = 'AG.SRF.TOTL.K2', extra = TRUE)
areas <- filter(areas.raw, region !="Aggregates", year == 2018) %>%
  select(iso3c, area = AG.SRF.TOTL.K2)
write.csv(areas, file = "../output/countryareas.csv", row.names = FALSE)

# Country level tree cover loss
treeloss.raw <- read_excel("../input/global.xlsx", sheet = "Country tree cover loss")?
treeloss <- filter(treeloss.raw, threshold == 30) %>%
  select(-threshold, -area_ha, -extent_2000_ha, -extent_2010_ha, -`gain_2000-2012_ha`) %>%
  pivot_longer(cols = 2:22, names_to = "year", values_to = "loss") %>%
  mutate(year = gsub("tc_loss_ha_", "", year)) %>%
  mutate(iso3c = countrycode(country, origin = "country.name", destination = "iso3c")) %>%
  filter(!is.na(iso3c)) %>%
  select(iso3c, year, loss)
write.csv(treeloss, file = "../output/treecoverlosscountries.csv", row.names = FALSE)

# Red list index
rli.raw <- read_excel("../input/ER_RSK_LST.xlsx", sheet = "Record format")
countries.regions <- select(countries, iso3c, region_iso3c)

rli <- select(rli.raw, GeoAreaName, TimePeriod, Value) %>%
  filter(Value != "NaN") %>%
  mutate(Value = as.numeric(Value)) %>%
  mutate(iso3c = countrycode(GeoAreaName, origin = "country.name", destination = "iso3c")) %>%
  filter(!is.na(iso3c)) %>%
  select(iso3c, year = TimePeriod, redlistindex = Value) %>%
  filter(year %in% c(2000, 2022)) %>%
  left_join(countries.regions, by = "iso3c") %>%
  filter(!is.na(region_iso3c)) %>%
  pivot_wider(names_from = year, values_from = redlistindex) %>%
  mutate(trend = `2022` - `2000`) %>%
  mutate(class = cut(trend, breaks = c(-0.26, -0.05, -0.02, 0, 0.03)))

colnames(rli) <- make.names(colnames(rli))
write.csv(rli, file = "../output/redlistindex.csv", row.names = FALSE)

# Tree cover by country
share.forests.raw <- read_excel("../input/AG_LND_FRST.xlsx")
share.forests <- select(share.forests.raw, GeoAreaName, `2000`, `2020`)
share.forests$iso3c <- countrycode(share.forests$GeoAreaName, origin = "country.name", destination = "iso3c")
share.forests <- mutate(share.forests, iso3c = ifelse(GeoAreaName == "Türkiye", "TUR", iso3c))
# Add World
share.forests <- mutate(share.forests, iso3c = ifelse(GeoAreaName == "World", "WLD", iso3c))
colnames(share.forests) <- c("country", "forests_2000", "forests_2020", "iso3c")
share.forests.ok <- select(share.forests, iso3c, forests_2000, forests_2020) %>%
  filter(!is.na(iso3c)) %>%
  filter(forests_2000 != "NaN") %>%
  mutate(forests_2000 = round(as.numeric(forests_2000), 1)) %>%
  mutate(forests_2020 = round(as.numeric(forests_2020), 1)) %>%
  mutate(trend = round((forests_2020 - forests_2000)/forests_2000*100,1))
write.csv(share.forests.ok, file = "../output/forest_shares.csv", row.names = FALSE)

# Driver by region
regions <- c("LCN", "EAS", "SAS", "NAC", "SSF", "ECS", "MEA")
drivers <- read_excel("../input/driver_by_region_new.xlsx", sheet="share_by_region_all_years")
drivers <- mutate(drivers, region = recode(region,
                                           "East Asia & Pacific" = "EAS",
                                           "Europe & Central Asia" = "ECS",
                                           "Latin America & Caribbean" = "LCN",
                                           "Middle East & North Africa" = "MEA",
                                           "North America" = "NAC",
                                           "South Asia" = "SAS",
                                           "Sub-Saharan Africa" = "SSF"
)) %>%
  mutate(region = factor(region, levels = regions)) %>%
  arrange(region) %>%
  mutate(driver = recode(tree_cover_loss_drivers,
                         "Commodity driven deforestation" = "commodity",
                         "Forestry" = "forestry",
                         "Shifting agriculture" = "shiftingagriculture",
                         "Urbanization" = "urbanization",
                         "Unknown" = "unknown",
                         "Wildfire" = "wildfire"
  )) %>%
  mutate(driver = factor(driver, levels = c("forestry", "wildfire", "shiftingagriculture", "commodity", "urbanization", "unknown"))) %>%
  arrange(region, driver) %>%
  filter(region != "MEA") %>%
  select(region, driver, share = percent)
write.csv(drivers, file = "../output/drivers_cover_loss.csv", row.names = FALSE)

# Key biodiversity drivers
mnt.raw <- read_excel("../input/ER_PTD_MNT.xlsx", sheet = "Goal15")
mnt <- filter(mnt.raw, GeoAreaName == "World") %>%
  select(SeriesCode, TimePeriod, Value) %>%
  mutate(Value = round(as.numeric(Value), 1))

terr_wtr.raw <- read_excel("../input/ER_PTD_FRHWTR-ER_PTD_TERR.xlsx", sheet = "data")
terr_wtr <- filter(terr_wtr.raw, GeoAreaName == "World") %>%
  select(SeriesCode, TimePeriod, Value) %>%
  mutate(Value = round(as.numeric(Value), 1))

mnt_terr_wtr <- rbind(mnt, terr_wtr)
colnames(mnt_terr_wtr) <- c("code", "year", "value")
write.csv(mnt_terr_wtr, file = "../output/kbas.csv", row.names = FALSE)
