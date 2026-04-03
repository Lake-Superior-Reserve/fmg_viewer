library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(sf)

# Read, clean, and standardize Lake Superior core water quality data
lake_filt <- read_csv("raw_data/lake_core.csv") %>%
  filter(source %in% c("WDNR", "NPS")) %>%
  mutate(
    site = case_when(
      site %in% c("site 1 - 10040814") ~ "Site 1",
      site %in% c("site 8 - 10052587", "10052509", "106") ~ "Site 8",
      site %in%
        c(
          "site 15 - 10054863",
          "Mawikwe Bay",
          "Mawikwe Bay OS",
          "Mawikwe Bay NS",
          "Mawikwe Beach"
        ) ~ "Mawikwe Bay",
      .default = site
    )
  ) %>%
  filter(site %in% c("Site 1", "Site 8", "Mawikwe Bay"))

# Read, filter, and standardize estuary water quality data
est_filt <- read_csv("raw_data/estuary_core.csv") %>%
  filter(source %in% c("LSNERR", "SciCollab")) %>%
  filter(site %in% c("lksba", "BASB", "BATU", "BANW")) %>%
  mutate(site == "lksba")

# Read and process Nemadji River discharge data (from 2015 onwards)
trib_nem <- read_csv("raw_data/trib_q.csv") %>%
  filter(site == "04024430") %>% #nemadji
  select(date, discharge_nemadji = discharge) %>%
  filter(year(date) > 2014)

# Read and process Bois Brule River discharge data (from 2015 onwards)
trib_bru <- read_csv("raw_data/trib_q.csv") %>%
  filter(site == "04025500") %>% # brule
  select(date, discharge_brule = discharge) %>%
  filter(year(date) > 2014)

# Read and process National Data Buoy Center (NDBC) offshore data
nbdc <- read_csv("raw_data/nbdc_daily.csv") %>%
  filter(site == "45028") %>%
  select(date, airtemp_buoy = atemp_c, watertemp = wtemp_c) %>%
  filter(year(date) > 2014)

# Read and process Port Wing buoy/station wind data
nbdc2 <- read_csv("raw_data/nbdc_daily.csv") %>%
  filter(site == "pngw3") %>%
  select(
    date,
    airtemp_wing = atemp_c,
    windspeed_lake = wspd_ms,
    winddir_lake = wdir_degt
  ) %>%
  filter(year(date) > 2014)

# Read and process estuary meteorological data
lksmet_dv <- read_csv("raw_data/lksmet_dv.csv") %>%
  select(
    date,
    airtemp_slre = airtemp,
    windspeed_slre = windspeed,
    winddir_slre = winddir,
    precip_slre = precip
  ) %>%
  filter(year(date) > 2014)

# Build the comprehensive master dataset matching daily records
water_data <- tibble(
  date = seq.Date(
    from = as.Date("2015-01-01"),
    to = as.Date("2024-12-31"),
    by = "day"
  )
) %>%
  # Merge lake and estuary sites and summarize daily averages
  left_join(bind_rows(lake_filt, est_filt), by = join_by(date)) %>%
  select(date, site, latitude, longitude, chl, temp, turb, tp) %>%
  summarise(
    across(c(latitude, longitude, chl, temp, turb, tp), ~ mean(., na.rm = T)),
    .by = c(date, site)
  ) %>%
  # Join physical/meteorological covariates
  left_join(trib_nem, by = join_by(date)) %>%
  left_join(trib_bru, by = join_by(date)) %>%
  left_join(nbdc, by = join_by(date)) %>%
  left_join(nbdc2, by = join_by(date)) %>%
  left_join(lksmet_dv, by = join_by(date)) %>%
  arrange(date) %>%
  mutate(year = year(date))

# Build a summary spatial dataframe containing explicit coordinates and clean names
water_data_loc <- tibble(
  site = c("lksba", "lkspo", "nemadji", "brule", "llo", "pngw3"),
  latitude = c(46.72177, 46.67236, 46.63333, 46.53778, 46.814, 46.792),
  longitude = c(-92.06352, -92.135614, -92.09389, -91.59528, -91.829, -91.386)
) %>%
  # Append lake filtered coordinates
  bind_rows(summarise(
    lake_filt,
    latitude = first(latitude),
    longitude = first(longitude),
    .by = site
  )) %>%
  # Assign readable map labels
  mutate(
    name = c(
      "Barker's Island - SLRE",
      "Pokegama Bay - SLRE",
      "Nemadji River",
      "Bois Brule River",
      "Offshore Buoy",
      "Port Wing",
      "Lake Superior - East",
      "Lake Superior - West",
      "Lake Superior - Mid"
    )
  )

# Export processed files for the Shiny App
write_rds(water_data, "app_data/water_data.rds")
write_rds(water_data_loc, "app_data/locations.rds")

# Process separate dataset for cyanobacteria blooms, standardizing to unified sites
ls_bloom <- read_csv("raw_data/ls_bloom.csv") %>%
  arrange(Date) %>%
  filter(year(Date) >= 2015) %>%
  filter(Lat < 47) %>%
  filter(Lon < -90.81 & Lon > -92.1) %>%
  filter(
    !(str_detect(Location, "riverine") &
      str_detect(Region, "Louis", negate = TRUE))
  ) %>%
  filter(`Spatial extent` != "Car") %>%
  filter(Location != "Inland water body in Lake Superior Basin") %>%
  mutate(
    site = case_when(
      str_detect(Location, "riverine") ~ "lksba",
      Lon < -91.7 ~ "Site 1",
      Lon < -91.2 ~ "Site 8",
      Lon > -91.2 ~ "Mawikwe Bay"
    )
  ) %>%
  summarise(.by = c(Date, site)) %>% # Aggregate unique dates by site
  rename(date = Date)

# Export the prepared blooms dataset
write_rds(ls_bloom, "app_data/blooms.rds")
