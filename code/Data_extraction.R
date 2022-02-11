###
#
# Extracting remotely sensed data
# to csv
###

rm(list = ls())
std <- function(x) sd(x)/sqrt(length(x))

library(tidyverse)
library(sf)
library(raster)
library(exactextractr)
library(lubridate)

temps <- stack("data/MN_7_COUNTY_WINTER_TEMP.tif")
st_crs(temps)

dat <- read.csv('data/JB_raw_data.csv') %>% 
  dplyr::select(1:7,9,10) %>%
  rename(instar_1st = X1st..instar,
         instar_2nd = X2nd.instar,
         instar_3rd = X3rd.instar) %>%
  group_by(location, date) %>%
  mutate(mean = mean(total)) 

dat_2 <- SpatialPointsDataFrame(dat[,c("Longitude", "Latitude")],dat)

proj4string(dat_2) <- CRS("+proj=longlat +datum=WGS84")

# raster


names <- names(temps)
names <- gsub('_soil_temperature_level_2', '', names)

names(temps) <- names

x2016_x2017 <- raster::subset(temps, c("X201612","X201701","X201702","X201703"), value = T) %>% mean()
x2017_x2018 <- raster::subset(temps, c("X201712","X201801","X201802","X201803"), value = T) %>% mean()
x2018_x2019 <- raster::subset(temps, c("X201812","X201901","X201902","X201903"), value = T) %>% mean()
x2019_x2020 <- raster::subset(temps, c("X201912","X202001","X202002","X202003"), value = T) %>% mean()
x2020_x2021 <- raster::subset(temps, c("X202012","X202101","X202102","X202103"), value = T) %>% mean()

stacked <- raster::stack(x2016_x2017,x2017_x2018,x2018_x2019,x2019_x2020,x2020_x2021)
projected_raster <- projectRaster(stacked, crs = "+proj=utm +zone=15 +ellps=GRS80 +datum=NAD83 +units=m +no_defs")

names(stacked) <- c("x2016_x2017","x2017_x2018","x2018_x2019","x2019_x2020","x2020_x2021")

plot(stacked,axes = FALSE,legend=FALSE)


compareCRS(stacked, dat_2)

bbox(dat_2)



season_2018 <- dat_2 %>% st_as_sf() %>%
  mutate(date = as.Date(parse_date_time(date,orders="mdy")),
         year = year(date)) %>%
  filter(year == "2018")

season_2019 <- dat_2 %>% st_as_sf() %>%
  mutate(date = as.Date(parse_date_time(date,orders="mdy")),
         year = year(date)) %>%
  filter(year == "2019")

season_2020 <- dat_2 %>% st_as_sf() %>%
  mutate(date = as.Date(parse_date_time(date,orders="mdy")),
         year = year(date)) %>%
  filter(year == "2020")

season_2021 <- dat_2 %>% st_as_sf() %>%
  mutate(date = as.Date(parse_date_time(date,orders="mdy")),
         year = year(date)) %>%
  filter(year == "2021")

names_2018 <- c("x2016_x2017","x2017_x2018")
names_2019 <- c("x2016_x2017","x2017_x2018","x2018_x2019")
names_2020 <- c("x2017_x2018","x2018_x2019","x2019_x2020")
names_2021 <- c("x2018_x2019","x2019_x2020","x2020_x2021")

rasters_2018 <- raster::stack(x2016_x2017,x2017_x2018)
rasters_2019 <- raster::stack(x2016_x2017,x2017_x2018,x2018_x2019)
rasters_2020 <- raster::stack(x2017_x2018,x2018_x2019,x2019_x2020)
rasters_2021 <- raster::stack(x2018_x2019,x2019_x2020,x2020_x2021)

names(rasters_2018) <- names_2018
names(rasters_2019) <- names_2019
names(rasters_2020) <- names_2020
names(rasters_2021) <- names_2021


extract_2018 <- extract(rasters_2018,season_2018,method="simple",sp=TRUE) %>%
  st_as_sf() %>%
  st_drop_geometry() %>%
  pivot_longer(cols=starts_with("x"),names_to = "winter",values_to = "temperatures")

extract_2019 <- extract(rasters_2019,season_2019,method="simple",sp=TRUE) %>% 
  st_as_sf() %>%
  st_drop_geometry() %>%
  pivot_longer(cols=starts_with("x"),names_to = "winter",values_to = "temperatures")

extract_2020 <- extract(rasters_2020,season_2020,method="simple",sp=TRUE) %>%
  st_as_sf() %>%
  st_drop_geometry() %>%
  pivot_longer(cols=starts_with("x"),names_to = "winter",values_to = "temperatures")

extract_2021 <- extract(rasters_2021,season_2021,method="simple",sp=TRUE) %>% 
  st_as_sf() %>%
  st_drop_geometry() %>%
  pivot_longer(cols=starts_with("x"),names_to = "winter",values_to = "temperatures")


extraction <- rbind(extract_2018,extract_2019,extract_2020,extract_2021) %>%
  pivot_wider(names_from = winter,values_from = temperatures)

extraction%>%
  mutate(month = month(date), year = year(date),
         pre_post = case_when(
           month == 10 ~ "October",
           month == 4 ~ "April",
           month == 5 ~ "May",
         )) %>% 
  group_by(location,year,pre_post) %>% dplyr::select(!date) %>%
  summarise(JB_mean = mean(total)) %>%
  pivot_wider(names_from = pre_post,values_from = JB_mean)



write.csv(extraction,file="data/processed/raw_JB_data_winter_temps.csv")

