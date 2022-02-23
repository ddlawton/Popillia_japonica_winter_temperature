###
# Reformatting
#   Soil temperature loggers
#    to compare with remotely
#     sensed data
###
rm(list=ls())

library(tidyverse)
library(lubridate)
library(readxl)

# Avg Monthly Soil Temp 1964-2021 SROC-Waseca MN-evidence of warming

years <- (seq(from=1964,to=2021,by=1)) #using this to get the actual data in this spreadsheet
mm <- c("Jan.","Feb.","Mar.","Apr.","May","June","July","Aug.","Sept.","Oct.","Nov.","Dec.")
mm <- gsub("\\.", "", mm)
mm <- substr(mm,1,3)
sapply(mm,function(x) grep(paste("(?i)",x,sep=""),month.abb))

unique(levels(dat$month))
F_to_C <- function(temp_F) {
  temp_C <- (temp_F - 32) * 5 / 9
  return(temp_C)
} #converting fahrenheit to celsius


data_reformating <- function(direct,sheet,depth) {
  read_excel(direct,sheet=sheet,skip=2) %>%
    filter(Year %in% years) %>% 
    select(!c(Annual,...2)) %>%
    pivot_longer(cols = c(!Year),names_to = "month",values_to = "temps") %>%
    mutate(temps = as.numeric(temps), Year = as.integer(Year), month = as.factor(month)) %>%
    drop_na(temps) %>%
    mutate(temps = F_to_C(temps), depth = depth)
}

dat_5cm <- data_reformating("data/Avg Monthly Soil Temp 1964-2021 SROC-Waseca MN-evidence of warming.xls",sheet=1,5)
dat_10cm <- data_reformating("data/Avg Monthly Soil Temp 1964-2021 SROC-Waseca MN-evidence of warming.xls",sheet=2,10)
dat_20cm <- data_reformating("data/Avg Monthly Soil Temp 1964-2021 SROC-Waseca MN-evidence of warming.xls",sheet=3,20)
dat_50cm <- data_reformating("data/Avg Monthly Soil Temp 1964-2021 SROC-Waseca MN-evidence of warming.xls",sheet=4,50)
dat_100cm <- data_reformating("data/Avg Monthly Soil Temp 1964-2021 SROC-Waseca MN-evidence of warming.xls",sheet=5,100)

dat <- rbind(dat_5cm,dat_10cm,dat_20cm,dat_50cm,dat_100cm) %>%
  mutate(month = sapply(substr(month,1,3),function(x) grep(paste("(?i)",x,sep=""),month.abb)), #switch the annoying month column to integers
         depth = factor(depth), ERA_depth = case_when(
           depth == "5" ~ "soil_temperature_level_1",
           depth == "10" ~ "soil_temperature_level_2",
           depth == "20" ~ "soil_temperature_level_2",
           depth == "50" ~ "soil_temperature_level_3",
           depth == "100" ~ "soil_temperature_level_3"),
         location = "rural") %>%
  rename(year = Year,temp = temps)


Logger_UMB_dat <- read_excel("data/StPaulSOilTemp1996_2015_Clean-checked-Ebbenga-wdh.xlsx",sheet=1)[,1:8] %>%
  pivot_longer(cols = starts_with("Average"),names_to = "depth", values_to = "temp") %>%
  mutate(depth = factor(parse_number(depth))) %>%
  group_by(year,month,depth) %>%
  summarize(temp = mean(temp)) %>%
  mutate(ERA_depth = case_when(
    depth == "1" ~ "soil_temperature_level_1",
    depth == "5" ~ "soil_temperature_level_2",
    depth == "10" ~ "soil_temperature_level_2",
    depth == "20" ~ "soil_temperature_level_3",
    depth == "40" ~ "soil_temperature_level_3"
  ),
  location = "urban")

combined_logger_dat <- rbind(Logger_UMB_dat,dat)
  
  
# Relating logger data to remotely sensed data

ERA_dat <- read_csv("data/MN_temps_all.csv") %>% #reading in the data and getting it into the format of the logger data
  mutate(date = substr(`system:index`,1,6)) %>% #system.index is from google earth engine and it is YYYYMM.
  select(!c(.geo,`system:index`)) %>%
  separate(date,into=c("Year","month"),sep=4) %>%
  mutate(month = as.integer(month), Year = as.integer(Year)) %>%
  pivot_longer(cols=starts_with("soil"),names_to = "ERA_depth",values_to = "ERA_temps") %>%
  mutate(location = "rural")

RS_UMN_dat <- read_csv("data/UMN_temps.csv")  %>% #reading in the data and getting it into the format of the logger data
  mutate(date = substr(`system:index`,1,6)) %>% #system.index is from google earth engine and it is YYYYMM.
  select(!c(.geo,`system:index`)) %>%
  separate(date,into=c("Year","month"),sep=4) %>%
  mutate(month = as.integer(month), Year = as.integer(Year)) %>%
  pivot_longer(cols=starts_with("soil"),names_to = "ERA_depth",values_to = "ERA_temps") %>%
  mutate(location = "urban") %>%
  select(!name)

RS_dat <- rbind(ERA_dat,RS_UMN_dat) %>%
  rename(year = Year)


# now combining the two

combined_dat <- combined_logger_dat %>% left_join(RS_dat,by=c("location","year","month","ERA_depth"))

write.csv(combined_dat,file="data/processed/ERA_logger_data_validation_site_comparison_Feb232022.csv")







