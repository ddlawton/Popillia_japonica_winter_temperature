library(tidyverse)
library(lubridate)
library(raster)
library(stars)
library(rnaturalearth)
library(sf)
library(patchwork)


#####
# Counties and temps


#county_list <- list('27003', '27019', '27037','27053','27123','27139','27163')

temps <- stack("data/MN_7_COUNTY_WINTER_TEMP.tif")
yearstoadd <- c("1984","1989","1994","1999","2004","2009","2014","2019")
years <- seq(1985,2020,by=5)
years2 <- rep(years, each=3)
years3 <- c(yearstoadd,years2)
years4 <- sort(yearss3)

years5 <- data.frame(years4) %>%
  mutate(months = rep(c("12","01","02","03"),times=8),
         combined=paste0(years4,months))



raster_bands <- years5$combined

raster_bands3 <- paste0("X", raster_bands)


MN <- ne_states(country="United States of America") %>% st_as_sf() %>%
  filter(gn_name %in% c("Minnesota","Wisconsin")) %>% dplyr::select(gn_name)

MN_counties <- st_read("data/County_shapefile/cb_2018_us_county_20m.shp") %>% dplyr::select(5,6)# %>%
#filter(GEOID %in% county_list) %>% 


MN_counties <- st_transform(MN_counties, crs = proj4string(temps))


###
# Raster



e <- extent(temps)
# coerce to a SpatialPolygons object
p <- as(e, 'SpatialPolygons') %>% st_as_sf()

p2 <- p %>% st_set_crs(st_crs(MN_counties)) 

MN_counties <- st_intersection(MN_counties, p2)



#plot(temps)
names <- names(temps)
names <- gsub('_soil_temperature_level_2', '', names)

names(temps) <- names


x1985 <- raster::subset(temps,raster_bands3[1:4], value = T) %>% mean()
x1990 <- raster::subset(temps,raster_bands3[5:8], value = T) %>% mean()
x1995 <- raster::subset(temps,raster_bands3[9:12], value = T) %>% mean()
x2000 <- raster::subset(temps,raster_bands3[13:16], value = T) %>% mean()
x2005 <- raster::subset(temps,raster_bands3[17:20], value = T) %>% mean()
x2010 <- raster::subset(temps,raster_bands3[21:24], value = T) %>% mean()
x2015 <- raster::subset(temps,raster_bands3[25:28], value = T) %>% mean()
x2020 <- raster::subset(temps,raster_bands3[29:32], value = T) %>% mean()







x1985 <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(x1985), 
                                      as_points = FALSE, merge = TRUE)
) %>% st_as_sf() %>% rename(`Temperature (°C)` = layer)

x1990 <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(x1990), 
                                      as_points = FALSE, merge = TRUE)
) %>% st_as_sf()  %>% rename(`Temperature (°C)` = layer)

x1995 <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(x1995), 
                                      as_points = FALSE, merge = TRUE)
) %>% st_as_sf()  %>% rename(`Temperature (°C)` = layer)

x2000 <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(x2000), 
                                      as_points = FALSE, merge = TRUE)
) %>% st_as_sf()  %>% rename(`Temperature (°C)` = layer)

x2005 <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(x2005), 
                                      as_points = FALSE, merge = TRUE)
) %>% st_as_sf() %>% rename(`Temperature (°C)` = layer)

x2010 <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(x2010), 
                                     as_points = FALSE, merge = TRUE)
) %>% st_as_sf() %>% rename(`Temperature (°C)` = layer)

x2015 <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(x2015), 
                                     as_points = FALSE, merge = TRUE)
) %>% st_as_sf() %>% rename(`Temperature (°C)` = layer)

x2020 <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(x2020), 
                                     as_points = FALSE, merge = TRUE)
) %>% st_as_sf() %>% rename(`Temperature (°C)` = layer)





x1990_graph <- ggplot() +  
  geom_sf(data=x1990, aes(fill=`Temperature (°C)`), lwd = 0,color = NA)  +
  viridis::scale_fill_viridis(option="magma",limits = c(-6,3)) +
  geom_sf(data=MN_counties,fill="transparent",color="grey",size=1)  +
  xlab("Longitude") + ylab("Latitude") +
  ggpubr::theme_pubr() + ggtitle("1990")  + theme(legend.position = "none") +
  scale_y_continuous(breaks = c(44.0,45.0,46.0)) +
  scale_x_continuous(breaks = c(-92.0,-93.0,-94.0,-95.0))




x1995_graph <- ggplot() +  
  geom_sf(data=x1995, aes(fill=`Temperature (°C)`), lwd = 0,color = NA)  +
  viridis::scale_fill_viridis(option="magma",limits = c(-6,3)) +
  geom_sf(data=MN_counties,fill="transparent",color="grey",size=1)  +
  xlab("Longitude") + ylab("Latitude") +
  ggpubr::theme_pubr() + ggtitle("1995")  + theme(legend.position = "none")  +
  scale_y_continuous(breaks = c(44.0,45.0,46.0)) +
  scale_x_continuous(breaks = c(-92.0,-93.0,-94.0,-95.0))



x2000_graph <- ggplot() +  
  geom_sf(data=x2000, aes(fill=`Temperature (°C)`), lwd = 0,color = NA)  +
  viridis::scale_fill_viridis(option="magma",limits = c(-6,3)) +
  geom_sf(data=MN_counties,fill="transparent",color="grey",size=1)  +
  xlab("Longitude") + ylab("Latitude") +
  ggpubr::theme_pubr() + ggtitle("2000")  + theme(legend.position = "none")  +
  scale_y_continuous(breaks = c(44.0,45.0,46.0)) +
  scale_x_continuous(breaks = c(-92.0,-93.0,-94.0,-95.0))



x2005_graph <- ggplot() +  
  geom_sf(data=x2005, aes(fill=`Temperature (°C)`), lwd = 0,color = NA)  +
  viridis::scale_fill_viridis(option="magma",limits = c(-6,3)) +
  geom_sf(data=MN_counties,fill="transparent",color="grey",size=1)  +
  xlab("Longitude") + ylab("Latitude") +
  ggpubr::theme_pubr() + ggtitle("2005") +
  scale_y_continuous(breaks = c(44.0,45.0,46.0)) +
  scale_x_continuous(breaks = c(-92.0,-93.0,-94.0,-95.0)) + theme(legend.position = "none") 


x2010_graph <- ggplot() +  
  geom_sf(data=x2010, aes(fill=`Temperature (°C)`), lwd = 0,color = NA)  +
  viridis::scale_fill_viridis(option="magma",limits = c(-6,3)) +
  geom_sf(data=MN_counties,fill="transparent",color="grey",size=1)  +
  xlab("Longitude") + ylab("Latitude") +
  ggpubr::theme_pubr() + ggtitle("2010") + theme(legend.position = "none")  +
  scale_y_continuous(breaks = c(44.0,45.0,46.0)) +
  scale_x_continuous(breaks = c(-92.0,-93.0,-94.0,-95.0)) + theme(legend.position="bottom") 


x2015_graph <- ggplot() +  
  geom_sf(data=x2015, aes(fill=`Temperature (°C)`), lwd = 0,color = NA)  +
  viridis::scale_fill_viridis(option="magma",limits = c(-6,3)) +
  geom_sf(data=MN_counties,fill="transparent",color="grey",size=1)  +
  xlab("Longitude") + ylab("Latitude") +
  ggpubr::theme_pubr() + ggtitle("2015") + theme(legend.position = "none")  +
  scale_y_continuous(breaks = c(44.0,45.0,46.0)) +
  scale_x_continuous(breaks = c(-92.0,-93.0,-94.0,-95.0))



x2020_graph <- ggplot() +  
  geom_sf(data=x2020, aes(fill=`Temperature (°C)`), lwd = 0,color = NA)  +
  viridis::scale_fill_viridis(option="magma",limits = c(-6,3)) +
  geom_sf(data=MN_counties,fill="transparent",color="grey",size=1)  +
  xlab("Longitude") + ylab("Latitude") +
  ggpubr::theme_pubr() + ggtitle("2020") + theme(legend.position = "none")  +
  scale_y_continuous(breaks = c(44.0,45.0,46.0)) +
  scale_x_continuous(breaks = c(-92.0,-93.0,-94.0,-95.0))




MN_graph <- 
  ggplot() +  
  geom_sf(data=MN, fill="grey")  +
  geom_sf(data=MN_counties,aes(fill=NAME),color="black",size=1)  +
  xlab("Longitude") + ylab("Latitude") +
  ggpubr::theme_pubr() + theme(legend.position = "none") +
  viridis::scale_fill_viridis(option="C",discrete=TRUE)

layout <- "
ABCD
EFGH
"

together <- x1990_graph + x1995_graph + x2000_graph + x2005_graph + x2010_graph  + x2015_graph + x2020_graph + MN_graph +
  plot_annotation(title = 'Winter (December - March) Soil Temperatures (7-28 cm)') +
  plot_layout(design = layout)





ggsave(together,file="output/decadal_time_steps.pdf",width=15,height=10,units="in",dpi=600)
