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
years <- seq(1985,2020,by=5)

years2 <- data.frame(rep(years, each=4)) %>%
  mutate(months = rep(c("01","02","03","12"),times=8),
         paste0(rep.years..each...4.,months))



raster_bands <- years2$`paste0(rep.years..each...4., months)`

raster_bands2 <- append(raster_bands,"198412",after=0)
raster_bands3 <- paste0("X", raster_bands2)


MN <- ne_states(country="United States of America") %>% st_as_sf() %>%
  filter(gn_name %in% c("Minnesota","Wisconsin")) %>% dplyr::select(gn_name)

MN_counties <- st_read("data/County_shapefile/cb_2018_us_county_20m.shp") %>% dplyr::select(5,6)# %>%
  #filter(GEOID %in% county_list) %>% 


MN_counties <- st_transform(MN_counties, crs = proj4string(temps))

counties_7<- st_read("data/County_shapefile/cb_2018_us_county_20m.shp") %>% dplyr::select(5,6) %>%
    filter(GEOID %in% county_list) 


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

subset(temps,raster_bands3[1:4])

x2016_x2017 <- raster::subset(temps, c("X201612","X201701","X201702","X201703"), value = T) %>% mean()
x2017_x2018 <- raster::subset(temps, c("X201712","X201801","X201802","X201803"), value = T) %>% mean()
x2018_x2019 <- raster::subset(temps, c("X201812","X201901","X201902","X201903"), value = T) %>% mean()
x2019_x2020 <- raster::subset(temps, c("X201912","X202001","X202002","X202003"), value = T) %>% mean()
x2020_x2021 <- raster::subset(temps, c("X202012","X202101","X202102","X202103"), value = T) %>% mean()


x16_17 <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(x2016_x2017), 
                                      as_points = FALSE, merge = TRUE)
) %>% st_as_sf() %>% rename(`Temperature (°C)` = layer)

x17_18 <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(x2017_x2018), 
                                      as_points = FALSE, merge = TRUE)
) %>% st_as_sf()  %>% rename(`Temperature (°C)` = layer)

x18_19 <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(x2018_x2019), 
                                      as_points = FALSE, merge = TRUE)
) %>% st_as_sf()  %>% rename(`Temperature (°C)` = layer)

x19_20 <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(x2019_x2020), 
                                      as_points = FALSE, merge = TRUE)
) %>% st_as_sf()  %>% rename(`Temperature (°C)` = layer)

x20_21 <- sf::as_Spatial(sf::st_as_sf(stars::st_as_stars(x2020_x2021), 
                                      as_points = FALSE, merge = TRUE)
) %>% st_as_sf() %>% rename(`Temperature (°C)` = layer)
  
x16_17graph <- ggplot() +  
  geom_sf(data=x16_17, aes(fill=`Temperature (°C)`), lwd = 0,color = NA)  +
  viridis::scale_fill_viridis(option="magma",limits = c(-6,3)) +
  geom_sf(data=MN_counties,fill="transparent",color="grey",size=1)  +
  xlab("Longitude") + ylab("Latitude") +
  ggpubr::theme_pubr() + ggtitle("2016 - 2017")  + theme(legend.position = "none") 

  

x17_18graph <- ggplot() +  
  geom_sf(data=x17_18, aes(fill=`Temperature (°C)`), lwd = 0,color = NA)  +
  viridis::scale_fill_viridis(option="magma",limits = c(-6,3)) +
  geom_sf(data=MN_counties,fill="transparent",color="grey",size=1)  +
  xlab("Longitude") + ylab("Latitude") +
  ggpubr::theme_pubr() + ggtitle("2017 - 2018")  + theme(legend.position = "none") 

  
x18_19graph <- ggplot() +  
  geom_sf(data=x18_19, aes(fill=`Temperature (°C)`), lwd = 0,color = NA)  +
  viridis::scale_fill_viridis(option="magma",limits = c(-6,3)) +
  geom_sf(data=MN_counties,fill="transparent",color="grey",size=1)  +
  xlab("Longitude") + ylab("Latitude") +
  ggpubr::theme_pubr() + ggtitle("2018 - 2019")  + theme(legend.position = "none") 


x19_20graph <- ggplot() +  
  geom_sf(data=x19_20, aes(fill=`Temperature (°C)`), lwd = 0,color = NA)  +
  viridis::scale_fill_viridis(option="magma",limits = c(-6,3)) +
  geom_sf(data=MN_counties,fill="transparent",color="grey",size=1)  +
  xlab("Longitude") + ylab("Latitude") +
  ggpubr::theme_pubr() + ggtitle("2019 - 2020") + theme(legend.position="bottom")

x20_21graph <- ggplot() +  
  geom_sf(data=x20_21, aes(fill=`Temperature (°C)`), lwd = 0,color = NA)  +
  viridis::scale_fill_viridis(option="magma",limits = c(-6,3)) +
  geom_sf(data=MN_counties,fill="transparent",color="grey",size=1)  +
  xlab("Longitude") + ylab("Latitude") +
  ggpubr::theme_pubr() + ggtitle("2020 - 2021") + theme(legend.position = "none") 


MN_graph <- 
  ggplot() +  
  geom_sf(data=MN, fill="grey")  +
  geom_sf(data=MN_counties,aes(fill=NAME),color="black",size=1)  +
  xlab("Longitude") + ylab("Latitude") +
  ggpubr::theme_pubr() + theme(legend.position = "none") +
  viridis::scale_fill_viridis(option="C",discrete=TRUE)

layout <- "
ABC
DEF
"
?plot_annotation
together <- x16_17graph + x17_18graph + x18_19graph + x19_20graph + x20_21graph  + MN_graph +
  plot_annotation(title = 'Winter (December - March) Soil Temperatures (7-28 cm)') +
  plot_layout(design = layout)#,guides='collect') & theme(legend.position = "bottom")

x16_17graph_scales_free <- ggplot() +  
  geom_sf(data=x16_17, aes(fill=`Temperature (°C)`), lwd = 0,color = NA)  +
  viridis::scale_fill_viridis(option="magma") +
  geom_sf(data=MN_counties,fill="transparent",color="grey",size=1)  +
  xlab("Longitude") + ylab("Latitude") +
  ggpubr::theme_pubr() + ggtitle("December 2016 - March 2017") 

x17_18graph_scales_free <- ggplot() +  
  geom_sf(data=x17_18, aes(fill=`Temperature (°C)`), lwd = 0,color = NA)  +
  viridis::scale_fill_viridis(option="magma") +
  geom_sf(data=MN_counties,fill="transparent",color="grey",size=1)  +
  xlab("Longitude") + ylab("Latitude") +
  ggpubr::theme_pubr() + ggtitle("2017 - 2018")

x18_19graph_scales_free <- ggplot() +  
  geom_sf(data=x18_19, aes(fill=`Temperature (°C)`), lwd = 0,color = NA)  +
  viridis::scale_fill_viridis(option="magma") +
  geom_sf(data=MN_counties,fill="transparent",color="grey",size=1)  +
  xlab("Longitude") + ylab("Latitude") +
  ggpubr::theme_pubr() + ggtitle("2018 - 2019")

x19_20graph_scales_free <- ggplot() +  
  geom_sf(data=x19_20, aes(fill=`Temperature (°C)`), lwd = 0,color = NA)  +
  viridis::scale_fill_viridis(option="magma") +
  geom_sf(data=MN_counties,fill="transparent",color="grey",size=1)  +
  xlab("Longitude") + ylab("Latitude") +
  ggpubr::theme_pubr() + ggtitle("2019 - 2020")

x20_21graph_scales_free <- ggplot() +  
  geom_sf(data=x20_21, aes(fill=`Temperature (°C)`), lwd = 0,color = NA)  +
  viridis::scale_fill_viridis(option="magma") +
  geom_sf(data=MN_counties,fill="transparent",color="grey",size=1)  +
  xlab("Longitude") + ylab("Latitude") +
  ggpubr::theme_pubr() + ggtitle("2020 - 2021")




ggsave(together,file="output/all_years_together.png",width=15,height=10,units="in",dpi=600)
ggsave(x16_17graph_scales_free,file="output/winter_16_17_scales_free.png",width=15,height=10,units="in",dpi=600)
ggsave(x17_18graph_scales_free,file="output/winter_17_18_scales_free.png",width=15,height=10,units="in",dpi=600)
ggsave(x18_19graph_scales_free,file="output/winter_18_19_scales_free.png",width=15,height=10,units="in",dpi=600)
ggsave(x19_20graph_scales_free,file="output/winter_19_20_scales_free.png",width=15,height=10,units="in",dpi=600)
ggsave(x20_21graph_scales_free,file="output/winter_20_21_scales_free.png",width=15,height=10,units="in",dpi=600)
