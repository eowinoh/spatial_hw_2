
############################################################
# Assignment Title: Spatial Epidemiology Homework 1
# Author: Group 4
# Question: 1
############################################################

rm(list = ls()) 


# packages ----------------------------------------------------------------
require(pacman)
pacman::p_load(tidyverse, here, sf,sp,xtable,
               tmap, spdep, nimble, coda, INLA,
               cowplot, stats4, formatR, readxl, rootSolve,sf,elevatr)

#load data
VO_data_Combodia <- read.table("VO_data_Combodia.txt", header = TRUE)

#shapefile and extract coordinates
data("world")
cambodia <- world %>% filter(name_long == "Cambodia")
coord_p <-st_coordinates(cambodia$geom) %>% 
  as.data.frame() %>% 
  dplyr::select(-c(3:5))

coordinates(coord_p) <- c("X", "Y")

coord_d <- VO_data_Combodia[c(1:2)]
coordinates(coord_d) <- c("X", "Y")

######=== Assign CRS WGS84 ===######
###### package data points
proj4string(coord_p) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
coord_p_new=spTransform(coord_p,CRS("+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84 +units=
km +no_defs +south"))

#####=== from given VO data
proj4string(coord_d) <- CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
coord_d_new=spTransform(coord_d,CRS("+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84 +units=
km +no_defs +south"))

####UTM coordinates
coord_p_new$utm_x <- coordinates(coord_p_new)[, 1]
coord_p_new$utm_y <- coordinates(coord_p_new)[, 2]
coord_d_new$utm_x <- coordinates(coord_d_new)[, 1]
coord_d_new$utm_y <- coordinates(coord_d_new)[, 2]

#######====append the data to package and given data====########
colomia_boundaries<-coord_p_new@data
VO_Cambodia_Updated=cbind(VO_data_Combodia,coord_d_new@data)



#################=== Exploraotory Analysis
summary_stats<- as.data.frame(sapply(VO_Cambodia_Updated, summary)) %>%
  dplyr::select(VO, elevation) %>% mutate(across(where(is.numeric), ~ round(
    ., 2)))

summary_clean <- as.data.frame(t(summary_stats)) %>% 
  xtable() %>% print(.,include.rownames = TRUE)








elev_points <- get_elev_point(cambodia, prj = st_crs(4326), src = "aws", z = 7)
kenya.predictors <- get_elev_point(elev, src="aws", prj=4326)






