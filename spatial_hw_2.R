
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
#install.packages("https://cran.r-project.org/src/contrib/Archive/PrevMap/PrevMap_1.5.tar.gz",repos = NULL, type = "source")
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


pv0 <- VO_Cambodia_Updated %>%
  ggplot()+
  geom_point(aes(utm_x, utm_y, color = VO), size = 1) +
  coord_fixed(ratio = 1) +
  labs(title="") +
  scale_color_viridis_c(option = "plasma") +
  #scale_color_gradient(low = "blue", high = "red") +
  geom_path(data = data.frame(colomia_boundaries), aes(utm_x,utm_y))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill=NA),
        plot.title = element_text(hjust=0.5),
        axis.title = element_blank())
ggsave("plots/eda_VO.png", plot = pv0, width = 6, height = 4, dpi = 300)


p_elev <-  VO_Cambodia_Updated %>%
  ggplot()+
  geom_point(aes(utm_x, utm_y, color = elevation), size = 1) +
  coord_fixed(ratio = 1) +
  labs(color="Elevation (m)") +
  scale_color_viridis_c(option = "plasma") +
  #scale_color_gradient(low = "blue", high = "red") +
  geom_path(data = data.frame(colomia_boundaries), aes(utm_x,utm_y))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill=NA),
        plot.title = element_text(hjust=0.5),
        axis.title = element_blank())
ggsave("plots/eda_elevation.png", plot = p_elev, width = 6, height = 4, dpi = 300)

#######==== VO vs elevation
png("plots/eda_VO_vs_elevation.png", width = 1200, height = 900, res = 150)

plot(VO ~ elevation,
     data = VO_Cambodia_Updated,
     main = "",
     xlab = "Elevation (m)",
     pch = 20,
     cex = 0.5)

dev.off()


##############======Modelling ======####################
##############Linear regression model

lm1 <- lm(VO ~ elevation, data = VO_Cambodia_Updated)
summary(lm1)


####check model assumption
png("plots/lm_diag_plots.png", width = 1200, height = 1200, res = 150)
par(mfrow = c(2, 2))
plot(lm1)
dev.off()

######===check for spatial correlatio
set.seed(122025)
library(PrevMap)
theta.start <- NULL
#####check distance distriution for uvec selection
coords <- cbind(VO_Cambodia_Updated$utm_x, VO_Cambodia_Updated$utm_y)
d <- dist(coords)
summary(d)


######plot variograms
png("plots/spatial_correlation_diagnostic.png", width = 1200, height = 900, res = 150)
spatial_cor <- spat.corr.diagnostic(formula=VO ~ elevation,
                                   coords=~utm_x+utm_y,ID.coords = NULL,
                                   data=VO_Cambodia_Updated,likelihood="Gaussian",
                                   uvec=seq(10,750,length=20),n.sim = 1000,
                                   lse.variogram = TRUE)
dev.off()

#########======Linear geostatistical model
geo_fit1 <- linear.model.MLE(VO ~elevation,
                             coords=~utm_x+utm_y, ID.coords=NULL,
                             start.cov.pars=c(spatial_cor[["lse.variogram"]][["phi"]],
                                              spatial_cor[["lse.variogram"]][["tau^2"]]/spatial_cor
                                              [["lse.variogram"]][["sigma^2"]]),
                             kappa=0.5,data=VO_Cambodia_Updated,
                             fixed.rel.nugget = NULL,
                             method="nlminb")
summary(geo_fit1)

#######===#########======Goodness of fit
png("plots/geo_spatial_correlation_diagnostic.png", width = 1200, height = 900, res = 150)
variog.diagnostic.lm(geo_fit1)
dev.off()

#######========Prediction on a grid
library(geosphere)
x=distm(d@coords[11,],d@coords[32,],fun=distGeo)/1000
x
y=distm(d@coords[1,],d@coords[24,],fun=distGeo)/1000
y
library(splancs)
colombia.grid <- gridpts(as.matrix(coord_p_new@coords),npts=1000)
col.grid=as.data.frame(colombia.grid)
coordinates(col.grid) <- c("V1", "V2")
proj4string(col.grid) <- CRS("+proj=utm +zone=37 +ellps=WGS84 +datum=WGS84
+units=km +no_defs +south")
col.grid=spTransform(col.grid,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
col.grid=as.data.frame(col.grid@coords)

elev<-col.grid %>% as_tibble() %>% setNames(c("x","y")) %>% as.data.frame()

colombia_predictors <- get_elev_point(elev, src="aws", prj=4326)
colombia_predictors<-as.data.frame(colombia_predictors) %>% dplyr::select(-c(1,3))

png("plots/geo_predictive_points.png", width = 900, height = 900, res = 150)
plot(colombia.grid,xlab = "", ylab = "")
lines(colomia_boundaries)
dev.off()

#########====predictive plots
summary(VO_Cambodia_Updated$VO)
mean_vo <- mean(VO_Cambodia_Updated$VO)
geo_pred_model <- spatial.pred.linear.MLE(geo_fit1,grid.pred = colombia.grid,
                                    predictors = colombia_predictors,
                                    scale.predictions = "logit",
                                    standard.errors = TRUE,
                                    thresholds = mean_vo,
                                    scale.thresholds = "logit")
geo_pred_model$exceedance.prob <- geo_pred_model$exceedance.prob


png("plots/VO_Index_Estimates.png", width = 900, height = 900, res = 150)
plot(geo_pred_model,type="logit",summary="predictions",main="",xlab="",ylab="")
contour(geo_pred_model,type="logit",add=TRUE,levels=mean_vo)
lines(colomia_boundaries)
dev.off()


png("plots/VO_Risk.png", width = 900, height = 900, res = 150)
plot(geo_pred_model,summary="exceedance.prob",zlim=c(0,1),main="", xlab="",ylab="")
contour(geo_pred_model,summary="exceedance.prob",add=TRUE,levels=c(0.25,0.75))
lines(colomia_boundaries)
dev.off()


columbia_pred_elev <- cbind(colombia_predictors,colombia.grid)
colnames(columbia_pred_elev)=c("elevation","utm_x","utm_y")



columbia_pred_elev %>%
  ggplot()+
  geom_point(aes(utm_x, utm_y, color = elevation), size = 1) +
  coord_fixed(ratio = 1) +
  labs(title=""
       , color="Metres") +
  scale_color_gradientn(colours = rev(terrain.colors(10))) +
  geom_path(data = data.frame(colomia_boundaries), aes(utm_x,utm_y)) +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(colour = "black", fill=NA),
        plot.title = element_text(hjust=0.5, face="bold"),
        axis.title = element_blank())
ggsave("plots/pred_elevation.png", width = 6, height = 4, dpi = 300)


