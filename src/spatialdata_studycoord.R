## Literature review data
## Created by: Paul Julian (pauljulianphd@gmail.com)
## Created on: 2025-09-15

library(googledrive)
library(openxlsx)
library(AnalystHelper)
library(plyr)
library(reshape2)
library(sf)
library(rnaturalearth)
library(elevatr)
library(terra)

library(parzer); # helps convert coordinates


library(ggplot2)
theme_set(theme_minimal(base_size = 16,base_family = "serif")+
            theme_bw()+
            theme(panel.border = element_rect("black",fill=NA,linewidth=1)))

library(tmap)
tmap_mode("view")
# paths
wd="C:/Julian_LaCie/_GitHub/PeatlandSythesis"
paths=paste0(wd,c("/Plots/","/export/","/Data/","/GIS","/src/"))

#Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]

wgs84.target <- "EPSG:4326"
wgs84 <- st_crs(wgs84.target)

nsidc.target <- "EPSG:6933"
nsidc <- st_crs(nsidc.target)

utm18S <- st_crs("EPSG:32718")

## Functions 


# Data --------------------------------------------------------------------
# drive_auth()
# link <- "https://docs.google.com/spreadsheets/d/1VRNf1IOiq-ied2CZJmfWWyido1iilZK9/edit?gid=831388957#gid=831388957"

file <- drive_get(as_id("1VRNf1IOiq-ied2CZJmfWWyido1iilZK9"))  # file ID only
tmp <- tempfile(fileext = ".xlsx")

drive_download(file, path = tmp, overwrite = TRUE)

spat_dat <- read.xlsx(tmp,sheet = "Lit geo Data")

spat_dat_utm <- subset(spat_dat,Datum=="UTM 18S")
spat_dat_wgs <- subset(spat_dat,Datum!="UTM 18S")


spat_dat_utm_sp <- st_as_sf(subset(spat_dat_utm,!is.na(Latitude)&!is.na(Longitude)),
                            coords = c("Longitude","Latitude"),crs = utm18S, remove = FALSE)|>st_transform(wgs84)
spat_dat_utm_sp <- cbind(spat_dat_utm_sp,st_coordinates(spat_dat_utm_sp))

## DMS to DD
spat_dat_wgs$lat_dd <- parzer::parse_lat(spat_dat_wgs$Latitude)
spat_dat_wgs$long_dd <- parzer::parse_lon(spat_dat_wgs$Longitude)

spat_dat_wgs_sp <- st_as_sf(subset(spat_dat_wgs,!is.na(long_dd)&!is.na(lat_dd)),
                            coords = c("long_dd","lat_dd"),crs = wgs84)
spat_dat_wgs_sp <- cbind(spat_dat_wgs_sp,st_coordinates(spat_dat_wgs_sp))

spat_dat_sp <- rbind(spat_dat_wgs_sp,spat_dat_utm_sp)

tm_shape(spat_dat_wgs_sp)+tm_dots()

