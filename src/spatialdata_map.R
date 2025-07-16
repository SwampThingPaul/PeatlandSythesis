## South America peatland map
## Created by: Paul Julian (pauljulianphd@gmail.com)
## Created on: 2025-05-07


library(sf)
library(rnaturalearth)
library(elevatr)
library(terra)


# paths
wd="C:/Julian_LaCie/_GitHub/PeatlandSythesis"
paths=paste0(wd,c("/Plots/","/export/","/Data/","/GIS","/src/"))

#Folder.Maker(paths);#One and done. Creates folders in working directory.
plot.path=paths[1]
export.path=paths[2]
data.path=paths[3]
GIS.path=paths[4]


wgs84 <- st_crs("EPSG:4326")


# Countries ---------------------------------------------------------------
world <- ne_countries(returnclass = "sf")|>
  st_transform(wgs84)
americas <- subset(world,region_un=="Americas")
Samerica <- ne_countries(continent = "south america",returnclass = "sf")|>
  st_transform(wgs84)

plot(st_geometry(world))
plot(st_geometry(Samerica))

plot(st_geometry(americas))

# Elevation ---------------------------------------------------------------
# 
# d <- get_elev_raster(locations = Samerica, z = 5, clip = "locations")
# Samer.dem <- rast(d)
# writeRaster(Samer.dem,paste0(GIS.path,"/DEM.tif"))

Samer.dem <- rast(paste0(GIS.path,"/DEM.tif"))
terra::plot(Samer.dem, plg = list(title = "Elevation (m)"))
