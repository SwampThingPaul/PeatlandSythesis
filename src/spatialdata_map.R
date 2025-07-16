## South America peatland map
## Created by: Paul Julian (pauljulianphd@gmail.com)
## Created on: 2025-05-07

library(AnalystHelper)
library(plyr)
library(reshape2)
library(sf)
library(rnaturalearth)
library(elevatr)
library(terra)

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

# Countries ---------------------------------------------------------------
world <- ne_countries(returnclass = "sf")|>
  st_transform(nsidc)
americas <- subset(world,region_un=="Americas")
Samerica <- ne_countries(continent = "south america",returnclass = "sf")|>
  st_transform(nsidc)

plot(st_geometry(world))
plot(st_geometry(Samerica))

plot(st_geometry(americas))


# Elevation ---------------------------------------------------------------
# 
# d <- get_elev_raster(locations = Samerica, z = 6, clip = "locations")
# Samer.dem <- rast(d)
# writeRaster(Samer.dem,paste0(GIS.path,"/DEM.tif"),overwrite=T)

Samer.dem <- rast(paste0(GIS.path,"/DEM.tif"))|>
  project(nsidc.target)
terra::plot(Samer.dem, plg = list(title = "Elevation (m)"))

slope  <- terrain(Samer.dem, v = "slope", unit = "radians")
aspect <- terrain(Samer.dem, v = "aspect", unit = "radians")
hillshade <- shade(slope, aspect, angle = 45, direction = 315)

dem_vals <- values(Samer.dem, na.rm = TRUE)
dem_min <- min(dem_vals)
dem_max <- max(dem_vals)
dem_scaled <- (Samer.dem - dem_min) / (dem_max - dem_min)

names(Samer.dem) <- "elev_m"
# PEATMAP -----------------------------------------------------------------
peatlands1 <- rast(paste0(GIS.path,"/PEATMAP/peatGPA22WGS_2cl.tif"))

peatlands <- rast(paste0(GIS.path,"/PEATMAP/peatGPA22WGS_2cl.tif"))|>
  project(nsidc.target)

plot(peatlands)

AOI.poly <- raster::extent(st_bbox(st_buffer(Samerica,200e3)))|>
  as("SpatialPolygons")|>
  st_as_sf()
st_crs(AOI.poly) <- nsidc

peatlands.crop.SA <- crop(peatlands,AOI.poly)
unique(peatlands.crop.SA)
peatlands.crop.SA <- round(peatlands.crop.SA)

peatlands.crop.SA.poly <- as.polygons(peatlands.crop.SA)

peatlands.crop.SA.poly.sf <- peatlands.crop.SA.poly|>
  st_as_sf()|>
  st_cast("POLYGON")

names(peatlands.crop.SA.poly.sf)[names(peatlands.crop.SA.poly.sf) == "peatGPA22WGS_2cl"] <- "peat_type"
length(peatlands.crop.SA.poly.sf$peat_type)
peatlands.crop.SA.poly.sf$ID <- 1:nrow(peatlands.crop.SA.poly.sf)
peatlands.crop.SA.poly.sf$area_m2 <- st_area(peatlands.crop.SA.poly.sf)|>as.numeric()

poly_vect <- vect(peatlands.crop.SA.poly.sf)
mean_elev <- extract(Samer.dem[["elev_m"]], poly_vect, fun = mean, na.rm = TRUE)

peat_elev_df <- data.frame(
  ID = peatlands.crop.SA.poly.sf$ID,
  type = peatlands.crop.SA.poly.sf$peat_type,
  mean.elev = mean_elev[,2],
  total.area = poly_vect$area_m2
)
# write.csv(peat_elev_df,paste0(export.path,"peatland_elevation_SAmer.csv"),row.names=F)

hist(peat.elev$mean.elev,breaks = seq(-100,6000,1.6))

elev.bins <- seq(-100,5000,150)

peat_elev_df$elev.bins.grp <- findInterval(peat.elev$mean.elev,elev.bins)
peatland.total.area <- ddply(subset(peat_elev_df,!is.na(mean.elev)),
                             c("elev.bins.grp"),summarise,total.area.km2 = sum(total.area,na.rm=T)*1e-9)
peatland.total.area$elev.bin.val <- elev.bins[peatland.total.area$elev.bins.grp]

# png(filename=paste0(plot.path,"PEATMAP_elev_SouthAmeria.png"),width=6.5,height=4.5,units="in",res=200,type="cairo",bg="white")
par(mar=c(2,4,0.5,0.5),oma=c(2,1,0.5,0.5));

ylim.val <- c(0.005,200);ymaj <- log.scale.fun(ylim.val,"major");ymin <- log.scale.fun(ylim.val,"minor")
xlim.val <- c(-100,5300);by.x <- 1000; 
xmaj <-c(-100,seq(pmax(by.x,xlim.val[1]),xlim.val[2],by.x)); 
xmin <-seq(pmax(by.x/2,xlim.val[1]),xlim.val[2],by.x/2)

plot(total.area.km2~elev.bin.val,peatland.total.area,log="y",ylim = ylim.val,xlim = xlim.val,ann=F,axes=F,type="n")
abline(h=ymaj,v=xmaj,lty=3,col="grey",lwd=0.5)
abline(v=0)
lines(total.area.km2~elev.bin.val,peatland.total.area,col=adjustcolor("forestgreen",0.5),lwd=2)
points(total.area.km2~elev.bin.val,peatland.total.area,pch=21,bg="lightgreen",lwd=0.1,cex=1.5)
axis_fun(1,xmaj,xmin,xmaj)
axis_fun(2,ymaj,ymin,ymaj)
box(lwd=1)
mtext(side=1,line=2,"Elevation (meters)")
mtext(side=2,line=3,expression("Total Peatland Area (km "^" 2"*")"))
dev.off()

peatland.total.area_type <- dcast(subset(peat_elev_df,!is.na(mean.elev)),
                                  elev.bins.grp~type,
                                  value.var = "total.area",
                                  fun.aggregate = function(x) sum(x,na.rm=T*1e-9))
peatland.total.area_type$elev.bin.val <- elev.bins[peatland.total.area_type$elev.bins.grp]
## MAP ---------------------------------------------------------------------

# png(filename=paste0(plot.path,"PEATMAP_SouthAmeria.png"),width=5.75,height=6,units="in",res=200,type="cairo",bg="white")
par(mar=c(0.1,0.1,0.1,0.1),oma=c(0.5,0.5,0.5,0.5),xpd=F);
layout(matrix(c(1,1,2,3),2,2,byrow=T),
       heights = c(0.75,1),widths = c(1,0.3))

bbox.lims <- st_bbox(st_buffer(world,-250e3))
plot(st_geometry(world),ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],col="grey",border="white",lwd=0.5,bg="lightblue")
plot(st_geometry(AOI.poly),add=T,border = "red")
box(lwd=1)

bbox.lims <- st_bbox(Samerica)
elev.bks <- seq(0,5000,200);elev.cols <- grey.colors(length(elev.bks)-1)
bks <- c(0,1,2); cols <- c("brown","khaki")
plot(st_geometry(americas),ylim=bbox.lims[c(2,4)],xlim=bbox.lims[c(1,3)],col="cornsilk",border=NA,bg="lightblue")
image(hillshade,add=T,col = gray.colors(100, 0, 1))
image(Samer.dem,add=T,breaks=elev.bks,col = adjustcolor(elev.cols,0.2))
image(peatlands.crop.SA,add=T,breaks=bks,col = cols)
plot(st_geometry(Samerica),add=T,col=NA,border="white")
mapmisc::scaleBar(Samerica,"bottomleft",bty="n",cex=1,outer=F)
box(lwd=1)

plot(0:1,0:1,ann=F,axes=F,type="n")
leg.fun(elev.bks,elev.cols,x.max = 0.6, x.min = 0.4,top.val=0.9,bot.val=0.4,
        leg.title="Elevation (meters)",leg.type = "continuous")

legend(0.5,0.25,legend=c("Peat Dominated","Peat in soil moasic"),
       pch=22,lty=NA,lwd=c(0),
       col=c(NA),pt.bg=cols,
       pt.cex=2,ncol=1,cex=0.8,bty="n",y.intersp=1,x.intersp=0.75,xpd=NA,xjust=0.5,yjust=1)
dev.off()