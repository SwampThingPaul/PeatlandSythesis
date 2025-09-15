
library(sf)
library(tmap)
tmap_mode("view")

## Coasta et al 2022
utm <- st_crs("EPSG:32723")
wgs84 <- st_crs("EPSG:4326")


dat <- data.frame(id=1,utmx = c(7983314),utmy = c(0677469))

dat2 <- st_as_sf(dat,coords=c("utmy","utmx"),crs=utm)|>
  st_transform(wgs84)

tm_shape(dat2)+tm_dots()

st_coordinates(dat2)
