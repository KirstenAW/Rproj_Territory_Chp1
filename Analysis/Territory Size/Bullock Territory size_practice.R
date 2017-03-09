####Territory Size with Kerneling####

library(adehabitatHR)
help(kernelUD)
help(SpatialPointsDataFrame)
library(raster) 
library(rgdal)
library(maptools)
library(adehabitatHR)
library(sp)



bull<-read.csv("GPSterritory_Masterlist_2016.csv", header = TRUE, as.is = TRUE)
str(bull)
bull$Name<-as.factor(bull$Name)
bull$Visit<-as.factor(bull$Visit)
bull$Stream<-as.factor(bull$Stream)

###create Spatial Points Data Fram
coordinates(bull) <- ~Easting+Northing
class(bull)

####creates xy to feed into identity
id=bull[,c("Name")]

###GPS points by bird ID
points.bull<-plot(id, col=bull$Name)
proj4string(bull) <- CRS("+init=epsg:32633") ###work on this, tied to projection
bull <- spTransform(bull, CRS("+init=epsg:32633"))
summary(bull)
plot(bull)

##Minimum Convex Polygons (100%), plot by bird id in hectares
mcpbull <- mcp(id, percent=95)
plotbull<-plot(mcpbull, col=c("dodgerblue", "maroon1", "purple", "black", "grey50", "white", "yellow"))


bullmcp.area.ha<- mcp.area(id, percent=seq(50, 100, by = 5), unout = c("ha"))
bullmcp.area.ha

mcp.bull.ha<-mcp(id, percent=100, unout = c("ha")) ########Average MCP area for Bullock
mcp.bull.ha$area
mean(mcp.bull.ha$area)

plot(mcp(id,percent=100))  #graph the 100% mcp
plot(id,pch=1,cex=.75, add=TRUE) 

#####kernel

bull.kud <- kernelUD(id, h="href")
bull.kud
image(bull.kud)

bull.kud.LSCV <- kernelUD(id, h="LSCV") ###########NOT CONVERGING, LOOK UP ISSUES
bull.kud.LSCV
image(bull.kud.LSCV)
plotLSCV(bull.kud.LSCV)

homerange <- getverticeshr(bull.kud, unin=c("m"), percent= c(50, 100, by = 20),  standardize = FALSE)
homerange2<-getverticeshr.estUDm(bull.kud)
plot(homerange, col=heat.colors(7))

plot(mcpbull, add=TRUE)


kernel.area(bull.kud, percent=seq(50,100, by=5))

ud <- kernelUD(id, h = "href", grid = 30, same4all = FALSE,
               hlim = c(0.1, 1.5), kern = c("bivnorm"), extent = 0.5)
image(ud)


###############trying 3D viz

library(rasterVis)
library(raster)
library(sp)
library(rgdal)
library(maptools)#writeAsciiGrid
library(ks)#hpikde.ud
library(adehabitatHR)
library(adehabitatMA)

udvol <- getvolumeUD(bull.kud, standardize=TRUE)
homerange.udvol <- getverticeshr(udvol)
homerange.udvol

if (require(rgl)) {
  #data(loc)
  r <- raster(udvol)
  extent(r) <- c(0, 610, 0, 870)
  drape <- cut(r, 5)
  plot3D(r, drape=drape, zfac=2)
}
}

##############################################################################


kd <- kernelUD(spdf[, 7])
image(kd)
# visualize individual utilization distribution
image(kd[[1]])
plot(getverticeshr(kd[[1]], 95), add = TRUE)

# creating SpatialPolygonsDataFrame
kd_names <- names(kd)
ud <- lapply(kd, function(x), try(getverticeshr(x, 95)))
# changing each polygons id to the species name for rbind call
sapply(1:length(ud), function(i) {
  row.names(ud[[i]]) <<- kd_names[i]
})
sdf_poly <- Reduce(rbind, ud)
plot(sdf_poly)


#########ggplot#############
bull.ggplot.1 <- ggplot(homerange, aes(x = long, y = lat, fill = id, group = group)) +
  geom_polygon(alpha = .6) +
  coord_equal() +
  theme_void()
bull.ggplot.1
bull.ggplot.1 + facet_wrap(~id)

bull.ggplot.2 <- ggplot(homerange.udvol, aes(x = long, y = lat, fill = id, group = group)) +
  geom_polygon(alpha = .6) +
  coord_equal() +
  theme_void()
bull.ggplot.2
bull.ggplot.2 + facet_wrap(~id)

################################################################################

kernbull<-kernelUD(id)
plot(kernbull)

bull.mcp.plot<- ggplot(data=mcpbull, aes(id))
mcpbull<- mcp(id, percent=95)
bull.HR <- stack(mcpbull, bull)


kernelUD(Longitude, Latitude)

coordinates(bull) <- ~Longitude+Latitude
SpatialPointsDataFrame(coordinates, data, coords.nrs = numeric(0), 
                       proj4string = CRS(as.character(NA)), match.ID, bbox = NULL)

###Coordinate system#
###EPSG:4326
##points by Location, birdID, Date, Coordinate


