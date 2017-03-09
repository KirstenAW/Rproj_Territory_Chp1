coordinates(bull) <- ~Longitude+Latitude
proj4string(bull) <- CRS('+init=epsg:4326')
dfutm <- spTransform(df, CRS('+init=epsg:32735')) # utm 35S for SAfrica
dfutm



library("reshape2")
bull.m<-melt(bull,id.vars=c("Longitude","Latitude"))
library("stringr")
bull.m$coor<-str_extract(bull.m$variable,"[A-Z]")
df.m$Age<-str_extract(df.m$variable,"[0-9]")
xybull<-SpatialPointsDataFrame(, data, coords.nrs = numeric(0), 
                               proj4string = CRS(as.character(NA)), match.ID, bbox = NULL)

#######################################################################################################

bull<-read.csv("GPSterritory_Masterlist_2016.csv", header = TRUE, as.is = TRUE)
loc <- bull[, c("Easting", "Northing")]
id <- bull[, "Name"]
udbis <- kernelUD(id, h = "href")
ud <- kernelUD(id, h = "href", grid = 200, same4all = FALSE,
               hlim = c(0.1, 1.5), kern = c("bivnorm"), extent = 1)

image(ud)
ver <- getverticeshr(ud, 95)
plot(ver)
z <- as.image.SpatialGridDataFrame(ud[[2]])
a<-as.image.SpatialGridDataFrame(ud[[3]])
b<-as.image.SpatialGridDataFrame(ud[[4]])
c<-as.image.SpatialGridDataFrame(ud[[5]])
contour(xyz, add=TRUE)
contour(z, add=TRUE)
contour(a, add=TRUE)
contour(b, add=TRUE)
contour(c, add=TRUE)

vud <- getvolumeUD(ud)
image(vud)
vud

image(ud[[1]])

xyz <- as.image.SpatialGridDataFrame(ud[[1]])
contour(xyz, add=TRUE)

ii <- kernel.area(ud, percent=seq(50, 95, by=5))
ii
ii["95",]

bull.kern.area.95<-kernel.area(ud, percent=95)
bull.kern.area.95

as.data.frame(bull.kern.area.95)





