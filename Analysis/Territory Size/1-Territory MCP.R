rm(list = ls())
library(adehabitatHR)
help(kernelUD)
help(SpatialPointsDataFrame)
library(raster) 
library(rgdal)
library(maptools)
library(adehabitatHR)
library(sp)
library(dplyr)
library(magrittr)
library(ggplot2)
library(lsmeans)

###Centroid csv files separated as when converted they won't subset and all observations <5 removed

setwd("~/SFU/Masters/Data/Territory Data/2016 Spot mapping centroids")

bull.ter<-read.csv("Bullock_Centroids.csv", header = TRUE, as.is = TRUE)
str(bull.ter)
bull.ter$Name<-as.factor(bull.ter$Name)
bull.ter$Visit<-as.factor(bull.ter$Visit)
bull.ter$Stream<-as.factor(bull.ter$Stream)

fannie.ter<-read.csv("Fannie_Centroids.csv", header = TRUE, as.is = TRUE)
str(fannie.ter)
fannie.ter$Name<-as.factor(fannie.ter$Name)
fannie.ter$Visit<-as.factor(fannie.ter$Visit)
fannie.ter$Stream<-as.factor(fannie.ter$Stream)

fancy.ter<-read.csv("Fancy_Centroids.csv", header = TRUE, as.is = TRUE)
str(fancy.ter)
fancy.ter$Name<-as.factor(fancy.ter$Name)
fancy.ter$Visit<-as.factor(fancy.ter$Visit)
fancy.ter$Stream<-as.factor(fancy.ter$Stream)

jane.ter<-read.csv("Jane_Centroids.csv", header = TRUE, as.is = TRUE)
str(jane.ter)
jane.ter$Name<-as.factor(jane.ter$Name)
jane.ter$Visit<-as.factor(jane.ter$Visit)
jane.ter$Stream<-as.factor(jane.ter$Stream)

clatse.ter<-read.csv("Clatse_Centroids.csv", header = TRUE, as.is = TRUE)
str(clatse.ter)
clatse.ter$Name<-as.factor(clatse.ter$Name)
clatse.ter$Visit<-as.factor(clatse.ter$Visit)
clatse.ter$Stream<-as.factor(clatse.ter$Stream)

hook.ter<-read.csv("Hooknose_Centroids.csv", header = TRUE, as.is = TRUE)
str(hook.ter)
hook.ter$Name<-as.factor(hook.ter$Name)
hook.ter$Visit<-as.factor(hook.ter$Visit)
hook.ter$Stream<-as.factor(hook.ter$Stream)

rip.ter<-read.csv("Ripley_Centroids.csv", header = TRUE, as.is = TRUE)
str(rip.ter)
rip.ter$Name<-as.factor(rip.ter$Name)
rip.ter$Visit<-as.factor(rip.ter$Visit)
rip.ter$Stream<-as.factor(rip.ter$Stream)



#########Check to see if less than 5 occurences########
tt <- table(bull.ter$Name)
tt
names(tt[tt < 5])

tt <- table(fannie.ter$Name)
tt
names(tt[tt < 5])

tt <- table(rip.ter$Name)
tt
names(tt[tt < 5])

tt <- table(clatse.ter$Name)
tt
names(tt[tt < 5])

tt <- table(fancy.ter$Name)
tt
names(tt[tt < 5])

tt <- table(jane.ter$Name)
tt
names(tt[tt < 5])

tt <- table(hook.ter$Name)
tt
names(tt[tt < 5])



#bull.ter <- subset(territory, Stream=="Bullock")
#fannie.ter<-subset(territory, Stream=="Fannie")
#fancy.ter<-subset(territory, Stream=="Fancy")
#jane.ter<-subset(territory, Stream=="Jane")
#clatse.ter<-subset(territory, Stream=="Clatse")
#hook.ter<-subset(territory, Stream=="Hooknose")
#rip.ter<-subset(territory, Stream=="Ripley")


###create Spatial Points Data Fram
coordinates(bull.ter) <- ~X+Y
coordinates(fannie.ter) <- ~X+Y
coordinates(fancy.ter) <- ~X+Y
coordinates(jane.ter) <- ~X+Y
coordinates(clatse.ter) <- ~X+Y
coordinates(hook.ter) <- ~X+Y
coordinates(rip.ter) <- ~X+Y
coordinates(territory) <- ~X+Y
class(territory)

####creates xy to feed into identity
id=territory[,c("Name")]
id.bull=bull.ter[,c("Name")]
id.fannie=fannie.ter[,c("Name")]
id.fancy=fancy.ter[,c("Name")]
id.jane=jane.ter[,c("Name")]
id.clatse=clatse.ter[,c("Name")]
id.hook=hook.ter[,c("Name")]
id.rip=rip.ter[,c("Name")]

###GPS points by bird ID
points.territory<-plot(id, col=territory$Name)
proj4string(territory) <- CRS("+init=epsg:32633") ###work on this, tied to projection
territory <- spTransform(territory, CRS("+init=epsg:32633"))
summary(territory)
plot(territory)


points.territory.bull<-plot(id.bull, col=bull.ter$Name)
proj4string(bull.ter) <- CRS("+init=epsg:32633") ###work on this, tied to projection
bull.ter <- spTransform(bull.ter, CRS("+init=epsg:32633"))
summary(bull.ter)
plot(bull.ter)

points.territory.fannie<-plot(id.fannie, col=fannie.ter$Name)
proj4string(fannie.ter) <- CRS("+init=epsg:32633") ###work on this, tied to projection
fannie.ter <- spTransform(fannie.ter, CRS("+init=epsg:32633"))
summary(bull.ter)
plot(fannie.ter)

points.territory.fancy<-plot(id.fancy, col=fancy.ter$Name)
proj4string(fancy.ter) <- CRS("+init=epsg:32633") ###work on this, tied to projection
fancy.ter <- spTransform(fancy.ter, CRS("+init=epsg:32633"))
summary(fancy.ter)
plot(fancy.ter)

points.territory.jane<-plot(id.jane, col=jane.ter$Name)
proj4string(jane.ter) <- CRS("+init=epsg:32633") ###work on this, tied to projection
jane.ter <- spTransform(jane.ter, CRS("+init=epsg:32633"))
summary(jane.ter)
plot(jane.ter)

points.territory.clatse<-plot(id.clatse, col=clatse.ter$Name)
proj4string(clatse.ter) <- CRS("+init=epsg:32633") ###work on this, tied to projection
clatse.ter <- spTransform(clatse.ter, CRS("+init=epsg:32633"))
summary(clatse.ter)
plot(clatse.ter)

points.territory.hook<-plot(id.hook, col=hook.ter$Name)
proj4string(hook.ter) <- CRS("+init=epsg:32633") ###work on this, tied to projection
hook.ter <- spTransform(hook.ter, CRS("+init=epsg:32633"))
summary(hook.ter)
plot(hook.ter)

points.territory.rip<-plot(id.rip, col=rip.ter$Name)
proj4string(rip.ter) <- CRS("+init=epsg:32633") ###work on this, tied to projection
rip.ter <- spTransform(rip.ter, CRS("+init=epsg:32633"))
summary(rip.ter)
plot(rip.ter)

##################################################################
##Minimum Convex Polygons BULLOCK (in hectares)#####
##################################################################

mcpterritory.bull <- mcp(id.bull, percent=100)
plotterritory<-plot(mcpterritory.bull, col=c("dodgerblue", "maroon1", "purple", "black", "grey50", "white", "yellow"))

territorymcp.area.bull.ha<- mcp.area(id.bull, percent=seq(50, 100, by = 5), unout = c("ha"))
territorymcp.area.bull.ha

mcp.territory.bull.ha<-mcp(id.bull, percent=100, unout = c("ha")) ########Average MCP area for territoryock
bullock.territories<-mcp.territory.bull.ha$area
mean.bull.terrsize<-mean(mcp.territory.bull.ha$area)



##################################################################
##Minimum Convex Polygons RIPLEY (in hectares)#####
##################################################################

mcpterritory.rip <- mcp(id.rip, percent=100)
plotterritory<-plot(mcpterritory.rip, col=c("dodgerblue", "maroon1", "purple", "black", "grey50", "white", "yellow"))

territorymcp.area.rip.ha<- mcp.area(id.rip, percent=seq(50, 100, by = 5), unout = c("ha"))
territorymcp.area.rip.ha

mcp.territory.rip.ha<-mcp(id.rip, percent=100, unout = c("ha")) ########Average MCP area for territoryock
ripley.territories<-mcp.territory.rip.ha$area
mean.rip.terrsize<-mean(mcp.territory.rip.ha$area)
mean.rip.terrsize

##################################################################
##Minimum Convex Polygons FANNIE (in hectares)#####
##################################################################

mcpterritory.fannie <- mcp(id.fannie, percent=100)
plotterritory<-plot(mcpterritory.fannie, col=c("dodgerblue", "maroon1", "purple", "black", "grey50", "white", "yellow"))

territorymcp.area.fannie.ha<- mcp.area(id.fannie, percent=seq(50, 100, by = 5), unout = c("ha"))
territorymcp.area.fannie.ha

mcp.territory.fannie.ha<-mcp(id.fannie, percent=100, unout = c("ha")) ########Average MCP area for territoryock
fannie.territories<-mcp.territory.fannie.ha$area
mean.fannie.terrsize<-mean(mcp.territory.fannie.ha$area)
mean.fannie.terrsize

##################################################################
##Minimum Convex Polygons CLATSE (in hectares)#####
##################################################################

mcpterritory.clatse <- mcp(id.clatse, percent=100)
plotterritory<-plot(mcpterritory.clatse, col=c("dodgerblue", "maroon1", "purple", "black", "grey50", "white", "yellow"))

territorymcp.area.clatse.ha<- mcp.area(id.clatse, percent=seq(50, 100, by = 5), unout = c("ha"))
territorymcp.area.clatse.ha

mcp.territory.clatse.ha<-mcp(id.clatse, percent=100, unout = c("ha")) ########Average MCP area for territoryock
clatse.territories<-mcp.territory.clatse.ha$area
mean.clatse.terrsize<-mean(mcp.territory.clatse.ha$area)
mean.clatse.terrsize

##################################################################
##Minimum Convex Polygons JANE (in hectares)#####
##################################################################

mcpterritory.jane <- mcp(id.jane, percent=100)
plotterritory<-plot(mcpterritory.jane, col=c("dodgerblue", "maroon1", "purple", "black", "grey50", "white", "yellow"))

territorymcp.area.jane.ha<- mcp.area(id.jane, percent=seq(50, 100, by = 5), unout = c("ha"))
territorymcp.area.jane.ha

mcp.territory.jane.ha<-mcp(id.jane, percent=100, unout = c("ha")) ########Average MCP area for territoryock
jane.territories<-mcp.territory.jane.ha$area
mean.jane.terrsize<-mean(mcp.territory.jane.ha$area)
mean.jane.terrsize

##################################################################
##Minimum Convex Polygons FANCY (in hectares) #####
##################################################################

mcpterritory.fancy <- mcp(id.fancy, percent=100)
plotterritory<-plot(mcpterritory.fancy, col=c("dodgerblue", "maroon1", "purple", "black", "grey50", "white", "yellow"))

territorymcp.area.fancy.ha<- mcp.area(id.fancy, percent=seq(50, 100, by = 5), unout = c("ha"))
territorymcp.area.fancy.ha

mcp.territory.fancy.ha<-mcp(id.fancy, percent=100, unout = c("ha")) ########Average MCP area for territoryock
fancy.territories<-mcp.territory.fancy.ha$area
mean.fancy.terrsize<-mean(mcp.territory.fancy.ha$area)
mean.fancy.terrsize

##################################################################
##Minimum Convex Polygons HOOKNOSE (in hectares) #####
##################################################################

mcpterritory.hook <- mcp(id.hook, percent=100)
plotterritory<-plot(mcpterritory.hook, col=c("dodgerblue", "maroon1", "purple", "black", "grey50", "white", "yellow"))

territorymcp.area.hook.ha<- mcp.area(id.hook, percent=seq(50, 100, by = 5), unout = c("ha"))
territorymcp.area.hook.ha

mcp.territory.hook.ha<-mcp(id.hook, percent=100, unout = c("ha")) ########Average MCP area for territoryock
hook.territories<-mcp.territory.hook.ha$area
mean.hook.terrsize<-mean(mcp.territory.hook.ha$area)
mean.hook.terrsize

#####create usable df######

Bird.ID.hook<-mcp.territory.hook.ha$id
salmon.biomass.hook<-wilcoxsalmonpointcount.avg12to14[11,4]
stream.name<-c("Hooknose", "Hooknose", "Hooknose", "Hooknose")
df.hooknose.mcp<-data.frame(stream.name, Bird.ID.hook, hook.territories,salmon.biomass.hook)
colnames(df.hooknose.mcp) <- c("Stream", "Bird_ID", "Territory_Size", "Salmon_biomass")
df.hooknose.mcp$Salmon_biomass<-as.numeric(df.hooknose.mcp$Salmon_biomass)


Bird.ID.rip<-mcp.territory.rip.ha$id
salmon.biomass.rip<-c("1", "1")
stream.name.rip<-c("Ripley", "Ripley")
df.ripley.mcp<-data.frame(stream.name.rip, Bird.ID.rip, ripley.territories, salmon.biomass.rip)
colnames(df.ripley.mcp) <- c("Stream", "Bird_ID", "Territory_Size", "Salmon_biomass")
df.ripley.mcp$Salmon_biomass<-as.numeric(df.ripley.mcp$Salmon_biomass)

Bird.ID.jane<-mcp.territory.jane.ha$id
salmon.biomass.jane<-wilcoxsalmonpointcount.avg12to14[12,4]
stream.name.jane<-c("Jane", "Jane")
df.jane.mcp<-data.frame(stream.name.jane, Bird.ID.jane, jane.territories, salmon.biomass.jane)
colnames(df.jane.mcp) <- c("Stream", "Bird_ID", "Territory_Size", "Salmon_biomass")
df.jane.mcp$Salmon_biomass<-as.numeric(df.jane.mcp$Salmon_biomass)

Bird.ID.clatse<-mcp.territory.clatse.ha$id
salmon.biomass.clatse<-wilcoxsalmonpointcount.avg12to14[6,4]
stream.name.clatse<-c("Clatse", "Clatse", "Clatse", "Clatse", "Clatse")
df.clatse.mcp<-data.frame(stream.name.clatse, Bird.ID.clatse, clatse.territories, salmon.biomass.clatse)
colnames(df.clatse.mcp) <- c("Stream", "Bird_ID", "Territory_Size", "Salmon_biomass")
df.clatse.mcp$Salmon_biomass<-as.numeric(df.clatse.mcp$Salmon_biomass)

Bird.ID.fancy<-mcp.territory.fancy.ha$id
salmon.biomass.fancy<-wilcoxsalmonpointcount.avg12to14[7,4]
stream.name.fancy<-c("fancy", "fancy")
df.fancy.mcp<-data.frame(stream.name.fancy, Bird.ID.fancy, fancy.territories, salmon.biomass.fancy)
colnames(df.fancy.mcp) <- c("Stream", "Bird_ID", "Territory_Size", "Salmon_biomass")
df.fancy.mcp$Salmon_biomass<-as.numeric(df.fancy.mcp$Salmon_biomass)

Bird.ID.fannie<-mcp.territory.fannie.ha$id
salmon.biomass.fannie<-wilcoxsalmonpointcount.avg12to14[8,4]
stream.name.fannie<-c("fannie", "fannie", "fannie", "fannie", "fannie", "fannie", "fannie")
df.fannie.mcp<-data.frame(stream.name.fannie, Bird.ID.fannie, fannie.territories, salmon.biomass.fannie)
colnames(df.fannie.mcp) <- c("Stream", "Bird_ID", "Territory_Size", "Salmon_biomass")
df.fannie.mcp$Salmon_biomass<-as.numeric(df.fannie.mcp$Salmon_biomass)

Bird.ID.bull<-mcp.territory.bull.ha$id
salmon.biomass.bull<-wilcoxsalmonpointcount.avg12to14[3,4]
stream.name.bull<-c("Bullock", "Bullock", "Bullock", "Bullock", "Bullock", "Bullock", "Bullock")
df.bull.mcp<-data.frame(stream.name.bull, Bird.ID.bull, bullock.territories, salmon.biomass.bull)
colnames(df.bull.mcp) <- c("Stream", "Bird_ID", "Territory_Size", "Salmon_biomass")
df.bull.mcp$Salmon_biomass<-as.numeric(df.bull.mcp$Salmon_biomass)

all.territories.mcp<-rbind(df.ripley.mcp, df.jane.mcp, df.fancy.mcp, df.hooknose.mcp,df.clatse.mcp,  df.bull.mcp, df.fannie.mcp ) ###bind them all into one df
###all.territories.mcp$Stream <- factor(all.territories.mcp$Stream, levels = all.territories.mcp$Stream[order(all.territories.mcp$Territory_Size)])## only orders increasing

####### GRAPHS######
boxplot(ripley.territories, jane.territories, fancy.territories, hook.territories,bullock.territories, fannie.territories, clatse.territories, names=c( "Ripley", "Jane", "Fancy", "Hooknose","Bullock", "Fannie", "Clatse"))

p <- ggplot (all.territories.mcp, aes(Stream, Territory_Size))
p+ geom_boxplot()+
  ggtitle("Wren territory size by stream")+
  ylab("Territory Size (ha)")

#####Statistical tests######
fit.lm.stream.territory<-lm(Territory_Size~Stream, data=all.territories.mcp)
anova(fit.lm.stream.territory)


##plot of territory by biomass
#linear model
p.biomass <- ggplot (all.territories.mcp, aes(Salmon_biomass, Territory_Size))
p.biomass+ geom_point()+
  ggtitle("Wren territory size salmon biomass")+
  ylab("Territory Size (ha)")+
  geom_smooth(method="lm")



#non-linear
all.territories.mcp$log.salmon.biomass<-log(all.territories.mcp$Salmon_biomass)
p.biomass <- ggplot (all.territories.mcp, aes(log.salmon.biomass, Territory_Size))
p.biomass+ geom_point()+
  ggtitle("Wren territory size salmon biomass")+
  ylab("Territory Size (ha)")+
  geom_smooth(method="lm")

wes<-wesanderson::wes_palette(name= "Royal1",1, type="discrete")

all.territories.mcp$log.salmon.biomass<-log(all.territories.mcp$Salmon_biomass)
p.biomass <- ggplot (all.territories.mcp, aes(log.salmon.biomass, Territory_Size))
p.biomass+ geom_point()+
  ylab("Territory Size (ha)")+
  xlab("Log salmon biomass (3 year average)")+
  geom_smooth(method="lm", colour="orange2", alpha=0.2, size=1.6)+
  geom_point(color="seagreen2", size =4)+
  theme(
    axis.text = element_text(size = 14, colour="white"),
    axis.title.x= element_text(size=17, colour="white"),
    axis.title.y=element_text(size=17, colour="white"),
    axis.line= element_line(colour="lightgoldenrodyellow"),
    axis.line.x= element_line (colour = "white"),
    axis.line.y=element_line(colour="white"),
    panel.grid.major = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "black"),
    plot.background= element_rect(fill = "black"))
    




###evidence the mean territory size varries between streams


my.fit.streamterritory.lsmo <- lsmeans:::lsmeans(fit.lm.stream.territory, ~Stream, adjust="tukey")
stream.territory.lsmeans<-summary(my.fit.streamterritory.lsmo)
cld    (my.fit.streamterritory.lsmo)
pairs  (my.fit.streamterritory.lsmo)
confint(pairs  (my.fit.streamterritory.lsmo))


plot.stream.terrirory.lsmeans <- ggplot(data=stream.territory.lsmeans, aes(x=Stream, y=lsmean))+
  ggtitle("Compare the mean terriotyr size with 95% ci - from lsmeans")+
  ylab("Territory Size (ha)")+xlab("Stream")+
  geom_point()+
  geom_errorbar(aes(ymin=lower.CL, ymax=upper.CL), width=0.2)+
  geom_line(aes(group=1))
plot.stream.terrirory.lsmeans



#######Salmon biomass stats linear##########
fit.lm.stream.biomass<-lm(Territory_Size~Salmon_biomass, data=all.territories.mcp)
anova(fit.lm.stream.biomass)
summary(fit.lm.stream.biomass)


#######Salmon biomass stats log- linear##########
fit.lm.stream.log.biomass<-lm(Territory_Size~log.salmon.biomass, data=all.territories.mcp)
anova(fit.lm.stream.log.biomass)
summary(fit.lm.stream.log.biomass)
plot(fit.lm.stream.log.biomass)
