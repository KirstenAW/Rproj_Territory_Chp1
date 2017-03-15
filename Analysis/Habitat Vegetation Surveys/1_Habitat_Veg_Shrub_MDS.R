####Habitat Veg####
rm(list=ls())
library(psych)
library(car)
library(plyr)
library(vegan)
library(reshape2)
library(tidyr)
library(ape)
library(picante)
library(ggplot2)
library(dplyr)
##########SHRUBS##############
setwd("~/SFU/Masters/R Projects/Rproj_Territory_Chp1/Raw_data/Habitat Data")
habvegshrub<-read.csv("Habitat_Veg_2016_Shrub_V1.csv", header=TRUE, as.is=TRUE, fill=TRUE)
head(habvegshrub)


####recode all blanks and NAs as zeros, all T and TR as 2
habvegshrub[habvegshrub==""]  <- 0 
habvegshrub[habvegshrub=="TR"]<-2
habvegshrub[habvegshrub=="T"]<-2
habvegshrub[is.na(habvegshrub)] <- 0 
str(habvegshrub)

any(habvegshrub==0, na.rm=TRUE)
any(habvegshrub=="NA", na.rm=TRUE)
any(habvegshrub=="T", na.rm=TRUE)
any(habvegshrub=="TR", na.rm=TRUE)
any(habvegshrub=="", na.rm=TRUE)


#habvegshrub$Stream<-as.factor(habvegshrub$Stream)
#habvegshrub$Location<-as.factor(habvegshrub$Location)
#habvegshrub$Shrub_Species<-as.factor(habvegshrub$Shrub_Species)
#habvegshrub$Low_Cover<-as.numeric(habvegshrub$Low_Cover)
#habvegshrub$Medium_Cover<-as.numeric(habvegshrub$Medium_Cover)
#habvegshrub$High_Cover<-as.numeric(habvegshrub$High_Cover)
#str(habvegshrub)


######CHECK occurences######
Stream.names <- table(habvegshrub$Stream)
Stream.names
names(Stream.names)

Location.names <- table(habvegshrub$Location)
Location.names
names(Location.names)

Species.names <- table(habvegshrub$Shrub_Species)
Species.names
names(Species.names)

Low.numb <- table(habvegshrub$Low_Cover)
Low.numb
names(Species.names)

########Change from long to wide format##############

habvegshrub$Stream.Location<-paste(habvegshrub$Stream,habvegshrub$Location) ####combine columns to add all shrub species to plots in next step
habvegshrub$Stream<-NULL
habvegshrub$Location<-NULL

habvegshrub[is.na(habvegshrub)] <- 0 

habvegshrubexpand<-tidyr::complete(habvegshrub, Stream.Location, Shrub_Species) #####fill in shrub species for all plots

habvegshrubexpand[is.na(habvegshrubexpand)] <- 0 ####convert new NAs to zero percent cover

habvegshrubwide<-reshape(habvegshrub, direction = "wide", idvar="Stream.Location", timevar="Shrub_Species") ###convert to wide format

habvegshrubwide[is.na(habvegshrubwide)] <- 0 ###remove NAs again (not sure how they are here again)

habvegshrubwide$Low_Cover.NA<-NULL ####random columns in here, nothing in them
habvegshrubwide$Medium_Cover.NA<-NULL
habvegshrubwide$High_Cover.NA<-NULL

habvegshrubwide$Low_Cover.0<-NULL ####random columns in here, nothing in them
habvegshrubwide$Medium_Cover.0<-NULL
habvegshrubwide$High_Cover.0<-NULL

####now we have correct number of columns (20 species x 3 heights plus 1 location column)
###remove low abundance species? remove species that show up in less than 1% of the plots

Species.names
###divide by number of plots=392
##remove ARDI, ILAQ, TABR
habvegshrubwide$Low_Cover.ARDI<-NULL
habvegshrubwide$Medium_Cover.ARDI<-NULL
habvegshrubwide$High_Cover.ARDI<-NULL

habvegshrubwide$Low_Cover.ILAQ<-NULL
habvegshrubwide$Medium_Cover.ILAQ<-NULL
habvegshrubwide$High_Cover.ILAQ<-NULL

habvegshrubwide$Low_Cover.TABR<-NULL
habvegshrubwide$Medium_Cover.TABR<-NULL
habvegshrubwide$High_Cover.TABR<-NULL

str(habvegshrubwide)
habvegshrubwide$Stream.Location<-as.factor(habvegshrubwide$Stream.Location)
colnames(habvegshrubwide)
nms <- c("Low_Cover.ABAM" ,   "Medium_Cover.ABAM" ,"High_Cover.ABAM", "Low_Cover.GASH",   "Medium_Cover.GASH", "High_Cover.GASH",
         "Low_Cover.MEFE" ,   "Medium_Cover.MEFE" ,"High_Cover.MEFE"  , "Low_Cover.TSHE"  ,  "Medium_Cover.TSHE", "High_Cover.TSHE" ,  "Low_Cover.VASP",   
         "Medium_Cover.VASP" ,"High_Cover.VASP" ,  "Low_Cover.ALRU" ,   "Medium_Cover.ALRU" ,"High_Cover.ALRU"  , "Low_Cover.RUSP"  ,  "Medium_Cover.RUSP",
         "High_Cover.RUSP" ,  "Low_Cover.THPL",    "Medium_Cover.THPL" ,"High_Cover.THPL" ,  "Low_Cover.PICO"  ,  "Medium_Cover.PICO", "High_Cover.PICO",  
         "Low_Cover.CHNO"   , "Medium_Cover.CHNO" ,"High_Cover.CHNO" ,  "Low_Cover.PISI"   , "Medium_Cover.PISI", "High_Cover.PISI"  , "Low_Cover.RIBR",   
         "Medium_Cover.RIBR" ,"High_Cover.RIBR" ,  "Low_Cover.OPHO",    "Medium_Cover.OPHO" ,"High_Cover.OPHO"   ,"Low_Cover.SARA"  ,  "Medium_Cover.SARA",
         "High_Cover.SARA"  , "Low_Cover.MAFU"  ,  "Medium_Cover.MAFU", "High_Cover.MAFU" ,  "Low_Cover.RIRU" ,   "Medium_Cover.RIRU", "High_Cover.RIRU",  
         "Low_Cover.TSME" ,   "Medium_Cover.TSME" ,"High_Cover.TSME")  
habvegshrubwide[nms] <- lapply(habvegshrubwide[nms], as.numeric) 
str(habvegshrubwide)


###subset data into different height categories or could average them? investigate further

colnames(habvegshrubwide)
habvegshrub.lowcov <- habvegshrubwide %>% dplyr:: select(grep("Stream.Location", colnames(habvegshrubwide)),
                                                         grep("Low_Cover", colnames(habvegshrubwide)))
habvegshrub.medcov <-habvegshrubwide %>% dplyr:: select(grep("Stream.Location", colnames(habvegshrubwide)),
                                                        grep("Medium_Cover", colnames(habvegshrubwide)))
habvegshrub.highcov <-habvegshrubwide %>% dplyr:: select(grep("Stream.Location", colnames(habvegshrubwide)),
                                                        grep("High_Cover", colnames(habvegshrubwide)))

##convert column name to row names for NMDS plot
rownames(habvegshrub.highcov) <- habvegshrub.highcov$Stream.Location
rownames(habvegshrub.medcov) <- habvegshrub.medcov$Stream.Location
rownames(habvegshrub.lowcov) <- habvegshrub.lowcov$Stream.Location

######Now wide cleaned up datasets, save as generated data
str(habvegshrub.highcov)
str(habvegshrub.medcov)
str(habvegshrub.lowcov)

list.files("C:/Users/kirsten/Documents/SFU/Masters/R Projects/Rproj_Territory_Chp1/Generated data/...") ##check file path for saving in generated data file
write.csv(habvegshrub.highcov, file="C:/Users/kirsten/Documents/SFU/Masters/R Projects/Rproj_Territory_Chp1/Generated data/Habitat_Veg_2016_Shrub_Wideformat_HighCover_V1.csv")
write.csv(habvegshrub.medcov, file="C:/Users/kirsten/Documents/SFU/Masters/R Projects/Rproj_Territory_Chp1/Generated data/Habitat_Veg_2016_Shrub_Wideformat_MediumCover_V1.csv")
write.csv(habvegshrub.lowcov, file="C:/Users/kirsten/Documents/SFU/Masters/R Projects/Rproj_Territory_Chp1/Generated data/Habitat_Veg_2016_Shrub_Wideformat_LowCover_V1.csv")

#####ADD IN 2015 DATA




#####Check number of occurences within each cover level <5% then remove####
#high cover
table(habvegshrub.highcov$High_Cover.PICO)
#need to remove PICO
table(habvegshrub.highcov$High_Cover.ABAM)
table(habvegshrub.highcov$High_Cover.OPHO)
#need to remove OPHO
table(habvegshrub.highcov$High_Cover.CHNO) ##keep CHNO
table(habvegshrub.highcov$High_Cover.PISI) ##keep PISI
table(habvegshrub.highcov$High_Cover.ALRU) ###in 4.6% plots, maybe remove?
table(habvegshrub.highcov$High_Cover.MAFU) ##remove MAFU
table(habvegshrub.highcov$High_Cover.RIBR) ##keep RIBR
table(habvegshrub.highcov$High_Cover.SARA) ##remove SARA
table(habvegshrub.highcov$High_Cover.RIRU) ##remove RIRU

habvegshrub.highcov$High_Cover.PICO<-NULL
habvegshrub.highcov$High_Cover.OPHO<-NULL
habvegshrub.highcov$High_Cover.MAFU<-NULL
habvegshrub.highcov$High_Cover.SARA<-NULL
habvegshrub.highcov$High_Cover.RIRU<-NULL

##medium cover
table(habvegshrub.medcov$Medium_Cover.ABAM)
table(habvegshrub.medcov$Medium_Cover.OPHO) #need to remove OPHO
table(habvegshrub.medcov$Medium_Cover.CHNO) ##keep CHNO
table(habvegshrub.medcov$Medium_Cover.PISI) ##keep PISI
table(habvegshrub.medcov$Medium_Cover.ALRU) ###in 5% keep
table(habvegshrub.medcov$Medium_Cover.MAFU) ##remove MAFU
table(habvegshrub.medcov$Medium_Cover.RIBR) ##keep RIBR
table(habvegshrub.medcov$Medium_Cover.SARA) ##remove SARA
table(habvegshrub.medcov$Medium_Cover.RIRU) #remove RIRU
table(habvegshrub.medcov$Medium_Cover.GASH)
table(habvegshrub.medcov$Medium_Cover.TSME) ##remove TSME

habvegshrub.medcov$Medium_Cover.OPHO<-NULL
habvegshrub.medcov$Medium_Cover.MAFU<-NULL
habvegshrub.medcov$Medium_Cover.SARA<-NULL
habvegshrub.medcov$Medium_Cover.RIRU<-NULL
habvegshrub.medcov$Medium_Cover.TSME<-NULL

######Diversity index######
###High Cover
##change column to row names and remove column
rownames(habvegshrub.highcov)
habvegshrub.highcov <- habvegshrub.highcov[, -1]

habvegshrub.highcov[is.na(habvegshrub.highcov)] <- 0

##many multivar sensite to total abundance
##check total abundance
str(habvegshrub.highcov)

apply(habvegshrub.highcov, 1 ,sum) ##check total abundance in each sample
#turn percent cover to relative abundance by dividing each value by sample total abundance
habvegshrub.highcov <- decostand(habvegshrub.highcov, method = "total")
apply(habvegshrub.highcov, 1 ,sum)
habvegshrub.highcov[1:5, 1:5] ##look at transformed data



##medium cover
rownames(habvegshrub.medcov)
habvegshrub.medcov <- habvegshrub.medcov[, -1]
habvegshrub.medcov[is.na(habvegshrub.medcov)] <- 0
str(habvegshrub.medcov)
apply(habvegshrub.medcov, 1 ,sum)
habvegshrub.medcov <- decostand(habvegshrub.medcov, method = "total")
apply(habvegshrub.medcov, 1 ,sum)
habvegshrub.medcov[1:5, 1:5]

####add in site metabata

####find which row is all zeros due to removals of too low species####
##High cover
write.csv(habvegshrub.highcov, "test.csv") ##excel sum to 0 remove rows as vegdist can't handle
#remove zero sum rows
remove<- c("Ada G3","Bullock H2", "Fell E6", "Hooknose B2", "Hooknose C2", "Hooknose F3", "Hooknose G4", "Hooknose H4", "Kunsoot E4", "Kunsoot E5")# list of rownames I would like to remove from file "data"
habvegshrub.highcov<- habvegshrub.highcov[!row.names(habvegshrub.highcov)%in%remove,]

##medium height
write.csv(habvegshrub.medcov, "medtest.csv") 
remove<- c("Hooknose H4", "Hooknose G4", "Ada E4")
habvegshrub.medcov<- habvegshrub.medcov[!row.names(habvegshrub.medcov)%in%remove,]
write.csv(habvegshrub.medcov, "medtest.csv") 

###disimilarity####
###high cover
habvegshrub.highcov.bc.dist <- vegdist(habvegshrub.highcov, method = "bray")
comm.bc.clust <- hclust(habvegshrub.highcov.bc.dist, method = "average")
# plot cluster diagram
plot(comm.bc.clust, ylab = "Bray-Curtis dissimilarity")
str(habvegshrub.highcov)

###medium cover
habvegshrub.medcov.bc.dist <- vegdist(habvegshrub.medcov, method = "bray")
comm.bc.med.clust <- hclust(habvegshrub.medcov.bc.dist, method = "average")
# plot cluster diagram
plot(comm.bc.med.clust, ylab = "Bray-Curtis dissimilarity")


#####METAMDS####
##see michael's code  n > 2*k + 1
##high cov
habvegshrub.highcov.mds <- metaMDS(habvegshrub.highcov, try=100, trace = FALSE, k = 7,  noshare=TRUE, dist="bray")
plot(habvegshrub.highcov.mds, type = "t")
stressplot(habvegshrub.highcov.mds)

habvegshrub.highcov.mds2 <- as.data.frame(habvegshrub.highcov.mds$points[,1:7]) # yourdata.mds$points[ , 1:k ] extracts the ordination points
##after that I added on my columns onto the zoop.mds2 dataframe for period, site, date, and additional abiotic variables (in your case wren territory Yes or No)
habvegshrub.highcov.mds2 <- tibble::rownames_to_column(habvegshrub.highcov.mds2, "Stream.Location") ###separate Stream and Location 
habvegshrub.highcov.mds2<- habvegshrub.highcov.mds2 %>% separate(Stream.Location, c("Stream.", "Location"))

##medium cover

habvegshrub.medcov.mds <- metaMDS(habvegshrub.medcov, try=100, trace = FALSE, k = 5,  noshare=TRUE, dist="bray")
habvegshrub.medcov.mds 
plot(habvegshrub.medcov.mds, type = "t")
stressplot(habvegshrub.medcov.mds)
habvegshrub.medcov.mds2 <- as.data.frame(habvegshrub.medcov.mds$points[,1:5]) 
habvegshrub.medcov.mds2 <- tibble::rownames_to_column(habvegshrub.medcov.mds2, "Stream.Location") ###separate Stream and Location 
habvegshrub.medcov.mds2<- habvegshrub.medcov.mds2 %>% separate(Stream.Location, c("Stream.", "Location"))

#from there I used ggplot, example:
#high cover
ggplot(habvegshrub.highcov.mds2, aes(MDS1,MDS2, colour = Stream.)) +
geom_point()

##medium cover
ggplot(habvegshrub.medcov.mds2, aes(MDS1,MDS2, colour = Stream.)) +
  geom_point()

##add elipses to site
mds.fig <- ordiplot(habvegshrub.highcov.mds, type = "none")
# plot just the samples, colour by habitat, pch=19 means plot a circle
points(mds.fig, "sites", pch = 19, col = "green", select = habvegshrub.highcov.mds2$Stream. == 
         "Ada")
points(mds.fig, "sites", pch = 19, col = "yellow", select = habvegshrub.highcov.mds2$Stream. == 
         "Bullock")
points(mds.fig, "sites", pch = 19, col = "red", select = habvegshrub.highcov.mds2$Stream. == 
         "Fell")
points(mds.fig, "sites", pch = 19, col = "blue", select = habvegshrub.highcov.mds2$Stream. == 
         "Hooknose")
points(mds.fig, "sites", pch = 19, col = "orange", select = habvegshrub.highcov.mds2$Stream. == 
         "Jane")
points(mds.fig, "sites", pch = 19, col = "purple", select = habvegshrub.highcov.mds2$Stream. == 
         "Troupe")
# add confidence ellipses around habitat types
ordiellipse(habvegshrub.highcov.mds, habvegshrub.highcov.mds2$Stream., conf = 0.90, label = TRUE)
# overlay the cluster results we calculated earlier

ggplot(habvegshrub.highcov.mds2, aes(MDS1,MDS2, colour = Stream.)) +
  geom_point()+
stat_ellipse(geom="polygon", level=0.90, aes(group = Stream.), fill = NA, size=1.2)

##ellipses for medium plot
ggplot(habvegshrub.medcov.mds2, aes(MDS1,MDS2, colour = Stream.)) +
  geom_point()+
  stat_ellipse(geom="polygon", level=0.90, aes(group = Stream.), fill = NA, size=1.2)


###ADD in slope and wren territory (yes or no) 

###########ABSOLUTE#####################
####recode all blanks and NAs as zeros, all T and TR as 2
habvegabsolute[habvegabsolute==""]  <- 0 
habvegabsolute[is.na(habvegabsolute)] <- 0
habvegabsolute[habvegabsolute=="TR"]<-2
habvegabsolute[habvegabsolute=="T"]<-2


any(habvegabsolute==0, na.rm=TRUE)
any(habvegabsolute=="NA")
any(habvegabsolute=="T", na.rm=TRUE)
any(habvegabsolute=="TR", na.rm=TRUE)
any(habvegabsolute=="", na.rm=TRUE)

####recode as factor
habvegabsolute$Stream<-as.factor(habvegabsolute$Stream)
habvegabsolute$Location<-as.factor(habvegabsolute$Location)
habvegabsolute$Abs_Forb_Cover<-as.numeric(habvegabsolute$Abs_Forb_Cover)
habvegabsolute$Logs_Stumps<-as.numeric(habvegabsolute$Logs_Stumps)
habvegabsolute$Bare_Ground<-as.numeric(habvegabsolute$Bare_Ground)
habvegabsolute$Standing_Water<-as.numeric(habvegabsolute$Standing_Water)
habvegabsolute$Moss<-as.numeric(habvegabsolute$Moss)
habvegabsolute$Litter<-as.numeric(habvegabsolute$Litter)
habvegabsolute$Rock<-as.numeric(habvegabsolute$Rock)
habvegabsolute$Running_Water<-as.numeric(habvegabsolute$Running_Water)
habvegabsolute$Salmon_Stream<-as.numeric(habvegabsolute$Salmon_Stream)
habvegabsolute$Cabin<- as.numeric(habvegabsolute$Cabin)

habvegabsolute[is.na(habvegabsolute)] <- 0

###
habvegabsolute$Stream <- NULL
habvegabsolute$Location<-NULL


####PCA?
habvegasolute_PCA<-prcomp(habvegabsolute, cor=TRUE)
summary(habvegasolute_PCA)
pcascores <- predict(habvegasolute_PCA)
biplot(habvegasolute_PCA)
plot(habvegasolute_PCA, main="")
pairs(habvegabsolute)
corhabvegabsolute<-cor(habvegabsolute)
corhabvegabsolute


########Marlene's 
wshedpca <-read.csv("forpcawatershedanlysisinputdata.csv", header=T)

head(wshedpca)
str(wshedpca)
wshedpca$X <- NULL
finalwshed_PCA=prcomp(wshedpca, scale=T) #run PCA with scaled data
summary(finalwshed_PCA) #provides stdev, proportion of variance, cumulative variance
finalwshed_PCA #displays rotations
pcascores <- predict(finalwshed_PCA) #calculates PC scores for each plot
biplot(finalwshed_PCA)
plot(finalwshed_PCA, main="")
pairs(wshedpca)
cor(wshedpca)
write.csv(pcascores, "pcascoresforwshedPCA3variables.csv")

####Multicollinearity#########


vif(habvegasolute_PCA)
