#Created October 2015 by Marlene Wagner
#updated November 2015 
#updated March 2015 to include Kirsten Wilcox study sites
rm(list=ls())
library(plyr)
library(dplyr)
library(ggplot2)
library(tidyr)
library(reshape2)

####ENTER DATA####

salmon <- read.csv("RevisedEscapementMaster2016.csv", header=TRUE, as.is=TRUE, strip.white=TRUE) #latest salmon data as of 11.20.2014

#check out data structure:
str(salmon)

#change some variables to correct type of data:
salmon[ salmon=="AP"] <- NA
salmon[ salmon=="FP"] <- NA
salmon$site <- as.factor(salmon$site)
salmon$species <- as.factor(salmon$species)
salmon$escapement<- as.numeric(salmon$escapement)
str(salmon) #looks good but need to figure out what to do with dates (probably a discussion for Jane and John)

# mean 2000's is a weird thing in here.  For now, remove this but see what John wants...
salmon <- salmon[!(salmon$year %in% "mean 2000s"),] #(revisit later)removes rows with mean 2000s so we can make date format
salmon$year <- factor(salmon$year)

#remove unneccessary columns:
salmon$Data.source <- NULL
salmon$stream.key <- NULL
salmon$X<-NULL
salmon$X.1<-NULL
salmon$X.2<-NULL
salmon$X.3<-NULL
salmon$X.4<-NULL
salmon$X.5<-NULL
salmon$X.6<-NULL

#check data structure:
str(salmon)
levels(salmon$year)
levels(salmon$site)
levels(salmon$species)

#Make dataframe into something you can summarize and make new variables with:
s <- spread(salmon, species, escapement)

####CALCULATE SALMON BIOMASS####
#compute biomass for each species
#for this region, the Reynolds Lab (to the best of my knowledge) has been using 3.5 kg for chum and 1.2 kg for pink
chumweight <- 3.5
pinkweight <- 1.2
s <- mutate(s, chumbiomass.kg=chum*chumweight) #make column of biomass for chum
s <- mutate(s, pinkbiomass.kg=pink*pinkweight) #make column of biomass for pink
s <- mutate(s, chumpinkbiomass.kg=chumbiomass.kg+pinkbiomass.kg) #make column of biomass for both pink and chum
str(s) #recheck data structure


####Field and Reynolds 2013####
#used 2008 and 2009 and this analysis is presented below here:
fieldreynolds2013 <- filter(s, year==2008 | year==2009) 
fieldreynolds2013$year <- factor(fieldreynolds2013$year)
str(fieldreynolds2013)

#picked out sites used in this study with exception of (for simplicity) Fannie Cove and Quartcha-Lee because those are summed together:
fieldreynolds2013 <- filter(fieldreynolds2013, site=="Bullock Main" | site=="Clatse" | site=="Evans Inlet" | site=="Farm Bay" | site=="Fannie Cove" | 
                              site=="Fell Creek" | site=="Hooknose" | site=="Kill Creek" | site=="Kunsoot Main" | site=="Kunsoot South" | 
                              site=="Mosquito Bay" | site=="Neekas" | site=="QuartchaLee" | site=="Roscoe Main" | site=="Sagar" | site=="Troupe North")
fieldreynolds2013$site <- factor(fieldreynolds2013$site)

#remove some unnecessary variables:
fieldreynolds2013$Data.source <- NULL
fieldreynolds2013$coho <- NULL
fieldreynolds2013$`pink even` <- NULL
fieldreynolds2013$`pink odd` <- NULL
fieldreynolds2013$sockeye <- NULL
fieldreynolds2013$stream.key <- NULL
str(fieldreynolds2013)

#calculate across year averages:
fieldreynolds2013.avg0809 <- ddply(fieldreynolds2013, ~site, summarize,
                           avgchumbiomass.kg0809 = mean(chumbiomass.kg, na.rm=T),
                           avgpinkbiomass.kg0809 = mean(pinkbiomass.kg, na.rm=T),
                           totalchumpinkbiomass.kg0809 = sum(avgpinkbiomass.kg0809, avgchumbiomass.kg0809, na.rm=T))


####Field and Reynolds 2011####
#used a 3 year mean of salmon biomass from 2006 to 2008 and this analysis is presented below here
fieldreynolds2011 <- filter(s, year==2006 | year==2007 | year==2008 ) 
fieldreynolds2011$year <- factor(fieldreynolds2011$year)
str(fieldreynolds2011)

#picked out sites used in this study with exception of (for simplicity) Fannie Cove and Quartcha-Lee because those are summed and streams with 0 fish:
fieldreynolds2011 <- filter(fieldreynolds2011, site=="Ada" | site=="Bullock Main" | site=="Clatse" | site=="Codville" |site=="Evans East" | site=="Fancy Head" | 
                              site=="Fell Creek" | site=="Hooknose" | site=="Kill Creek" | site=="Kunsoot Main" | site=="Kunsoot South" | 
                              site=="Mosquito Bay" | site=="Neekas" | site=="QuartchaLee" | site=="Rainbow" |site=="Roscoe Main" | site=="Sagar" | site=="Troupe North" | site=="Troupe South" )
fieldreynolds2011$site <- factor(fieldreynolds2011$site)
str(fieldreynolds2011)

#remove some unnecessary variables:
fieldreynolds2011$Data.source <- NULL
fieldreynolds2011$coho <- NULL
fieldreynolds2011$`pink even` <- NULL
fieldreynolds2011$`pink odd` <- NULL
fieldreynolds2011$sockeye <- NULL
fieldreynolds2011$stream.key <- NULL
str(fieldreynolds2011)

#calculate across year averages:
fieldreynolds2011.avg06to08 <- ddply(fieldreynolds2011, ~site, summarize,
                                   avgchumbiomass.kgavg06to08 = mean(chumbiomass.kg, na.rm=T),
                                   avgpinkbiomass.kgavg06to08 = mean(pinkbiomass.kg, na.rm=T),
                                   totalchumpinkbiomass.kgavg06to08 = sum(avgpinkbiomass.kgavg06to08, avgchumbiomass.kgavg06to08, na.rm=T))

####WAGNER####
#Wagner used a 3 year mean from 2009 to 2011 and this analysis is presented below here:
wagnersalmonpointcount <- filter(s, year==2009 | year==2010 | year==2011 ) 
wagnersalmonpointcount$year <- factor(wagnersalmonpointcount$year)
str(wagnersalmonpointcount)

#picked out sites used in this study:
wagnersalmonpointcount <- filter(wagnersalmonpointcount, site=="Beales Left" | site=="Bullock Main" | site=="Clatse" | site=="Fancy Right" | 
                              site=="Fannie Left" | site=="Fell Creek" | site=="Goat Bushu" | site=="Hooknose" | site=="Kill Creek" | site=="Kunsoot Main" |
                              site=="Neekas" | site=="Quartcha" )
wagnersalmonpointcount$site <- factor(wagnersalmonpointcount$site)
str(wagnersalmonpointcount)

#what percentage of coho and sockeye are on the streams?
wagnersalmonnumbers <- ddply(wagnersalmonpointcount, ~site, summarize,
                             totalchum=sum(chum, na.rm=T),
                             totalpink=sum(pink, na.rm=T),
                             totalsockeye=sum(sockeye, na.rm=T),
                             totalcoho=sum(coho, na.rm=T))
wagnersalmonnumbers <- mutate(wagnersalmonnumbers, pinkchum=totalchum+totalpink)
wagnersalmonnumbers <- mutate(wagnersalmonnumbers, cohosock=totalsockeye+totalcoho)
summary(wagnersalmonnumbers)
summarize(wagnersalmonnumbers, sumpinkchum=sum(pinkchum))#=409390
summarize(wagnersalmonnumbers, sumcohosock=sum(cohosock))#=2883
#divide cohosock by pinkchum:
#percentage is 0.7% of fish were coho or sockeye

#calculate across year averages:
wagnersalmonpointcount.avg09to11 <- ddply(wagnersalmonpointcount, ~site, summarize,
                                    avgchumbiomass.kg.avg09to11 = mean(chumbiomass.kg, na.rm=T),
                                    avgpinkbiomass.kg.avg09to11 = mean(pinkbiomass.kg, na.rm=T),
                                    totalchumpinkbiomass.kg.avg09to11 = sum(avgpinkbiomass.kg.avg09to11, 
                                    avgchumbiomass.kg.avg09to11, na.rm=T))

write.csv(wagnersalmonpointcount.avg09to11, "marlenefinalsalmoncounts2.csv")

####WILCOX####
#Wilcox (exploratory) used a 3 year mean from 20012 to 2014 and this analysis is presented below here:
wilcoxsalmonpointcount <- filter(s, year==2014 | year==2013 | year==2012 ) 
wilcoxsalmonpointcount$year <- factor(wilcoxsalmonpointcount$year)
str(wilcoxsalmonpointcount)

#picked out sites used and other potential sites for 2016 field work:
wilcoxsalmonpointcount <- filter(wilcoxsalmonpointcount, site=="Beales Left" | site=="Bullock Main" | site=="Clatse" | site=="Fancy Right" | 
                                   site=="Fannie Left" | site=="Fell Creek" | site=="Goat Bushu" | site=="Hooknose" | site=="Kill Creek" | site=="Kunsoot Main" |
                                   site=="Mosquito Bay Right" | site=="Mosquito Bay Left" | site=="Ada" | site=="Jane" | site=="Goat Bushu" | 
                                   site=="Cheenis"| site=="Bullock Square"| site=="Roscoe Main"| site=="Sagar" | site=="Troupe North" )
wilcoxsalmonpointcount$site <- factor(wilcoxsalmonpointcount$site)
                                  
wilcoxsalmonpointcount$site <- factor(wilcoxsalmonpointcount$site)
str(wilcoxsalmonpointcount)

#what percentage of coho and sockeye are on the streams?
wilcoxsalmonnumbers <- ddply(wilcoxsalmonpointcount, ~site, summarize,
                             totalchum=sum(chum, na.rm=T),
                             totalpink=sum(pink, na.rm=T),
                             totalsockeye=sum(sockeye, na.rm=T),
                             totalcoho=sum(coho, na.rm=T))
wilcoxsalmonnumbers <- mutate(wilcoxsalmonnumbers, pinkchum=totalchum+totalpink)
wilcoxsalmonnumbers <- mutate(wilcoxsalmonnumbers, cohosock=totalsockeye+totalcoho)
summary(wilcoxsalmonnumbers)
summarize(wilcoxsalmonnumbers, sumpinkchum=sum(pinkchum))#=130238
summarize(wilcoxsalmonnumbers, sumcohosock=sum(cohosock))#=520
#percentage is >1% of fish were coho or sockeye

#calculate across year averages:
wilcoxsalmonpointcount.avg12to14 <- ddply(wilcoxsalmonpointcount, ~site, summarize,
                                          avgchumbiomass.kg.avg12to14 = mean(chumbiomass.kg, na.rm=T),
                                          avgpinkbiomass.kg.avg12to14 = mean(pinkbiomass.kg, na.rm=T),
                                          totalchumpinkbiomass.kg.avg12to14 = sum(avgpinkbiomass.kg.avg12to14, 
                                          avgchumbiomass.kg.avg12to14, na.rm=T))

#reorder site based on smallest to largest biomass
wilcoxsalmonpointcount.avg12to14$site<-factor(wilcoxsalmonpointcount.avg12to14$site, levels=wilcoxsalmonpointcount.avg12to14$site[order(wilcoxsalmonpointcount.avg12to14$totalchumpinkbiomass.kg.avg12to14)])
###x$name <- factor(x$name, levels = x$name[order(x$val)])

write.csv(wilcoxsalmonpointcount.avg12to14, "kirstenexploresalmoncounts.csv")
str(wilcoxsalmonpointcount.avg12to14)

#plot the data:

k <- ggplot(data=wilcoxsalmonpointcount.avg12to14, aes(x=site, y=totalchumpinkbiomass.kg.avg12to14))+ 
            geom_point()+
            ggtitle("Average Salmon Biomass for 2012 to 2014 by Stream")+
            ylab("Average Chum and Pink Biomass-kg")
k



##########WILCOX without Roscoe Main############
wilcoxsalmonpointcount1 <- filter(wilcoxsalmonpointcount, site=="Beales Left" | site=="Bullock Main" | site=="Clatse" | site=="Fancy Right" | 
                                   site=="Fannie Left" | site=="Fell Creek" | site=="Goat Bushu" | site=="Hooknose" | site=="Kill Creek" | site=="Kunsoot Main" |
                                   site=="Mosquito Bay Right" | site=="Mosquito Bay Left" | site=="Ada" | site=="Jane" | site=="Goat Bushu" | 
                                   site=="Cheenis"| site=="Bullock Square"| site=="Sagar" | site=="Troupe North" )
wilcoxsalmonpointcount1$site <- factor(wilcoxsalmonpointcount1$site)

wilcoxsalmonpointcount1$site <- factor(wilcoxsalmonpointcount1$site)
str(wilcoxsalmonpointcount1)

#what percentage of coho and sockeye are on the streams?
wilcoxsalmonnumbers1 <- ddply(wilcoxsalmonpointcount1, ~site, summarize,
                             totalchum=sum(chum, na.rm=T),
                             totalpink=sum(pink, na.rm=T),
                             totalsockeye=sum(sockeye, na.rm=T),
                             totalcoho=sum(coho, na.rm=T))
wilcoxsalmonnumbers1 <- mutate(wilcoxsalmonnumbers1, pinkchum=totalchum+totalpink)
wilcoxsalmonnumbers1 <- mutate(wilcoxsalmonnumbers1, cohosock=totalsockeye+totalcoho)
summary(wilcoxsalmonnumbers1)
summarize(wilcoxsalmonnumbers1, sumpinkchum=sum(pinkchum))#=130238
summarize(wilcoxsalmonnumbers1, sumcohosock=sum(cohosock))#=520
#percentage is >1% of fish were coho or sockeye

#calculate across year averages:
wilcoxsalmonpointcount1.avg12to14 <- ddply(wilcoxsalmonpointcount1, ~site, summarize,
                                          avgchumbiomass.kg.avg12to14 = mean(chumbiomass.kg, na.rm=T),
                                          avgpinkbiomass.kg.avg12to14 = mean(pinkbiomass.kg, na.rm=T),
                                          totalchumpinkbiomass.kg.avg12to14 = sum(avgpinkbiomass.kg.avg12to14, 
                                                                                  avgchumbiomass.kg.avg12to14, na.rm=T))

#reorder site based on smallest to largest biomass
wilcoxsalmonpointcount1.avg12to14$site<-factor(wilcoxsalmonpointcount1.avg12to14$site, levels=wilcoxsalmonpointcount1.avg12to14$site[order(wilcoxsalmonpointcount1.avg12to14$totalchumpinkbiomass.kg.avg12to14)])
#####x$name <- factor(x$name, levels = x$name[order(x$val)])

write.csv(wilcoxsalmonpointcount.avg12to14, "kirstenexploresalmoncounts.csv")
str(wilcoxsalmonpointcount1.avg12to14)

#plot the data with highlighted streams you've already done

subdata <- subset(wilcoxsalmonpointcount1.avg12to14, site %in% c("Fancy Right", "Kill Creek", "Fannie Left", "Clatse", "Beales Left"))

k1 <- ggplot(data=wilcoxsalmonpointcount1.avg12to14, aes(x=site, y=totalchumpinkbiomass.kg.avg12to14))+ 
  geom_point(size=5)+
  ggtitle("Average Salmon Biomass for 2012 to 2014 by Stream")+
  ylab("Average Chum and Pink Biomass (kg)")+
  geom_point(data=subdata, colour="red", size =5.2) 
k1

k1+ theme(axis.title.x = element_text(face="bold", size=20),
        axis.text.x  = element_text( angle=90, vjust=0.5, size=16),
        axis.text.y=element_text( vjust=0.5, size=16), 
        axis.title.y = element_text( size=20))

