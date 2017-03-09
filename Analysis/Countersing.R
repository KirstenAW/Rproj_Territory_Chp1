#####Territory size by Salmon biomass#####
##one year before or average of 3 year

counter<-read.csv("Countersingpartial.csv", header = TRUE, as.is = TRUE)
str(counter)
counter$Stream<-as.factor(counter$Stream)

counter$counter.bybird<-counter$Total.countersing/counter$Num.bird
  

counter$Salmon.log.biomass<-log(counter$Salmon.biomass)

counter.bird.biomass <- ggplot (counter, aes(Salmon.log.biomass, counter.bybird))
counter.bird.biomass+ geom_point()+
  ylab("Countersing events by bird")+
  xlab("Log salmon biomass (3 year average)")+
  coord_cartesian(ylim=c(0, 11.5))+
  geom_smooth(method="lm", colour="orange2", alpha=0.2, size=1.6)+
  geom_point(color="tomato2", size =4)+
  theme(
    axis.text = element_text(size = 14, colour="white"),
    axis.title.x= element_text(size=17, colour="white"),
    axis.title.y=element_text(size=17, colour="white", angle=0),
    axis.line= element_line(colour="lightgoldenrodyellow"),
    axis.line.x= element_line (colour = "white"),
    axis.line.y=element_line(colour="white"),
    panel.grid.major = element_line(colour = "black"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "black"),
    plot.background= element_rect(fill = "black"))

fit.lm.counter.bird.biomass<-lm(counter.bybird~Salmon.log.biomass, data=counter)
anova(fit.lm.counter.bird.biomass)
summary(fit.lm.counter.bird.biomass)

fit.lm.counter.biomass<-lm(Total.countersing~Salmon.log.biomass, data=counter)
anova(fit.lm.counter.biomass)
summary(fit.lm.counter.biomass)

fit.lm.counter.biomass<-lm(Total.countersing~Salmon.biomass, data=counter)
anova(fit.lm.counter.biomass)
summary(fit.lm.counter.biomass)



counter.log.biomass <- ggplot (counter, aes(Salmon.log.biomass, Total.countersing ))
counter.log.biomass+ geom_point()+
  geom_smooth(method="lm", colour="orange2", alpha=0.2, size=1.6)+
  coord_cartesian(ylim=c(0, 60))+
  xlab("Log salmon biomass (3 year average)")+
  ylab("Number of countersing events")+
  geom_point(color="red", size =4)+
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

counter.log.biomass <- ggplot (counter, aes(Salmon.biomass, Total.countersing ))
counter.log.biomass+ geom_point()+
  geom_smooth(method="lm", colour="orange2", alpha=0.2, size=1.6)

anove()