setwd("C:\\Users\\Bahus\\Desktop\\Coursera\\Reproducible-Research\\RepData_PeerAssessment2")

data<-read.csv(bzfile("repdata-data-StormData.csv.bz2"))

names(data)

hrph<-data[(data$FATALITIES>0 & data$INJURIES>0),]

library(plyr)
ph<-ddply(hrph, .(EVTYPE), summarize, mean_fatalities = round(mean(FATALITIES), 0), total_fatalities=sum(FATALITIES), mean_injuries=round(mean(INJURIES),0),total_injuries=sum(INJURIES))

head(ph)

par(mfcol=c(1,2))

mf<-ph[order(-ph$mean_fatalities),]
tf<-ph[order(-ph$total_fatalities),]

mi<-ph[order(-ph$mean_injuries),]
ti<-ph[order(-ph$total_injuries),]

barplot(mf[1:5,"mean_fatalities"], names.arg = oph[1:5,"EVTYPE"],col="Blue",
        main="Top five events by highest average number of Fatalities",
        ylab="Average # of Fatalities", cex.names = 0.3, cex.main=0.5)

barplot(tf[1:5,"total_fatalities"], names.arg = oph[1:5,"EVTYPE"],col="Blue",
        main="Top five events by highest total number of Fatalities",
        ylab="Total # of Fatalities", cex.names = 0.3, cex.main=0.5)

par(mfcol=c(1,2))

barplot(mi[1:5,"mean_fatalities"], names.arg = oph[1:5,"EVTYPE"],col="Blue",
        main="Top five events by highest average number of Injuries",
        ylab="Average # of Fatalities", cex.names = 0.3, cex.main=0.5)

barplot(ti[1:5,"total_fatalities"], names.arg = oph[1:5,"EVTYPE"],col="Blue",
        main="Top five events by highest total number of Injuries",
        cex.names = 0.3, cex.main=0.5)


head(ph[order(-ph$mean_fatalities),])
head(ph[order(-ph$total_fatalities),])
head(ph[order(-ph$mean_injuries),])
head(ph[order(-ph$total_injuries),])


econ_dmg<-data[(data$PROPDMG>0 | data$CROPDMG>0),]

ed<-ddply(econ_dmg, .(EVTYPE,PROPDMGEXP), summarize, mean_prop = round(mean(PROPDMG), 0), total_prop=sum(PROPDMG))

ed<-ed[ed$PROPDMGEXP=="M",]
ed<-ed[order(-ed$mean_prop),]
ed<-ed[order(-ed$total_prop),]

barplot(oed[1:5,"mean_prop"], names.arg = oed[1:5,"EVTYPE"],col="Blue")