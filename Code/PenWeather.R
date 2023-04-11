#load required packages
library(dplyr)

#read data
pen_wx<-read.csv("~/GitRepos/JuvenileEmigrationPhenology/Data/pen_weather.csv")

#format date column
pen_wx$DATE<-as.Date(pen_wx$DATE,format = "%m/%d/%Y")

#add in day of year and day of study columns
pen_wx$DOY <- as.numeric(format(as.Date(as.character(pen_wx$DATE), "%Y-%m-%d"), "%j"))

#calculate average temp from max/min
pen_wx$TAVG<-rowMeans(pen_wx[,c("TMAX","TMIN")])

#filter opacum weather
ao_wx<-filter(pen_wx,DATE>"2019-06-06")

#adjust day of year by start date of study
DOY.adj <- 158                           ## Standardize DOY to date of first release- 07 June 2019
ao_wx$DOY_adj <- ifelse(ao_wx$DOY >= DOY.adj, yes=ao_wx$DOY-(DOY.adj-1), no=ao_wx$DOY+(365-DOY.adj+1))

#adjust DOY by start date for overlapping years
ao_wx$elapseddays<-as.numeric(ao_wx$DATE-as.Date("2019-06-07"))+1

ao_days<-ao_df%>%
  group_by(Period)%>%
  summarise(days=unique(DOY_adj1))
ao_extraday<-ao_df%>%
  group_by(Period)%>%
  summarise(days=(min(DOY_adj1)-1))
ao_days<-bind_rows(ao_days,ao_extraday)

ao_wx1<-merge(ao_days,ao_wx,by.x="days",by.y="elapseddays",all.x=T)

#calculate weather summary by recapture period
ao_wx_sum<-ao_wx1%>%
  mutate(Period=as.numeric(as.character(Period)))%>%
  group_by(Period)%>%
  summarise(Tmin=mean(TMIN,na.rm=T),
            Tmax=mean(TMAX,na.rm=T),
            Tavg=mean(TAVG,na.rm=T),
            temp.sd=sd(TAVG, na.rm=T),
            Prcp=sum(PRCP,na.rm=T))
saveRDS(ao_wx_sum,"~/GitRepos/JuvenileEmigrationPhenology/ao_abiotic.rds")

#filter annulatum weather
aa_wx<-filter(pen_wx,DATE<"2019-07-07")

#adjust day of year by start date of study
DOY.adj <- 172                           ## Standardize DOY to date of first release- 07 June 2019
aa_wx$DOY_adj <- ifelse(aa_wx$DOY >= DOY.adj, yes=aa_wx$DOY-(DOY.adj-1), no=aa_wx$DOY+(365-DOY.adj+1))

#adjust DOY by start date for overlapping years
aa_wx$elapseddays<-as.numeric(aa_wx$DATE-as.Date("2018-06-21"))+1

aa_days<-aa_recaps%>%
  group_by(Period)%>%
  summarise(days=unique(DOY_adj))
aa_extraday<-aa_recaps%>%
  group_by(Period)%>%
  summarise(days=(min(DOY_adj)-1))
aa_days<-bind_rows(aa_days,aa_extraday)

aa_wx1<-merge(aa_days,aa_wx,by.x="days",by.y="elapseddays",all.x=T)

#calculate weather summary by recapture period
aa_wx_sum<-aa_wx1%>%
  mutate(Period=as.numeric(Period))%>%
  group_by(Period)%>%
  summarise(Tmin=mean(TMIN,na.rm=T),
            Tmax=mean(TMAX,na.rm=T),
            Tavg=mean(TAVG,na.rm=T),
            temp.sd=sd(TAVG, na.rm=T),
            Prcp=sum(PRCP,na.rm=T))
saveRDS(aa_wx_sum,"~/GitRepos/JuvenileEmigrationPhenology/aa_abiotic.rds")
