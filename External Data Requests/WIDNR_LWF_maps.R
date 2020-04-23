
##library
library(tidyverse)
library(doBy)
library(readxl)
library(ggplot2)
library(plotrix)
library(psych)
library(rlang)
library(dplyr)
library(purrr)
library(forcats)
library(viridis)
library(reshape)
library(rgdal)
library(xlsx)
library(lubridate)
library(plyr)
library(gganimate)
library(magick)
library(grid)
library(ggforce)
library(ggrepel)
library(here)


##load the raw RVCAT data file
##NOTE: this code is designed to process the ENTIRE RVCAT output, you can subset out target codes, species, years, etc later
##you DO NOT need to spool off a specialized RVCAT file with just the data you want to analyze
raw.data<-read.csv(here('Data','RVCAT.csv'))
raw.data$SPECIES<-as.factor(raw.data$SPECIES)

##change date format into usable form
raw.data$OP_DATE<-as.character(raw.data$OP_DATE)
raw.data$OP_DATE<-parse_date(raw.data$OP_DATE, format='%d-%b-%y')

raw.data[is.na(raw.data[,"END_LATITUDE_DD"]), "END_LATITUDE_DD"] <- raw.data[is.na(raw.data[, "END_LATITUDE_DD"]),"BEG_LATITUDE_DD"]
raw.data[is.na(raw.data[,"BEG_LATITUDE_DD"]), "BEG_LATITUDE_DD"] <- raw.data[is.na(raw.data[, "BEG_LATITUDE_DD"]),"END_LATITUDE_DD"]

raw.data[is.na(raw.data[,"END_LONGITUDE_DD"]), "END_LONGITUDE_DD"] <- raw.data[is.na(raw.data[, "END_LONGITUDE_DD"]),"BEG_LONGITUDE_DD"]
raw.data[is.na(raw.data[,"BEG_LONGITUDE_DD"]), "BEG_LONGITUDE_DD"] <- raw.data[is.na(raw.data[, "BEG_LONGITUDE_DD"]),"END_LONGITUDE_DD"]

raw.data$Mid.Lat.DD<-(raw.data$BEG_LATITUDE_DD+raw.data$END_LATITUDE_DD)/2
raw.data$Mid.Long.DD<-(raw.data$BEG_LONGITUDE_DD+raw.data$END_LONGITUDE_DD)/2

##load the species names file for when needed
codes.to.names<-read_xlsx(here('Data','Species_Taxonomy.xlsx'))
sci.names<-select(codes.to.names, c(2:4))

##set default themes for all plots and maps
map_theme<-theme(axis.text=element_text(size=20, family='serif'), 
                 axis.title=element_text(size=20, family='serif'), 
                 plot.title=element_text(size=24, family='serif'),
                 plot.subtitle=element_text(size=18, family='serif'),
                 plot.caption=element_text(size=16, family='serif'), 
                 legend.position=c(0.08,0.7),
                 legend.text=element_text(size=16, family='serif'))

plot_theme<-theme(axis.line=element_line(size=1, color='black'),
                  panel.background = element_rect(NA),
                  axis.text=element_text(size=20, family='serif'),
                  axis.title=element_text(size=20, family='serif'),
                  plot.margin = margin(.5,.5,.5,.5,"cm"),
                  legend.text=element_text(size=16, family='serif'),
                  axis.ticks=element_line(size=1, color='black'),
                  plot.title=element_text(size=24, family='serif'),
                  plot.subtitle=element_text(size=16, family='serif'),
                  plot.caption=element_text(size=16, family='serif'),
                  legend.background = element_blank(),
                  legend.key = element_blank(),
                  strip.text=element_text(size=16, family='serif'))

ann_data_access<-'Data: U.S. Geological Survey, doi.org/10.5066/F75M63X0'

##to calculate NOHA and KGHA, need to subset out different vessels, as they have different conversion factors for area sampled

##r/v siscowet from start to 1976 **note, conversion factor only verified from 1973-1976, an estimate for years prior
siscowet76<-subset(raw.data, VESSEL==1 & YEAR<1977)
siscowet76$NOHA<-((siscowet76$NUM*60)/siscowet76$TOW_TIME)/2.01
siscowet76$KGHA<-((siscowet76$WT*.06)/siscowet76$TOW_TIME)/2.01
siscowet76$HA_SWEPT<-(siscowet76$TOW_TIME/60)*2.01

##r/v siscowet from 1977-1999
siscowet77.99<-subset(raw.data, VESSEL==1 & YEAR>1976 & YEAR<2000)
siscowet77.99$NOHA<-((siscowet77.99$NUM*60)/siscowet77.99$TOW_TIME)/2.45
siscowet77.99$KGHA<-((siscowet77.99$WT*.06)/siscowet77.99$TOW_TIME)/2.45
siscowet77.99$HA_SWEPT<-(siscowet77.99$TOW_TIME/60)*2.45

##r/v grayling 1973-1999 (all years)
grayling<-subset(raw.data, VESSEL==11)
grayling$NOHA<-((grayling$NUM*60)/grayling$TOW_TIME)/2.05
grayling$KGHA<-((grayling$WT*.06)/grayling$TOW_TIME)/2.05
grayling$HA_SWEPT<-(grayling$TOW_TIME/60)*2.05

##r/v coaster, USGS, 1993 serials 1-120
coaster93<-subset(raw.data, VESSEL==99 & YEAR==1993 & SERIAL<121)
coaster93$NOHA<-((coaster93$NUM*60)/coaster93$TOW_TIME)/.6097
coaster93$KGHA<-((coaster93$WT*.06)/coaster93$TOW_TIME)/.6097
coaster93$HA_SWEPT<-(coaster93$TOW_TIME/60)*.6097

##r/v coaster, USGS, 1988-1992 and 1994-2004  and 1993 serials>120
coasterUSGS1<-subset(raw.data, VESSEL==99 & YEAR<1993)
coasterUSGS2<-subset(raw.data, VESSEL==99 & YEAR>1993)
coasterUSGS3<-subset(raw.data, VESSEL==99 & YEAR==1993 & SERIAL>120)
coasterUSGS<-rbind(coasterUSGS1, coasterUSGS2, coasterUSGS3)
coasterUSGS$NOHA<-((coasterUSGS$NUM*60)/coasterUSGS$TOW_TIME)/.785
coasterUSGS$KGHA<-((coasterUSGS$WT*.06)/coasterUSGS$TOW_TIME)/.785
coasterUSGS$HA_SWEPT<-(coasterUSGS$TOW_TIME/60)*.785

##r/v coaster/shoveler USFWS 2009 (calling it shoveler to differentiate from coaster above)
shoveler<-subset(raw.data, VESSEL==95)
shoveler$NOHA<-(shoveler$NUM/(((shoveler$DISTANCE*5280)*14.76)/107639.1))
shoveler$KGHA<-(shoveler$WT*.001/(((shoveler$DISTANCE*5280)*14.76)/107639.1))
shoveler$HA_SWEPT<-((shoveler$DISTANCE*5280)*14.76)/107639.1

##r/v whitefish
whitefish<-subset(raw.data, VESSEL==50)
whitefish$NOHA<-((whitefish$NUM*60)/whitefish$TOW_TIME)/1.174
whitefish$KGHA<-((whitefish$WT*.06)/whitefish$TOW_TIME)/1.174
whitefish$HA_SWEPT<-(whitefish$TOW_TIME/60)*1.174

##johnboat
johnboat<-subset(raw.data, VESSEL==92)
johnboat$NOHA<-((johnboat$NUM*60)/johnboat$TOW_TIME)/.6568
johnboat$KGHA<-((johnboat$WT*.06)/johnboat$TOW_TIME)/.6568
johnboat$HA_SWEPT<-(johnboat$TOW_TIME/60)*.6568

##r/v kaho **note, don't have a conversion factor for this ship yet, using the conversion factor for the siscowet for now
kaho<-subset(raw.data, VESSEL==4)
kaho$NOHA<-((kaho$NUM*60)/kaho$TOW_TIME)/2.45
kaho$KGHA<-((kaho$WT*.06)/kaho$TOW_TIME)/2.45
kaho$HA_SWEPT<-(kaho$TOW_TIME/60)*2.45

##r/v kiyi
kiyi<-subset(raw.data, VESSEL==25)
kiyi$NOHA<-(kiyi$NUM/(((kiyi$DISTANCE*5280)*25.49)/107639.1))
kiyi$KGHA<-(kiyi$WT*.001/(((kiyi$DISTANCE*5280)*25.49)/107639.1))
kiyi$HA_SWEPT<-(((kiyi$DISTANCE*5280)*25.49)/107639.1)

##Daphnia **note, no bottom trawls done off this ship, so doesn't have all the same data as the other vessels
daphnia<-subset(raw.data, VESSEL==9)
daphnia$NOHA<-NA
daphnia$KGHA<-NA
daphnia$HA_SWEPT<-NA

##bind all data frames together now that they have NOHA and KGHA
all.data<-rbind(kiyi, siscowet76, siscowet77.99, grayling, coaster93, coasterUSGS, whitefish, johnboat, kaho, shoveler, daphnia)

all.data$M_UNIT<-as.character(as.factor(all.data$M_UNIT))

##subset age 1 fish---------------------------------------------------------------------------------
lengths<-read.csv(here('Data','LENGTHS_RVCAT.csv'))
op.id.details<-select(all.data, c(1,2,5,6,8,11,17,36))
lengths2<-merge.data.frame(lengths, op.id.details, by='OP_ID', all=F)
lengths3<-unique(lengths2)


lwf.a1<-lengths3%>%
  group_by(YEAR)%>%
  filter(SPECIES==203)%>%
  filter(LENGTH<161)%>%
  filter(TARGET==2)

age1<-lwf.a1 ##bind all species evaluated using nearshore data
##need to process kiyi separate from the nearshore species so you can cast in zeros for nearshore sites where age 1 fish were not caught
age1.station.sum<-aggregate(age1$EXP_N, by=list(OP_ID=age1$OP_ID,YEAR=age1$YEAR, OP_DATE=age1$OP_DATE, VESSEL=age1$VESSEL,
                                                LOCATION=age1$LOCATION, SPECIES=age1$SPECIES,
                                                HA_SWEPT=age1$HA_SWEPT), FUN=sum)%>%
  renameCol('x','N.Age1')

ns<-subset(all.data, TARGET==2 & YEAR>1976)
##need to cast in sites where no fish of that species were caught
##to do this, need to figure out which sites were sampled each year
ns.sites.byyear<-ns%>%
  group_by(YEAR)%>%
  distinct(LOCATION)
ns.sites.byyear$SITECHECK<-'SURVEYED'


age1.station.sum<-merge.data.frame(age1.station.sum, ns.sites.byyear, all=T)
age1.station.sum$NOHA<-age1.station.sum$N.Age1/age1.station.sum$HA_SWEPT
age1.cast<-cast(age1.station.sum, YEAR+LOCATION~SPECIES, value='NOHA', fun.aggregate = 'mean')
age1.cast[is.na(age1.cast)]<-0
age1.cast<-select(age1.cast, c(1:3))
age1.cast<-melt(age1.cast, id=c('YEAR', 'LOCATION')) ##use this file later to map species densities

age1.cast<-renameCol(age1.cast, 'value','NOHA')
age1.cast$TARGET<-2

cheq.sites.byyear<-all.data%>%
  filter(TARGET==106)%>%
  filter(TR_DESIGN==4|TR_DESIGN==26)%>%
  group_by(YEAR)%>%
  distinct(LOCATION)
cheq.sites.byyear$SITECHECK<-'SURVEYED'

lwf.a1.cheq<-lengths3%>%
  group_by(YEAR)%>%
  filter(SPECIES==203)%>%
  filter(LENGTH<161)%>%
  filter(TARGET==106)%>%
  filter(TR_DESIGN==4|TR_DESIGN==26)
age1.station.sum.cheq<-aggregate(lwf.a1.cheq$EXP_N, by=list(OP_ID=lwf.a1.cheq$OP_ID,YEAR=lwf.a1.cheq$YEAR, OP_DATE=lwf.a1.cheq$OP_DATE,
                                                            LOCATION=lwf.a1.cheq$LOCATION, SPECIES=lwf.a1.cheq$SPECIES,
                                                            HA_SWEPT=lwf.a1.cheq$HA_SWEPT), FUN=sum)%>%
  renameCol('x','N.Age1')


age1.station.sum.cheq<-merge.data.frame(age1.station.sum.cheq, cheq.sites.byyear, all=T)
age1.station.sum.cheq$NOHA<-age1.station.sum.cheq$N.Age1/age1.station.sum.cheq$HA_SWEPT
age1.station.sum.cheq$SPECIES<-203
age1.cast.cheq<-cast(age1.station.sum.cheq, YEAR+LOCATION~SPECIES, value='NOHA', fun.aggregate = 'mean')
age1.cast.cheq[is.na(age1.cast.cheq)]<-0
age1.cast.cheq<-select(age1.cast.cheq, c(1:3))
age1.cast.cheq<-melt(age1.cast.cheq, id=c('YEAR', 'LOCATION')) ##use this file later to map species densities
age1.cast.cheq<-renameCol(age1.cast.cheq, 'value','NOHA')
age1.cast.cheq$TARGET<-106

age1.cast<-rbind(age1.cast,age1.cast.cheq)
age1.cast<-subset(age1.cast, YEAR>1977)

lwf<-all.data%>%
  filter(SPECIES==203)%>%
  filter(M_UNIT=='WI2  '| M_UNIT=='WI1  ')%>%
  filter(YEAR>1977)%>%
  filter(TARGET==2|TARGET==106)%>%
  filter(TR_DESIGN==4|TR_DESIGN==25|TR_DESIGN==26)

wi2sites<-as.data.frame(unique(lwf$LOCATION))
wi2sites<-renameCol(wi2sites, 1, 'LOCATION')

age1.wi2<-merge.data.frame(age1.cast, wi2sites, keep.all=F)



munits<-all.data%>%
  filter(TARGET==2|TARGET==106)%>%
  filter(YEAR>1977)%>%
  filter(TR_DESIGN==4|TR_DESIGN==25|TR_DESIGN==26)%>%
  filter(M_UNIT=='WI2  '| M_UNIT=='WI1  ')
munits<-select(munits, c(8,9))
munits<-distinct(munits)

age1.wi<-merge.data.frame(age1.wi2, munits, all=F)

write.xlsx(age1.wi, here('External Data Requests/Products','age1_lwf_wi_dcarl.xlsx'), row.names = F)

##all trawl data------------------------------------------------------------------------------------

lwf.simplified<-select(lwf,1, 2,4,5,6,8,9,11,16,17,29:36)

sites.byyear<-rbind(ns.sites.byyear,cheq.sites.byyear)
lwf.simplified2<-merge.data.frame(lwf.simplified, sites.byyear, all=TRUE)
#lwf.simplified2<-merge.data.frame(lwf.simplified2, wi2sites, all=F)
lwf.simplified2$NOHA[is.na(lwf.simplified2$NOHA)]<-0
lwf.simplified2$KGHA[is.na(lwf.simplified2$KGHA)]<-0

lwf.simplified2<-subset(lwf.simplified2, YEAR>1977)

##to add back in site details for stations where there were no whitefish
lwf.present<-subset(lwf.simplified2, NUM>0)
lwf.absent<-subset(lwf.simplified2, NOHA==0)
lwf.absent<-select(lwf.absent, c(1,2,15,16,17,19))

site.details<-select(all.data, c(1,2,4,5,6,8,9,11,16,17,32,33,36))
site.details<-as.data.frame(unique(site.details))
site.details<-site.details%>%
  filter(M_UNIT=='WI2  '| M_UNIT=='WI1  ')%>%
  filter(YEAR>1977)%>%
  filter(TARGET==2|TARGET==106)%>%
  filter(TR_DESIGN==4|TR_DESIGN==25|TR_DESIGN==26)

lwf.absent2<-merge.data.frame(lwf.absent, site.details)
lwf.absent2$SPECIES<-203
lwf.absent2$NUM<-0
lwf.absent2$WT<-0

lwf.all.sites.wi2<-rbind(lwf.present, lwf.absent2)
lwf.all.sites.wi2<-select(lwf.all.sites.wi2, c(3,1,4,2,5:10,14,15,18,19,11:13,16,17))

write.xlsx(lwf.all.sites.wi2, here('External Data Requests/Products','lwf_wi_dcarl.xlsx'), row.names = F)


##map of WI sites----------------------
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

map_theme<-theme(axis.text=element_text(size=20, family='serif'), 
                 axis.title=element_text(size=20, family='serif'), 
                 plot.title=element_text(size=24, family='serif'),
                 plot.subtitle=element_text(size=18, family='serif'),
                 plot.caption=element_text(size=16, family='serif'), 
                 legend.position=c(0.1,0.7),
                 legend.text=element_text(size=16, family='serif'))

station.details.formap<-site.details[match(unique(site.details$LOCATION), site.details$LOCATION),]
station.details.formap<-subset(station.details.formap, TARGET==2)
##create map with stations color coded by biomass
ggplot(station.details.formap, aes(Mid.Long.DD, Mid.Lat.DD)) +
  theme_bw() +
  scale_y_continuous(limits = c(46.6,47.25), expand=c(0,0))+
  scale_x_continuous(limits=c(-92.25,-90),
                     breaks=c(-92,-91.5,-91,-90.5,-90), 
                     labels=c('-92','-91.5','-91','-90.5','-90'),
                     expand=c(0,0))+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = 0.5)+
  geom_point(data=station.details.formap, mapping=aes(Mid.Long.DD, Mid.Lat.DD, color=M_UNIT), size=6, stroke=1.5)+
  map_theme+
  geom_text(aes(label=LOCATION))+
  labs( x='Longitude', y='Latitude',
        title='Lake Superior Wisconsin bottom trawl locations',
        caption='USGS annual nearshore bottom trawl assessment',
        color='Management Unit')
ggsave(here('External Data Requests/Products','WI_sitemap_dcarl.png'), height=20, width=25, units='cm')

##cheq bay map
cheq<-subset(all.data, TARGET==106)

cheq.current.yr<-cheq %>%
  filter(YEAR==max(YEAR))
cheq.current.yr<-aggregate(cheq.current.yr$NOHA, by=list(LOCATION=cheq.current.yr$LOCATION, BEG_LONGITUDE_DD=cheq.current.yr$BEG_LONGITUDE_DD,
                                                         BEG_LATITUDE_DD=cheq.current.yr$BEG_LATITUDE_DD, END_LONGITUDE_DD=cheq.current.yr$END_LONGITUDE_DD,
                                                         END_LATITUDE_DD=cheq.current.yr$END_LATITUDE_DD, YEAR=cheq.current.yr$YEAR), FUN=sum)

ggplot(cheq.current.yr, aes(BEG_LONGITUDE_DD, BEG_LATITUDE_DD)) +
  theme_bw() +
  scale_y_continuous(name='Latitude', breaks=seq(46.6,46.7, by=.1),limits=c(46.58,46.72), labels=c(46.6, 46.7))+
  scale_x_continuous(name='Longitude', limits=c(-91,-90.68))+
  geom_path(data = ls_poly.fort, aes(long, lat, group = group), size = .5, color='grey50')+
  geom_segment(data=cheq.current.yr, aes(x=BEG_LONGITUDE_DD, xend=END_LONGITUDE_DD, 
                                         y=BEG_LATITUDE_DD, yend=END_LATITUDE_DD), size=2, color='sienna1', lineend='round')+
  map_theme+
  labs(caption=ann_data_access,
       title='Chequamegon Bay Bottom Trawl Locations',
       subtitle=(paste('USGS bottom trawl assessment, ', cheq.current.yr$YEAR)))+
  geom_text_repel(aes(label=unique(LOCATION)),
    nudge_y      = 0.004,
    direction    = "x",
    vjust        = 0,
    segment.size = 0.05
  )

ggsave(here('External Data Requests/Products','CheqBay_sitemap_dcarl.png'), height=20, width=20, units='cm')

##ALL WHITEFISH LENGTHS IN WI WATERS--------------------------------------------------------------------------------------------
lwf.lengths<-lengths3%>%
  group_by(YEAR)%>%
  filter(SPECIES==203)%>%
  filter(TARGET==2|TARGET==106)%>%
  filter(YEAR>1977)%>%
  filter(TR_DESIGN==4|TR_DESIGN==25|TR_DESIGN==26)
lwf.lengths<-merge.data.frame(lwf.lengths, munits, all=F)

write.xlsx(lwf.lengths, here('External Data Requests/Products','WI_LWF_lengths_dcarl.xlsx'), row.names = F)
