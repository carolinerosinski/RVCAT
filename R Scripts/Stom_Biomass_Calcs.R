library(tidyverse)
library(ggplot2)
library(readxl)
library(tibble)
library(quantreg)
library(plotrix)
library(doBy)
library(viridis)
library(reshape)
library(xlsx)
library(spaa)
library(here)

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

options(scipen=999)


##load data 
Prey <- read_excel(here('Data',"LSBS_Stomach_Contents.xlsx"), sheet ="RawData") 

Prey$AvgPreyWt<-Prey$PreyWT_g/Prey$PreyCount
Prey2<-select(Prey, 'Prey4','AvgPreyWt')
Prey2<-na.omit(Prey2)

##calculate mean for each prey type, change here if you want to calculate based on a diff. level of 
##classification (ie. Prey1-4)
Mean.Prey.Wts<-aggregate(Prey2$AvgPreyWt, by=list(Prey4=Prey2$Prey4), FUN=mean)%>%
  renameCol('x','PreyItem.WT')

Prey<-merge.data.frame(Prey, Mean.Prey.Wts, all=T)

replace1<-subset(Prey, is.na(PreyWT_g))
good.data<-subset(Prey, !is.na(PreyWT_g))

replace1$PreyWT_g<-with(replace1, PreyCount*PreyItem.WT)

Prey.Complete<-rbind(good.data, replace1)

Prey.Complete<-select(Prey.Complete, c(1:39))

##Stannard Rock & Superior Shoal Lake Trout 
sr.ss<-Prey.Complete%>%
  filter(Agency=='GLFC')%>%
  filter(Predator=='humper lake trout'|Predator=='redfin lake trout'|Predator=='siscowet lake trout'|
           Predator=='lean lake trout')%>%
  filter(Site=='Superior Shoal'|Site=='Stannard Rock')

sample.size<-aggregate(sr.ss$PreyWT_g, by=list(FishRecord=sr.ss$FishRecord,
                                                 Site=sr.ss$Site, Predator=sr.ss$Predator), FUN=sum)%>%
  renameCol('x','Sum')
sample.size<-aggregate(sample.size$FishRecord, by=list(Site=sample.size$Site, 
                                                       Predator=sample.size$Predator), FUN=length)%>%
  renameCol('x','SampleSize')

n.empty<-sr.ss%>%
  filter(Empty=='Y')
n.empty<-aggregate(n.empty$Specimen, by=list(Site=n.empty$Site, Predator=n.empty$Predator), FUN=length)%>%
    renameCol('x','Number.Empty')

biomass<-sr.ss%>%
  filter(Empty=='N')%>%
  select(c('FishRecord','Predator','Site','Prey2','Prey3','PreyWT_g'))
biomass<-subset(biomass, Prey2=='aquatic invertebrate'|Prey2=='terrestrial invertebrate'|
                  Prey2=='fish eggs'|Prey2=='fish')

biomass$Prey<-paste(biomass$Prey2, biomass$Prey3, sep='_')
biomass2<-select(biomass, c(1:3,6,7))
biomass3<-cast(biomass2, FishRecord+Predator+Site~Prey, value='PreyWT_g', fun.aggregate = 'sum')
biomass3[is.na(biomass3)]<-0
biomass4<-melt(biomass3, id=c('FishRecord','Predator','Site'))

ind.totals<-aggregate(biomass4$value, by=list(FishRecord=biomass4$FishRecord), FUN=sum) %>%
  renameCol('x','Ind.Total')

biomass4<-merge.data.frame(biomass4, ind.totals)
biomass4$Ind.Prop<-biomass4$value/biomass4$Ind.Total

biomass.props<-aggregate(biomass4$Ind.Prop, by=list(Predator=biomass4$Predator, Site=biomass4$Site, 
                                                    Prey=biomass4$Prey), FUN=mean)%>%
  renameCol('x','Mean.Prop')

biomass.props2<-cast(biomass.props, Prey~Site+Predator, value='Mean.Prop')
write.xlsx(biomass.props2, here('Plots and Tables','StanRock_SupShoal_LT_AvgBiomass.xlsx'), row.names = F)


##to calculate the averages for both sites combined
biomass.both<-cast(biomass2, FishRecord+Predator~Prey, value='PreyWT_g',fun.aggregate = 'sum')
biomass.both[is.na(biomass.both)]<-0
biomass.both2<-melt(biomass.both, id=c('FishRecord','Predator'))
biomass.both.ind.totals<-aggregate(biomass.both2$value, by=list(FishRecord=biomass.both2$FishRecord), FUN=sum)%>%
  renameCol('x','Ind.Total')
biomass.both2<-merge.data.frame(biomass.both2, biomass.both.ind.totals)
biomass.both2$Ind.Prop<-biomass.both2$value/biomass.both2$Ind.Total

biomass.both.props<-aggregate(biomass.both2$Ind.Prop, by=list(Predator=biomass.both2$Predator, 
                                                              Prey=biomass.both2$Prey), FUN=mean)%>%
  renameCol('x','Mean.Prop')
biomass.both.props2<-cast(biomass.both.props, Prey~Predator, value='Mean.Prop')

##totals check to make sure all species/sites proportions sum to 1
totals.check<-aggregate(biomass.props$Mean.Prop, by=list(Predator=biomass.props$Predator,
                                                         Site=biomass.props$Site), FUN=sum)

##Percent Occurrence Calculations
biomass5<-biomass4
biomass5$Prey<-gsub('aquatic invertebrate_adult insect','aquatic invertebrate_aquatic invertebrate', biomass5$Prey)
perc.occ1<-subset(biomass5, value>0)
perc.occ<-aggregate(perc.occ1$FishRecord, by=list(Predator=perc.occ1$Predator, Site=perc.occ1$Site,
                                                 Prey=perc.occ1$Prey), FUN=length)%>%
  renameCol('x','N.fish.wpreytype')

perc.occ<-merge.data.frame(perc.occ, sample.size)
perc.occ<-merge.data.frame(perc.occ, n.empty)
perc.occ$N.stom.full<-perc.occ$SampleSize-perc.occ$Number.Empty
perc.occ$Percent.Occurrence<-(perc.occ$N.fish.wpreytype/perc.occ$N.stom.full)*100
perc.occ<-cast(perc.occ, Prey~Predator+Site, value='Percent.Occurrence')
perc.occ[is.na(perc.occ)]<-0

write.xlsx(perc.occ, here('Plots and Tables','SS_SR_LT_percent_occurrence.xlsx'), row.names = F)

##Percent Occurrence for both sites together
perc.occ.2sites<-aggregate(perc.occ1$FishRecord, by=list(Predator=perc.occ1$Predator, 
                                                         Prey=perc.occ1$Prey), FUN=length)%>%
  renameCol('x','N.fish.wpreytype')
sample.size.spp<-data.frame(Predator=c('humper lake trout','lean lake trout','redfin lake trout',
                                       'siscowet lake trout'), Sample.Size=c(52,57,45,136))
perc.occ.2sites<-merge.data.frame(perc.occ.2sites, sample.size.spp)
perc.occ.2sites$Percent.Occurrence<-(perc.occ.2sites$N.fish.wpreytype/perc.occ.2sites$Sample.Size)*100
perc.occ.2sites<-cast(perc.occ.2sites, Prey~Predator, value='Percent.Occurrence')
perc.occ.2sites[is.na(perc.occ.2sites)]<-0

write.xlsx(perc.occ.2sites, here('Plots and Tables','2Shoals_LT_percent_occurrence.xlsx'), row.names = F)

##prey types summary stats
prey.types<-subset(biomass4, value>0)
n.prey.types.perind<-aggregate(prey.types$Prey, by=list(FishRecord=prey.types$FishRecord,
                                                        Predator=prey.types$Predator,
                                                        Site=prey.types$Site), FUN=length)%>%
  renameCol('x','N.preytypes.perind')

##for SS and SR separate
prey.types.summary<-n.prey.types.perind%>%
  group_by(Site,Predator)%>%
  summarise(mean=mean(N.preytypes.perind), max=max(N.preytypes.perind))

##for both shoals together
prey.types.summary2<-n.prey.types.perind%>%
  group_by(Predator)%>%
  summarise(mean=mean(N.preytypes.perind), max=max(N.preytypes.perind))
##SCHOENER'S CALCULATIONS
stan.rock.props<-select(biomass.props2, c(2:4))
sup.shoal.props<-select(biomass.props2, c(5:8))

Overlap <- function (pred1, pred2, na.rm = TRUE) 
{
  p1 <- pred1/sum(pred1, na.rm = na.rm)
  p2 <- pred2/sum(pred2, na.rm = na.rm)
  SchoenerD <- 1 - 0.5 * sum(abs(p1 - p2), na.rm = na.rm)
  }

hump.lean.sr<-Overlap(pred1=stan.rock.props$`Stannard Rock_humper lake trout`, pred2=stan.rock.props$`Stannard Rock_lean lake trout`)
hump.sisc.sr<-Overlap(pred1=stan.rock.props$`Stannard Rock_humper lake trout`, pred2=stan.rock.props$`Stannard Rock_siscowet lake trout`)
lean.sisc.sr<-Overlap(pred1=stan.rock.props$`Stannard Rock_lean lake trout`, pred2=stan.rock.props$`Stannard Rock_siscowet lake trout`)

hump.lean.ss<-Overlap(pred1=sup.shoal.props$`Superior Shoal_humper lake trout`, pred2=sup.shoal.props$`Superior Shoal_lean lake trout`)
hump.rf.ss<-Overlap(pred1=sup.shoal.props$`Superior Shoal_humper lake trout`, pred2=sup.shoal.props$`Superior Shoal_redfin lake trout`)
hump.sisc.ss<-Overlap(pred1=sup.shoal.props$`Superior Shoal_humper lake trout`, pred2=sup.shoal.props$`Superior Shoal_siscowet lake trout`)
lean.rf.ss<-Overlap(pred1=sup.shoal.props$`Superior Shoal_lean lake trout`, pred2=sup.shoal.props$`Superior Shoal_redfin lake trout`)
lean.sisc.ss<-Overlap(pred1=sup.shoal.props$`Superior Shoal_lean lake trout`, pred2=sup.shoal.props$`Superior Shoal_siscowet lake trout`)
rf.sisc.ss<-Overlap(pred1=sup.shoal.props$`Superior Shoal_redfin lake trout`, pred2=sup.shoal.props$`Superior Shoal_siscowet lake trout`)


##Individual specialization values
library(RInSp)

is.ss.humper<-biomass3%>%
  filter(Predator=='humper lake trout' & Site=='Superior Shoal')
is.sr.humper<-biomass3%>%
  filter(Predator=='humper lake trout' & Site=='Stannard Rock')
is.ss.lean<-biomass3%>%
  filter(Predator=='lean lake trout' & Site=='Superior Shoal')
is.sr.lean<-biomass3%>%
  filter(Predator=='lean lake trout' & Site=='Stannard Rock')
is.ss.redfin<-biomass3%>%
  filter(Predator=='redfin lake trout' & Site=='Superior Shoal')
is.ss.siscowet<-biomass3%>%
  filter(Predator=='siscowet lake trout' & Site=='Superior Shoal')
is.sr.siscowet<-biomass3%>%
  filter(Predator=='siscowet lake trout' & Site=='Stannard Rock')

is.ss.humper<-import.RInSp(is.ss.humper, col.header=TRUE,row.names=1,info.cols=1:3, data.type="integer")
is.ss.humper2<-PSicalc(is.ss.humper, pop.diet='average', replicates=10000)

is.sr.humper<-import.RInSp(is.sr.humper, col.header=TRUE,row.names=1,info.cols=1:3, data.type="integer")
is.sr.humper2<-PSicalc(is.sr.humper, pop.diet='average', replicates=10000)

is.ss.lean<-import.RInSp(is.ss.lean, col.header=TRUE,row.names=1,info.cols=1:3, data.type="integer")
is.ss.lean2<-PSicalc(is.ss.lean, pop.diet='average', replicates=10000)

is.sr.lean<-import.RInSp(is.sr.lean, col.header=TRUE,row.names=1,info.cols=1:3, data.type="integer")
is.sr.lean2<-PSicalc(is.sr.lean, pop.diet='average', replicates=10000)

is.ss.redfin<-import.RInSp(is.ss.redfin, col.header=TRUE,row.names=1,info.cols=1:3, data.type="integer")
is.ss.redfin2<-PSicalc(is.ss.redfin, pop.diet='average', replicates=10000)

is.ss.siscowet<-import.RInSp(is.ss.siscowet, col.header=TRUE,row.names=1,info.cols=1:3, data.type="integer")
is.ss.siscowet2<-PSicalc(is.ss.siscowet, pop.diet='average', replicates=10000)

is.sr.siscowet<-import.RInSp(is.sr.siscowet, col.header=TRUE,row.names=1,info.cols=1:3, data.type="integer")
is.sr.siscowet2<-PSicalc(is.sr.siscowet, pop.diet='average', replicates=10000)

##WIC/TNW measures
is.ss.humper.wictnw<-WTcMC(is.ss.humper, replicates=10000, print.ris=TRUE)
is.sr.humper.wictnw<-WTcMC(is.sr.humper, replicates=10000, print.ris=TRUE)

is.ss.lean.wictnw<-WTcMC(is.ss.lean, replicates=10000, print.ris=TRUE)
is.sr.lean.wictnw<-WTcMC(is.sr.lean, replicates=10000, print.ris=TRUE)

is.ss.redfin.wictnw<-WTcMC(is.ss.redfin, replicates=10000, print.ris=TRUE)

is.ss.siscowet.wictnw<-WTcMC(is.ss.siscowet, replicates=10000, print.ris=TRUE)
is.sr.siscowet.wictnw<-WTcMC(is.sr.siscowet, replicates=10000, print.ris=TRUE)
