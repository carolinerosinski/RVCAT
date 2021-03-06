##library
library(tidyverse)
library(doBy)
library(readxl)
library(xlsx)
library(lubridate)
library(here)
library(ggpubr)
library(ggpmisc)

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


##load the raw data file
data<-read.csv(here('Data','ChequamegonBay_IceOut.csv')) 
  data$Date<-as.character(data$Date)
  data$Date <-  parse_date(data$Date, format='%m/%d/%Y') 

data<- data %>%
  mutate(year = year(Date), jday = yday(Date)) 

###################################
my.formula <- y ~ x
ggplot(data, aes(x=year, y=jday)) +
  geom_point(size=2) +
  geom_line() +
  geom_smooth(method=lm, se = FALSE, colour='red') +
  geom_segment(aes(x=min(data$year-1), xend=max(data$year+1), y=mean(data$jday), yend=mean(data$jday)), size=1, color='black')+
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               color = "red", size=7, family='serif', parse = TRUE) +
  plot_theme +
  theme(legend.position=c(0.8, 0.8)) + 
  labs(x='Year', y='Ice breakup day', title='Lake Superior Chequamegon Bay Ice Breakup Day',
       caption='U.S. Geological Survey Lake Superior Biological Station, Ashland, WI\nData: www.sciencebase.gov/catalog/item/5cd07b1ce4b09b8c0b79a358')+
  scale_x_continuous(expand=c(0,0), breaks = scales::pretty_breaks(5), 
                     limits=c(min(data$year-1),max(data$year+5)))+
  scale_y_continuous(expand=c(0,0),breaks = scales::pretty_breaks(5), 
                     limits=c(min(data$jday-5),max(data$jday+10))) +
  annotate("text", x=1972, y = 90, label = "Long-term mean is day 111, April 21", size=7, family='serif' )

#  stat_cor(label.x = 1940, label.y = 140, color = "red") +
#  stat_regline_equation(label.x = 1940, label.y = 135, color = "red") +

ggsave(here('Plots and Tables/Ice_Temp','CheqBay_IceOut.png'), dpi = 300, width = 30, height = 15, units = "cm") 


#########################################
