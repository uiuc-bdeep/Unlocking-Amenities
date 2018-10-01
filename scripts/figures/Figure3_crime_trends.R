##########################################################
# author: Ignacio Sarmiento-Barbieri
#
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo



#Load Packages
pkg<-list("dplyr","ggplot2")
lapply(pkg, require, character.only=T)
rm(pkg)


#Wd
#setwd("/Volumes/share/projects/CrimeAndParks/")
#setwd("/home/guest1/share/CrimeAndParks/") #bdeep
setwd("~/Dropbox/Phd Illinois/Research/Neigh_crime/Unlocking_amenities")


#TD<-readRDS(file="stores/shift_share/data_ready_v5_cross.rds")
TD<-readRDS("~/Dropbox/Phd Illinois/Research/Neigh_crime/Unlocking_amenities/GithubRepo/Unlocking-Amenities/stores/data_unlocking_amenities.rds")
table(TD$city,TD$year)

TD<-data.frame(TD)
TD<- TD %>% ungroup()

#Homicides<-TD %>% group_by(city,year) %>% summarize(homicides=min(total_city.homicides)) %>% ungroup()
#write.csv(Homicides,"homicides.csv")


#Chicago area: 234 mi²
#NY area: 304.6 mi²
#Philadelphia area: 141.7 mi²


Homicides$homicides[Homicides$city=="Chicago"]<-Homicides$homicides[Homicides$city=="Chicago"]/234
Homicides$homicides[Homicides$city=="NY"]<-Homicides$homicides[Homicides$city=="NY"]/304.6
Homicides$homicides[Homicides$city=="Philly"]<-Homicides$homicides[Homicides$city=="Philly"]/141.7


ggplot(Homicides,aes(year, homicides, group=city,linetype=factor(city) ) )+
  geom_line( size=.25) +
  #scale_linetype_manual(name="",values=c("dashed","solid","dotted"),labels=c("Chicago","New York","Philadelphia"))+
  geom_text(data = Homicides %>% filter(year == last(year)),
            aes(label = c("Chicago","New York","Philadelphia"), x = year -0.4 , y = homicides +0.15), size=1.5) +
  xlab("Year") +
  ylab("Homicides Rate") +
  scale_x_continuous(breaks = seq(2001, 2016, by=1)) +
  expand_limits(y=0) +
  theme_bw() +
  ylim(0.5,3.5) +
  theme(#axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    #axis.title.y=element_blank(),
    #axis.text.y=element_blank(),
    panel.grid.minor.x=element_blank(),
    axis.ticks.y=element_blank()) +
  theme(#plot.margin=grid::unit(c(0,0,0,0), "mm"), 
        panel.grid.major = element_line(size = 0.1),
        legend.justification=c(.95,.95), 
        legend.position="none",#c(.95,.95), 
        text = element_text(size=4)) 
ggsave("views/crime_trends_rate_three_cities.png", height = 1.5, width = 2.7, units="in")

