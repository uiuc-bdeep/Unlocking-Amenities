##########################################################
# author: Ignacio Sarmiento-Barbieri
#
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo



#Load Packages
pkg<-list("dplyr","lfe", "stargazer","McSpatial","texreg","tidyr")
lapply(pkg, require, character.only=T)
rm(pkg)


#Wd
#setwd("/Volumes/share/projects/CrimeAndParks/")
#setwd("/home/guest1/share/CrimeAndParks/") #bdeep


setwd("~/Dropbox/Phd Illinois/Research/Neigh_crime/Unlocking_amenities/GithubRepo/Unlocking-Amenities/")

TD<-readRDS("stores/data_unlocking_amenities.rds")

crimes<- TD %>% group_by(park_city,city) %>% summarize(neigh.e_mean_homicides=min(neigh.e_mean_homicides))

##########################################################
#park values
##########################################################

chicago<-readRDS(paste0("~/Dropbox/Phd Illinois/Research/Neigh_crime/Unlocking_amenities/stores/park_value_","Chicago",".Rds"))
ny<-readRDS(paste0("~/Dropbox/Phd Illinois/Research/Neigh_crime/Unlocking_amenities/stores/park_value_","NY",".Rds"))
ny <- ny[grepl("Error",ny$name)==F,]
ny$value<-as.numeric(as.character(ny$value))
philly<-readRDS(paste0("~/Dropbox/Phd Illinois/Research/Neigh_crime/Unlocking_amenities/stores/park_value_","Philly",".Rds"))

park_value<-rbind(chicago,ny,philly)


z<- park_value %>% mutate(value=value*1.36/1e6) #puts it in 2016 values
z<- z %>%  mutate(value_amenity=(value/1.024)*(.024)) #Calculate the value of the park, which is Column (4) of IV regression. Properties within 200mts sale at xx% more than those further away, that is the value of the park
#z<- x %>%  mutate(value=(value))
z<- z %>%  mutate(housing_stock=(value))
z<-data.frame(z)
z  %>% dplyr::summarize(value_amenity=sum(value_amenity))
z %>%  dplyr::summarize(value_amenity=sum(value*(0.011/1.011)))

#z  %>% group_by(city) %>% summarize(value=sum(value_amenity))  %>% ungroup()
colnames(z)[colnames(z)=="name"]<-"park_name"
z$park_city<-paste0(z$park_name,"_",z$city)
####################################################################################

z<-left_join(z,crimes)

colnames(z)[colnames(z)=="neigh.e_mean_homicides"]<-"homicides"

z<-na.omit(z)


z$group<-NA
z$group[z$homicides<1]<-"Low Homicide Risk"
z$group[z$homicides>=1 & z$homicides<3]<-"Medium Homicide Risk"
z$group[z$homicides>3 ]<-"High Homicide Risk"
table(z$group)
z$group<-factor(z$group,levels=c("Low Homicide Risk","Medium Homicide Risk","High Homicide Risk"),ordered = T)
z$park_name_city<-paste0(z$park_name,"_",z$city)



z<- z %>%  mutate(locked_value=value*homicides*(-0.01)) #Table (6) (IV estimates) col (4) interaction
z<- z %>%  mutate(value_amenity_naive=value*(0.01/1.01)) #Table (6) (IV estimates) col (3) pkpremium
stargazer(z,type="text")

##########################################################
# A: Naive Value
##########################################################
cities<- z  %>% group_by(city,group) %>% summarize(value_amenity_naive=sum(value_amenity_naive))  %>% ungroup()
total<- z  %>% group_by(group) %>% summarize(value_amenity_naive=sum(value_amenity_naive))  %>% ungroup()
total$city<-"Total"
cities<-rbind(cities,total)
cities<-spread(cities,group,value_amenity_naive)
cities

city<- z  %>% group_by(city) %>% summarize(value_amenity_naive=sum(value_amenity_naive))  %>% ungroup()
tot<- z   %>% summarize(value_amenity_naive=sum(value_amenity_naive))  %>% ungroup()
tot$city<-"Total"
city<-rbind(city,tot)

cities<-left_join(cities,city)
panelA<-data.frame(cities)
panelA

##########################################################
# B  Realized park proximity value with interaction 
##########################################################


cities<- z  %>% group_by(city,group) %>% summarize(value=sum(value_amenity)+sum(locked_value)) %>% ungroup()
total<- z  %>% group_by(group) %>% summarize(value=sum(value_amenity)+sum(locked_value))  %>% ungroup()
total$city<-"Total"
cities<-rbind(cities,total)
cities<-spread(cities,group,value)
cities

city<- z  %>% group_by(city) %>% summarize(value=sum(value_amenity)+sum(locked_value)) %>% ungroup()
tot<- z   %>% summarize(value=sum(value_amenity)+sum(locked_value)) %>% ungroup()
tot$city<-"Total"
city<-rbind(city,tot)

cities<-left_join(cities,city)
panelB<-data.frame(cities)
panelB


##########################################################
# C Interaction
##########################################################
cities<- z  %>% group_by(city,group) %>% summarize(locked_value=(-1)*sum(locked_value)) %>% ungroup()
total<- z  %>% group_by(group) %>% summarize(locked_value=(-1)*sum(locked_value))  %>% ungroup()
total$city<-"Total"
cities<-rbind(cities,total)
cities<-spread(cities,group,locked_value)
cities

city<- z  %>% group_by(city)  %>% summarize(locked_value=(-1)*sum(locked_value)) %>% ungroup()
tot<- z    %>% summarize(locked_value=(-1)*sum(locked_value)) %>% ungroup()
tot$city<-"Total"
city<-rbind(city,tot)

cities<-left_join(cities,city)
panelC<-data.frame(cities)
panelC



##########################################################
# Potential park proximity value with no crime in parks (park prox coef. interacted regression)
##########################################################

z<-z %>% mutate(total_value=value_amenity)
head(z)
cities<- z  %>% group_by(city,group) %>% summarize(total_value=sum(total_value))  %>% ungroup()
total<- z  %>% group_by(group) %>% summarize(total_value=sum(total_value))  %>% ungroup()
total$city<-"Total"
cities<-rbind(cities,total)
cities<-spread(cities,group,total_value)
cities

city<- z  %>% group_by(city) %>% summarize(total_value=sum(total_value))  %>% ungroup()
tot<- z   %>% summarize(total_value=sum(total_value))  %>% ungroup()
tot$city<-"Total"
city<-rbind(city,tot)

cities<-left_join(cities,city)
panelD<-data.frame(cities)
panelD



stargazer(panelA,type="text",summary=F, digits=0, rownames=F,out="views/table5_panelA.tex")
stargazer(panelB,type="text",summary=F, digits=0, rownames=F,out="views/table5_panelB.tex")
stargazer(panelC,type="text",summary=F, digits=0, rownames=F,out="views/table5_panelC.tex")
stargazer(panelD,type="text",summary=F, digits=0, rownames=F,out="views/table5_panelD.tex")












