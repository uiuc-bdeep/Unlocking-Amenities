###########################################################
# Unlocking Amentities
# Table 3:  Park Premium and Homicide Risk:
# Pooled Panel Estimator across 1,337 Neighborhoods over Time
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")


#Load Packages
pkg<-list("dplyr","lfe", "stargazer","tidyr")
lapply(pkg, require, character.only=T)
rm(pkg)


setwd("~/Dropbox/Phd Illinois/Research/Neigh_crime/Unlocking_amenities/GithubRepo/Unlocking-Amenities/")
#Read data
TD<-readRDS("stores/data_unlocking_amenities.rds")

TD$treated<-TD$park.proximity
TD$treated[TD$treated==1]<-"treated"
TD$treated[TD$treated==0]<-"control"

table(TD$city,TD$year)

##########################################################
# Descriptive Stats Table
##########################################################
n_parks<-TD[!duplicated(TD$park_city),]


parks_in_each_city<- n_parks %>% group_by(city) %>% summarise(Parks= n()) %>% ungroup()%>% spread(city, Parks, fill=0)
parks_in_each_city$variable<-"Number of parks examined"
parks_in_each_city$Total<-parks_in_each_city$Chicago+parks_in_each_city$NY+parks_in_each_city$Philly

#Area park
area_park_in_each_city<- n_parks %>% group_by(city) %>% summarise(Area_park= mean(area_park)) %>% ungroup()%>% spread(city, Area_park, fill=0)
area_park_in_each_city$variable<-"Average park size (in mi2)"
area_park_in_each_city$Total<-mean(n_parks$area_park)
area_park_in_each_city

#Area neighborhood
area_in_each_city<- n_parks %>% group_by(city) %>% summarise(Area= mean(area)) %>% ungroup()%>% spread(city, Area, fill=0)
area_in_each_city$variable<-"Average neighborhood size (in mi2)"
area_in_each_city$Total<-mean(n_parks$area)
area_in_each_city

homicides<- TD %>% group_by(city) %>% summarise(Homicides= mean(homicide.risk)) %>% ungroup()%>% spread(city, Homicides, fill=0)
homicides$variable<-"Average local homicide risk"
homicides$Total<-mean(TD$homicide.risk)
homicides

homicides.neigh<- TD %>% group_by(city) %>% summarise(homicides.neigh= mean(neigh.homicide.risk)) %>% ungroup()%>% spread(city, homicides.neigh, fill=0)
homicides.neigh$variable<-"Average neighborhood homicide risk"
homicides.neigh$Total<-mean(TD$neigh.homicide.risk)
homicides.neigh

properties_treat_control<- TD %>% group_by(city,treated) %>% summarise(n= n()) %>% ungroup() %>% spread(city, n, fill=0)
properties_treat_control
colnames(properties_treat_control)[1]<-"variable"
properties_treat_control$variable[1]<-"Properties sold from 1/8 to 3/8 mile"
properties_treat_control$variable[2]<-"Properties sold within 1/8 mile"
properties_treat_control$Total<-properties_treat_control$Chicago+properties_treat_control$NY+properties_treat_control$Philly
properties_treat_control<-properties_treat_control[c(2,1),]
properties_treat_control


price_treat_control<- TD %>% group_by(city,treated) %>% summarise(price= mean(exp(logprice))) %>% ungroup() %>% spread(city, price, fill=0)
price_treat_control$Total<-data.frame(TD %>% group_by(treated) %>% summarise(price= mean(exp(logprice))) %>% ungroup())$price
price_treat_control
colnames(price_treat_control)[1]<-"variable"
price_treat_control$variable[1]<-"Average price from 1/8 to 3/8 mile"
price_treat_control$variable[2]<-"Average price within 1/8 mile"
price_treat_control<-price_treat_control[c(2,1),]
price_treat_control

panela<-rbind(parks_in_each_city,area_park_in_each_city,area_in_each_city,homicides,homicides.neigh)
names(panela)[names(panela)=="Philly"]<- "Philadelphia"
panela<-panela[c("variable","Chicago", "NY", "Philadelphia","Total")]
panela

panela<-data.frame(panela)
stargazer(panela,summary=F,rownames=F,type="text", digits=2, digits.extra = 1 ,out="views/1_descriptive_panel_a.tex")


# ##########################################################
# Panel B Dwelling Characteristics
# ##########################################################

#log distance to the CBD, age of the dwelling, square footage, number ofrooms, bedrooms, and bathrooms, and dwelling type
TD<- TD %>% mutate( age=ifelse(age<=0,NA,age),
                    sqfeet=ifelse(sqfeet<=0,NA,sqfeet),
                    TotalBedrooms=ifelse(TotalBedrooms<=0,NA,TotalBedrooms),
                    FullBath =ifelse(FullBath<=0,NA,FullBath))

TDx<-na.omit(TD)
dwelling_bycity<- TDx %>% group_by(city) %>% dplyr::summarise(logDCBD= mean(logDCBD,na.rm=T),
                                                             age=mean(age,na.rm=T),
                                                             sqfeet=mean(sqfeet,na.rm=T),
                                                             TotalBedrooms=mean(TotalBedrooms,na.rm=T),
                                                             FullBath =mean(FullBath,na.rm=T)
) %>% ungroup()

dwelling_all<- TDx %>% dplyr::summarise(logDCBD= mean(logDCBD,na.rm=T),
                                       age=mean(age,na.rm=T),
                                       sqfeet=mean(sqfeet,na.rm=T),
                                       TotalBedrooms=mean(TotalBedrooms,na.rm=T),
                                       FullBath =mean(FullBath,na.rm=T)
)

dwelling_all$city<-"Total"
dwelling<-rbind(dwelling_bycity,dwelling_all)
dwelling<-as_tibble(cbind(nms = names(dwelling), t(dwelling)))
names(dwelling)<-cbind("variable", "Chicago",         "NY",    "Philly",      "Total")
dwelling<-dwelling[-1,]
dwelling$Chicago<-as.numeric(dwelling$Chicago)
dwelling$NY<-as.numeric(dwelling$NY)
dwelling$Philly<-as.numeric(dwelling$Philly)
dwelling$Total<-as.numeric(dwelling$Total)



missing_char<-TD %>% group_by(city) %>% summarize(missing=sum(is.na(TotalBedrooms))/n())
missing_char<-as_tibble(cbind(nms = names(missing_char), t(missing_char)))
missing_char$Total<-sum(is.na(TD$TotalBedrooms))/length(TD$TotalBedrooms)
names(missing_char)<-cbind("variable", "Chicago",         "NY",    "Philly",      "Total")
missing_char$variable<-"perc. missing"
missing_char<-missing_char[2,]

panelb<-rbind(properties_treat_control,price_treat_control,dwelling,missing_char)
colnames(panelb)[4]<-"Philadelphia"
panelb<-data.frame(panelb)
stargazer(panelb,summary=F,rownames=F,type="text", digits=1, digits.extra = 1 ,out="views/1_descriptive_panel_b.tex")


###########################################################################


sociodemographics_bycity<- TD %>% group_by(city) %>% summarise(pop_dens= mean(pop.density_block,na.rm=T),
                                                        whites=mean(white.p_block,na.rm=T),
                                                        blacks=mean(black.p_block,na.rm=T),
                                                        hispanics=mean(hispanic.p_block,na.rm=T),
                                                        #other=mean(other.p_block,na.rm=T),
                                                        age =mean(median_age_block,na.rm=T),
                                                        income=mean(median_income_block,na.rm=T),
                                                        vacant=mean(vacant.p_block,na.rm=T),
                                                        renter=mean(renter.p_block,na.rm=T)
                                                        ) %>% ungroup()

TD<-TD %>% ungroup()

sociodemographics_all<- TD %>% dplyr::summarise(pop_dens= mean(pop.density_block,na.rm=T),
                                                        whites=mean(white.p_block,na.rm=T),
                                                        blacks=mean(black.p_block,na.rm=T),
                                                        hispanics=mean(hispanic.p_block,na.rm=T),
                                                        #other=mean(other.p_block,na.rm=T),
                                                        age =mean(median_age_block,na.rm=T),
                                                        income=mean(median_income_block,na.rm=T),
                                                        vacant=mean(vacant.p_block,na.rm=T),
                                                        renter=mean(renter.p_block,na.rm=T)
                                                        )

sociodemographics_all$city<-"Total"
sociodemographics<-rbind(sociodemographics_bycity,sociodemographics_all)
sociodemographics<-as_tibble(cbind(nms = names(sociodemographics), t(sociodemographics)))
names(sociodemographics)<-cbind("variable", "Chicago",         "NY",    "Philly",      "Total")
sociodemographics<-sociodemographics[-1,]
sociodemographics[,2:5]<-apply(sociodemographics[,2:5],2,as.numeric)

sociodemographics<-data.frame(sociodemographics)
sociodemographics

stargazer(sociodemographics,summary=F,rownames=F,type="text", digits=2, digits.extra =1 ,out="views/1_descriptive_panel_c.tex")


 TD$PropertyLandUseStndCode[TD$PropertyLandUseStndCode=="RR104"]<-"RR101"
 TD$PropertyLandUseStndCode[TD$PropertyLandUseStndCode=="RR107"]<-"RR106"
round(prop.table(table(TD$PropertyLandUseStndCode,TD$city)),2)
round(prop.table(table(TD$PropertyLandUseStndCode)),2)
# #end4


#'RR101',  # SFR
#'RR999',  # Inferred SFR
# 'RR102',  # Rural Residence   (includes farm/productive land?)
#'RR104',  # Townhouse
#'RR105',  # Cluster Home
#'RR106',  # Condominium
#'RR107',  # Cooperative
#'RR108',  # Row House
#'RR109',  # Planned Unit Development
#'RR113',  # Bungalow
#'RR116',  # Patio Home
#'RR119',  # Garden Home
#'RR120'   # Landominium



