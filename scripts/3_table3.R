##########################################################
# author: Ignacio Sarmiento-Barbieri
#
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo



#Load Packages
pkg<-list("dplyr","lfe", "stargazer", "McSpatial","texreg")
lapply(pkg, require, character.only=T)
rm(pkg)


#Wd
#setwd("/Volumes/share/projects/CrimeAndParks/")
#setwd("/home/guest1/share/CrimeAndParks/") #bdeep

setwd("~/Dropbox/Phd Illinois/Research/Neigh_crime/Unlocking_amenities/GithubRepo/Unlocking-Amenities/")

TD<-readRDS("stores/data_unlocking_amenities.rds")

TD_matched<-readRDS("stores/data_unlocking_amenities_matched_sample.rds")

#Create Homicide Risk Sq and Interaction
TD$homicide.risk.sq<-(TD$homicide.risk^2)/100
TD$park_homicide.risk.sq<-(TD$park.proximity*TD$homicide.risk.sq)

#Repeated Sales Sample
rep_sale_TD<-repsaledata(TD$logprice,TD$year,TD$ImportParcelID)
rep_sale_crime<-repsaledata(TD$homicide.risk,TD$year,TD$ImportParcelID)
colnames(rep_sale_crime)[colnames(rep_sale_crime)=="price0"]<-"homicide.risk.0"
colnames(rep_sale_crime)[colnames(rep_sale_crime)=="price1"]<-"homicide.risk.1"

rep_sale_crime_park<-repsaledata(TD$park_homicide.risk,TD$year,TD$ImportParcelID)
colnames(rep_sale_crime_park)[colnames(rep_sale_crime_park)=="price0"]<-"park_homicide.risk.0"
colnames(rep_sale_crime_park)[colnames(rep_sale_crime_park)=="price1"]<-"park_homicide.risk.1"


TD_rep_sale<-left_join(rep_sale_TD,rep_sale_crime)
TD_rep_sale<-left_join(TD_rep_sale,rep_sale_crime_park)


TD_rep_sale<- TD_rep_sale %>% mutate(delta.time=time1-time0,
                                     delta.price=price1-price0,
                                     delta.hom.risk=homicide.risk.1- homicide.risk.0, 
                                     delta.park.hom.risk=park_homicide.risk.1- park_homicide.risk.0)


TD_rep_sale$ImportParcelID<-TD_rep_sale$id


tract_city<- TD %>% dplyr::select(ImportParcelID,tract_city, year, median_age_block, median_income_block, pop.density_block, white.p_block, black.p_block, hispanic.p_block, vacant.p_block, renter.p_block )
tract_city<- tract_city %>% distinct(ImportParcelID,.keep_all = TRUE)
TD_rep_sale<-left_join(TD_rep_sale,tract_city)
TD_rep_sale$time0<--1*TD_rep_sale$time0



#No Interaction
reg1<- felm(logprice ~  park.proximity + homicide.risk
            + age + age2
            + sqfeet
            + TotalBedrooms
            + FullBath
            + miss_age
            + miss_sqfeet
            + miss_bedrooms
            + miss_bath
            + logDCBD
            + median_age_block + median_income_block
            + pop.density_block + white.p_block + black.p_block + hispanic.p_block
            + vacant.p_block + renter.p_block
            |   PropertyLandUseStndCode + park_city + year_fc    |0|
              tract_city,
            data = TD)




# (1) + Interaction
reg2<- felm(logprice ~  park.proximity  + homicide.risk + park_homicide.risk
            + age + age2
            + sqfeet
            + TotalBedrooms
            + FullBath
            + miss_age
            + miss_sqfeet
            + miss_bedrooms
            + miss_bath
            + logDCBD
            + median_age_block + median_income_block
            + pop.density_block + white.p_block + black.p_block + hispanic.p_block
            + vacant.p_block + renter.p_block
            |   PropertyLandUseStndCode + park_city + year_fc    |0|
              tract_city,
            data = TD)



# Repeated Sales
repsales3<-felm(delta.price ~ delta.hom.risk + delta.park.hom.risk 
                + median_age_block + median_income_block
                + pop.density_block + white.p_block + black.p_block + hispanic.p_block
                + vacant.p_block + renter.p_block  | time1 + time0  |0|  tract_city, data=TD_rep_sale)

# Matched Sample
match4<- felm(logprice ~  park.proximity + homicide.risk + park_homicide.risk
              + age + age2
              + sqfeet
              + TotalBedrooms
              + FullBath
              + miss_age
              + miss_sqfeet
              + miss_bedrooms
              + miss_bath
              + logDCBD
              + median_age_block + median_income_block
              + pop.density_block + white.p_block + black.p_block + hispanic.p_block
              + vacant.p_block + renter.p_block 
              |   PropertyLandUseStndCode  + park_city   + year_fc         |0|
                tract_city,
              data=TD_matched)

# (4) + Quadratic Effects 
reg5<- felm(logprice ~  park.proximity  + homicide.risk  + homicide.risk.sq + park_homicide.risk + park_homicide.risk.sq
            + age + age2
            + sqfeet
            + TotalBedrooms
            + FullBath
            + miss_age
            + miss_sqfeet
            + miss_bedrooms
            + miss_bath
            + logDCBD
            + median_age_block + median_income_block
            + pop.density_block + white.p_block + black.p_block + hispanic.p_block
            + vacant.p_block + renter.p_block
            |  PropertyLandUseStndCode  + park_city  + year_fc    |0|
              tract_city,
            data = TD)



##########################################################
#Homicide Risk that locks the neighborhood
##########################################################
msm::deltamethod(~x1/x3,coef(reg2),vcov(reg2))
-1*coef(reg2)[1]/coef(reg2)[3]

##########################################################

stargazer(reg1,
          reg2,
          repsales3,
          match4,
          reg5,
          omit=c("NoOfStories", "FullBath", "sqfeet", "TotalBedrooms","age*",
                 "miss_stories","miss_bath","miss_sqfeet","miss_bedrooms","miss_age",
                 "logDCBD", "median*", "*block", "PropertyLandUseStndCode*", "park_city*", "year_fc*","factor*"),
          omit.stat = c("f","ser","rsq","adj.rsq"),
          star.cutoffs=c(0.1,0.05,.01),
          type="text")


#nice to export
texreg(list(reg1,
            reg2,
            repsales3,
            match4,
            reg5
        ),
        custom.coef.names = c("Park Proximity",
                             "Homicide Risk",
                              rep(NA, 18),
                             "Park Prox. $ \times$ Homicide Risk",
                             "Homicide Risk", 
                            "Park Prox. $ \times$ Homicide Risk",
                             "Homicide Risk Sq.",
                             "Park Prox. $ \times$ Homicide Risk Sq."),
       digits = 3,
       stars = c(0.01, 0.05, 0.1),
       file = "views/table3.tex")


#end3