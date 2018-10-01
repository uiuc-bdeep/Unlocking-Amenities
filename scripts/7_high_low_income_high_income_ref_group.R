##########################################################
# author: Ignacio Sarmiento-Barbieri
#
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo



#Load Packages
pkg<-list("dplyr","lfe", "stargazer","McSpatial","texreg")
lapply(pkg, require, character.only=T)
rm(pkg)


#Wd
#setwd("/Volumes/share/projects/CrimeAndParks/")
#setwd("/home/guest1/share/CrimeAndParks/") #bdeep


setwd("~/Dropbox/Phd Illinois/Research/Neigh_crime/Unlocking_amenities/GithubRepo/Unlocking-Amenities/")

TD<-readRDS("stores/data_unlocking_amenities.rds")


TD<- TD %>% group_by(city) %>% filter(year>(min(year)+1)) %>% ungroup()

TD0<-TD



cutoff<-0.5
TD<- TD0 %>% mutate(low_income=ifelse(median_income_block<quantile(median_income_block,cutoff,na.rm=T),1,0),
                    high_income=ifelse(median_income_block>=quantile(median_income_block,cutoff,na.rm=T),1,0)) %>% ungroup()



mm<-model.matrix(~ park.proximity:low_income
                 + park.proximity:high_income
                 + low_income:homicide.risk
                 + high_income:homicide.risk
                 + park.proximity:low_income:homicide.risk
                 + park.proximity:high_income:homicide.risk
                 + low_income:iv.homicide.risk
                 + high_income:iv.homicide.risk
                 + park.proximity:low_income:iv.homicide.risk
                 + park.proximity:high_income:iv.homicide.risk -1,TD)
colnames(mm)
colnames(mm)<-c("park_low_income","park_high_income", "low_homicide", "high_homicide",  "park_low_homicides",   "park_high_homicides", "low_pred_homicide", "high_pred_homicide", "park_low_pred_homicides","park_high_pred_homicides")
mm<-data.frame(mm)

TD<-bind_cols(TD,mm)


reg1<- felm(logprice ~ park_low_income + park_high_income + high_income + homicide.risk
               + age + age2
               + sqfeet
               + TotalBedrooms
               + FullBath
               + miss_age
               + miss_sqfeet
               + miss_bedrooms
               + miss_bath
               + logDCBD
               + median_age_block 
               + pop.density_block + white.p_block + black.p_block + hispanic.p_block
               + vacant.p_block + renter.p_block
               |  PropertyLandUseStndCode  + park_city  + year_fc   |0|  tract_city,
               data = TD)

reg2<- felm(logprice ~  park_high_income+ park_low_income  + high_income + park_homicide.risk +homicide.risk
              + age + age2
              + sqfeet
              + TotalBedrooms
              + FullBath
              + miss_age
              + miss_sqfeet
              + miss_bedrooms
              + miss_bath
              + logDCBD
              + median_age_block 
              + pop.density_block + white.p_block + black.p_block + hispanic.p_block
              + vacant.p_block + renter.p_block
              |  PropertyLandUseStndCode  + park_city  + year_fc   |0|  tract_city,
              data = TD)




iv1<- felm(logprice ~   park_high_income+ park_low_income  + high_income
           + age + age2
           + sqfeet
           + TotalBedrooms
           + FullBath
           + miss_age
           + miss_sqfeet
           + miss_bedrooms
           + miss_bath
           + logDCBD
           + median_age_block 
           + pop.density_block + white.p_block + black.p_block + hispanic.p_block
           + vacant.p_block + renter.p_block
           |  PropertyLandUseStndCode  + park_city  + year_fc   
           | (homicide.risk  ~ iv.homicide.risk   )|
             tract_city,
           data = TD)#, na.action=na.pass)

iv2<- felm(logprice ~ park_high_income+ park_low_income  + high_income
          + age + age2
          + sqfeet
          + TotalBedrooms
          + FullBath
          + miss_age
          + miss_sqfeet
          + miss_bedrooms
          + miss_bath
          + logDCBD
          + median_age_block 
          + pop.density_block + white.p_block + black.p_block + hispanic.p_block
          + vacant.p_block + renter.p_block
          |  PropertyLandUseStndCode  + park_city  + year_fc   
         | (homicide.risk + park_homicide.risk ~ iv.homicide.risk + iv.park_homicide.risk  )|
            tract_city,
          data = TD)#, na.action=na.pass)



stargazer(reg1,
          reg2,
          iv1,
          iv2,
          omit=c("NoOfStories", "FullBath", "sqfeet", "TotalBedrooms","age*",
                 "miss_stories","miss_bath","miss_sqfeet","miss_bedrooms","miss_age",
                 "logDCBD", "median*", "*block", "PropertyLandUseStndCode*", "park_city*", "year_fc*","factor*"),
          omit.stat = c("f","ser","rsq","adj.rsq"),
          type="text")

screenreg(list(reg1,
               reg2,
               iv1,
               iv2),
          digits = 3,
          stars = c(0.01, 0.05, 0.1))

texreg(list(reg1,
            reg2,
            iv1,
            iv2),
       custom.coef.names = c(
         "Park Proximity $\times$ Low Income",
         "Park Proximity $\times$ High Income",
         "High Income",
         "Homicide Risk",
         rep(NA, 17),
         "Park Prox. $\times$ Homicide Risk",
         "Homicide Risk",
         "Park Prox. $\times$ Homicide Risk"
       ),
       digits = 3,
       stars = c(0.01, 0.05, 0.1)
       ,file = "views/table10_high_income.tex")        


#end1
