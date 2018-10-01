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

TD0<-readRDS("stores/data_unlocking_amenities.rds")

TD<-TD0

TD<- TD %>% group_by(city) %>% filter(year>(min(year)+1)) %>% ungroup()

TD<-data.frame(TD) %>% ungroup()
cutoff1<-0.5


TD <- TD %>% group_by(city) %>% mutate(big=ifelse(area_park>=quantile(area_park,cutoff1),1,0),
                                       small=ifelse(area_park<quantile(area_park,cutoff1),1,0)) %>% ungroup()
head(TD)


descriptive <- TD %>% group_by(city,big) %>% summarize(area_park=min(area_park))


mm<-model.matrix(~ park.proximity:small
                 + park.proximity:big
                 + park.proximity:small:homicide.risk
                 + park.proximity:big:homicide.risk
                 + park.proximity:small:iv.homicide.risk
                 + park.proximity:big:iv.homicide.risk -1,TD)
colnames(mm)
colnames(mm)<-c("park_small","park_big",   "park_small_homicides",   "park_big_homicides","park_small_pred_homicides","park_big_pred_homicides")
mm<-data.frame(mm)

TD<-bind_cols(TD,mm)



reg1<- felm(logprice ~  park_big  +  park_small +  homicide.risk
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

reg2<- felm(logprice ~  park_big  + park_small +  park_big_homicides  + park_small_homicides + homicide.risk
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

iv1<- felm(logprice ~  park_big   + park_small
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
           |  PropertyLandUseStndCode  + park_city  + year_fc  
            | ( homicide.risk  ~ iv.homicide.risk )|
             tract_city,
           data = TD)

iv2<- felm(logprice ~  park_big    + park_small
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
           |  PropertyLandUseStndCode  + park_city  + year_fc   
           | ( homicide.risk  + park_big_homicides  + park_small_homicides ~ iv.homicide.risk  + park_big_pred_homicides  + park_small_pred_homicides)|
             tract_city,
           data = TD)



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
                      iv2))  
texreg(list(reg1,
            reg2,
            iv1,
            iv2),
        custom.coef.names = c(
          "Proximity to Large Park",
          "Proximity to Small Park",
          "Homicide Risk", 
          rep(NA, 18),
          "Large Park $ \times$ Homicide Risk",
          "Small Park $ \times$ Homicide Risk",     
          "Homicide Risk", 
          "Large Park $ \times$ Homicide Risk",
          "Small Park $ \times$ Homicide Risk"
        ),
       digits = 3,
       stars = c(0.01, 0.05, 0.1)
       ,file = "views/table7.tex")        
          
          
#end
