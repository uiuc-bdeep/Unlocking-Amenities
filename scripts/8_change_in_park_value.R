###########################################################
# Unlocking Amentities
# Table 3:  Park Premium and Homicide Risk:
# Pooled Panel Estimator across 1,337 Neighborhoods over Time
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")


#Load Packages
pkg<-list("dplyr","lfe", "stargazer","McSpatial","texreg")
lapply(pkg, require, character.only=T)
rm(pkg)

setwd("~/Dropbox/Phd Illinois/Research/Neigh_crime/Unlocking_amenities/GithubRepo/Unlocking-Amenities/")

#Read data
TD<-readRDS("stores/data_unlocking_amenities.rds")
TD_matched<-readRDS("stores/data_unlocking_amenities_matched_sample.rds")


#Drop first two years of sample for each city
TD<- TD %>% group_by(city) %>% filter(year>(min(year)+1)) %>% ungroup()
TD_matched<-TD_matched %>% group_by(city) %>% filter(year>(min(year)+1)) %>% ungroup()

TD<- TD %>% filter(year<2016)
TD_matched<- TD_matched %>% filter(year<2016)

#Repeated Sales Sample
rep_sale_TD<-repsaledata(TD$logprice,TD$year,TD$ImportParcelID)
rep_sale_crime<-repsaledata(TD$homicide.risk,TD$year,TD$ImportParcelID)
colnames(rep_sale_crime)[colnames(rep_sale_crime)=="price0"]<-"homicide.risk.0"
colnames(rep_sale_crime)[colnames(rep_sale_crime)=="price1"]<-"homicide.risk.1"
rep_sale_crime_park<-repsaledata(TD$park_homicide.risk,TD$year,TD$ImportParcelID)
colnames(rep_sale_crime_park)[colnames(rep_sale_crime_park)=="price0"]<-"park_homicide.risk.0"
colnames(rep_sale_crime_park)[colnames(rep_sale_crime_park)=="price1"]<-"park_homicide.risk.1"

rep_sale_crime_iv<-repsaledata(TD$iv.homicide.risk,TD$year,TD$ImportParcelID)
colnames(rep_sale_crime_iv)[colnames(rep_sale_crime_iv)=="price0"]<-"instrument.homicide.risk.0"
colnames(rep_sale_crime_iv)[colnames(rep_sale_crime_iv)=="price1"]<-"instrument.homicide.risk.1"

rep_sale_crime_park_iv<-repsaledata(TD$iv.park_homicide.risk,TD$year,TD$ImportParcelID)
colnames(rep_sale_crime_park_iv)[colnames(rep_sale_crime_park_iv)=="price0"]<-"park_instrument.homicide.risk.0"
colnames(rep_sale_crime_park_iv)[colnames(rep_sale_crime_park_iv)=="price1"]<-"park_instrument.homicide.risk.1"


TD_rep_sale<-left_join(rep_sale_TD,rep_sale_crime)
TD_rep_sale<-left_join(TD_rep_sale,rep_sale_crime_park)
TD_rep_sale<-left_join(TD_rep_sale,rep_sale_crime_iv)
TD_rep_sale<-left_join(TD_rep_sale,rep_sale_crime_park_iv)

TD_rep_sale<- TD_rep_sale %>% mutate(delta.time=time1-time0,
                                     delta.price=price1-price0,
                                     delta.hom.risk=homicide.risk.1- homicide.risk.0,
                                     delta.park.hom.risk=park_homicide.risk.1- park_homicide.risk.0,
                                     delta.iv.hom.risk=instrument.homicide.risk.1-instrument.homicide.risk.0,
                                     delta.iv.park.hom.risk=park_instrument.homicide.risk.1-park_instrument.homicide.risk.0)

colnames(TD_rep_sale)[colnames(TD_rep_sale)=="id"]<-"ImportParcelID"

tract_city<- TD %>% dplyr::select(ImportParcelID,tract_city, park_city, year, median_age_block, median_income_block, pop.density_block, white.p_block, black.p_block, hispanic.p_block, vacant.p_block, renter.p_block )
tract_city<- tract_city %>% distinct(ImportParcelID,.keep_all = TRUE)
TD_rep_sale<-left_join(TD_rep_sale,tract_city)
TD_rep_sale$time0<--1*TD_rep_sale$time0
rm(tract_city)

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



iv1<-  felm(logprice ~  park.proximity
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
            |   PropertyLandUseStndCode + park_city + year_fc
            | (homicide.risk  ~ iv.homicide.risk) |
              park_city,
            data = TD)


reg2<- felm(logprice ~  park.proximity + homicide.risk + park_homicide.risk
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


iv2<- felm(logprice ~  park.proximity
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
            |   PropertyLandUseStndCode + park_city  + year_fc
            | (homicide.risk + park_homicide.risk ~ iv.homicide.risk + iv.park_homicide.risk  ) |
             tract_city,
            data = TD)

coef(reg2)[1]/coef(reg2)[3]
msm::deltamethod(~ x1/x3 , coef(reg2), vcov(reg2))
coef(iv2)[1]/coef(iv2)[grepl("park_homicide",names(coef(iv2)))]
coef(iv2)[1]/coef(iv2)[21]
msm::deltamethod(~ x1/x21 , coef(iv2), vcov(iv2))




# Repeated Sales
reg3<-felm(delta.price ~ delta.hom.risk + delta.park.hom.risk + median_age_block + median_income_block
           + pop.density_block + white.p_block + black.p_block + hispanic.p_block
           + vacant.p_block + renter.p_block  | time1 + time0  + park_city  |0|  tract_city, data=TD_rep_sale)

iv3<- felm(delta.price ~    1 + median_age_block + median_income_block
           + pop.density_block + white.p_block + black.p_block + hispanic.p_block
           + vacant.p_block + renter.p_block
           |time1 + time0 + park_city | (delta.hom.risk + delta.park.hom.risk ~ delta.iv.hom.risk + delta.iv.park.hom.risk  ) | tract_city,
           data=TD_rep_sale)


# Matched Sample
match4<- felm(logprice ~   park.proximity + homicide.risk + park_homicide.risk
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
            data = TD_matched)


match_iv4<- felm(logprice ~    park.proximity
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
           |   PropertyLandUseStndCode + park_city  + year_fc
           | (homicide.risk + park_homicide.risk ~ iv.homicide.risk + iv.park_homicide.risk  ) |
             tract_city,
           data = TD_matched)





stargazer(reg1,
           iv1,
          reg2,
           iv2,
          reg3,
          iv3,
          match4,
          match_iv4,
          omit=c("NoOfStories", "FullBath", "sqfeet", "TotalBedrooms","age*",
                 "miss_stories","miss_bath","miss_sqfeet","miss_bedrooms","miss_age",
                 "logDCBD", "median*", "*block", "PropertyLandUseStndCode*", "park_city*", "year_fc*","factor*"),
          omit.stat = c("f","ser","rsq","adj.rsq"),
          type="text")

#nice to export
#screenreg
texreg(list(reg1,
            iv1,
            reg2,
            iv2,
            reg3,
            iv3,
            match4,
            match_iv4),
       custom.coef.names = c(
         "Park Proximity",
         "Homicide Risk", 
         rep(NA, 18),
          "Homicide Risk", 
         "Park Prox. $ \times$ Homicide Risk",
         "Park Prox. $ \times$ Homicide Risk",                             "Homicide Risk", 
         "Park Prox. $ \times$ Homicide Risk",
         "Homicide Risk", 
         "Park Prox. $ \times$ Homicide Risk"),
       digits = 3,
       stars = c(0.01, 0.05, 0.1)
       ,file = "views/table6_appendix.tex")
          




#fin
