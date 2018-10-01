###########################################################
# Unlocking Amentities
# Table 3:  Park Premium and Homicide Risk:
# Pooled Panel Estimator across 1,337 Neighborhoods over Time
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")


#Load Packages
pkg<-list("dplyr","lfe", "stargazer","texreg")
lapply(pkg, require, character.only=T)
rm(pkg)


setwd("~/Dropbox/Phd Illinois/Research/Neigh_crime/Unlocking_amenities/GithubRepo/Unlocking-Amenities/")
#Read data
TD<-readRDS("stores/data_unlocking_amenities.rds")





# Property Level
reg1<- felm(logprice ~  park.proximity  + homicide.risk + park_homicide.risk
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



# Neighborhood Level

TD<-TD %>% mutate(park_neigh.homicide.risk=park.proximity*neigh.homicide.risk,
                  neigh.homicide.risk_ex.18=neigh.homicide.risk- neigh.homicide.risk.w.1.8.mi,
                  park_neigh.homicide.risk_ex.18=park.proximity*neigh.homicide.risk_ex.18,
                  neigh.homicide.risk_ex.pk=neigh.homicide.risk-neigh.homicide.risk.at.park,
                  park_neigh.homicide.risk_ex.pk=park.proximity*neigh.homicide.risk_ex.pk)



reg2<- felm(logprice ~  park.proximity  + neigh.homicide.risk + park_neigh.homicide.risk
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

#Exclude crime within 1/8 miles
reg3<- felm(logprice ~  park.proximity  + neigh.homicide.risk_ex.18 + park_neigh.homicide.risk_ex.18
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

#Exclude crime within park
reg4<- felm(logprice ~  park.proximity  + neigh.homicide.risk_ex.pk + park_neigh.homicide.risk_ex.pk
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

#Nice view of relevant coefficients
stargazer(reg1,
          reg2,
          reg3,
          reg4,
          omit=c("NoOfStories", "FullBath", "sqfeet", "TotalBedrooms","age*",
                 "miss_stories","miss_bath","miss_sqfeet","miss_bedrooms","miss_age",
                 "logDCBD", "median*", "*block", "PropertyLandUseStndCode*", "park_city*", "year_fc*","factor*"),
          omit.stat = c("f","ser","rsq","adj.rsq"),
          star.cutoffs=c(0.1,0.05,.01),
          type="text")

#nice to export
texreg(list(reg1, reg2,reg3,reg4),
       custom.coef.names = c("Park Proximity", 
                             "Homicide Risk", "Park Prox. $ \times$ Homicide Risk",rep(NA, 18), 
                             "Homicide Risk", "Park Prox. $ \times$ Homicide Risk", 
                             "Homicide Risk", "Park Prox. $ \times$ Homicide Risk", 
                             "Homicide Risk", "Park Prox. $ \times$ Homicide Risk"),digits = 3,
       stars = c(0.01, 0.05, 0.1),
       file = "views/table4.tex")


#end

