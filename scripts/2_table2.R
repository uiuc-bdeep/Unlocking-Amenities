###########################################################
# Unlocking Amentities
# Table 2 extension in Appendix
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


reg1<- felm(logprice ~  bin1 + bin2 + bin3 + bin4 + bin5  + eMean_homicides
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
            |   PropertyLandUseStndCode + park_city  + year_fc |0|
              tract_city,
            data = TD)


reg2<- felm(logprice ~  bin1 + bin2 + bin3 + bin4 + bin5  + bin1_Mean_homicide_risk + bin2_Mean_homicide_risk + bin3_Mean_homicide_risk + bin4_Mean_homicide_risk + bin5_Mean_homicide_risk + eMean_homicides
            + age + age2
            + sqfeet
            + TotalBedrooms
            + FullBath
            + miss_age
            + miss_sqfeet
            + miss_bedrooms
            + miss_bath
            + logDCBD
            # + median_age_block + median_income_block
            # + pop.density_block + white.p_block + black.p_block + hispanic.p_block
            # + vacant.p_block + renter.p_block
            |   PropertyLandUseStndCode + park_city  + year_fc |0|
              tract_city,
            data = TD)



reg3<- felm(logprice ~  bin1 + bin2 + bin3 + bin4 + bin5  + bin1_Mean_homicide_risk + bin2_Mean_homicide_risk + bin3_Mean_homicide_risk + bin4_Mean_homicide_risk + bin5_Mean_homicide_risk  + eMean_homicides
            + age + age2
            + sqfeet
            + TotalBedrooms
            + FullBath
            + miss_age
            + miss_sqfeet
            + miss_bedrooms
            + miss_bath
            + logDCBD
            # + median_age_block + median_income_block
            # + pop.density_block + white.p_block + black.p_block + hispanic.p_block
            # + vacant.p_block + renter.p_block
            |   PropertyLandUseStndCode + park_city + tract_city + year_fc   |0|
              park_city,
            data = TD)


reg4<- felm(logprice ~  bin1 + bin2 + bin3 + bin4 + bin5  + bin1_Mean_homicide_risk + bin2_Mean_homicide_risk + bin3_Mean_homicide_risk + bin4_Mean_homicide_risk + bin5_Mean_homicide_risk +eMean_homicides 
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
            |   PropertyLandUseStndCode+ park_city  + year_fc |0|
              park_city,
            data = TD)




#Neighborhood reg 5  estimates from a model that uses a single estimate of homicide risk per neighborhood and thus drops the coefficient on homicide risk
reg5<- felm(logprice ~  bin1 + bin2 + bin3 + bin4 + bin5  + bin1_neigh.mean_homicide_risk + bin2_neigh.mean_homicide_risk + bin3_neigh.mean_homicide_risk + bin4_neigh.mean_homicide_risk + bin5_neigh.mean_homicide_risk + neigh.e_mean_homicides  
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
            |   PropertyLandUseStndCode+ park_city  + year_fc |0|
              tract_city,
            data = TD)



stargazer(reg1,
          reg2,
          reg3,
          reg4,
          omit=c("NoOfStories", "FullBath", "sqfeet", "TotalBedrooms","age*",
                 "miss_stories","miss_bath","miss_sqfeet","miss_bedrooms","miss_age",
                 "logDCBD", "median*", "*block ", "PropertyLandUseStndCode*", "park_city*", "year_fc*","factor*"),
           # covariate.labels = c("Within 1/16mi. of a Park",
           #                       "Within 2/16mi. of a Park",
           #                       "Within 3/16mi. of a Park",
           #                       "Within 4/16mi. of a Park",
           #                       "Within 5/16mi. of a Park",
           #                       "Within 1/16 mi. x Homicide Risk",
           #                       "Within 2/16 mi. x Homicide Risk",
           #                       "Within 3/16 mi. x Homicide Risk",
           #                       "Within 4/16 mi. x Homicide Risk",
           #                       "Within 5/16 mi. x Homicide Risk"
           #  ),
          omit.stat = c("f","ser","rsq","adj.rsq"),
          star.cutoffs=c(0.1,0.05,.01),
          type="text")
          #,out="views/table2_appendix.tex")


screenreg(list(reg1,
            reg2,
            reg3,
            reg4,
            reg5
))

#nice to export
texreg(list(reg1,
            reg2,
            reg3,
            reg4,
            reg5
            ),
       custom.coef.names = c("Within 1/16 mile of a Park",
                             "Within 2/16 mile of a Park",
                             "Within 3/16 mile of a Park",
                             "Within 4/16 mile of a Park",
                             "Within 5/16 mile of a Park",
                             "Homicide Risk",
                             rep(NA, 18),
                             "Within 1/16 mile   $\times$ Homicide Risk",
                             "Within 2/16 mile   $\times$ Homicide Risk",
                             "Within 3/16 mile   $\times$ Homicide Risk",
                             "Within 4/16 mile   $\times$ Homicide Risk",
                             "Within 5/16 mile   $\times$ Homicide Risk",
                             "Within 1/16 mile   $\times$ Homicide Risk",
                             "Within 2/16 mile   $\times$ Homicide Risk",
                             "Within 3/16 mile   $\times$ Homicide Risk",
                             "Within 4/16 mile   $\times$ Homicide Risk",
                             "Within 5/16 mile   $\times$ Homicide Risk",
                             "Homicide Risk"
                             
       ),
            digits = 3,
            stars = c(0.01, 0.05, 0.1)
            , file = "views/table2_appendix.tex")

#end5



