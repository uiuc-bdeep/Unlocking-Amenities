###########################################################
# Unlocking Amentities
# Table 3:  Park Premium and Homicide Risk:
# Pooled Panel Estimator across 1,337 Neighborhoods over Time
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")


#Load Packages
pkg<-list("dplyr","lfe", "stargazer","ggplot2")
lapply(pkg, require, character.only=T)
rm(pkg)


setwd("~/Dropbox/Phd Illinois/Research/Neigh_crime/Unlocking_amenities/GithubRepo/Unlocking-Amenities/")
#Read data
TD<-readRDS("stores/data_unlocking_amenities.rds")





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

stargazer(reg5,
          omit=c("NoOfStories", "FullBath", "sqfeet", "TotalBedrooms","age*",
                 "miss_stories","miss_bath","miss_sqfeet","miss_bedrooms","miss_age",
                 "logDCBD", "median*", "*block", "PropertyLandUseStndCode*", "park_city*", "year_fc*","factor*"),
          omit.stat = c("f","ser","rsq","adj.rsq"),
          star.cutoffs=c(0.1,0.05,.01),
          type="text")

######################################################################
# Plot using mean
# Delta Method
######################################################################

#Base results
db<-coef(summary(reg5))
db<-cbind(db,confint(reg5,level=.95))
#db<-apply(db,2,function(x) as.numeric(x))
db<-data.frame(db)
db$names<-rownames(db)
db<-db[1:2,]
#db <- db[grepl("d[0-9][0-9][0-9]",db$names),]



colnames(db)<-c("Coefficient","SE","t","Pval","lower","upper","Variable")
db$Variable<-as.character(db$Variable)
db<-db[c("Variable","Coefficient","lower","upper")]
db$group<-"Low Homicide Risk"


db0<-db
#rm(db)
### High homicide
# summary(reg5)
# summary(TD$neigh.e_mean_homicides)
# quantile(TD$neigh.e_mean_homicides,seq(0,1,0.01))

Nhomicides<-10
Coef_4<-data.frame(Variable=db$Variable)
Coef_4$group<-"High Homicide Risk" #95
Coef_4$Coefficient<-coef(summary(reg5))[1:2]+coef(summary(reg5))[6:7]*Nhomicides
#summary(ols4)
#sqrt(vcov(reg5)[1,1]+25*vcov(reg5)[6,6]+2*5*vcov(reg5)[1,6])
require("msm")
Coef_4$se<-0
Coef_4$se[1]<-deltamethod(~ x1 + Nhomicides*x6 , coef(reg5), vcov(reg5))
Coef_4$se[2]<-deltamethod(~ x2 + Nhomicides*x7 , coef(reg5), vcov(reg5))



Coef_4$lower<-Coef_4$Coefficient-qtnorm(0.975)*Coef_4$se
Coef_4$upper<-Coef_4$Coefficient+qtnorm(0.975)*Coef_4$se

Coef_4$se<-NULL





db<-rbind(db0,Coef_4)

#db$Variable<-as.numeric(as.character(db$Variable))
db$Variable<-c(100,200,100,200)
db<-rbind(db,c(300,0,0,0,"Low Homicide Risk"))
db<-rbind(db,c(300,0,0,0,"High Homicide Risk"))

db$Variable<-as.numeric(db$Variable)


db$Coefficient<-as.numeric(db$Coefficient)
db$lower<-as.numeric(db$lower)
db$upper<-as.numeric(db$upper)


db$group<-factor(db$group,ordered = T, levels=c("Low Homicide Risk","High Homicide Risk"))
db<-db[order(db$group,db$Variable),]
db
db_plot<-db



# Plot
ggplot(db_plot,aes(x=Variable, y=Coefficient, group=group, lty=group)) +
  geom_point(size=.8, position=position_dodge(width = 10)) +
  geom_errorbar(aes(ymin=lower, ymax=upper), width=10, position=position_dodge(width = 10), size=.3) +
  geom_hline(yintercept = 0, colour = gray(1/2), lty = 2,size=.3) +
  xlab("Distance to Park (in miles)") +
  ylab("Park Premium") +
  ylim(-.135,.10)+
  scale_shape_manual(values=c(15,16)) +                  # Change shapes
  #scale_linetype_manual(values=c("dashed", "dashed"))  + # Change linetypes
  scale_x_continuous(breaks=c(100,200,300),labels=c("1/16","2/16","[3/16-6/16]")) +
  theme_bw() +
  theme(#plot.margin=grid::unit(c(0,0,0,0), "mm"), 
    legend.position="bottom", 
    text = element_text(size=8)) +
  theme(legend.title=element_blank(), legend.position = "bottom",
        axis.text.x = element_text(color="black", angle = 45, hjust = 1, size=8),
        axis.text.y = element_text(color="black", hjust = 1, size=8),
        #panel.grid.major.x = element_blank(),
        panel.grid.major = element_line(size = 0.1),
        #panel.border = element_blank(),
        #panel.grid.major = element_blank(),
        #axis.ticks.x=element_blank(),
        panel.grid.minor = element_blank()) 
  
ggsave("views/bins_crossectional_neigh_crime.png", height = 3, width = 4, units="in")
dev.off()

#end

