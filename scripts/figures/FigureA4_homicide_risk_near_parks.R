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
parks<-unique(as.character(TD2$park_city))


get.ratio<-function(city){
  homicides<-readRDS(paste0("stores/shift_share/crimes_shift_share/density_estimates_",city,"_by_park_total_period.rds"))
  park_ft<-readRDS(paste0("stores/shift_share/parks_features/parks_features_",city,".rds"))
  homicides<-left_join(homicides,park_ft)
  homicides<-homicides %>% mutate(area.600=area.600*3.86102e-7,area.200=area.200*3.86102e-7,area.200.600=area.200.600*3.86102e-7)
  homicides<-homicides %>% mutate(homicides.200=e_hom_200_t/area.200,
                                  homicides.200.600=e_hom_2_6_t/area.200.600,
                                  homicides.600=e_hom_600_t/area.600)
  if(city=="Chicago"){
    homicides<-homicides %>% mutate(homicides.600=homicides.600/16)}
  else{homicides<-homicides %>% mutate(homicides.600=homicides.600/11)
  }
  
  homicides<-homicides %>% mutate(ratio=homicides.200/homicides.200.600) %>% dplyr::select(park_name,ratio,homicides.600)
  homicides<-homicides %>% mutate(log_ratio=log(ratio), log_homicides.600=log(homicides.600)) 
  homicides$city<-city
  return(homicides)
}
####################################################################################
#expected homicides Chicago
####################################################################################
cty<-"Chicago"
chi.homicides<-get.ratio(cty)
summary(chi.homicides)


####################################################################################
#expected homicides NY
####################################################################################
cty<-"NY"
ny.homicides<-get.ratio(cty)
summary(ny.homicides)
####################################################################################
#expected homicides Philly
cty<-"Philly"
philly.homicides<-get.ratio(cty)
summary(philly.homicides)
####################################################################################


homicides<-rbind(chi.homicides,ny.homicides,philly.homicides)
homicides$park_city<-paste0(homicides$park_name,"_",homicides$city)
homicides<-homicides %>% dplyr::filter(park_city%in%parks)


# quantile(homicides$homicides.600,seq(0,1,0.01))
# View(homicides %>% filter(homicides.600>=8 & grepl("Philly",park_city)==T))

ggplot(homicides,aes(x=homicides.600,y=ratio)) + 
  geom_point(shape=21, fill="white", color="black", size=.25, stroke = .25) +
  xlab("Homicide Risk in Neighborhood") +
  ylab("Homicide Risk: Near Park/Rest Neighborhood") +
  theme_bw() + 
  geom_smooth(method=lm,  color="black", fill="grey", size=.25) +
  theme(#axis.title.x=element_blank(),
    #axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    #axis.title.y=element_blank(),
    #axis.text.y=element_blank(),
    axis.ticks.y=element_blank()) +
  theme(#plot.margin=grid::unit(c(0,0,0,0), "mm"), 
        panel.grid.major = element_line(size = 0.1),
        legend.justification=c(.95,.95), 
        legend.position=c(.95,.95), 
        text = element_text(size=4)) 
ggsave("views/homicide_neighborhoods.png", height = 1.5, width = 2.5, units="in")
