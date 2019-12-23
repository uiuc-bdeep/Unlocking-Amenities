##########################################################
# author: Ignacio Sarmiento-Barbieri
#
##########################################################

#Clean the workspace
#rm(list=ls())
#cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo



#Load Packages
pkg<-list("ggmap","raster","dplyr")
lapply(pkg, require, character.only=T)
rm(pkg)


#Wd
#setwd("/Volumes/share/projects/CrimeAndParks/")
#setwd("/home/guest1/share/CrimeAndParks/") #bdeep

setwd("~/Dropbox/Phd\ Illinois/Research/Neigh_crime/Unlocking_amenities")

cty<-"NY"
# register_google(key = "AIzaSyANLZLBQgpPun_rqkbfbAeQ9eXWN4AtuWI")
# map = get_map(location = "New York",  maptype = c("toner"), source = c("stamen"), zoom = 11)
# saveRDS(map,paste0("stores/city_",cty,".rds"))

load("stores/city_",cty,".rds")




park<-readRDS(paste0("stores/parks/parks_",cty,"_fixed.rds"))
crs<-CRS("+proj=longlat +datum=WGS84")
park <- spTransform(park, crs)
parks_in_db_fortify<-fortify(park)

#house data
TD<-readRDS("stores/shift_share/data_ready_v6_cross.rds")
TD$park_logical<-ifelse(TD$park==1,TRUE,FALSE)
TD$park_logical<-factor(TD$park_logical,levels=c("TRUE","FALSE"),labels=c("Proximate to a Park (Treatment)", "Not Proximate to a Park (Control)"),ordered=T)
chicago<- TD %>% filter(city==cty)


chicago$dist2<-as.character(chicago$dist)
chicago$dist2[chicago$dist2=="100"]<-'0 to 1/16'
chicago$dist2[chicago$dist2=="200"]<-'1 to 2/16'
chicago$dist2[chicago$dist2=="300"]<-'3 to 5/16'
chicago$dist2[chicago$dist2=="400"]<-'3 to 5/16'
chicago$dist2[chicago$dist2=="500"]<-'3 to 5/16'
chicago$dist2[chicago$dist2=="600"]<-'5 to 6/16'
chicago$dist2<-factor(chicago$dist2,levels=c('0 to 1/16','1 to 2/16','3 to 5/16','5 to 6/16'),ordered=TRUE)






p<-ggmap(map) +
  geom_polygon(data=parks_in_db_fortify,aes(long, lat, group = group), fill="#31a354" , alpha=0.5,color="black")  

p +    geom_point(data=chicago, 
                  aes(x=PropertyAddressLongitude, y=PropertyAddressLatitude, color=dist2,group=dist2), size=.5, alpha=0.85,stroke=.2) +
  scale_color_manual(values=c("#1a9850",
                              #"#91cf60",
                              "#d9ef8b",
                              #"#fee08b",
                              #"#fc8d59",
                              "#fc4e2a",
                              "#808080")) + #,"#fb6a4a","#fc9272","#fcbba1")) +
  labs(color = "Distance to Park \n (miles)") +
  theme(plot.margin=grid::unit(c(0,0,0,0), "cm"), 
        #legend.justification=c(.95,.95), 
        legend.position="right",#c(.95,.95), 
        legend.key = element_rect(fill = "white"),
        text = element_text(size=10),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  guides(colour = guide_legend(override.aes = list(size=2, alpha=1, nrow=2))) 
ggsave(paste0("github/Rescaled/views/house_sales_treated_control_",cty,".png"), height = 5, width = 5, units="in")






