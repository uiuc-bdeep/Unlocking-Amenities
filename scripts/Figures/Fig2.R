##########################################################
# author: Ignacio Sarmiento-Barbieri
#
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo



# #Load Packages
# pkg<-list("dplyr","KernSmooth", "ggplot2", "ggmap","raster","rgeos","data.table")
# lapply(pkg, require, character.only=T)
# rm(pkg)
# 
# 
# #Wd
# #setwd("/Volumes/share/projects/CrimeAndParks/")
# #setwd("/home/guest1/share/CrimeAndParks/") #bdeep
# 
# setwd("~/Dropbox/Phd Illinois/Research/Neigh_crime/Unlocking_amenities/github/Unlocking-Amenities_testing/")
# 
# # register_google(key = "AIzaSyANLZLBQgpPun_rqkbfbAeQ9eXWN4AtuWI")
# # chicago.ggplot = c(lon = -87.7, lat = 41.83)
# # chicago.map = get_map(location = chicago.ggplot,  maptype = c("toner"), source = c("stamen"), zoom = 11)
# # saveRDS(chicago.map,"stores/chicago_map_stamen.rds")
# chicago.map<-readRDS("stores/chicago_map_stamen.rds")
# 
# city<-"Chicago"
# 
# homicides<-readRDS("stores/homicides_chicago.rds")
# homicides<-homicides %>% group_by(year) %>% summarize(n=n()) %>% ungroup()
# hom.03<-homicides$n[homicides$year==2003]
# hom.15<-homicides$n[homicides$year==2015]
# 
# 
# raster<-readRDS(paste0("~/Dropbox/Phd Illinois/Research/Neigh_crime/Unlocking_amenities/stores/shift_share/crimes_shift_share/density_estimates_by_year_",city,".rds"))
# 
# rst_2_poly<-function(id,homicide){
#   y<-raster[[id]][[2]]
#   z1<-raster[[id]][[3]]*0.000621371
#   z2<-raster[[id]][[4]]*0.000621371
#   z3<-1/(z1*z2)
#   values(y)<-values(y)*z3*homicide
#   
#   crs(y) <- sp::CRS("+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
#   r <- projectRaster(y, crs = CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
#   map<-rasterToPolygons(r)
#   map@data$id = rownames(map@data)
#   map_data = fortify(map, region="id")
#   map = left_join(map_data, map@data, by="id")
# 
# }
# 
# 
# 
# map.2003<-rst_2_poly(2,hom.03)
# 
# map.2003$id<-as.numeric(map.2003$id)
# map.2003<-map.2003[order(-map.2003$id),]
# map.2003$layer2<-ifelse(map.2003$layer<1,NA,map.2003$layer)
# map.2003f<-map.2003[!is.na(map.2003$layer2),]
# 
# # View(head(data.df))
# 
# 
# 
# # 2015 --------------------------------------------------------------------
# map.2015<-rst_2_poly(15,hom.15)
# 
# map.2015$id<-as.numeric(map.2015$id)
# map.2015<-map.2015[order(-map.2015$id),]
# map.2015$layer2<-ifelse(map.2015$layer<1,NA,map.2015$layer)
# map.2015f<-map.2015[!is.na(map.2015$layer2),]
# 
# 
# # join --------------------------------------------------------------------
# 
# map.2003f$year<-"2003"
# map.2015f$year<-"2015"
# map<-rbind(map.2003f,map.2015f)
# 
# map<- map %>% filter(long> -87.79 & long< -87.49,
#                      lat > 41.7  & lat< 41.99)
# map<-map[is.na(map$layer2)==F,]
# colorCount<-c("#FFFFFF",
#               "#FFF7EC", 
#               "#FDD9A8",
#               "#fb6a4a",
#               "#de2d26",
#               "#a50f15")
# # colorRampPalette(c("transparent", "red"))(10)
# # colorCount<-c("#FFFFFF",
# #               "#FFF7EC", 
# #               "#FDD9A8",
# #               "#fb6a4a",
# #               "#de2d26",
# #               "#a50f15")
# 
# #map
#save.image("../Rescaled/stores/heat_map.Rda")
setwd("~/Dropbox/Phd Illinois/Research/Neigh_crime/Unlocking_amenities/github/Rescaled/")
load("stores/heat_map.Rda")
z<-ggmap(chicago.map) +
  geom_polygon(data = map, aes(x = long, y = lat, group = id, fill = layer2), 
               color = "#FFFFFF",
               size = 0.001, 
               alpha = 1)  + 
  scale_fill_gradientn("Annual Homicide Risk \n  per Square Mile", colors =colorCount)+
  facet_wrap(~ year, ncol=2) +
  theme(
    legend.position="bottom",#none
    text = element_text(size=10)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        strip.background =element_rect(fill="white"), 
        strip.text = element_text(colour = 'black'))
z+ coord_fixed(xlim = c(-87.5, -87.8),  ylim = c(41.7, 41.99), ratio = 1.3)
z
ggsave("../Rescaled/views/homicide_risk_raster_with_labels.png", height = 5, width = 5, units="in")
#dev.off()
