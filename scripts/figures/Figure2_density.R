##########################################################
# author: Ignacio Sarmiento-Barbieri
#
##########################################################

#Clean the workspace
rm(list=ls())
cat("\014")
local({r <- getOption("repos"); r["CRAN"] <- "http://cran.r-project.org"; options(repos=r)}) #set repo



#Load Packages
pkg<-list("dplyr","KernSmooth", "ggplot2", "ggmap","raster","rgeos","data.table")
lapply(pkg, require, character.only=T)
rm(pkg)


#Wd
#setwd("/Volumes/share/projects/CrimeAndParks/")
#setwd("/home/guest1/share/CrimeAndParks/") #bdeep

setwd("~/Dropbox/Phd Illinois/Research/Neigh_crime/Unlocking_amenities/GithubRepo/Unlocking-Amenities/")


chicago.ggplot = c(lon = -87.7, lat = 41.83)
chicago.map = get_map(location = chicago.ggplot,  maptype = c("toner"), source = c("stamen"), zoom = 11)

city<-"Chicago"

homicides<-readRDS("stores/homicides_chicago.rds")


##########################################################
#Contour Gen Function
##########################################################

contour_gen<-function(db,etos){
  homicides.ss<-base::subset(db, year%in%etos, select=c("longitude","latitude"))
  
  spdata.hom <- SpatialPointsDataFrame(homicides.ss,homicides.ss, proj4string = CRS("+proj=longlat +datum=WGS84"))
  
  
  # compute the 2D binned kernel density estimate
  #Projections to meters
  crs <- CRS("+proj=utm +zone=16 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0") # Illinois is in the zone #16 for NAD83 projection
  
  spdata.hom <- spTransform(spdata.hom,crs)
  
  coords.hom<-coordinates(spdata.hom)
  
  grid_size<-250
  band<-300
  
  fhat.hom <- bkde2D(coords.hom,
                     bandwidth=c(band,band),
                     gridsize=c(grid_size,grid_size))
  
  
  x1<-fhat.hom$x1
  x2<-fhat.hom$x2
  y<-unique(diff(fhat.hom$x1))[1]
  x<-unique(diff(fhat.hom$x2))[2]
  #z<-fhat.hom$fhat*y*x*dim(homicides)[1]
  #z<-fhat.hom$fhat*y*x
  z<-fhat.hom$fhat
  
  # generates countour plots returns a data frame for ggmap
  lines <- contourLines(x=x1,y=x2,z = z ,nlevels = 10)

  dd1 <- sapply(1:length(lines),function(i) Polygon(as.matrix(cbind(lines[[i]]$x,lines[[i]]$y))))

  dd2 <- sapply(1:length(lines),function(i) Polygons(list(dd1[[i]]),i))

  poly_data <- data.frame(value = sapply(1:length(lines),function(i) lines[[i]]$level))
  maps<-SpatialPolygonsDataFrame(SpatialPolygons(dd2),data = poly_data)

  proj4string(maps)<-crs
  maps<-spTransform(maps,CRS("+proj=longlat +datum=WGS84 +no_defs"))
  maps@data$id = rownames(maps@data)
  maps_data = fortify(maps, region="id")
  data.df = left_join(maps_data, maps@data, by="id")
  return(data.df)
}

##########################################################
#2003
##########################################################
map.2003<-contour_gen(homicides,c(2001,2002,2003))

map.2003$value<-cut(map.2003$value,5)
colourCount = length(unique(map.2003$value))
colorCount<-c("#FFF7EC", 
              "#FDD9A8",
              "#fb6a4a",
              "#de2d26",
              "#a50f15")



map.2003$value_lab<-factor(map.2003$value, labels=c("Low",  "",  "  ", "   ", "High"), ordered=TRUE)

map.2003$id<-as.numeric(map.2003$id)
map.2003<-map.2003[order(-map.2003$id),]

map.2003$year<-"2001-2003"

##########################################################
#2015
##########################################################
map.2015<-contour_gen(homicides,c(2013,2014,2015))


map.2015$value<-cut(map.2015$value,breaks=c(9.88e-10, 3.4e-09, 5.8e-09,8.2e-09, 1.06e-08 ,1.3e-08))
# table(map.2003$value)
# table(map.2015$value)
colourCount = length(unique(map.2015$value))
colorCount<-c("#FFF7EC", 
              "#FDD9A8",
              "#fb6a4a",
              "#de2d26",
              "#a50f15")

map.2015$value_lab<-factor(map.2015$value, labels=c("Low",  "",  "  ", "   ", "High"), ordered=TRUE)

map.2015$id<-as.numeric(map.2015$id)
map.2015<-map.2015[order(-map.2015$id),]

map.2015$year<-"2003-2015"
##########################################################
#Plot
##########################################################
colnames(map.2003)
colnames(map.2015)
map<-rbind(map.2003,map.2015)

map<- map %>% filter(long> -87.79 & long< -87.49,
                               lat > 41.7  & lat< 41.99)

map<-map[is.na(map$value_lab)==F,]
table(map$value_lab,useNA = 'always')

#map
z<-ggmap(chicago.map) +
  geom_polygon(data = map, aes(x = long, y = lat, group = id, fill=value_lab), color = "grey", size = 0.25, alpha=1) +
  scale_fill_manual("Homicide Risk",
                    values = colorCount) +
  facet_wrap(~ year, ncol=2) +
  theme(
    legend.position="bottom",#none
    text = element_text(size=8)) +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        strip.background =element_rect(fill="white"), 
        strip.text = element_text(colour = 'black'))
z
z + coord_fixed(xlim = c(-87.5, -87.8),  ylim = c(41.7, 41.99), ratio = 1.3)
ggsave("views/homicide_risk.png", height = 5, width = 5, units="in")
#dev.off()
