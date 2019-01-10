require(rgdal)
require(ggplot2)
require(ggmap)
require(plyr)
require(viridis)
require(maptools)
library(raster)
library(rgeos)
require(sp)
require(cowplot)
dem <- raster("C:/Users/TMoore24/Documents/Erin/dem2.tif")
#dem <- raster("C:/Users/TMoore24/Documents/ArcGIS/dem90_hf_Clip1.tif")
#dem<-projectRaster(dem,crs="+proj=longlat +datum=WGS84")
map.p <- rasterToPoints(dem)
df <- data.frame(map.p)
colnames(df) <- c('Longitude', 'Latitude', 'MAP')
#st<-geom_polygon(data=state, aes(x=long, y=lat), size=1.2)

geo<-read.csv("C:/Users/TMoore24/Documents/Erin/GeoChem_1980sub1.csv")
geo[18:128]<-lapply(geo[18:128], function(x) as.numeric(as.character(x)))
geo$Tree<-geo[,38]+geo[,39]+geo[,40]+geo[,41]+geo[,42]+geo[,43]+geo[,44]+geo[,45]+geo[,46]+geo[,47]+geo[,48]+geo[,49]+geo[,50]+geo[,36]         
geo$TreeY<-geo[,38]+geo[,39]+geo[,40]+geo[,41]+geo[,42]+geo[,43]+geo[,44]+geo[,45]+geo[,46]+geo[,47]+geo[,48]+geo[,49]+geo[,50]+geo[,36]+geo[,99]
geo$CreeY<-geo[,40]+geo[,42]+geo[,44]+geo[,45]+geo[,47]+geo[,99]
geo1<-read.csv("C:/Users/TMoore24/Documents/Erin/CM_Geochem_greater.csv")
geo1[4:70]<-lapply(geo1[4:70], function(x) as.numeric(as.character(x)))
geo1$Tree<-geo1[,33]+geo1[,22]+geo1[,37]+geo1[,36]+geo1[,39]+geo1[,27]+geo1[,29]+geo1[,43]+geo1[,25]+geo1[,32]+geo1[,26]+geo1[,45]+geo1[,50]+geo1[,34]         
geo1$TreeY<-geo1[,33]+geo1[,22]+geo1[,37]+geo1[,36]+geo1[,39]+geo1[,27]+geo1[,29]+geo1[,43]+geo1[,25]+geo1[,32]+geo1[,26]+geo1[,45]+geo1[,50]+geo1[,34]+geo1[,49]
geo1$CreeY<-geo1[,36]+geo1[,27]+geo1[,43]+geo1[,25]+geo1[,26]+geo1[,49]
###############Make dataframes for figure
geoa<-geo[,c(2,7:8,66:67,51,64,103,71,110,30,123,60,95,62,129,130,131)]
geo1a<-geo1[,c(1:3,63,67,65,23,59,28,55,13,36,69,46,47,71,72,73)]
###prepare each row prior to loop 8 is te

i=18
for (i in 6:ncol(geoa[,4:18])){
co<-subset(geoa,!is.na(geoa[,i]))
co1<-subset(geo1a,!is.na(geo1a[,i]))
q<-quantile(co[i], .99, na.rm=TRUE)
co[,i]<-ifelse(co[,i]>q,q,co[,i])
co1[,i]<-ifelse(co1[,i]>q,q,co1[,i])
co$orderrank <- rank(co[,i],ties.method="first")
ID<-names(co)[i]
t<-ggplot(data=df, aes(y=Latitude, x=Longitude)) +
  geom_raster(aes(fill=MAP))+ scale_fill_gradient2(low='gray99',mid='gray52',high='gray19',midpoint=2200,limits=c(740,4000))+ ylab("Latitude")+xlab("Longitude")+theme(axis.title=element_text(size=10), axis.text=element_text(size=8))+ geom_point(data=co,aes(x=LONGITUDE, y=LATITUDE, size=co[,i], color=co[,i]),alpha=.4)+
  geom_point(data=co1,aes(x=Longitude, y=Latitude, size=co1[,i], color=co1[,i]),alpha=.9,shape=17)+
  scale_color_viridis(name='ppm')+
  theme(axis.line = element_line(colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        plot.background = element_rect(colour = "black"))+
  scale_size(breaks=c(500,400,300,200,100),labels=c('','','','',''))+scale_shape(solid = FALSE)+coord_quickmap()+ guides(fill=FALSE)
ggsave(filename=paste("C:/Users/TMoore24/Documents/Erin/CM_",ID,".pdf",sep=""),t)

}
