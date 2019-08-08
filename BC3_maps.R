#BC3 make map of sites
install.packages("rgdal")
install.packages("ggsn")
library(rgdal)
library(ggplot2)

#followed this tutorial: http://zevross.com/blog/2014/07/16/mapping-in-r-using-the-ggplot2-package/

#read in GPS data

BC3_GPS <- read.csv(file = "Map/BC3_GPS.csv")

#rename "Terrebone" to "Terrebonne" in transect factor
levels(BC3_GPS$Transect)[levels(BC3_GPS$Transect)=="Terrebone"] <- "Terrebonne"

#read in USA shapefile
states.shp <- readOGR("Map/S6_ArcGIS/states.shp")

proj4string(states.shp)

# Next the shapefile has to be converted to a dataframe for use in ggplot2
states.shp_df <- fortify(states.shp)

# does our data have a projection/coordinate system assigned?
proj4string(BC3_GPS) # of course not, but since it came from a garmin GPS I'm going to assume its NAD83

#make it spatial points
coordinates(BC3_GPS) <- ~ Long + Lat

# we know that the coordinate system is NAD83 so we can manually
# tell R what the coordinate system is
proj4string(BC3_GPS)<-CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")

# now we can use the spTransform function to project. We will project
# the mapdata and for coordinate reference system (CRS) we will
# assign the projection from counties

BC3_GPS <- spTransform(BC3_GPS, CRS(proj4string(states.shp)))

# double check that they match
identical(proj4string(BC3_GPS),proj4string(states.shp))

# ggplot can't deal with a SpatialPointsDataFrame so we can convert back to a data.frame
BC3_GPS_df <- data.frame(BC3_GPS)

#subset for LA

#subset to states and make into data frame
LA.shp <- fortify(states.shp[states.shp$STATE_NAME=="Louisiana",])
BC3_GPS_df.LA <- BC3_GPS_df[BC3_GPS_df$ï..Site=="LA",]
GA.shp <- fortify(states.shp[states.shp$STATE_NAME=="Georgia",])
BC3_GPS_df.GA <- BC3_GPS_df[BC3_GPS_df$ï..Site=="GA",]
SC.shp <- fortify(states.shp[states.shp$STATE_NAME=="South Carolina",])
BC3_GPS_df.SC <- BC3_GPS_df[BC3_GPS_df$ï..Site=="SC",]

map <- ggplot() +
  geom_polygon(data = LA.shp, 
               aes(x = long, y = lat, group = group),
               color = "black", fill = "white", size = .2)

map.styled.LA <- map +
  geom_point(data=BC3_GPS_df.LA[c(1,4),], aes(x=Long, y=Lat), size = rel(6)) +
  theme_void() +
  #scale_color_manual(values = c("grey", "black")) +
  theme(legend.position = "right") +
  geom_text(data = BC3_GPS_df.LA[c(1,4),], aes(x=Long, y=Lat, label = c("Terrebonne", "Barataria")), hjust = 1, nudge_x = -0.1)

#panel.border = element_rect(colour = "gray", fill=NA, size=rel(1)), 

#Add north arrow and scale bar

library(ggsn)

LA.done <- map.styled.LA +
  north(LA.shp, location = "topright") +
  scalebar(LA.shp, dist = 50, dd2km = TRUE, model = 'WGS84', location = "bottomleft", st.size = 2.5 )   ; LA.done

ggsave("Map/LA_dots.png", width = 6, height = 6, units = "in")

#GA
map <- ggplot() +
  geom_polygon(data = GA.shp, 
               aes(x = long, y = lat, group = group),
               color = "black", fill = "white", size = .2)

map.styled.GA <- map +
  geom_polygon(data=BC3_GPS_df.GA, aes(x=Long, y=Lat, color = Transect), size = rel(4)) +
  theme_void() +
  scale_color_manual(values = c("grey", "black")) +
  theme(legend.position = "right")

#Add north arrow and scale bar

GA.done <- map.styled.GA +
  north(GA.shp, location = "topright") +
  scalebar(GA.shp, dist = 50, dd2km = TRUE, model = 'WGS84', location = "bottomleft", st.size = 2.5 )   ; GA.done

#SC

map <- ggplot() +
  geom_polygon(data = SC.shp, 
               aes(x = long, y = lat, group = group),
               color = "black", fill = "white", size = .2)

map.styled.SC <- map +
  geom_polygon(data=BC3_GPS_df.SC, aes(x=Long, y=Lat, color = Transect), size = rel(4)) +
  theme_void() +
  scale_color_manual(values = c("grey", "black")) +
  theme(legend.position = "right")

#Add north arrow and scale bar

SC.done <- map.styled.SC +
  north(SC.shp, location = "topright") +
  scalebar(SC.shp, dist = 50, dd2km = TRUE, model = 'WGS84', location = "bottomleft", st.size = 2.5 )   ; SC.done


#make southwest shape
install.packages("rgeos")
library(rgeos)


states.shp <- readOGR("Map/S6_ArcGIS/states.shp")

SW.states.shp <- states.shp[states.shp$STATE_NAME %in% c("Louisiana", "Alabama", "Mississippi", "Florida", "Georgia", "South Carolina"),]

SW.states.union <- gUnaryUnion(SW.states.shp)

SW.states_union_df <- fortify(SW.states.union)
SW.states_df <- fortify(SW.states.shp)

#SW Union

map <- ggplot() +
  geom_polygon(data = SW.states_union_df,
               aes(x = long, y = lat, group = group),
               color = "black", fill = "white", size = .2)

map.styled.SW_union <- map +
  geom_polygon(data=BC3_GPS_df, aes(x=Long, y=Lat, color = Transect), size = rel(4)) +
  theme_void() +
  #scale_color_manual(values = c("grey", "black")) +
  theme(legend.position = "right")

#Add north arrow and scale bar

SW_union.done <- map.styled.SW_union +
  north(SW.states_union_df, location = "topleft") +
  scalebar(SW.states_union_df, dist = 100, dd2km = TRUE, model = 'WGS84', location = "bottomleft", st.size = 2.5 )   ; SW_union.done

#ggsave("Map/SW_union.png", width = 8, height = 6, units = "in")

#SW states

map <- ggplot() +
  geom_polygon(data = SW.states_df,
               aes(x = long, y = lat, group = group),
               color = "black", fill = "white", size = .2)

map.styled.SW <- map +
  geom_polygon(data=BC3_GPS_df, aes(x=Long, y=Lat, color = Transect), size = rel(4)) +
  theme_void() +
  #scale_color_manual(values = c("grey", "black")) +
  theme(legend.position = "right")

#Add north arrow and scale bar

SW_union.done <- map.styled.SW +
  north(SW.states_df, location = "topleft") +
  scalebar(SW.states_df, dist = 100, dd2km = TRUE, model = 'WGS84', location = "bottomleft", st.size = 2.5 )   ; SW_union.done

#ggsave("Map/SW_states.png", width = 8, height = 6, units = "in")

#black and white version
map.styled.SW <- map +
  geom_point(data=BC3_GPS_df[c(1,4,6,10),], aes(x=Long, y=Lat), shape=15, size = rel(4)) +
  theme_void() +
  #scale_color_manual(values = c("grey", "black")) +
  theme(legend.position = "none")


SW_union.done <- map.styled.SW +
  north(SW.states_df, location = "topleft") +
  scalebar(SW.states_df, dist = 100, dd2km = TRUE, model = 'WGS84', location = "bottomleft", st.size = 2.5 ) +
  geom_text(data = BC3_GPS_df[c(1,6,10),], aes(x=Long, y=Lat, label = c("Terrebonne", "Savannah" , "Waccamaw")), vjust = 0, hjust = 1, nudge_x = -0.15) +
  geom_text(data = BC3_GPS_df[c(4),], aes(x=Long, y=Lat, label = c("Barataria")), vjust = 1, hjust = 1, nudge_x = -0.15) ; SW_union.done

ggsave("Map/SW_states_BW.png", width = 8, height = 6, units = "in")

#ggsave("Map/SW_states_BW.svg", width = 8, height = 6, units = "in")

install.packages("svglite")
