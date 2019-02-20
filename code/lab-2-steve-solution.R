library(sf)
library(tidyverse)
library(ggspatial)
library(maps)
library(purrr)

p <- ggplot() +
  geom_sf(data = read_sf("data/ME-GIS-master/Coastline2.shp"), 
          colour="grey10", fill="grey90") +
  geom_sf(data = read_sf("data/ME-GIS-master/Rivers19.shp"), 
          colour="steelblue", size=0.3) +
  geom_sf(data = read_sf("data/ME-GIS-master/PrimaryRoads.shp"), 
          size = 0.7, colour="grey30") +
  geom_sf(data = read_sf("data/ME-GIS-master/Cities.shp")) +
  theme_bw()
p

#add labels and city names
p + geom_sf_text(aes(label=Name),nudge_x=.5,nudge_y=-1,data = read_sf("data/ME-GIS-master/Cities.shp")) +
  annotation_scale() + annotation_north_arrow()

#########################
states <- map_data("state")
head(states)
kansas <- states %>% 
  filter(region=="kansas") %>%
  group_by(group) %>% 
  summarize(
    region = region[1],
    n = n())
kansas
ggplot(states, aes(x=long, y=lat, group=group)) + geom_polygon()

ozbig <- read_sf("data/gadm36_AUS_1.shp")
oz_st <- maptools::thinnedSpatialPoly(
  as(ozbig, "Spatial"), tolerance = 0.1, 
  minarea = 0.001, topologyPreserve = TRUE)
oz <- st_as_sf(oz_st)
oz
is.list(oz$geometry)
str(oz$geometry[[1]])
head(oz$geometry[[1]][[3]][[1]])
head(oz$geometry[[1]])

#three helper functions
ptdf <- function(poly){
  poly <- data.frame(poly);
  dfout <- data.frame(long=poly[,1],lat=poly[,2], order=seq(1:nrow(poly)))
  return(dfout)
}

unngroup <- function(polylist){
polylength <- length(polylist)
pdf <- data.frame(slist=seq(1:polylength)) %>% mutate(df= purrr::map(polylist, ptdf)) %>% unnest()
return(pdf)
}

poly_to_df<- function(p, poly, group){
  group = p %>% length %>% purrr::map(seq, from=1) %>% unlist
  polydf <- tibble(group=group) %>% mutate(polyg = p %>% purrr::map(unngroup))
  return(polydf)
}

ungroup_poly<-function(geomdataset){
  totallength <- geomdataset %>% length
  outdf <- tibble(grp = seq(1:totallength))
  outdf <- outdf %>% mutate(plist = (geomdataset%>% purrr::map(poly_to_df) %>% purrr::map(unnest))) #%>%
    #mutate(group=paste(grp,',',slist)) %>% select(-slist)
  return(outdf)
}

lapply(oz$geometry, function(x) lapply(x,length)) %>% unlist

# map of australia
ozplus <- oz$geometry %>% ungroup_poly() %>% unnest() %>% mutate(group= paste(grp,'.',group,'.',slist))
ozplus %>% ggplot(aes(x = long, y = lat, group = group, fill=as.factor(grp))) + geom_polygon()
                 
#test on mexico shape file    
mxbig <- read_sf("data/gadm36_MEX_1.shp")
mx_st <- maptools::thinnedSpatialPoly(
  as(mxbig, "Spatial"), tolerance = 0.1, 
  minarea = 0.001, topologyPreserve = TRUE)
mx <- st_as_sf(mx_st)

mxplus <- mx$geometry %>% ungroup_poly() %>% unnest() %>% mutate(group= paste(grp,'.',group,'.',slist))
mxplus %>% ggplot(aes(x = long, y = lat, group = group,fill=as.factor(grp))) + geom_polygon()
