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

#four helper functions
#one to format the data frame
#one to un-nest each level of the list (3 total)
ptdf <- function(poly){
  #take the data frame
  #format it into latitude, longitude, point order
  poly <- data.frame(poly);
  dfout <- data.frame(long=poly[,1],lat=poly[,2], order=seq(1:nrow(poly)))
  return(dfout)
}

unnest_lowest <- function(polylist){
  #unnest the lowest level list of each polygon
  #most are just lists with length 1, some have 2
  polylength <- length(polylist)
  #slist is an index of the dataframe in the list
  #map the dataframe function for each element of the list, unnest into a dataframe
  pdf <- data.frame(slist=seq(1:polylength)) %>%
    mutate(df= purrr::map(polylist, ptdf)) %>%
    unnest()
return(pdf)
}

unnest_division<- function(p, poly, group){
  #for each division in each state, get the polygons in dataframe above
  #first, generate a sequence of indices
  group = p %>% length %>%
    purrr::map(seq, from=1) %>%
    unlist
  #next, get the list of dataframes from above for each division
  polydf <- tibble(group=group) %>%
    mutate(polyg = p %>% purrr::map(unnest_lowest))
  return(polydf)
}

unnest_state<-function(geomdataset){
  #for each state, get the list of dataframes and unnest
  #first, generate a sequence of state-level indices
  totallength <- geomdataset %>% length
  outdf <- tibble(grp = seq(1:totallength))
  #get the list of polygon dataframes and then unnest each division within the state
  outdf <- outdf %>% mutate(plist = (geomdataset %>% purrr::map(unnest_division) %>% purrr::map(unnest)))
  #now, unnest the dataframe within each state
  #then get a unique group index for each polygon by combining each lower level index
  outdf <- outdf %>% unnest() %>%
    mutate(group= paste(grp,'.',group,'.',slist)) 
  return(outdf)
}


# map of australia
ozplus <- oz$geometry %>% unnest_state 
ozplus %>% ggplot(aes(x = long, y = lat, group = group)) + geom_polygon()
#with color by state
ozplus %>% ggplot(aes(x = long, y = lat, group = group, fill=as.factor(grp))) + geom_polygon()
                 
#test on mexico shape file    
mxbig <- read_sf("data/gadm36_MEX_1.shp")
mx_st <- maptools::thinnedSpatialPoly(
  as(mxbig, "Spatial"), tolerance = 0.1, 
  minarea = 0.001, topologyPreserve = TRUE)
mx <- st_as_sf(mx_st)

#plot of mexico
mxplus <- mx$geometry %>% unnest_state() %>% unnest() %>% mutate(group= paste(grp,'.',group,'.',slist))
mxplus %>% ggplot(aes(x = long, y = lat, group = group)) + geom_polygon()
