---
title: "README"
author: "Team 4"
output:
  html_document:
    keep_md: True
---

## Link
[https://github.com/xiyuansun/stat585-lab2](https://github.com/xiyuansun/stat585-lab2)

## Team Members
- Steven Harms
- Yang Qiao
- Xiyuan Sun
- Jing Zhao



## More map elements

```r
library(tidyverse)
library(sf)
library(ggspatial)

p <- ggplot(data = read_sf("data/ME-GIS/Cities.shp")) +
  geom_sf(data = read_sf("data/ME-GIS/Coastline2.shp"),
          colour = "grey10", fill = "grey90") +
  geom_sf(data = read_sf("data/ME-GIS/Rivers19.shp"),
          colour = "steelblue", size = 0.3) +
  geom_sf(data = read_sf("data/ME-GIS/PrimaryRoads.shp"),
          size = 0.7, colour="grey30") +
  geom_sf(color = "red") +
  geom_sf_text(aes(label = Name), size = 4, color = "black", position = "jitter") +  # add labels
  annotation_scale(style = "ticks") +  # add scale
  annotation_north_arrow(which_north = "true", location = "tr", style = north_arrow_nautical()) +  # add North
  labs(x = NULL, y = NULL) +
  theme_bw()
p
```

<img src="README_files/figure-html/unnamed-chunk-1-1.png" style="display: block; margin: auto;" />

## Australia shapefile

```r
# apply to each state/row of geometry feature in the sf dataset
poly2df <- function(feature, unlist = T) {
  if (unlist) feature <- unlist(feature, recursive = F)
  lapply(feature, function(x) {
    mutate(rename_all(data.frame(x), ~ c("long", "lat")), order = row_number())
  }) %>% tibble(polygon = .)
}


# create new dataset reforming geometry to long, lat, order, group
makedata <- function(data_sf, thin = T, unlist = T) {
  if (thin) {
    oz_st <- maptools::thinnedSpatialPoly(as(data_sf, "Spatial"), tolerance = 0.1, minarea = 0.001, topologyPreserve = TRUE)
    oz <- st_as_sf(oz_st)  # install package: rgeos
  } else oz <- data_sf
  
  as_tibble(oz) %>% mutate(new = map(geometry, poly2df, unlist = unlist)) %>% select(-geometry) %>%
    unnest() %>% mutate(group = row_number()) %>% unnest()
}


australia <- read_sf("data/gadm36_AUS_shp/gadm36_AUS_1.shp")
australia_plus <- makedata(australia, thin = T, unlist = T)
australia_plus %>% ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", fill = "white", size = 0.2) +
  labs(x = "Longitude", y = "Latitude", title = "Australia") +
  coord_fixed() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
```

<img src="README_files/figure-html/unnamed-chunk-2-1.png" style="display: block; margin: auto;" />

## Mexico shapefile

```r
mexico <- read_sf("data/gadm36_MEX_shp/gadm36_MEX_1.shp")
mexico_plus <- makedata(mexico, thin = T, unlist = T)
mexico_plus %>% ggplot(aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "black", fill = "white", size = 0.2) +
  labs(x = "Longitude", y = "Latitude", title = "Mexico") +
  coord_fixed() +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
```

<img src="README_files/figure-html/unnamed-chunk-3-1.png" style="display: block; margin: auto;" />


-----------------------------


**In order to do this second part, I used 4 helper functions.**

```r
#four helper functions
#one to format the data frame
#one to un-nest each level of the list (3 total)
#could probably be combined into one if we wanted to
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
```

Using the functions above, we only need one call to get from the shapefile to the dataframe. First, we use the function to get it down to a feasible size.

```r
#thin the data
ozbig <- read_sf("data/gadm36_AUS_shp/gadm36_AUS_1.shp")
oz_st <- maptools::thinnedSpatialPoly(
  as(ozbig, "Spatial"), tolerance = 0.1, 
  minarea = 0.001, topologyPreserve = TRUE)
oz <- st_as_sf(oz_st)

# map of australia
ozplus <- oz$geometry %>% unnest_state()

ozplus %>% ggplot(aes(x = long, y = lat, group = group)) + geom_polygon()
#with color by state
ozplus %>% ggplot(aes(x = long, y = lat, group = group, fill=as.factor(grp))) + geom_polygon()
```

We can try it on another country as well:

```r
#test on mexico shape file    
mxbig <- read_sf("data/gadm36_MEX_1.shp")
mx_st <- maptools::thinnedSpatialPoly(
  as(mxbig, "Spatial"), tolerance = 0.1, 
  minarea = 0.001, topologyPreserve = TRUE)
mx <- st_as_sf(mx_st)

#map of mexico
mxplus <- mx$geometry %>% unnest_state() %>% unnest() %>% mutate(group= paste(grp,'.',group,'.',slist))
mxplus %>% ggplot(aes(x = long, y = lat, group = group)) + geom_polygon()
#with color by state
mxplus %>% ggplot(aes(x = long, y = lat, group = group, fill=as.factor(grp))) + geom_polygon()
```
