# - Read in CSV of places
# - Plot on map, showing name and description
# - Plot hotel on map
# - Differentiate between places of interest vs food
# - Indicate if priority


library(googleway)
library(dplyr)
library(leaflet)
library(googlesheets)
options(scipen=999)
options(stringsAsFactors = FALSE)



city <- 'London' #Used in getting geocodes, and identifying relevant worksheet in Google Sheet

gs_ls()
ss <- gs_title("Europe Planning")
gs_places <- gs_read(ss=ss, ws = city)
places <- as.data.frame(gs_places)
# places <- read.csv("places.csv", header=TRUE, stringsAsFactors = FALSE)
places[is.na(places)] <- 0


api_key = 'AIzaSyDaQaN_XfG1XiZ8IcEiex3ElHMa2SaNbDg' #REMOVE BEFORE MAKING PUBLIC
set_key(api_key)


#Initialize empty dataframe
geocodes = data.frame(matrix(vector(), 0, 3, dimnames=list(c(), c("place", "lat", "lon"))), stringsAsFactors=FALSE)

for(temp_place in places$place){
  result <- google_geocode(address = c(paste(temp_place, city, sep = ", ")), simplify = TRUE)    
  if(result$status == 'OK'){
    temp_lat <- result$results$geometry$location$lat[1]
    temp_lon <- result$results$geometry$location$lng[1]
  } else {
    print("Error in result")
    temp_lat <- NULL
    temp_lon <-NULL
  }
  
  new_row <- list(place=temp_place, lat=temp_lat, lon=temp_lon)
  geocodes <- rbind (geocodes, new_row)
}


#Left join original places dataframe, with geocodes dataframe
geocodes2 <- merge(x = places, y = geocodes, by = "place", all.x = TRUE)


geocodes2_hotel <- droplevels(filter(geocodes2, hotel == 1))
geocodes2_food <- droplevels(filter(geocodes2, food == 1))
geocodes2_attractions <- droplevels(filter(geocodes2, (food != 1) & (hotel != 1)))


#-----------------------
#Using color to differentiate between attraction/food, and symbol as priority

getSymbol <- function(geocodes2) {
  sapply(geocodes2$priority, function(priority) {
    if(priority == 1) {
      "star"
    }  else {
      ""
    }})
}

icons_attractions <- awesomeIcons(
  icon = getSymbol(geocodes2_attractions),
  markerColor = 'blue'
)

icons_food <- awesomeIcons(
  icon = getSymbol(geocodes2_food),
  markerColor = 'orange'
)

icons_hotel <- awesomeIcons(
  icon = 'home',
  markerColor = 'red'
)


p <- leaflet() %>%
  addTiles() %>%  
  addAwesomeMarkers(lng=geocodes2_attractions$lon, lat=geocodes2_attractions$lat, icon=icons_attractions, label=geocodes2_attractions$place) %>%
  addAwesomeMarkers(lng=geocodes2_food$lon, lat=geocodes2_food$lat, icon=icons_food, label=geocodes2_food$place) %>%
  addAwesomeMarkers(lng=geocodes2_hotel$lon, lat=geocodes2_hotel$lat, icon=icons_hotel, label=geocodes2_hotel$place)
p  
#Orange -> Food
#Blue -> Attractions
#Star -> Priority Food or Priority Attraction




#-----------------------
#Using icons to differentiate between attraction/food, and color as priority

# getColor <- function(geocodes2) {
#   sapply(geocodes2$priority, function(priority) {
#     if(priority == 1) {
#       "orange"
#     }  else {
#       "blue"
#     }})
# }
# 
# icons_attractions <- awesomeIcons(
#   icon = 'bookmark',
#   markerColor = getColor(geocodes2_attractions)
# )
# 
# icons_food <- awesomeIcons(
#   icon = 'glass',
#   markerColor = getColor(geocodes2_food)
# )
# 
# icons_hotel <- awesomeIcons(
#   icon = 'home',
#   markerColor = 'red'
# )
# 
# p <- leaflet() %>%
#   addTiles() %>%  
#   addAwesomeMarkers(lng=geocodes2_attractions$lon, lat=geocodes2_attractions$lat, icon=icons_attractions, label=geocodes2_attractions$place) %>%
#   addAwesomeMarkers(lng=geocodes2_food$lon, lat=geocodes2_food$lat, icon=icons_food, label=geocodes2_food$place) %>%
#   addAwesomeMarkers(lng=geocodes2_hotel$lon, lat=geocodes2_hotel$lat, icon=icons_hotel, label=geocodes2_hotel$place)
# p  






