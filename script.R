# [DONE] - Read in CSV of places
# [DONE] - Plot on map, showing name and description
# [DONE] - Plot hotel on map
# [DONE] - Differentiate between places of interest vs food
# [DONE] - Indicate if priority
# [DONE] - Should restrict to city, not just country
# [DONE] - What if there are more than one branch - Should show all branches on map
# [DONE] - Show address extracted from API, to verify
# [DONE] - There seems to be an issue with "&"; should replace "&" with "and"
# - Should flag if there are multiple locations
# - Is there a way to flag if advanced reservations are required

library(googleway)
library(dplyr)
library(leaflet)
library(googlesheets)
options(scipen=999)
options(stringsAsFactors = FALSE)


api_key <- readline(prompt="Paste API key: ") #REMOVE BEFORE MAKING PUBLIC
set_key(api_key)

#Used in getting geocodes
#Used in identifying relevant worksheet in Google Sheet
city <- 'London' 
country <- 'GB' #GB, FR, ES
components <- data.frame(component = c("locality", "administrative_area", "country"), value = c(city, city, country))

# Application ---------------------------------
gs_ls()
ss <- gs_title("Europe Planning")
gs_places <- gs_read(ss=ss, ws = city)
places <- as.data.frame(gs_places)
# places <- read.csv("places.csv", header=TRUE, stringsAsFactors = FALSE)
places$food[is.na(places$food)] <- 0
places$priority[is.na(places$priority)] <- 0
places$hotel[is.na(places$hotel)] <- 0
places$place <- gsub("&", "and", places$place)


#Initialize empty dataframe
geocodes = data.frame(matrix(vector(), 0, 4, dimnames=list(c(), c("place", "lat", "lon", "address"))), stringsAsFactors=FALSE)

for(temp_place in places$place){
  
  result <- google_geocode(address = c(paste(temp_place, city, sep = ", ")), components = components, simplify = TRUE)  
  print(paste("Extracting geocodes of: ", temp_place, " (" ,length(result$results$address_components), " locale)", sep=""))
  
  if(result$status == 'OK'){
    
    #TESTING PURPOSES
    # temp_place = "Portobello Market"
    # result <- google_geocode(address = c(paste(temp_place, city, sep = ", ")), components = components, simplify = TRUE)  
    # print(paste(temp_place, length(result$results$address_components)))
    
    for(i in seq_along(result$results$address_components)){
      temp_lat <- result$results$geometry$location$lat[i]
      temp_lon <- result$results$geometry$location$lng[i]
      temp_address <- result$results$formatted_address[i]
      
      new_row <- list(place=temp_place, lat=temp_lat, lon=temp_lon, address=temp_address)
      geocodes <- rbind (geocodes, new_row)
    }
    
  } else {
    print("Error in result")
    temp_lat <- NULL
    temp_lon <-NULL
    temp_address <- NULL
    
    new_row <- list(place=temp_place, lat=temp_lat, lon=temp_lon, address=temp_address)
    geocodes <- rbind (geocodes, new_row)
  }
  

}


#Left join original places dataframe, with geocodes dataframe
geocodes2 <- merge(x = places, y = geocodes, by = "place", all.x = TRUE)


geocodes2_hotel <- droplevels(filter(geocodes2, hotel == 1))
geocodes2_food <- droplevels(filter(geocodes2, food == 1))
geocodes2_attractions <- droplevels(filter(geocodes2, (food != 1) & (hotel != 1)))





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
  addAwesomeMarkers(lng=geocodes2_attractions$lon, lat=geocodes2_attractions$lat, icon=icons_attractions, label=geocodes2_attractions$place, popup=geocodes2_attractions$comments) %>%
  addAwesomeMarkers(lng=geocodes2_food$lon, lat=geocodes2_food$lat, icon=icons_food, label=geocodes2_food$place, popup=geocodes2_food$comments) %>%
  addAwesomeMarkers(lng=geocodes2_hotel$lon, lat=geocodes2_hotel$lat, icon=icons_hotel, label=geocodes2_hotel$place, popup=geocodes2_hotel$comments)
p  
#Orange -> Food
#Blue -> Attractions
#Star -> Priority Food or Priority Attraction







