library(tidyverse)
library(rvest)
library(XML)
library(RCurl)
library(ggmap)
library(leaflet)

wiki_URL <- "https://en.wikipedia.org/wiki/2022_FIFA_World_Cup_squads" #Storing the URL since it will be called upon multiple times.

#Setting up the xpaths to extract the WC squads
xpaths <- vector(mode = "character", length = 32) #First setting an empty vector that the for loop below will store.

for (i in 1:32) {
  xpaths[i] <- paste0('//*[@id="mw-content-text"]/div/table[', i, ']') #Store xpaths for each of the 32 teams
}

#The following takes the table of contents from the squads page, 
#and turns it into a dataframe with the group each NT belonged to, 
#along with the NT names and the paths to extract the squad. Then,
#it iterates through each country and extracts its squad and stores
#it as a list column.
Countries_df <- read_html(wiki_URL) %>% #Read the HTML page...
  html_node(xpath = '//*[@id="toc"]') %>%  #Extract the node storing the table of contents
  html_text() %>% #Read the table of contents as text
  str_split("\n") %>% #Split the string by the newline code separating the 32 nations
  unlist() %>% #Flatten to a vector
  as_tibble() %>% #Set it to a data frame
  rename(Countries = value) %>% #Change the variable name to "Countries"
  filter(str_detect(Countries, "^[1-8]\\."), !str_detect(Countries, "Group")) %>% #Keep only those rows that list the countries
  separate(Countries, c("Groups", "delete", "Countries"), sep = c(1, 3)) %>% #Split the number indicating the group each NT was in from the country name and store them in separate variables
  mutate(Groups = LETTERS[as.numeric(Groups)], #Change the numbers indicating the group to its corresponding letter
         Countries = str_trim(Countries), #Remove white space around Country name
         xpaths = xpaths, #Store country's xpaths from the earlier for loop
         squads = map(xpaths, function (x) read_html(wiki_URL) %>%  #For each NT, read the page...
                        html_node(xpath = x) %>% #Extract the squad table...
                        html_table())) #And read it as a table

Players_df <- Countries_df %>% 
  unnest(squads) %>%  #Spread out the data frame so that each row is a player.
  filter(Player != "") #Remove blank rows

#Now extracting the players' wiki pages so that I can look up their personal information (place of birth, height, playing position)
player_links <- read_html(wiki_URL) %>% #Read the page
  html_nodes("th > a:nth-child(1)") %>% #Find the node for the list of players
  html_attr("href") #Extract the redirect link for each player
player_links <- player_links[1:nrow(Players_df)] #Not sure where those links at the end came from. *shrug* buh-bye

player_info <- function(x) { #Creating a function grabbing players' info that would be applied to every player.
  player_info <- html_session("https://en.wikipedia.org/") %>% #Go to wikipedia
    jump_to(x) %>% #And go to the player's link x
    html_node('.infobox') %>% #Grab the node's infobox
    html_table(fill = TRUE) #Turn that info into a table
  player_info <- player_info[, 1:2] #Grab just the first couple of columns
  player_info <- player_info %>% 
    janitor::clean_names() %>% 
    as_tibble() %>% #Turn that into a tibble
    filter(.[[1]] %in% c("Place of birth", "Height", "Position(s)", "Full name")) %>% #Select only the rows of info I want
    rename(attribute = 1, info = 2)
  # names(player_info) <- c("attribute", "info") #Change the names of the variables
  return(player_info)
}

# x <- player_links[5]
# player_info(player_links[5])

Players_df <- Players_df %>% #Take the players data frame
  mutate(player_links = player_links, #Store their respective links
         player_data = map(player_links, player_info)) %>% #and apply the function created above to extract info from the player's wikipage
  unnest() %>% #open the player_data. This will repeat the line 3x for each player.
  spread(key = attribute, value = info) #Spread the data so that those 3 pieces of info have their own variable.

#Now grabbing the images in the infobox
player_image <- function (x) { #Creating a function that will...
  html_session("https://en.wikipedia.org/") %>% #Back to Wikipedia we go
    jump_to(x) %>% #And go to the player's link x
    html_node('.image img') %>% #Find the node of the image in the infobox
    html_attr("src") #Grab its URL location
}

final_Players_df <- mutate(Players_df, image = map(player_links, player_image)) #Apply the function created above for each player

#Cleaning up the data 
ex_players_df <- final_Players_df %>% 
  mutate(Captain = if_else(str_detect(Player, "\\(c\\)"), "Captain", ""), #Add a captain variable if "(c)" is in the player's name.
         Player = Player %>% str_remove("\\(c\\)") %>% str_trim(), #Removing the "(c)" from the player's name and any whitespace.
         `Date of birth (age)` = `Date of birth (age)` %>% str_replace("^\\(\\d{4}-\\d{2}-\\d{2}\\)", "") %>% str_trim(), #Removing the "(yyyy-mm-dd)" part in this field
         Player = Player %>% str_remove("\\[[^]]*\\]") %>% str_trim(), #Remove footnotes
         Club = Club %>% str_remove("\\[[^]]*\\]") %>% str_trim(), #Remove footnotes
         Height = Height %>% str_remove("\\[[^]]*\\]") %>% str_trim(), #Remove footnotes
         `Place of birth` = `Place of birth` %>% str_remove("\\[[^]]*\\]") %>% str_trim(), #Remove footnotes
         `Position(s)` = `Position(s)` %>% str_remove("\\[[^]]*\\]") %>% str_trim()) %>% #Remove footnotes
  separate(Club, c("Club", "club_delete"), sep = "\\[", extra = "merge", fill = "right") %>% #Remove footnotes for those that didn't work above for some reason
  separate(Height, c("Height", "height_delete"), sep = "\\[", extra = "merge", fill = "right") %>% #Remove footnotes for those that didn't work above for some reason 
  separate(`Place of birth`, c("Place of birth", "birth_delete"), sep = "\\[", extra = "merge", fill = "right") %>% #Remove footnotes for those that didn't work above for some reason
  separate(`Position(s)`, c("Playing position", "position_delete"), sep = "\\[", extra = "merge", fill = "right") %>% #Remove footnotes for those that didn't work above for some reason
  select(-delete, -xpaths, -Pos., -player_links, -club_delete, -height_delete, -birth_delete, -position_delete) #Remove unneeded columns

#For each birthplace, get long and lat. Beforehand, I need to set up an alternate version of the place of birth 
#variable to remove country names that don't exist anymore. Somehow the geolocation API works better if they only have
#the city name.

library(tidygeocoder)
locations_df <- distinct(ex_players_df_loc, location_lookup) %>% 
  geocode(location_lookup, lat = latitude, long = longitude)


missing_loc <- locations_df %>% 
  filter(is.na(latitude)) %>% 
  select(-c(latitude, longitude)) %>% 
  separate(location_lookup, sep = ", ", into = c("city", "state", "country"), remove = FALSE) %>% 
  mutate(country = case_when(str_detect(state, "SR|Saudi|Ghana|Tunisia") ~ state,
                             TRUE ~ country),
         state = case_when(str_detect(state, "SR|Saudi|Ghana|Tunisia") ~ NA_character_,
                             TRUE ~ state)) %>% 
  geocode(city = city, lat = latitude, long = longitude)

still_missing <- tribble(
  ~city, ~latitude, ~longitude,
  "Darreh-ye Badam", 33.600556, 48.014722,
  "Ta'if", 21.275094, 40.406156,
  "Aryanah", 36.8625, 10.195556
)

final_missing <- left_join(missing_loc, still_missing, by = "city") %>% 
  mutate(latitude = coalesce(latitude.x, latitude.y),
         longitude = coalesce(longitude.x, longitude.y)) %>% 
  select(location_lookup, city, country, latitude, longitude)

final_locations <- locations_df %>% 
  left_join(final_missing, by = "location_lookup") %>% 
  mutate(latitude = coalesce(latitude.x, latitude.y),
         longitude = coalesce(longitude.x, longitude.y)) %>% 
  select(location_lookup, latitude, longitude)

# rio::export(here::here("locations.rds"), x = final_locations)

library(countrycode)

#Add a very slight random shock to the latitude and longitude coordinates so that the markers don't end up on top of each other.
final_export <- ex_players_df_loc %>% 
  left_join(final_locations) %>% 
  mutate(lat = jitter(latitude, amount = 0.02),
         lon = jitter(longitude, amount = 0.02),
         links = paste0("https://en.wikipedia.org", player_links),
         popup_text = paste0("<center>", #Setting up poopup info
                            ifelse(!is.na(image), paste0("<img src = https:", image, " width='100'>"), ""),
                            "</br><b>", Player, "</b>",
                            "</br><b>Date of birth</b>: ", `Date of birth (age)`, 
                            "</br><b>Place of birth</b>: ", `Place of birth`,
                            "</br><b>Playing position</b>: ", `Playing position`,
                            "</br><b>Club</b>: ", Club, 
                            "</br><a href='", links, "' target='_blank'>More info...</a></center>")) %>% 
  mutate(iso2 = countrycode(Countries, origin = "country.name", destination = "iso2c"),
         flag = countrycode(Countries, "country.name", "unicode.symbol"),
         flag = case_when(Countries == "England" ~ "\u1f981",
                          Countries == "Wales" ~ "\u1f432",
                          TRUE ~ flag))



# Players_df <- Players_df %>% 
#   left_join(locations_df) %>% 
#   mutate(lat_final = jitter(lat, amount = 0.02),
#          lon_final = jitter(lon, amount = 0.02))

# Players_df <- mutate(Players_df,
#                      links = paste0("https://en.wikipedia.org", player_links), #Bringing in player links
#                      popup_text = paste0("<center>", #Setting up poopup info
#                        ifelse(!is.na(image), paste0("<img src = https:", image, " width='100'>"), ""),
#                        "</br><b>", Player, "</b>",
#                        "</br><b>Date of birth</b>: ", Date.of.birth..age., 
#                        "</br><b>Place of birth</b>: ", Place.of.birth,
#                        "</br><b>Playing position</b>: ", Playing.position,
#                        "</br><b>Club</b>: ", Club, 
#                        "</br><a href='", links, "' target='_blank'>More info...</a></center>"))

#Saving for the app
write_rds(final_export, "Players_df.rds") 
write_rds(Countries_df, "Countries_df.rds") 





#Setting up icons - Flags taken from https://www.iconfinder.com

# Flags 
#####

iconWidth <- 25
iconHeight <- 25
shadowWidth <- 25
shadowHeight <- 25

flagIcon_cs <- leaflet::iconList(
  "Argentina" = makeIcon(
    iconUrl = "Country_flags/Argentina.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Australia" = makeIcon(
    iconUrl = "Country_flags/Australia.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Belgium" = makeIcon(
    iconUrl = "Country_flags/Belgium.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Brazil" = makeIcon(
    iconUrl = "Country_flags/Brazil.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Cameroon" = makeIcon(
    iconUrl = "Country_flags/Cameroon.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Canada" = makeIcon(
    iconUrl = "Country_flags/Canada.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Costa Rica" = makeIcon(
    iconUrl = "Country_flags/Costa_Rica.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Croatia" = makeIcon(
    iconUrl = "Country_flags/Croatia.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Denmark" = makeIcon(
    iconUrl = "Country_flags/Denmark.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Ecuador" = makeIcon(
    iconUrl = "Country_flags/Ecuador.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "England" = makeIcon(
    iconUrl = "Country_flags/England.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "France" = makeIcon(
    iconUrl = "Country_flags/France.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Germany" = makeIcon(
    iconUrl = "Country_flags/Germany.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Ghana" = makeIcon(
    iconUrl = "Country_flags/Ghana.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Iran" = makeIcon(
    iconUrl = "Country_flags/Iran.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Japan" = makeIcon(
    iconUrl = "Country_flags/Japan.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Mexico" = makeIcon(
    iconUrl = "Country_flags/Mexico.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Morocco" = makeIcon(
    iconUrl = "Country_flags/Morocco.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Netherlands" = makeIcon(
    iconUrl = "Country_flags/Netherlands.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Poland" = makeIcon(
    iconUrl = "Country_flags/Poland.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Portugal" = makeIcon(
    iconUrl = "Country_flags/Portugal.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Qatar" = makeIcon(
    iconUrl = "Country_flags/Qatar.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Saudi Arabia" = makeIcon(
    iconUrl = "Country_flags/Saudi_Arabia.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Senegal" = makeIcon(
    iconUrl = "Country_flags/Senegal.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Serbia" = makeIcon(
    iconUrl = "Country_flags/Serbia.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "South Korea" = makeIcon(
    iconUrl = "Country_flags/South_Korea.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Spain" = makeIcon(
    iconUrl = "Country_flags/Spain.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Switzerland" = makeIcon(
    iconUrl = "Country_flags/Switzerland.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Tunisia" = makeIcon(
    iconUrl = "Country_flags/Tunisia.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "United States" = makeIcon(
    iconUrl = "Country_flags/United_States.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Uruguay" = makeIcon(
    iconUrl = "Country_flags/Uruguay.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  ),
  "Wales" = makeIcon(
    iconUrl = "Country_flags/Wales.png",
    iconWidth = iconWidth, iconHeight = iconHeight,
    shadowWidth = shadowWidth, shadowHeight = shadowHeight
  )
)

#####

#Map test
leaflet(final_export) %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addMarkers(~lon, ~lat, 
             icon = ~ flagIcon_cs[Countries],
             label = ~ Player, 
             labelOptions = labelOptions(textsize = "12px"),
             popup = ~popup_text)
