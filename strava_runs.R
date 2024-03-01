library(tidyverse)
library(leaflet)
library(rStrava)
library(sf)
library(stringr)
library(httr)
library(rvest)

locations_link <- "https://www.bklynlibrary.org/browse-the-branches/about"
library_locations <- read_html(locations_link) %>% 
  html_nodes("table") %>% 
  html_nodes("p[class='lead']") %>% 
  html_text() %>% 
  str_replace_all(., "\\*", "")

images <- read_html(locations_link)  %>% 
  html_nodes("table") %>% 
  html_nodes("img") %>% 
  html_attr("src")

locations_df <- tibble(
  branch = library_locations,
  sticker = images
)

app_name <- Sys.getenv("STRAVA_APP_NAME")
client_id <- Sys.getenv("STRAVA_CLIENT_ID")
client_secret <- Sys.getenv("STRAVA_CLIENT_SECRET")

branches_browsed <- c("Clinton Hill", "Marcy", "Macon", "Eastern Parkway", "Bedford",
                      "McKinley Park", "Dyker", "New Utrecht", "Highlawn",
                      "Sunset Park", "Bay Ridge", "Fort Hamilton",
                      "Windsor Terrace", "Borough Park", "Kensington", "Midwood", "Ryder", "Mapleton",
                      "Brownsville", "Stone Avenue", "East Flatbush", "Rugby", "Flatbush", "Clarendon",
                      "Brower Park", "Crown Heights", "Cortelyou",
                      "Coney Island", "Brighton Beach", "Sheepshead Bay",
                      "Williamsburgh", "Leonard", "Greenpoint",
                      "Cypress Hills", "New Lots", "Spring Creek", "Jamaica Bay", "Canarsie"
                      #"Adams Street", "Center for Brooklyn History", "Brooklyn Heights", "Bookmobile",
                      #"Central", "Pacific", "Walt Whitman", "Park Slope", "Red Hook", "Carroll Gardens"
                      )

library_coords <- read_csv("~/Downloads/brooklyn public library- Untitled layer.csv") %>% 
  filter(str_detect(WKT, "POINT")) %>% 
  tidyr::separate(WKT, into = c("point", "coords", "space"), sep = "\\(|\\)") %>% 
  tidyr::separate(coords, into = c("lon", "lat"), sep = " ") %>% 
  mutate_at(vars(lat, lon), as.numeric) %>% 
  dplyr::select(name, lat, lon) %>% 
  mutate(name = str_replace(name, " Branch", ""),
         name = str_replace(name, " & Learning Center", ""),
         name = ifelse(name == "Brooklyn Historical Society", "Center for Brooklyn History", name)) %>% 
  full_join(locations_df, by = c("name" = "branch")) %>% 
  filter(!is.na(lat)) %>% 
  mutate(browsed = ifelse(name %in% paste0(branches_browsed, " Library") 
                          ##| name %in% c("Center for Brooklyn History", "Bookmobile")
                          , T, F)) %>% 
  filter(browsed)

gp2sf <- function(gp) {
  gp %>%  
    googlePolylines::decode() %>%  
    map_dfr(
      function(df) {
        df %>%  
          st_as_sf(coords = c("lon", "lat")) %>%  
          st_combine() %>%  
          st_cast("LINESTRING") %>%  
          st_sf() 
      }) %>%  
    pull(1)
}

# Get activities
activities <- httr::config(
  token = strava_oauth(
    app_name,
    client_id,
    client_secret,
    app_scope = "activity:read_all",
    cache = TRUE)) %>%  
  get_activity_list() %>%  
  compile_activities() 
  
# activities %>% 
#   filter(str_detect(name, "Browse the Branches")) %>% 
#   write_csv(., file = "strava_runs.csv")
# library_coords %>% 
#   write_csv(., "library_coords.csv")

# cols <- c("#12b259", "#3c5daa", "#fff45f", "#e00079", "#f26648", "#00b3d8")
cols <- "#3c5daa"
icons <- map(library_coords$sticker, ~makeIcon(iconUrl = .x, iconWidth = 20, iconHeight = 20)) %>% 
  set_names(library_coords$name) %>% 
  structure(., class = "leaflet_icon_set")

# Map 
p <- activities %>%  
  filter(str_detect(name, "Browse the Branches")) %>% 
  mutate(geom = gp2sf(map.summary_polyline)) %>%  
  st_sf(crs = "EPSG:4326") %>%  
  leaflet() %>%  
  addTiles() %>%  
  addPolylines(color = cols, opacity = 0.8, weight = 4) %>% 
  addProviderTiles("Esri.WorldGrayCanvas") %>% 
  addMarkers(lat = library_coords$lat, lng = library_coords$lon,
             icon = icons[library_coords$name])
               #paste0("<img src = ", library_coords$sticker, ">"))


mapshot(p, file = "strava_runs.png", vwidth = 650, vheight = 650, zoom = 5)

