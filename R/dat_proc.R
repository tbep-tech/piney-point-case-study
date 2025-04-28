# setup ---------------------------------------------------------------------------------------

library(gmailr)
library(tidyverse)
library(sf)
library(here)

# get emails ----------------------------------------------------------------------------------

gm_auth_configure()
gm_oauth_client()

search_query <- "subject:\"Piney Point Update -\""

# Initialize variables for pagination
all_message_ids <- c()
next_page <- NULL
more_messages <- TRUE

# Loop to get all pages of results
while(more_messages) {
  # Get a page of messages using the page token if available
  emails <- gm_messages(search = search_query, page_token = next_page)
  
  # Add these message IDs to our collection
  if(length(gm_id(emails)) > 0) {
    all_message_ids <- c(all_message_ids, gm_id(emails))
    
    next_page <- emails[1][[1]]$nextPageToken
    
    # Check if there are more pages
    if(!is.null(next_page)) {
      cat("Retrieved", length(all_message_ids), "emails so far, fetching more...\n")
    } else {
      more_messages <- FALSE
      cat("Retrieved all matching emails: total count =", length(all_message_ids), "\n")
    }
  } else {
    more_messages <- FALSE
    cat("No more messages found.\n")
  }
}

# Function to extract body text from an email
extract_email_body <- function(message_id) {
  # Get the full message
  message <- gm_message(message_id)
  
  # Extract the body
  body_text <- gm_body(message)
  
  # Get the subject for reference
  subject <- gm_subject(message)
  
  # Return as a list
  return(list(
    subject = subject,
    body = body_text,
    date = gm_date(message)
  ))
}

# Apply the function to each email found
email_bodies <- lapply(all_message_ids, extract_email_body)

save(email_bodies, file = here::here("data/email_bodies.RData"))

# three bay counties --------------------------------------------------------------------------

counties <- st_read('T:/05_GIS/BOUNDARIES/CitiesCountiesTBEP/CountyFDEP.shp') |> 
  filter(NAME %in% c('HILLSBOROUGH', 'PINELLAS', 'MANATEE')) |> 
  mutate(
    County = case_when(
      NAME == 'PINELLAS' ~ 'Pinellas', 
      NAME == 'HILLSBOROUGH' ~ 'Hillsborough', 
      NAME == 'MANATEE' ~ 'Manatee'
    )
  ) |>
  select(-NAME) |> 
  st_transform(crs = 4326) |> 
  st_make_valid()

save(counties, file = here('data/counties.RData'))

# fish kill data ------------------------------------------------------------------------------

load(file = here('data/counties.RData'))

# via email 4/25/25
fishdat <- read.csv(here('data-raw/MarcusBeck_TBEP_1995-2025.csv')) |> 
  select(
    date = DateOfCall, 
    lat = LatDD, 
    lon = LonDD
  ) |> 
  mutate(
    date = ymd(date), 
  ) |> 
  filter(!(is.na(lat) | is.na(lon))) |> 
  filter(year(date) < 2025) |> 
  st_as_sf(
    coords = c('lon', 'lat'), 
    crs = 4326
  ) |> 
  st_join(counties, join = st_nearest_feature)

# # verify join
# library(leaflet)
# pal <- leaflet::colorFactor(c("red", "blue", "green"), domain = unique(tmp$County))
# leaflet() %>%
#   addTiles(group = 'OSM', url = 'https://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}{r}.png') %>%
#   addPolygons(data = counties, fill = NA, color = 'black', weight = 2, label = ~County) %>%
#   addCircleMarkers(data = fishdat, radius = 4, stroke = F, fillColor = pal(fishdat$County), fillOpacity = 0.5, 
#                    group = 'County', label = ~County) %>%
#   addLayersControl(
#     baseGroups = c('OSM'), 
#     overlayGroups = c('County'), 
#     options = layersControlOptions(collapsed = FALSE)
#   ) %>%
#   setView(lng = -82.5, lat = 27.5, zoom = 9)

fishdat <- fishdat |> 
  st_set_geometry(NULL) |> 
  mutate(
    week = floor_date(date, unit = 'week')
  ) |> 
  summarise(
    cnt = n(), 
    .by = c(week, County)
  )

save(fishdat, file = here('data/fishdat.RData'))
