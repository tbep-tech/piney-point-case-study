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
emailbody <- lapply(all_message_ids, extract_email_body)

save(emailbody, file = here::here("data/emailbody.RData"))

# parse email ---------------------------------------------------------------------------------

load(file = here::here('data/emailbody.RData'))

email_df <- data.frame(
  subject = sapply(emailbody, function(x) x$subject),
  date = sapply(emailbody, function(x) x$date),
  body = sapply(emailbody, function(x) ifelse(length(x$body) == 0, NA, x$body[[1]])),
  stringsAsFactors = FALSE
)

gallons_pattern <- "^.*Approximately(.*)are currently held within the NGS-South compartment.*$"
capacity_pattern <- "^.*The current storage capacity for additional rainfall at the site is approximately(.*) This capacity.*$"
transfer_pattern <- "^.*to date(.*)have been transferred.*$"
inject_pattern <- "[^.!?]*\\bUIC\\b[^.!?]*[.!?]"

emailproc <- email_df |> 
  dplyr::filter(grepl('^Piney Point Update –.*', subject)) |> 
  dplyr::filter(!is.na(body)) |> 
  mutate(
    instorcur = gsub(capacity_pattern, '\\1', body),
    mgallcur = gsub(gallons_pattern, '\\1', body),
    mgallcur = gsub('\\D+', '', mgallcur), 
    # trancur = gsub(transfer_pattern, '\\1', body), 
    # injccur = regexpr(inject_pattern, body, perl = TRUE),
    # injccur = ifelse(injccur != -1, trimws(regmatches(body, injccur)[[1]]), NA)
    .by = c('subject', 'date', 'body')
  ) |> 
  mutate(
    mgallcur = as.numeric(gsub('\\D+', '', mgallcur)), 
    instorcur = gsub("[^0-9.]", "", instorcur),
    instorcur = as.numeric(gsub('\\.$', '', instorcur)), 
    date = as.Date(strptime(date, format="%a, %d %b %Y %H:%M:%S %z"))
  ) 

emailpars <- emailproc |> 
  select(date, instorcur, mgallcur) |> 
  bind_rows(
    tibble( # not in emails, see updates at  bottom of page here: https://floridadep.gov/water/mining-mitigation-0
      date = as.Date(c('2024-08-09', '2024-10-25')),
      instorcur = c(90, 76),
      mgallcur = c(160.2, 193)
    )
  ) |>
  arrange(date)

save(emailpars, file = here('data/emailpars.RData'))

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
