
library(dplyr)
library(data.table)
library(mapboxapi)
library(leaflet)
library(shiny)
library(hereR)
library(sf)
library(readr)

# SETUP -------------------------------------------------------------------------------------------
# NOTE: Data cleaning script is data_cleaning.R, run to generate input files

## LOAD DATA ----
all_wells <- read_rds("input_data.rds") 
sens_receptors <- readRDS("sensitive_receptors.rds")
field_boundaries <- read_sf("Field_Boundary_Cleaned")

## PROJECT CONSTANTS ----
date_of_download <- "10/04/2021" # Update with date of most recent download of the wells data

# Set up CRS
crs_nad83 <- st_crs("+init=epsg:4269 +proj=longlat +ellps=GRS80
                        +datum=NAD83 +no_defs +towgs84=0,0,0")  

# projected CRS, for creating buffers
crs_projected <- st_crs("+proj=utm +zone=11 +datum=WGS84")

## WELL STATUS ----
# Vector of well types (from WellStatus column of all_wells data) desired in map
types <- c("Active", "New", "Plugged", "Idle", "Other") 

# Aggregating Plugged and PluggedOnly here, and putting all other types into "Other"
all_wells <- all_wells %>%
  mutate(WellStatus = ifelse(all_wells$WellStatus == "PluggedOnly", 
                             "Plugged", WellStatus)) 

# Update with desired label format on clicking wells
all_wells <- all_wells %>%
  mutate(label = paste0("This <b>", all_wells$WellTypeLa, "</b> well is <b>", 
                        all_wells$WellStatus, "</b>.<br>It was drilled on <b>", 
                        all_wells$SpudDate, "</b> and operated by <b>", 
                        all_wells$OperatorNa, "</b>.", 
                        ifelse (!is.na(AbdDate), 
                                paste0(" It was abandoned in <b>", 
                                       all_wells$AbdDate, "</b>."), "")))

# Create individual datasets to be placed in map (NOTE if number/name of well statuses
# is changed, need to also change the addCircleMarkers command in output$map)
wells_active <- all_wells %>%
  filter(WellStatus == "Active")

wells_new <- all_wells %>%
  filter(WellStatus == "New")

wells_plugged <- all_wells %>%
  filter(WellStatus == "Plugged") 

wells_idle <- all_wells %>%
  filter(WellStatus == "Idle")

wells_other <- all_wells %>%
  filter(!(WellStatus %in% c("Active", "New", "Plugged", "Idle"))) %>%
  mutate(WellStatus = "Other") 

## RADII ----
# distances (in m) of buffer options
radii_distances <- c(975.36, 4000)
# labels (as strings) describing buffer options, 
# must be in same order/same length as radii_distances
radii_labels <- c("3200 feet -- proposed buffer distance", 
                  "2.5 mi -- air quality effects observed")

## API CALLS -----
# Geocoding (address -> lat/lon, MapBox API)
token <- Sys.getenv("TOKEN")

# Address suggestions (Here API)
token_here <- Sys.getenv("TOKEN_HERE")
set_key(token_here)

## GEOMETRIES ----
# Fix shape
sens_receptors <- sf::st_as_sf(sens_receptors, 
                               coords = c("Longitude", "Latitude"), crs = crs_nad83)
# convert wells data to sf (for counting wells within radii)
all_wells_sf <- sf::st_as_sf(all_wells, 
                             coords = c("Longitude", "Latitude"), crs = crs_nad83)

# UI ----------------------------------------------------------------------------------------------
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      # set up UI elements within panel (inputs)
      textInput("text", "Start typing address"), # type address
      uiOutput("geocoder"), # address suggestions
      actionButton("run", "Run"), # geocodes
      radioButtons("radius", "Select Radius", # radius selection
                   choiceValues = radii_distances,
                   choiceNames = radii_labels),
      textOutput("date") # date of last data update
    ),
    mainPanel(
      leafletOutput("map", height = "100vh"), # main map
      absolutePanel(bottom = "5%", left = "5%", # counts wells in radius
                    htmlOutput("numWells"))
    )
  )
)

# SERVER ------------------------------------------------------------------------------------------
server <- function(input, output, session) {
  # show date of most recent update of wells data
  output$date <- renderText({paste0("Date of last update to wells data: ", date_of_download)})

  ## GEOCODING ----
  # sets up valid address options
  output$geocoder <- renderUI({
    selectInput("address", "Select valid address", 
                # suggest top 3 addresses based on input
                choices = autosuggest(paste0(
                  input$text, "CA, USA"))[1:3, 3],
                selected = NULL, multiple = FALSE)
  })
  
  # run geocoding on selected address from address options when "run" button pressed
  geocoded <- eventReactive(input$run, 
                            {mb_geocode(search_text = paste0(" ", input$address[1]),
                                        output = "coordinates",
                                        access_token = token)})

  ## BASE MAP
  # values$def is 0 if "run" hasn't been pressed yet
  values <- reactiveValues(def = 0)
  
  observeEvent(input$run,{
    values$def <- input$run
  })
  
  val_geo_1 <- reactive({
    if(values$def == 0) {
      return(-118.2437) # Set map to central LA if "run" hasn't been pressed yet
    }
    else{
      return(geocoded()[1]) # Set map to geocoded value if "run" has been pressed
    }
  })
  
  val_geo_2 <- reactive({
    if(values$def == 0) {
      return(34.0522) # Set map to central LA if "run" hasn't been pressed yet
    }
    else{
      return(geocoded()[2]) # Set map to geocoded value if "run" has been pressed
    }
  })
  
  # create basemap with wells centered over geocoded address point
  output$map <- renderLeaflet({
    # preferCanvas option speeds up map
    leaflet(options = leafletOptions(preferCanvas = TRUE)) %>% 
      addProviderTiles(providers$CartoDB.Positron, options = providerTileOptions(
        updateWhenZooming = FALSE,      # map won't update tiles until zoom is done
        updateWhenIdle = FALSE,           # map won't load new tiles when panning
        zIndex = 400
      )) %>%
      addMapPane(name = "radius", zIndex = 2) %>%
      addMapPane(name = "upper", zIndex = 402) %>% # wells will always be on top of address circle
      # options to select well types, show sensitive receptors, show field boundaries
      addLayersControl( 
        overlayGroups = c(types, "Show Nearby Schools and Hospitals", 
                          "Show Oil Field Boundaries"),
        options = layersControlOptions(
          collapsed = T
        )
      )  %>%
      # Header for well types selection
      htmlwidgets::onRender("
        function() {
            $('.leaflet-control-layers-overlays').prepend('<label style=\"text-align:center\">Select Well Types to Display</label>');
        }
    ") %>%
      setView(lng = val_geo_1(), lat = val_geo_2(), zoom = 12) %>% # center on address/central LA
      # dynamic legend for schools/hospitals
      addLegend(layerId = "legend", group = "Show Nearby Schools and Hospitals", 
                values = c("School", "Hospital"), position = "bottomright",
                labels = c("School", "Hospital"), colors = c("#CE71ED", "#2441C0")) %>%
      # oil field boundaries
      addPolygons(group = "Show Oil Field Boundaries", data = field_boundaries,
                  stroke = F,
                  fillColor = "red",
                  fillOpacity = .3) %>%
      hideGroup("Show Nearby Schools and Hospitals") %>%
      clearGroup("Show Nearby Schools and Hospitals") %>%
      hideGroup("Show Oil Field Boundaries") %>%
      removeControl(layerId = "legend") 
  })

  
  ## MAP ADDRESS CIRCLE ----
  # update map's radius around geocoded point when new radius selected
  observe({
    leafletProxy("map") %>%
      clearGroup(group = "radius") %>%
      addCircles(group = "radius", options = list(zIndex = 401),
                 lat = geocoded()[2], lng = geocoded()[1], fill = FALSE, fillOpacity = 0, 
                 radius = as.numeric(input$radius),
                 stroke = TRUE, color = "black", opacity = 1, 
                 highlightOptions = highlightOptions(fill = FALSE, sendToBack = TRUE)) %>%
      addCircles(group = "radius", options = list(zIndex = 401),
                 lat = geocoded()[2], lng = geocoded()[1], fill = TRUE, radius = 20,
                 stroke = FALSE,
                 fillColor = "black",
                 fillOpacity = 1,
                 highlightOptions = highlightOptions(fill = FALSE, sendToBack = TRUE))
  })
  
  ## MAP SENSITIVE RECEPTORS ----
  # find sensitive receptors intersecting with 10 km buffer around chosen point
  sens_recept <- reactive ({
    geo <- data.frame(lon = val_geo_1(), lat = val_geo_2()) %>%
      st_as_sf(coords = c("lon", "lat"), crs = crs_nad83) %>%
      st_transform(crs_projected) %>%
      st_buffer(dist = as.numeric(10000)) %>%
      st_transform(crs_nad83)
    st_intersection(sens_receptors, geo) %>%
      dplyr::mutate(lon = sf::st_coordinates(.)[,1],
                    lat = sf::st_coordinates(.)[,2]) %>%
      st_drop_geometry()
  })
  
  observe({
    leafletProxy("map") %>%
      clearGroup(group = "Show Nearby Schools and Hospitals") %>%
      addCircles(group = "Show Nearby Schools and Hospitals", data = sens_recept(), 
                 radius = as.numeric(input$radius),
                 stroke = F,
                 fillColor = ifelse(sens_recept()$type == "school", "#CE71ED", "#2441C0"),
                 fillOpacity = .1,
                 lat = as.numeric(sens_recept()$lat), lng = as.numeric(sens_recept()$lon),
                 popup = paste0("<b>", ifelse(sens_recept()$type == "school", paste0("School: "), 
                                              paste0("Hospital: ")), "</b>", 
                                sens_recept()$Name)) %>%
      addCircles(group = "Show Nearby Schools and Hospitals", data = sens_recept(), 
                 radius = 20, stroke = F,
                 fillColor = ifelse(sens_recept()$type == "school", "#CE71ED", "#2441C0"),
                 fillOpacity = 1,
                 lat = as.numeric(sens_recept()$lat), lng = as.numeric(sens_recept()$lon),
                 popup = paste0("<b>", ifelse(sens_recept()$type == "school", paste0("School: "), 
                                              paste0("Hospital: ")), "</b>",
                                sens_recept()$Name))
  })
  
  ## MAP WELLS ----
  # add wells, wells will be updated each time new well type is selected or when new address 
  # is submitted
  observeEvent(val_geo_1(), {
    leafletProxy("map") %>%
      clearMarkers() %>%
      addCircleMarkers(group = "Active", options = pathOptions(pane = "upper"),
                       lat = wells_active$Latitude, lng = wells_active$Longitude,
                       clusterOptions = markerClusterOptions(disableClusteringAtZoom = 14),
                       stroke = FALSE, fillOpacity = .8, radius = 3, color = "red",
                       popup = wells_active$label) %>%
      addCircleMarkers(group = "New", options = pathOptions(pane = "upper"),
                       lat = wells_new$Latitude, lng = wells_new$Longitude,
                       clusterOptions = markerClusterOptions(disableClusteringAtZoom = 14),
                       stroke = FALSE, fillOpacity = .8, radius = 3, color = "red",
                       popup = wells_new$label) %>%
      addCircleMarkers(group = "Plugged", options = pathOptions(pane = "upper"),
                       lat = wells_plugged$Latitude, lng = wells_plugged$Longitude,
                       clusterOptions = markerClusterOptions(disableClusteringAtZoom = 14),
                       stroke = FALSE, fillOpacity = .8, radius = 3, color = "red",
                       popup = wells_plugged$label) %>%
      addCircleMarkers(group = "Idle", options = pathOptions(pane = "upper"),
                       lat = wells_idle$Latitude, lng = wells_idle$Longitude,
                       clusterOptions = markerClusterOptions(disableClusteringAtZoom = 14),
                       stroke = FALSE, fillOpacity = .8, radius = 3, color = "red",
                       popup = wells_idle$label) %>%
      addCircleMarkers(group = "Other", options = pathOptions(pane = "upper"),
                       lat = wells_other$Latitude, lng = wells_other$Longitude,
                       clusterOptions = markerClusterOptions(disableClusteringAtZoom = 14),
                       stroke = FALSE, fillOpacity = .8, radius = 3, color = "red",
                       popup = wells_other$label)
  })
  
  ## WELL COUNTS ----
  # Determine radius location for intersection with wells
  geocoded_sf <- reactive({
    data.frame(lon = geocoded()[1], lat = geocoded()[2]) %>%
      st_as_sf(coords = c("lon", "lat"), crs = crs_nad83) %>%
      st_transform(crs_projected) %>%
      st_buffer(dist = as.numeric(input$radius)) %>%
      st_transform(crs_nad83)
  })
  
  # figure out which wells are being shown on the map to do an exposure assessmnet
  selected_wells <- reactive ({
    all_wells_sf %>%
      filter(WellStatus %in% input$map_groups)
  })
  
  # count number of wells within radius (exposure assessment)
  numWells <- reactive ({
    # find the intersection of wells and buffer
    as.character(sum(unlist(st_intersects(selected_wells(), geocoded_sf()))))
  })
  
  # text for well counts output
  output$numWells <- renderText({paste("<b>", "Number of Wells Within Radius: ", numWells(), 
                                       "</b>")})
}
shinyApp(ui, server)

