library(shiny)
library(leaflet)
library(leaflet.extras)
library(sp)
library(DT)
library(maps)
library(maptools)
#library(sf, lib.loc = './Library')
library(sf)
library(spData)
library(rerddap)
library(twitteR)

# function for finding the locations inside the shapes we draw
findLocations <- function(shape, location_coordinates, location_id_colname){
  
  # derive polygon coordinates and feature_type from shape input
  polygon_coordinates <- shape$geometry$coordinates
  feature_type <- shape$properties$feature_type
  
  
  if(feature_type %in% c("rectangle","polygon")) {
    # transform into a spatial polygon
    drawn_polygon <- Polygon(do.call(rbind,lapply(polygon_coordinates[[1]],function(x){c(x[[1]][1],x[[2]][1])})))
    
    # use 'over' from the sp package to identify selected locations
    selected_locs <- sp::over(location_coordinates
                              , sp::SpatialPolygons(list(sp::Polygons(list(drawn_polygon),"drawn_polygon"))))
    
    # get location ids
    x = (location_coordinates[which(!is.na(selected_locs)), location_id_colname])
    
    selected_loc_id = as.character(x[[location_id_colname]])
    
    return(selected_loc_id)
    
  } else if (feature_type == "circle") {
    
    center_coords <- matrix(c(polygon_coordinates[[1]], polygon_coordinates[[2]])
                            , ncol = 2)
    
    # get distances to center of drawn circle for all locations in location_coordinates
    # distance is in kilometers
    dist_to_center <- spDistsN1(location_coordinates, center_coords, longlat=TRUE)
    
    # get location ids
    # radius is in meters
    x <- location_coordinates[dist_to_center < shape$properties$radius/1000, location_id_colname]
    
    selected_loc_id = as.character(x[[location_id_colname]])
    
    return(selected_loc_id)
  }
}

latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs)
  #proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF) #(x = -90, y = 44)
  #proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  #indices <- over(counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}

# source: https://opendata.socrata.com/dataset/Airport-Codes-mapped-to-Latitude-Longitude-in-the-/rxrh-4cxm
#airports <- read.csv('26087_longlat_area.csv') 
airports <- read.csv('channel_grower_geolocations_2019_m2.csv')
airports$Longitude <- airports$long
airports$Latitude <- airports$lat
airports$locationID <- airports$grower_SAPID #field_id
airports$State <- airports$Ship.To.State.Province
airports$City <- airports$Ship.To.City
airports$polyCounty <- latlong2county(airports[,c('Longitude','Latitude')])
airports$County <- sapply(strsplit(airports$polyCounty,","), `[`, 2)
airports <- merge(airports, county.fips, by.x = c("polyCounty"), by.y = c("polyname"), all.x = TRUE, all.y = FALSE)
airports <- airports[,c('locationID', 'Longitude','Latitude', 'State','County','fips','City')] #

airports_zoom <- subset(airports, fips == "20041")
# airports <- subset(airports, State == "KS")
# airports <- subset(airports, County == "barton")

# longitudinal coordinates in dataset are off, reverse to negative values to place them in the western hemisphere
#airports$Longitude <- airports$Longitude - 2 * airports$Longitude

# generate second set of unique location IDs for second layer of selected locations
airports$secondLocationID <- paste(as.character(airports$locationID), "_selectedLayer", sep="")
coordinates <- SpatialPointsDataFrame(airports[,c('Longitude', 'Latitude')] , airports)

# download draw file
# read.all <- readRDS('./recorded_user_data/cut_location/1442571_location.Rds')
# raw <- do.call(rbind,read.all$geometry$coordinates[[1]])
# raw <- apply(raw, 2,as.numeric)

shinyApp(
  ui <- fluidPage(
    #actionButton("show", "Please select crop location"),
    hr(),
    h4("Customer crop location"),
    helpText("Help us, Help you! Your input can help MARS better!"),
    actionButton("show2", "Customer Crop Map"),
    leafletOutput("mymap"),
    DT::dataTableOutput("mytable"),
    actionButton("show","Save")
  ),
  
  server <- function(input, output) {
    
    ################################################# section one #################################################
    # list to store the selections for tracking
    airports <- subset(airports, fips == "20041")
    data_of_click <- reactiveValues(clickedMarker = list())
    
    ###########################################section two_copy######################
    data2 <- eventReactive(input$show2,{
      leaflet() %>%
      addTiles() %>%
      addScaleBar() %>%
      setView(lng = airports_zoom$Longitude[1], lat = airports_zoom$Latitude[1],  zoom = 12) %>%
      addCircles(data = airports,
                 radius = 100,
                 lat = airports$Latitude,
                 lng = airports$Longitude,
                 fillColor = "white",
                 fillOpacity = 1,
                 color = "Gold",
                 #group = "new_point",
                 weight = 3,
                 stroke = T,
                 layerId = as.character(airports$locationID),
                 highlightOptions = highlightOptions(color = "mediumseagreen",
                                                     opacity = 3.0,
                                                     weight = 3,
                                                     bringToFront = TRUE)) %>%
      addDrawToolbar(
        targetGroup='Selected',
        polylineOptions=FALSE,
        markerOptions = FALSE,
        polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                          ,color = 'blue'
                                                                          ,weight = 2)),
        rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                              ,color = 'blue'
                                                                              ,weight = 2)),
        circleOptions = drawCircleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0
                                                                          ,color = 'blue'
                                                                          ,weight = 2)),
        editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions())) #%>%
      
      #addPolylines(data = raw, weight = 2,color = 'blue')
    })
     
    output$mymap <- renderLeaflet({
     data2()
    })
    
    ################################################# section two #################################################
    # base map
    # output$mymap <- renderLeaflet({
    #   leaflet() %>%
    #     addTiles() %>%
    #     addCircles(data = airports,
    #                radius = 30,
    #                lat = airports$Latitude,
    #                lng = airports$Longitude,
    #                fillColor = "white",
    #                fillOpacity = 1,
    #                color = "Gold",
    #                weight = 2,
    #                stroke = T,
    #                layerId = as.character(airports$locationID),
    #                highlightOptions = highlightOptions(color = "mediumseagreen",
    #                                                    opacity = 1.0,
    #                                                    weight = 2,
    #                                                    bringToFront = TRUE)) %>%
    #     addDrawToolbar(
    #       targetGroup='Selected',
    #       polylineOptions=FALSE,
    #       markerOptions = FALSE,
    #       polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
    #                                                                         ,color = 'blue'
    #                                                                         ,weight = 2)),
    #       rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
    #                                                                             ,color = 'blue'
    #                                                                             ,weight = 2)),
    #       circleOptions = drawCircleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0
    #                                                                         ,color = 'blue'
    #                                                                         ,weight = 2)),
    #       editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions()))
    # })
    
    ############################################### section three #################################################
    observeEvent(input$mymap_draw_new_feature,{
      #Only add new layers for bounded locations
      found_in_bounds <- findLocations(shape = input$mymap_draw_new_feature
                                       , location_coordinates = coordinates
                                       , location_id_colname = "locationID")

      raw <- do.call(rbind,input$mymap_draw_new_feature$geometry$coordinates[[1]])
      raw <- apply(raw, 2,as.numeric)
      print(raw)
      #where to save location file
      #filnam_l <- paste("poly_col.Rds", sep = "", collapse = NULL)
      #file_l <- paste('./recorded_user_data/cut_location/',filnam_l, sep = "", collapse = NULL)
      #saveRDS(input$mymap_draw_new_feature, file = file_l)
      #read.all <- readRDS("MYLIST.Rds")
      
      for(id in found_in_bounds){
        if(id %in% data_of_click$clickedMarker){
          # don't add id
        } else {
          # add id
          data_of_click$clickedMarker<-append(data_of_click$clickedMarker, id, 0)
        }
      }
      
      # look up airports by ids found
      selected <- subset(airports, locationID %in% data_of_click$clickedMarker)
      
      proxy <- leafletProxy("mymap")
      proxy %>% 
        #clearGroup("new_point") %>%
        addCircles(data = selected,
                           radius = 30,
                           lat = selected$Latitude,
                           lng = selected$Longitude,
                           fillColor = "wheat",
                           fillOpacity = 1,
                           color = "mediumseagreen",
                           weight = 3,
                           stroke = T,
                           layerId = as.character(selected$secondLocationID),
                           #group = "new_point",
                           highlightOptions = highlightOptions(color = "Gold",
                                                               opacity = 1.0,
                                                               weight = 2,
                                                               bringToFront = TRUE))
      
      
    })
    
    selectedLocations <- reactive({
      selectedLocations <- subset(airports, locationID %in% data_of_click$clickedMarker)
      # return this output
      #selectedLocations <- selectedLocations[,c('Longitude','Latitude')]
      selectedLocations <- selectedLocations[,c('Longitude','Latitude','County','City','State')]
      selectedLocations
    })
    
    output$mytable <- renderDataTable({
      datatable(selectedLocations())
    })

    ###################################################
    # data <- eventReactive(input$show2,{
    #   datatable(selectedLocations())
    # })
    # 
    # output$mytable <- renderDataTable({
    #   data()
    # })
    # 
      
    ######################################################
    # observeEvent(input$show, {
    #   showModal(modalDialog(
    #     title = "Please select crop location",
    #     "Help us, Help you! To improve the MARS model and precise intention, ...",
    #     easyClose = TRUE,
    #     footer = NULL,
    #   ))
    # })
    
    ############################################### section four ##################################################
    observeEvent(input$mymap_draw_deleted_features,{
      # loop through list of one or more deleted features/ polygons
      for(feature in input$mymap_draw_deleted_features$features){

        # get ids for locations within the bounding shape
        bounded_layer_ids <- findLocations(shape = feature
                                           , location_coordinates = coordinates
                                           , location_id_colname = "secondLocationID")


        # remove second layer representing selected locations
        proxy <- leafletProxy("mymap")
        proxy %>% removeShape(layerId = as.character(bounded_layer_ids))

        first_layer_ids <- subset(airports, secondLocationID %in% bounded_layer_ids)$locationID

        data_of_click$clickedMarker <- data_of_click$clickedMarker[!data_of_click$clickedMarker
                                                                   %in% first_layer_ids]
      }
    })
    
  },
  
  options = list(height = 400)
)

