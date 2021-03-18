library(shiny)
library(leaflet)
library(leaflet.extras)
library(sp)
library(DT)
source('./Library/sf.R')
source('./Library/sfc.R')
library(raster)
library(rgeos)
library(ggplot2)
library(stringi)
library(h2o)
library(plotly)
library(data.table)
library(maps)
library(maptools)
library(sf)
library(spData)
library(rerddap)
library(ggplot2)
library(gridExtra)

source('functionsUI_2.R')

h2o.init()

extractCoords <- function(sp.df)
{
  results <- list()
  for(i in 1:length(sp.df@polygons[[1]]@Polygons))
  {
    results[[i]] <- sp.df@polygons[[1]]@Polygons[[i]]@coords
  }
  results <- Reduce(rbind, results)
  results
}

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


# source: https://opendata.socrata.com/dataset/Airport-Codes-mapped-to-Latitude-Longitude-in-the-/rxrh-4cxm
fips_code_comp <- '26087'
filnam <- paste(fips_code_comp,"_longlat_area.csv", sep = "", collapse = NULL)
path <-'./Data/tif/'
airports <- read.csv(paste(path,filnam, sep = "", collapse = NULL)) # New one doesn't work
#airports <- read.csv('26087_longlat_area.csv')
airports$Longitude <- airports$long
airports$Latitude <- airports$lat
airports$locationID <- airports$field_id
airports <- airports[,c('Longitude','Latitude', 'locationID')]

filnam_ec <- paste(fips_code_comp,"_longlat_area_ec.csv", sep = "", collapse = NULL)
path_ec <- './Data/tif_ec/'
airports_ec <- read.csv(paste(path_ec,filnam_ec, sep = "", collapse = NULL))
#airports_ec <- read.csv('26087_longlat_area_ec.csv')
airports_ec$long <- as.numeric(substr(airports_ec$field_id, 2, 6))/100
airports_ec$lat <- as.numeric(substr(airports_ec$field_id, 7, 10))/100

usda_data <- read.csv('countylevel_usda_data_imputed.csv')

vp_directory_path <- './R_Models/bootstrap_vp_model_train_mojo'
vp_bootstrap_files <- list.files(vp_directory_path)
nbootstrap <- length(vp_bootstrap_files)
ae_bootstrap_list <- list()
for (i in 1:nbootstrap){
  ae_path_i <- paste0(getwd(), '/', vp_directory_path, '/', vp_bootstrap_files[i])
  ae_bootstrap_list[[i]] <- h2o.import_mojo(ae_path_i)
}

#ae_path <- paste0('./R_Models/point_vp_model/', 'DeepLearning_model_R_1608822044488_8')
ae_point <- ae_bootstrap_list[[1]]

df_valueprop <- read.csv('valueprop_countylevel_mars.csv')
colnames_test <- colnames(df_valueprop)
colnames_test <- colnames_test[1:(length(colnames_test)-1)]

# longitudinal coordinates in dataset are off, reverse to negative values to place them in the western hemisphere
#airports$Longitude <- airports$Longitude - 2 * airports$Longitude

# generate second set of unique location IDs for second layer of selected locations
airports$secondLocationID <- paste(as.character(airports$locationID), "_selectedLayer", sep="")

coordinates <- SpatialPointsDataFrame(airports[,c('Longitude', 'Latitude')] , airports)

shinyApp(
  ui <- fluidPage(
    leafletOutput("mymap"),
    #DT::dataTableOutput("mytable2"),
    DT::dataTableOutput("mytable"),
    plotlyOutput(outputId="disty_plot_monyield"),
    plotlyOutput(outputId="disty_plot_compyield"),
    plotlyOutput(outputId="disty_plot_yieldadv"),
    plotlyOutput(outputId="disty_plot_seedpricedif"),
    plotlyOutput(outputId="disty_plot_valueprop"),
    plotOutput("gmplot")
  ),

  server <- function(input, output) {

    ################################################# section one #################################################
    # list to store the selections for tracking
    data_of_click <- reactiveValues(clickedMarker = list())

    ################################################# section two #################################################
    # base map
    output$mymap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        addCircles(data = airports,
                   radius = 30,
                   lat = airports$Latitude,
                   lng = airports$Longitude,
                   fillColor = "white",
                   fillOpacity = 1,
                   color = "Gold",
                   weight = 2,
                   stroke = T,
                   layerId = as.character(airports$locationID),
                   highlightOptions = highlightOptions(color = "mediumseagreen",
                                                       opacity = 1.0,
                                                       weight = 2,
                                                       bringToFront = TRUE)) %>%
        addDrawToolbar(
          targetGroup='Selected',
          polylineOptions=FALSE,
          markerOptions = FALSE,
          polygonOptions = drawPolygonOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                            ,color = 'white'
                                                                            ,weight = 3)),
          rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                                ,color = 'white'
                                                                                ,weight = 3)),
          # circleOptions = drawCircleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0
          #                                                                   ,color = 'white'
          #                                                                   ,weight = 3)),
          editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions()))
    })

    ############################################### section three #################################################
    observeEvent(input$mymap_draw_new_feature,{
      #Only add new layers for bounded locations
      found_in_bounds <- findLocations(shape = input$mymap_draw_new_feature
                                       , location_coordinates = coordinates
                                       , location_id_colname = "locationID")

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
      proxy %>% addCircles(data = selected,
                           radius = 30,
                           lat = selected$Latitude,
                           lng = selected$Longitude,
                           fillColor = "wheat",
                           fillOpacity = 1,
                           color = "mediumseagreen",
                           weight = 3,
                           stroke = T,
                           layerId = as.character(selected$secondLocationID),
                           highlightOptions = highlightOptions(color = "Gold",
                                                               opacity = 1.0,
                                                               weight = 2,
                                                               bringToFront = TRUE))

    })

    selectedLocations <- reactive({
      selectedLocations <- subset(airports, locationID %in% data_of_click$clickedMarker)
      # return this output
      selectedLocations <- selectedLocations[,c('Longitude','Latitude')]

      selectedLocations
    })

    poly_file <- reactive({
      poly_file <- 'mypoly.csv' # filename should be specific to user/session
    })

    observe({
      if (nrow(selectedLocations())==0) {
        output$mytable <- NULL
        output$disty_plot_monyield <- NULL
        output$disty_plot_compyield <- NULL
        output$disty_plot_yieldadv <- NULL
        output$disty_plot_seedpricedif <- NULL
        output$disty_plot_valueprop <- NULL
        #poly_file <- 'mypoly.csv' # filename should be linked to 'map' button click; file should be deleted on map close
        try({
          file.remove(poly_file())
        })
      }
    })

    observeEvent(input$mymap_draw_new_feature,{
        if (nrow(selectedLocations() > 0)) {
          inter1= selectedLocations()

          #add a category (required for later rasterizing/polygonizing)
          inter1 <- cbind(inter1, cat = rep(1L, nrow(inter1)),stringsAsFactors = FALSE)

          #convert to spatial points
          sp::coordinates(inter1) = ~Longitude + Latitude

          #gridify your set of points
          sp::gridded(inter1) <- FALSE

          #convert to raster
          r <- raster::raster(inter1)

          #convert raster to polygons
          sp = raster::rasterToPolygons(r, dissolve = FALSE)

          #draw convex polygon around selected boundary
          convex_poly <- rgeos::gConvexHull(sp, byid = FALSE)
          df_poly <- ggplot2::fortify(convex_poly)

          #for (long, lat) pairs for which we have ec data, select those pairs that are inside polygon
          pol_x <- df_poly$long
          pol_y <- df_poly$lat
          point_x <- airports_ec[,'long']
          point_y <- airports_ec[,'lat']
          #if env class pair is inside polygon, poly_bool <- 1
          #else poly_bool <- 0
          poly_bool <- point.in.polygon(point_x, point_y, pol_x, pol_y, mode.checked=FALSE)

          #append poly_bool to df_ec (which contains env class data for the county, plus poly_bool columns for any previously-selected field boundaries)
          #save appended dataframe as a csv file
          if (file.exists(poly_file())) {
            df_ec <- read.csv(poly_file())
            df_ec[[stri_rand_strings(1,15)]] <- poly_bool
          } else {
            df_ec <- airports_ec
            df_ec[[stri_rand_strings(1,15)]] <- poly_bool
          }
          write.csv(df_ec, poly_file(), row.names = FALSE)

          #select those rows in df_ec that have (long, lat) coords inside one of the selected field boundaries
          if (ncol(df_ec) > 7) {
            df_ec$bool <- rowMeans(df_ec[,7:ncol(df_ec)])
          } else {
            df_ec$bool <- df_ec[,7]
          }
          df_ec_relevant <- df_ec[which(df_ec$bool > 0), 1:6]

          #convert df_ec_relevant to a 100-component vector,
          #where the jth component corresponds to proportion of selected field boundaries that fall within env class j
          if(nrow(df_ec_relevant) > 0) {
            dfj <- df_ec_relevant
            dfj <- dfj[,c('hac2_0100_label','count')]
            dfj <- aggregate(dfj$count,by = list(hac2_0100_label = dfj$hac2_0100_label),FUN = sum)
            colnames(dfj) <- c('hac2_0100_label','counts')
            dfj$total_counts <- sum(dfj$counts)
            dfj$frac <- dfj$counts/dfj$total_counts
            dfj$hac2_0100_label <- as.integer(gsub('us-', '',dfj$hac2_0100_label))
            dfj <- dfj[,c('hac2_0100_label', 'frac')]
            zvecj <- rep(as.numeric(0),100)
            for (j in 1:nrow(dfj)) {
              fracj <- dfj[j,'frac']
              indexj <- dfj[j,'hac2_0100_label']
              zvecj[indexj] <- fracj
            }
            #zvecj := env class proportions, aggregated over selected farms

            usda_county <- usda_data[which(usda_data$fips_code == '26087'),]
            usda_county$fips_code <- NULL
            model_input <- unname(unlist(c(usda_county,zvecj)))
            model_input <- c(model_input, rep(NA, 7))
            df_input <- data.frame()
            df_input <- rbind(df_input, model_input)
            testset <- df_input
            colnames(testset) <- colnames_test

            testset.hex=as.h2o(testset)
            #vps <- c()
            vps <- data.frame()
            for (i in 1:nbootstrap) {
              testset_i <- testset
              predns = h2o.predict(ae_bootstrap_list[[i]],testset.hex)
              df_predns = as.data.frame(predns)

              naloc=as.matrix(is.na(testset_i))
              testset_i[naloc]=df_predns[naloc]

              vp <- testset_i[, (ncol(testset_i)-4):ncol(testset_i)]
              vps <- rbind(vps, vp)
            }

            avgs <- c()
            avg_vps <- data.frame()
            for (i in 1:ncol(vps)) {
              avg_coli <- round(mean(vps[,i]),2)
              avgs <- c(avgs, avg_coli)
            }
            avg_vps <- rbind(avg_vps, avgs)
            colnames(avg_vps) <- colnames(vps)
            colnames(avg_vps)[5] <- 'ValueProp'

            output$mytable <- renderDataTable({
              data.table(as.data.frame(avg_vps))
            })
                        
            output$disty_plot_valueprop <- renderPlotly({
              make_plot1(vps$valueprop,5,'Value Proposition [$/ac.]')
            })
            output$disty_plot_seedpricedif <- renderPlotly({
              make_plot1(vps$SeedPriceDif,5,'Seed Price Difference [$/bag]')
            })
            output$disty_plot_yieldadv <- renderPlotly({
              make_plot1(vps$YieldAdv,5,'Yield Advantage [bu./ac.]')
            })
            output$disty_plot_compyield <- renderPlotly({
              make_plot1(vps$CompYield,5,'Competitor Yield [bu./ac.]')
            })
            output$disty_plot_monyield <- renderPlotly({
              make_plot1(vps$MonYield,5,'Bayer Yield [bu./ac.]')
            })
            # output$gmplot <- renderPlotly({
            #   p1 <- make_plot1(vps$MonYield,5,'Bayer Yield [bu./ac.]')
            #   p2 <- make_plot1(vps$CompYield,5,'Competitor Yield [bu./ac.]')
            #   grid.arrange(p1, p2, nrow = 1)
            # })
          }

          #if nrow(df_ec_relevant) == 0, replace with zvecj county-level env class proportions
          #grab usda, PI data for the county
          #combine zvecj with county-level data
          #feed same into yield, value prop models
          #print distributions
        }
    })

    output$mytable2 <- renderDataTable({
      datatable(selectedLocations())
    })

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
