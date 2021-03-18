make_plot <- function(preds,preds2,stdN, obp_y,usda_trend) {
  x <- preds
  
  # calculate some stats needed to correctly fill area under the distribution curve
  mean_x <- mean(x)
  high_sd <- mean(x) + stdN*sd(x)
  low_sd <- mean(x) - stdN*sd(x)
  sd_x <- sd(x)
  
  # create data frame for density plotting and shading
  dens <- density(x)
  dd <- with(dens, data.frame(x, y))
  #print(dd)
  
  x2 <- preds2
  
  # calculate some stats needed to correctly fill area under the distribution curve
  mean_x2 <- mean(x2)
  high_sd2 <- mean(x2) + stdN*sd(x2)
  low_sd2 <- mean(x2) - stdN*sd(x2)
  sd_x2 <- sd(x2)
  
  # create data frame for density plotting and shading
  dens2 <- density(x2)
  x2 <- data.frame(dens2$x)
  dd2 <- with(dens2, data.frame(x, y))
  
  dd3 <- cbind(dd, dd2)
  colnames(dd3) <- c('x','y')
  
  # use created data frame to render plot 
  distPlot <-  qplot(x,y,data=dd, geom="line") + # plot base
    geom_line(data=dd2) + # plot base

    # fill discount range
    geom_ribbon(data=subset(dd, x>=low_sd, x<high_sd),aes(ymax=y),ymin=0,
                fill="blue",colour=NA,alpha=0.7) +
    
    # fill discount range
    geom_ribbon(data=subset(dd2, x>=low_sd2, x<high_sd2),aes(ymax=y),ymin=0,
                fill="yellow",colour=NA,alpha=0.7) +

    # set correct y limits
    ylim(0, max(dd3$y)) +
    
    # Put text for discount range on bottom of plot
    xlab("Expected yield [bushels/acre]") +
    ylab("Probability") +
    
    # add vertical line at mean discount offer
    geom_vline(aes(xintercept = mean_x,color='Bayer avg'), linetype = 'dashed', size=1.25) +
    geom_vline(aes(xintercept = mean_x2,color='Comp avg'), linetype = 'dashed', size=1.25) +
    geom_vline(aes(xintercept = obp_y,color='2019 USDA co avg'), linetype = 'dashed', size=1.25) +
    geom_vline(aes(xintercept = usda_trend,color='2020 USDA co pred'), linetype = 'dashed', size=1.25) +
    scale_color_manual(name = "", values = c('Bayer avg' = "blue4", 'Comp avg' = 'gold', '2019 USDA co avg' = "red", '2020 USDA co pred' = 'black')) +
  
  # Add minimial theme to match white background
  theme_minimal(base_size = 16) +

  theme(legend.position='top')
  distPlot
}

make_plot1 <- function(preds,stdN,x_label) {

  x <- preds

  # calculate some stats needed to correctly fill area under the distribution curve
  mean_x <- mean(x)
  high_sd <- mean(x) + stdN*sd(x)
  low_sd <- mean(x) - stdN*sd(x)
  sd_x <- sd(x)

  # create data frame for density plotting and shading
  dens <- density(x)
  dd <- with(dens, data.frame(x, y))
  #print(dd)

  # use created data frame to render plot
  distPlot <-  qplot(x,y,data=dd, geom="line") + # plot base

    # fill discount range
    geom_ribbon(data=subset(dd, x>=low_sd, x<high_sd),aes(ymax=y),ymin=0,
                fill="green",colour=NA,alpha=0.7) +

    # set correct y limits
    ylim(0, max(dd$y)) +

    # Put text for discount range on bottom of plot
    xlab(x_label) +
    ylab("Probability") +
   

    # add vertical line at mean discount offer
    geom_vline(aes(xintercept = mean_x,color='avg'), linetype = 'dashed', size=1.25) +
    scale_color_manual(name = "", values = c('avg' = "green4")) +


    # Add minimial theme to match white background
    theme_minimal(base_size = 16) +

  theme(legend.position='top')
  distPlot
}

getPredHelper <- function(model, testVars){
  
  pred_not_transformed <- predict(model, data.matrix(testVars))
  pred <- as.numeric(pred_not_transformed)
  
  return(pred)
}

predictRF <- function(model_results, test_df, features) {
  
  testVars <- dplyr::select(data.frame(test_df), all_of(features))
  pred_list <- unlist(lapply(model_results, function(i) getPredHelper(i, testVars)))
  
  return(pred_list)
  
}

ecapi_call <- function(df_input) {
  library(data.table)
  library(devtools)
  library(tibble)
  
  rm(list = ls())
  source("./R/ec_api_functions.R")
  source("./R/connect_functions.R")
  source("./R/geo_functions.R")
  source("./R/field_ec.R")
  
  # Options
  #filepath <- filepath

  shape <- 'square' #"square"  # Rough shape of the polygon (alternative: "circle")
  ec_set <- 100  # Number of classes in the desired EC classification
  vector <- TRUE  # If TRUE, only aggregate field EC statistics will be returned
  # api <- "opencpu" # access API deployed on OpenCPU - not working at the moment
  # api <- "none"  # access API deployed locally (by running "start_server.R"
  # api <- "rsconn"  # access API deployed on RSconnect
  # api <- "none"  # local installation of the ECapi package
  
  api <- 'none'
  
  fields_dt <- df_input
  

  if ("polygon" %in% names(fields_dt)) {
    # If polygons present, use them
    polygons <- fields_dt$polygon
  } else if (length(intersect(c("long", "lat", "area"), names(fields_dt))) == 3) {
    # If long/lats and areas are present, use them to construct polygons
    # The geometry operations require installing package `sf`
    polygons <- get_polygons(fields_dt, shape = shape)
  } else {
    stop("Input file doesn't contain sufficient information to infer polygons.")
  }
  
  # Construct a list with field information and parameters
  data_list <- list(fields = list(id = fields_dt$field_id,
                                  geom = polygons),
                    params = list(ec_set = ec_set,
                                  vector = vector))

  # Call ECapi and get data.table with results
  ec_dt <- call_ecapi(data_list, url_fun = "field_ec", api = api)

  # # Save outputs
  # fwrite(ec_dt, file.path(dirname(filepath),
  #                         sub(".csv", "_ec.csv", basename(filepath))))
  
  return(ec_dt)
  
}


create_df <- function(area, long, lat) { 
  df_loc <- data.frame(field_id = paste0("B",round(100*long,0), round(100*lat,0)), area = area, long = long, lat = lat)
  return(df_loc)
  }

do_merge <- function(df) {
  # df_loc <- data.frame(field_id = paste0("B",round(100*long,0), round(100*lat,0)), area = area, long = long, lat = lat)
  # df <<- df_loc

  #pull ec data
  df_ecapi <<- ecapi_call(df_input = df_input) #, shape = square, ec_set = 100, vector = TRUE, api = none)
  if(nrow(df_ecapi) == 0) {
    stop('Invalid coordinates. E.g., coordinates may point to a body of water.')
  } else {
    
  #one-hot encode ec data
  df_ec <- onehotencoder(df_ecapi)

  # merge ec data with rents data
  #prep ec data for merge by adding state_county column to df_ecapi
  long_nums <- c()
  lat_nums <- c()
  for (i in 1:nrow(df_ec)){
    longlat_str <-unname(df_ec[i,'field_id'])
    long_num <- as.numeric(substr(longlat_str, 2, 6))/100
    lat_num <- as.numeric(substr(longlat_str, 7, 10))/100
    long_nums <- c(long_nums,long_num)
    lat_nums <- c(lat_nums,lat_num)
  }
  df_coords <<- data.frame(x = long_nums, y = lat_nums)
  st_co <- lonlat_to_state_sp(df_coords)
  st_co <- gsub(" ", "", st_co, fixed = TRUE)
  df_ec$state_county <- st_co

  #load and prep rents data
  df_rents <- read.csv("./Data/OBP_County_Data.csv", header = T)
  df_rents <- df_rents[,c("state","county","Farm_Land_Value", "Education","CropAcres","CropLandPercent","InsuredCropLand","InsurancePercent","LossRisk","NetCashFarm","OwnershipPercent","RentValue","CornAcres")]
  rents_stco <- paste0(df_rents$state,',',df_rents$county)
  rents_stco <- tolower(gsub(" ", "", rents_stco, fixed = TRUE))
  df_rents$state_county <- rents_stco

  #merge df_ecapi and df_rents
  df_merge <- merge(df_rents,df_ec, by = c("state_county"),all.x = TRUE)
  df_merge <- na.omit(df_merge)
  return(df_merge)
  }
}

make_table <- function(df_ids, pred_b,pred_c, usda_trend) {
  df_yields <<- cbind(df_ids,bayer_yield_pred = mean(pred_b))
  df_yields <<- cbind(df_yields,comp_yield_pred = mean(pred_c))
  df_yields <<- cbind(df_yields,yield_adv_pred = mean(pred_y))
  df_final <- merge(df_input,df_yields, by = c("field_id"),all.x = TRUE)
  #df_final <- df_final[,c('state','county','long','lat','area','bayer_yield_pred','comp_yield_pred','yield_adv_pred','usda_yield')]
  df_final$usda_yield_pred <- usda_trend
  df_final <- df_final[,c('state','county','usda_yield','usda_yield_pred', 'bayer_yield_pred','comp_yield_pred','yield_adv_pred')]
  #df_final <- na.omit(df_final)
  return(df_final)
  }

meta_func <- function(df){
  df_merge <- do_merge(df)
  if (nrow(df_merge) == 0) {
    stop('Rent data for the county corresponding to those coordinates is currently unavailable. A prediction cannot be made.')
  } else {
    df_test <- df_merge[,-(c(1,2,3,15)), drop = FALSE]
    df_ids <<- df_merge[,c('field_id','state','county')]
    df_ids <<- merge(df_ids, df_obp, by = c('county','state'))
    obp_y <<- df_ids$usda_yield
    stco <- paste0(df_ids$state, ',', df_ids$county)
    cotrend <- df_cotrends[df_cotrends$state_county == stco,]
    slp <- cotrend$m_co
    intpt <- cotrend$b_co
    
    st_co <- paste0(df_ids$state,',',df_ids$county)
    df_co <- df_cotrends[df_cotrends$state_county == st_co,]
    usda_trend <<- df_co$m_co*20 + df_co$b_co
    
    features_b <- colnames(df_test)
    pred_b <- predictRF(rfModel_b, df_test, features_b)
    print(pred_b)
    pred_bayer <<- pred_b + 20*slp +intpt
    pred_c <- predictRF(rfModel_c, df_test, features_b)
    pred_comp <<- pred_c + 20*slp +intpt
    pred_y <<- predictRF(rfModel_y, df_test, features_b)
    dist_plt <- make_plot(pred_bayer, pred_comp,4,obp_y,usda_trend)
    dist_plt <- ggplotly(dist_plt)
    return(dist_plt)
  } 
}

make_dist_plt <- function(pred_y, obp_y) {
  dist_plt <- make_plot1(pred_y,4,obp_y)
  dist_plt <- ggplotly(dist_plt)
  return(dist_plt)
}
