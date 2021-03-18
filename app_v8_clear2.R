# ====================================================================================================
# Market Funding Recommender System (MARS) Shiny App
# Version 2.0
# Corn + Soy
# ====================================================================================================

tryCatch({
# Clear working environment
rm(list=ls())
#options(readr.num_columns = 0)
options(warning.length = 1000L)

# Load required libraries
# suppressPackageStartupMessages(library(memoise, lib.loc="./Library"))
# library(devtools)
# load_all("./memoise2")
# library(filelock, lib.loc="./Library")
# suppressPackageStartupMessages(library(memoise))
# suppressPackageStartupMessages(library(shiny))
# suppressPackageStartupMessages(library(shinyWidgets))
# suppressPackageStartupMessages(library(shinyjs))
# suppressPackageStartupMessages(library(dplyr))
# suppressPackageStartupMessages(library(xgboost))
# suppressPackageStartupMessages(library(data.table))
# suppressPackageStartupMessages(library(readr))
# suppressPackageStartupMessages(library(caret))
# suppressPackageStartupMessages(library(shinybusy))
# suppressPackageStartupMessages(library(gridExtra))
# suppressPackageStartupMessages(library(ggpubr))
# suppressPackageStartupMessages(library(tidyr))
# suppressPackageStartupMessages(library(Cairo)) # included to work around plotting issues in RSConnect
# suppressPackageStartupMessages(library(parallel))
# suppressPackageStartupMessages(library(shinydashboard))
# suppressPackageStartupMessages(library(ShinyDash))
# suppressPackageStartupMessages(library(DT))
# suppressPackageStartupMessages(library(shinyBS))
# suppressPackageStartupMessages(library(shinythemes))
# suppressPackageStartupMessages(library(datasets))
# suppressPackageStartupMessages(library(magrittr))
# suppressPackageStartupMessages(library(plotly))
# suppressPackageStartupMessages(library(shinythemes))
# suppressPackageStartupMessages(library(feather))
# suppressPackageStartupMessages(library(tidyverse))

#library(memoise)
#library(filelock)
library(memoise, lib.loc="./Library")
library(filelock, lib.loc - "./Library")
# library(devtools)
# load_all("./memoise2")
library(shiny)
library(shinyWidgets)
library(shinyjs)
library(dplyr)
library(xgboost)
library(data.table)
library(readr)
library(caret)
library(shinybusy)
library(gridExtra)
library(ggpubr)
library(tidyr)
library(Cairo) # included to work around plotting issues in RSConnect
library(parallel)
library(shinydashboard)
library(ShinyDash)
library(DT)
library(shinyBS)
library(shinythemes)
library(datasets)
library(magrittr)
library(plotly)
library(shinythemes)
library(feather)
library(tidyverse)
library(flock)
library(stringr)
library(shinyjs)
library(ShinyRatingInput, lib.loc - "./Library")
library(shinyalert)
library(sf)
library(sp)
library(leaflet)
library(leaflet.extras)
library(spData)
library(rerddap)
library(maps)
library(maptools)
library(raster)
library(rgeos)
library(ggplot2)
library(stringi)
library(h2o)
library(aws.s3)
# library(paws.storage)
# library(dirr)
# Souce file configsUI.R and functionsUI.R ===========================================================
# Various helper functions and parameters used in the server potion of app.R are set here,
# see files for more information
source('functionsUI.R')
source('functionsUI_2.R')
source('configsUI.R')
source('dollar.r')
source('percent.r')
source('displayedfeatures.r') #Modularized feature display code
source('KPIMetricstab.R') #Modularized KPI Metrics tab
source('./Library/sf.R')
source('./Library/sfc.R')

h2o.init()

#connect to AWS.S3
aws_bucket_name <- 'rsconnect-mars-np'
aws_key <- 'AKIAQ4OBUKDQVZOPP24H'
aws_secret <- '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'
Sys.setenv("AWS_ACCESS_KEY_ID" = aws_key,
           "AWS_SECRET_ACCESS_KEY" = aws_secret)

# TODO: WORK IN PROGRESS--DEALING WITH MCLAPPLY/SHINY CONFLICT
# Register parallel processing if indicated in configs file ==========================================
#if (pllel == TRUE){
#registerDoParallel()
#}

# Memoise predict_discount and pre-population functions ==============================================
# tell memoise package where to store cache
# predict functions
cache_db <- cache_filesystem("./tmp_cache", algo = 'spookyhash')
# prepopulation functions
cache_db_prepop <- cache_filesystem("./tmp_cache_prepop", algo = 'spookyhash')
# generate report functions 
cache_db_generate_report <- cache_filesystem("./tmp_cache_generate_report", algo = 'spookyhash')
# plot data for fsr tab
cache_db_lcr <- cache_filesystem("./tmp_cache_lcr", algo = 'spookyhash')

# memoise ############################################################################################
# GENERATE REPORT (CORN + SOY)
generate_report <- memoise(generate_report_raw, cache=cache_db_generate_report)
#generate_report <- wrapMemoise(generate_report)
# CORN -----------------------------------------------------------------------------------------------
predict_discount <- memoise(predict_discount_raw, cache = cache_db)
predict_discount_new_cust <- memoise(predict_discount_new_cust_raw, cache = cache_db)
prePopulatePortfolioCorn <- memoise(prePopulatePortfolioCornRaw, cache = cache_db_prepop)

# SOY ------------------------------------------------------------------------------------------------
predict_discount_soy <- memoise(predict_discount_raw_soy, cache = cache_db)
predict_discount_new_cust_soy <- memoise(predict_discount_raw_soy_new_cust, cache = cache_db)
prePopulatePortfolioSoy <- memoise(prePopulatePortfolioSoyRaw, cache=cache_db_prepop)

# Data import ========================================================================================
#grower_report <- read.csv("Data/GrowerReportCorn.csv")
#grower_report_soy <- read.csv("Data/GrowerReportSoy.csv")
#View(grower_report)

#Load and parse data related to budget run rate

# #Create df linking FSR and TeamID
# fsr_list <-unique(growerList$FSR)
# team_list <- c()
# for (i in 1:length(fsr_list)){
#   growerList_fsr <- growerList[which(growerList$FSR == fsr_list[i]),]
#   team <- unique(growerList_fsr$'ABM ID')
#   team_list <- c(team_list,team[1])
# }
# fsr_team_df <- cbind(fsr_list,team_list)
# x <- c('FSR', 'TeamID')
# colnames(fsr_team_df) <- x
# 
# #Creates dfs (one for corn, one for soy) linking runrate and TeamID
# budget_df_corn <- setDF(read_csv(paste0(trainDataPath, runRateName))) #budget data for corn
# budget_df_soy <- setDF(read_csv(paste0(trainDataPath, runRateNameSoy))) #budget data for soy
# runrate_team_df_corn <- cbind(budget_df_corn$TeamID, budget_df_corn$PrevailingLCRPercentage)
# runrate_team_df_soy <- cbind(budget_df_soy$TeamID, budget_df_soy$PrevailingLCRPercentage)
# x <- c('TeamID', 'PrevailingLCRPercentage')
# colnames(runrate_team_df_corn) <- x
# colnames(runrate_team_df_soy) <- x
# 
# #Join FSR and corn/soy runrate on TeamID

# Load active seedsmen list
active_seedsmen <- setDF(read_csv(ActiveSeedsmenName))

# Load data. (The script above can be used to recreate csv files below when new runrate becomes available.)
runrate_df_corn <- setDF(read.csv('./Data/runrate_df_corn.csv',header = TRUE))
runrate_df_soy <- setDF(read.csv('./Data/runrate_df_soy.csv',header = TRUE))


#Code below has been deprecated
#Code was used to load and parse cust_df and EffPrice dataframes for corn + soy
#cust_df and EffPrice dataframes are now loaded in 'pre-parsed'
if(TRUE) {
  
  # Load in data the server portion uses for existing customers ########################################
  # CORN -----------------------------------------------------------------------------------------------
  cust_df <- setDF(readRDS(paste0(trainDataPath, trainDataName)))
  EffPrice <- setDF(readRDS(paste0(trainDataPath, EffPriceName)))
  
  # SOY ------------------------------------------------------------------------------------------------
  cust_df_soy <- setDF(readRDS(paste0(trainDataPath, trainDataNameSoy)))
  EffPrice_soy <- setDF(readRDS(paste0(trainDataPath, EffPriceNameSoy)))
 
  colnames(cust_df_soy)[1:4] <- c("fips_code", "Year", "GrowerSAPID", "SeedsmanSAPID")
  colnames(cust_df_soy)[7:8] <- c("GrowerAcctNm", "SeedsmanAcctNm")
  
  cust_df <- plyr::rbind.fill(cust_df, cust_df_soy[!(cust_df_soy$GrowerSAPID %in% cust_df$GrowerSAPID),
                                               c("SeedsmanSAPID","GrowerSAPID","Year",                              
                                                 "REGION_ID", "REGION", "TEAM_ID",                           
                                                 "TERR_ID", "fips_code", "GrowerAcctNm",                      
                                                 "SeedsmanAcctNm", "RBD", "ABM", "ABM.ID",                            
                                                 "FSR", "FSR.ID",  "FSR_NAME")])

  
  cust_df_soy <- plyr::rbind.fill(cust_df_soy, cust_df[!(cust_df$GrowerSAPID %in% cust_df_soy$GrowerSAPID),
                                               c("SeedsmanSAPID","GrowerSAPID","Year",                              
                                                 "REGION_ID", "REGION", "TEAM_ID",                           
                                                 "TERR_ID", "fips_code", "GrowerAcctNm",                      
                                                 "SeedsmanAcctNm", "RBD", "ABM", "ABM.ID",                            
                                                 "FSR", "FSR.ID", "FSR_NAME")]) 

  colnames(cust_df_soy)[1:4] <- c("FIPS", "MKT_YR", "GRWR_SAP_ID", "DLR_SAP_ID")
  colnames(cust_df_soy)[7:8] <- c("GRWR_ACCT_NAME", 'DLR_NAME')
  
  cust_df_soy[c(11:20,80:106,108:135)][is.na(cust_df_soy[c(11:20,80:106,108:135)])] <- 0
  #Hack: Manual fix to Territory N4K closure, N4E opening
  cust_df[cust_df$TERR_ID == "N4K", "TERR_ID"] <- "N4E"
  cust_df_soy[cust_df_soy$TERR_ID == "N4K", "TERR_ID"] <- "N4E"
  cust_df[cust_df$TERR_ID == "N4E", "FSR"] <- "Boehm, Jacob"
  cust_df_soy[cust_df_soy$TERR_ID == "N4E", "FSR"] <- "Boehm, Jacob"
  cust_df[cust_df$TERR_ID == "N4E", "FSR_NAME"] <- "Boehm, Jacob"
  cust_df_soy[cust_df_soy$TERR_ID == "N4E", "FSR_NAME"] <- "Boehm, Jacob"
  
  # Remove any observations that don't have seedsman id in both data sets ##############################
  # CORN -----------------------------------------------------------------------------------------------
  cust_df <- cust_df[!is.na(cust_df[[seedsmanIDColumn]]), ]
  EffPrice <- EffPrice[!is.na(EffPrice[[epSeedsmanIDColumn]]), ]
  
  # SOY ------------------------------------------------------------------------------------------------
  cust_df_soy <- cust_df_soy[!is.na(cust_df_soy[[seedsmanIDColumnSoy]]), ]
  EffPrice_soy <- EffPrice_soy[!is.na(EffPrice_soy[[epSeedsmanIDColumnSoy]]), ]

  # Check if there is an active FSR in the active_seedsman frame that is not in the cust_df. If so, replace "OPEN" with FSR name
  FSR_Territory_DF <- unique(active_seedsmen[, c("FSR", "TERR_ID")])
  colnames(FSR_Territory_DF) <- c("FSR_Supplement", "TERR_ID_Supplement")
  
  # CORN -----------------------------------------------------------------------------------------------
  cust_df <- merge(cust_df, FSR_Territory_DF, by.x = "TERR_ID", by.y = "TERR_ID_Supplement", all.x = TRUE, all.y = FALSE)
  cust_df <- data.frame(data.table(cust_df)[FSR_NAME == "Open" | FSR_NAME == "OPEN", FSR_NAME := FSR_Supplement])
  cust_df <- cust_df[!(names(cust_df) %in% "FSR_Supplement")]
  
  # SOY -----------------------------------------------------------------------------------------------
  cust_df_soy <- merge(cust_df_soy, FSR_Territory_DF, by.x = "TERR_ID", by.y = "TERR_ID_Supplement", all.x = TRUE, all.y = FALSE)
  cust_df_soy <- data.frame(data.table(cust_df_soy)[FSR_NAME == "Open" | FSR_NAME == "OPEN", FSR_NAME := FSR_Supplement])
  cust_df_soy <- cust_df_soy[!(names(cust_df_soy) %in% "FSR_Supplement")]
  
  rm(FSR_Territory_DF)
  
  # source: https://opendata.socrata.com/dataset/Airport-Codes-mapped-to-Latitude-Longitude-in-the-/rxrh-4cxm
  # airports <- read.csv('channel_grower_geolocations_2019_m2.csv')
  # airports$Longitude <- airports$long
  # airports$Latitude <- airports$lat
  # airports$locationID <- airports$grower_SAPID #field_id
  # airports$State <- airports$Ship.To.State.Province
  # airports$City <- airports$Ship.To.City
  # airports$County <- latlong2county(airports[,c('Longitude','Latitude')])
  # airports$County <- sapply(strsplit(airports$County,","), `[`, 2)
  # airports <- airports[,c('Longitude','Latitude', 'locationID','State','City','County')]

  #airports <- subset(airports, State == "IA")
  #airports <- subset(airports, County == "dickinson")
  # longitudinal coordinates in dataset are off, reverse to negative values to place them in the western hemisphere
  #airports$Longitude <- airports$Longitude - 2 * airports$Longitude
  
  # generate second set of unique location IDs for second layer of selected locations
  # airports$secondLocationID <- paste(as.character(airports$locationID), "_selectedLayer", sep="")
  # 
  # coordinates <- SpatialPointsDataFrame(airports[,c('Longitude', 'Latitude')] , airports)
  usda_data <- read.csv('countylevel_usda_data_imputed.csv')
  
  vp_directory_path <- './R_Models/bootstrap_vp_model_train_mojo'
  vp_bootstrap_files <- list.files(vp_directory_path)
  nbootstrap <- 10 #length(vp_bootstrap_files)
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
  
  # Preload models that are used in UI #################################################################
  # CORN -----------------------------------------------------------------------------------------------
  discountModel <- readRDS(paste0(discountModelPath, discountModelName))
  epModel <- readRDS(paste0(discountModelPath, epPointModelName))
  discountPointModel <- readRDS(paste0(discountModelPath, discountPointModelName))
  
  # SOY ------------------------------------------------------------------------------------------------
  discountModelSoy <- readRDS(paste0(discountModelPath, discountModelNameSoy))
  epModelSoy <- readRDS(paste0(discountModelPath, epPointModelNameSoy))
  discountPointModelSoy <- readRDS(paste0(discountModelPath, discountPointModelNameSoy))
  
  # Sort according to customer name for cleaner presentation in UI #####################################
  # CORN -----------------------------------------------------------------------------------------------
  cust_df <- cust_df[order(cust_df[[custNameColumn]]), ]
  
  # SOY ------------------------------------------------------------------------------------------------
  cust_df_soy <- cust_df_soy[order(cust_df_soy[[custNameColumnSoy]]), ]
  # Create customer name choices for subsetting by seedsman ============================================
  # convert names to title case ########################################################################
  # CORN -----------------------------------------------------------------------------------------------
  cust_df <- convert_names(cust_df, custNameColumn)
  cust_df <- convert_names(cust_df, seedsmanNameColumn)
  
  # SOY ------------------------------------------------------------------------------------------------
  cust_df_soy <- convert_names(cust_df_soy, custNameColumnSoy)
  cust_df_soy <- convert_names(cust_df_soy, seedsmanNameColumnSoy)
  
  # append blank character and new customer to item called choices, these are added on to relevant drop
  #  down menu later
  choices <- append("", "New Customer")
  
  # create formatted column names to display in UI with FSR, seedsman, and customer choices -- NAME|ID ##
  # CORN -----------------------------------------------------------------------------------------------
  pasteColsCust <- c(custNameColumn, custIDColumn)
  pasteColsSeedsman <- c(seedsmanNameColumn, seedsmanIDColumn)
  pasteColsFSR <- c('FSR_NAME', terrIDColumn)
  cust_df[, 'Customer Account Name | Customer ID'] <- do.call(paste, c(cust_df[pasteColsCust], sep=" | "))
  cust_df[, 'Seedsman Account Name | Seedsman ID'] <- do.call(paste, c(cust_df[pasteColsSeedsman], sep=" | "))
  cust_df[, 'FSR Name | FSR Territory ID'] <- do.call(paste, c(cust_df[pasteColsFSR], sep= " | "))
  
  # SOY ------------------------------------------------------------------------------------------------
  pasteColsCustSoy <- c(custNameColumnSoy, custIDColumnSoy)
  pasteColsSeedsmanSoy <- c(seedsmanNameColumnSoy, seedsmanIDColumnSoy)
  pasteColsFSRSoy <- c('FSR_NAME', terrIDColumnSoy)
  cust_df_soy[, 'Customer Account Name | Customer ID'] <- do.call(paste, c(cust_df_soy[pasteColsCustSoy], sep=" | "))
  cust_df_soy[, 'Seedsman Account Name | Seedsman ID'] <- do.call(paste, c(cust_df_soy[pasteColsSeedsmanSoy], sep=" | "))
  cust_df_soy[, 'FSR Name | FSR Territory ID'] <- do.call(paste, c(cust_df_soy[pasteColsFSRSoy], sep= " | "))
  
  # write.csv(cust_df,"./Data/cust_df.csv")
  # write.csv(cust_df_soy,"./Data/cust_df_soy.csv")
  # write.csv(EffPrice,"./Data/EffPrice.csv")
  # write.csv(EffPrice_soy,"./Data/EffPrice_soy.csv")
}

#Append growers from one dataset to the other

  
# cust_df_soy <-  plyr::rbind.fill(cust_df_soy, cust_df[!(cust_df_soy$GrowerSAPID %in% cust_df$GrowerSAPID), 
#                                                      c("TERR_ID", "SeedsmanSAPID", "GrowerSAPID", "Year", "REGION_ID", "REGION", "TEAM_ID", "fips_code", 
#                                                        "GrowerAcctNm", "SeedsmanAcctNm", "RBD", "ABM", "ABM.ID", "FSR", "FSR.ID", "DLR_NAME", 
#                                                        "GRWR_ACCT_NAME", "CORN_ZONE_DESCR", "FSR_NAME", "Customer Account Name | Customer ID",
#                                                        "Seedsman Account Name | Seedsman ID", "FSR Name | FSR Territory ID")])
# 
# cust_df <- plyr::rbind.fill(cust_df, cust_df_soy[!(cust_df$GrowerSAPID %in% cust_df_soy$GrowerSAPID), 
#                                                  c("TERR_ID", "SeedsmanSAPID", "GrowerSAPID", "Year", "REGION_ID", "REGION", "TEAM_ID", "fips_code", 
#                                                    "GrowerAcctNm", "SeedsmanAcctNm", "RBD", "ABM", "ABM.ID", "FSR", "FSR.ID", "DLR_NAME", 
#                                                    "GRWR_ACCT_NAME", "CORN_ZONE_DESCR", "FSR_NAME", "Customer Account Name | Customer ID",
#                                                    "Seedsman Account Name | Seedsman ID", "FSR Name | FSR Territory ID")])
# 

# Load customer and effective price dataframes (corn + soy)
# CORN -----------------------------------------------------------------------------------------------
# cust_df <- setDF(read_feather('./Data/cust_df.feather'))
# EffPrice <- setDF(read_feather('./Data/EffPrice.feather'))
cust_df_displaynames <- cust_df[ which(cust_df$SeedsmanSAPID %in% active_seedsmen$SAP_ID),] #Keeping only seedsmen in active seedsmen list for display
# SOY ------------------------------------------------------------------------------------------------
# cust_df_soy <- setDF(read_feather('./Data/cust_df_soy.feather'))
# EffPrice_soy <- setDF(read_feather('./Data/EffPrice_soy.feather'))
cust_df_soy_displaynames <- cust_df_soy[ which(cust_df_soy$DLR_SAP_ID %in% active_seedsmen$SAP_ID),] #Keeping only seedsmen in active seedsmen list for display



# cust_df_soy <- cust_df
# cust_df_soy_displaynames <- cust_df
# print(cust_df[100,])
# print(cust_df_soy[100,])
# print(EffPrice[100,])
# print(EffPrice_soy[100,])

# Load model files
# # CORN -----------------------------------------------------------------------------------------------
# discountModel <- read_rds(paste0(discountModelPath, discountModelName))
# epModel <- read_rds(paste0(discountModelPath, epPointModelName))
# discountPointModel <- read_rds(paste0(discountModelPath, discountPointModelName))
# # SOY ------------------------------------------------------------------------------------------------
# discountModelSoy <- read_rds(paste0(discountModelPath, discountModelNameSoy))
# epModelSoy <- read_rds(paste0(discountModelPath, epPointModelNameSoy))
# discountPointModelSoy <- read_rds(paste0(discountModelPath, discountPointModelNameSoy))

# choices <- append("", "New Customer")
css <- ".shiny-input-container > label {margin-bottom: -10px;}"

# Grower report ####################################################################################
# CORN ---------------------------------------------------------------------------------------------
generate_report_args <- list(discountPointModel,
                             cust_df,
                             reportAggCols,
                             reportAggCleanNames,
                             reportAggColOrder,
                             features,
                             epModel,
                             features_effP,
                             EffPrice,
                             epReportAggCols,
                             priceColumn,
                             quantityColumn)

grower_report <- wrapMemoise(generate_report_raw, generate_report, generate_report_args)

# SOY -----------------------------------------------------------------------------------------------
generate_report_args_soy <- list(discountPointModelSoy,
                                 cust_df_soy,
                                 reportAggColsSoy,
                                 reportAggCleanNamesSoy,
                                 reportAggColOrderSoy,
                                 featuresSoy,
                                 epModelSoy,
                                 features_EffPSoy,
                                 EffPrice_soy,
                                 epReportAggColsSoy,
                                 priceColumnSoy,
                                 quantityColumnSoy)

grower_report_soy <- wrapMemoise(generate_report_raw, generate_report, generate_report_args_soy)

discount_table <- get_feature_importance(discountPointModel,
                                         nFeaturesDisplay)
discount_table <- describe_features(discount_table,
                                    featDict)

rest_of_disc_features <- get_rest_of_features(discountPointModel,
                                              nFeaturesDisplay)
rest_of_disc_features <- describe_features(rest_of_disc_features,
                                           featDict)


# get displayed features and describe them
ep_table <- get_feature_importance(epModel,
                                   nFeaturesDisplay)
ep_table <- describe_features(ep_table,
                              featDict)
# get hidden features and describe them
rest_of_ep_features <- get_rest_of_features(epModel,
                                            nFeaturesDisplay)
rest_of_ep_features <- describe_features(rest_of_ep_features,
                                         featDict)


# get displayed features and describe them

discount_table_soy <- get_feature_importance(discountPointModelSoy,
                                             nFeaturesDisplay)

discount_table_soy <- describe_features(discount_table_soy,
                                        featDict)

rest_of_disc_features_soy <- get_rest_of_features(discountPointModelSoy,
                                                  nFeaturesDisplay)
rest_of_disc_features_soy <- describe_features(rest_of_disc_features_soy,
                                               featDict)

# get displayed features and describe them
ep_table_soy <- get_feature_importance(epModelSoy,
                                       nFeaturesDisplay)
ep_table_soy <- describe_features(ep_table_soy,
                                  featDict)
# get hidden features and describe them
rest_of_ep_features_soy <- get_rest_of_features(epModelSoy,
                                                nFeaturesDisplay)
rest_of_ep_features_soy <- describe_features(rest_of_ep_features_soy,
                                             featDict)

# set up user list for logging
User_List <- setDF(read_csv('./recorded_user_data/MARS_USER_LIST.csv'))
User_List_Access_Email <- "chiawei.yeh.ext@bayer.com"
TimeZone <- "US/Central"

# populate initial drop-down lists and select fsr, seedsman (corn + soy)

choices_fsr <- sort(unique(cust_df_displaynames$`FSR Name | FSR Territory ID`))
selected_fsr <- choices_fsr[1]
choices_seedsman <-cust_df_displaynames[which(cust_df_displaynames$`FSR Name | FSR Territory ID` == selected_fsr),'Seedsman Account Name | Seedsman ID']
selected_seedsman <- choices_seedsman[[1]][1]
choices_cust <- append(choices, cust_df_displaynames[cust_df_displaynames[[seedsmanIDColumn]] == get_id(selected_seedsman),'Customer Account Name | Customer ID'])
cust_list <- cust_df_displaynames[cust_df_displaynames[[seedsmanIDColumn]] == get_id(selected_seedsman),'Customer Account Name | Customer ID']
selected_cust <- cust_list[[1]][1]
#selected_fips <- subset(cust_df$Customer, State == selected_cust)
choices_newcust <- append("", unique(stateCountyToFips$statename))
#choices_product <- products_df$Product

choices_fsr_soy <- sort(unique(cust_df_soy_displaynames$`FSR Name | FSR Territory ID`))
selected_fsr_soy <- choices_fsr_soy[1]
choices_seedsman_soy <-cust_df_soy_displaynames[which(cust_df_soy_displaynames$`FSR Name | FSR Territory ID` == selected_fsr_soy),'Seedsman Account Name | Seedsman ID']
selected_seedsman_soy <- choices_seedsman_soy[[1]][1]
choices_cust_soy <- append(choices, cust_df_soy_displaynames[cust_df_soy_displaynames[[seedsmanIDColumnSoy]] == get_id(selected_seedsman_soy),'Customer Account Name | Customer ID'])
choices_newcust_soy <- append("", unique(stateCountyToFips$statename))
choices_product_soy <- products_df_soy$Product

#define products dropdown list based on cust_id
cust_id <- get_id(selected_cust)
fips_code_comp <- as.numeric(as.character(unlist(cust_df[cust_df[[custIDColumn]] == cust_id, fipsCodeColumn])))[1]
cust_zone <- data.frame(zoneLookUp)[zoneLookUp$fips == fips_code_comp, 'Corn_AZR']
recent_zone_pricing <- recentPricing[recentPricing$`Zone Label` == cust_zone, c('Product', 'Price')]
products_df <<- make_products_df_zone(recent_zone_pricing)
choices_product <- products_df$Product


# MEMOIZE FSR/SEEDSMAN TAB PLOT/TABLE DEPENDENCIES (corn + soy)
grower_report_updated_1_raw <- memoise(grower_report_updated_1_raw0, cache = cache_db_lcr)
grower_report_updated_2_raw <- memoise(grower_report_updated_2_raw0, cache = cache_db_lcr)
grower_report_stats_raw <- memoise(grower_report_stats_raw0, cache = cache_db_lcr)

seedsmanInfo1function <- memoise(seedsmanInfo1function_raw , cache = cache_db_lcr)
seedsmanInfo2function <- memoise(seedsmanInfo2function_raw , cache = cache_db_lcr)

LCRTotals_raw <- memoise(LCRTotals_raw0, cache = cache_db_lcr)
LCRTotals3_raw <- memoise(LCRTotals3_raw0, cache = cache_db_lcr)
LCRTotals4_raw <- memoise(LCRTotals4_raw0, cache = cache_db_lcr)
LCRTotals5_raw <- memoise(LCRTotals5_raw0, cache = cache_db_lcr)
LCRTotals5B_raw <- memoise(LCRTotals5B_raw0, cache = cache_db_lcr)
LCRTotals6_raw <- memoise(LCRTotals6_raw0, cache = cache_db_lcr)
LCRTotals7_raw <- memoise(LCRTotals7_raw0, cache = cache_db_lcr)
LCRTotals8_raw <- memoise(LCRTotals8_raw0, cache = cache_db_lcr)

dtLCRDF <- memoise(dtLCRDF_raw, cache = cache_db_lcr)
dtLCRDF4 <- memoise(dtLCRDF4_raw, cache = cache_db_lcr)
dtLCRDF5 <- memoise(dtLCRDF5_raw, cache = cache_db_lcr)
dtLCRDF6 <- memoise(dtLCRDF6_raw, cache = cache_db_lcr)
dtLCRDF7 <- memoise(dtLCRDF7_raw, cache = cache_db_lcr)
dtLCRDF8 <- memoise(dtLCRDF8_raw, cache = cache_db_lcr)

selected_fsr_init <- unlist(strsplit(selected_fsr, split=' \\| '))[1]
LCRTotals_raw_args <- list(selected_fsr_init, grower_report, 'corn')
LCRTotals0 <- wrapMemoise(LCRTotals_raw0, LCRTotals_raw, LCRTotals_raw_args)

selected_fsr_init_soy <- unlist(strsplit(selected_fsr_soy, split=' \\| '))[1]
LCRTotals_raw_args_soy <- list(selected_fsr_init_soy, grower_report_soy, 'soy')
LCRTotals0_soy <- wrapMemoise(LCRTotals_raw0, LCRTotals_raw, LCRTotals_raw_args_soy)


User_List <- setDF(read_csv('./recorded_user_data/MARS_USER_LIST.csv'))
User_List_Access_Email <- "chiawei.yeh.ext@bayer.com"
TimeZone <- "US/Central"

if(file.exists("./recorded_user_data/UserLog.csv") == FALSE){
  df <- data.frame(CWID = character(0),START_TIME = numeric(0), URL_PATHNAME = integer(0), URL_SEARCH = integer(0))
  file.lock = lock("./recorded_user_data/UserLog.csv.lock")
  write.table(df, file="./recorded_user_data/UserLog.csv", sep=",", row.names=FALSE, 
              col.names=TRUE)
  unlock(file.lock)
}

if(file.exists("./recorded_user_data/UserLogFinal.csv") == FALSE){
  df <- data.frame(CWID = character(0), START_TIME = character(0), END_TIME = character(0), TOTAL_TIME = numeric(0), URL_PATHNAME = integer(0), URL_SEARCH = integer(0))
  file.lock = lock("./recorded_user_data/UserLogFinal.csv.lock")
  write.table(df, file="./recorded_user_data/UserLogFinal.csv", sep=",", row.names=FALSE, 
              col.names=TRUE)
  unlock(file.lock)
}


#read in UserLog for KPI Metrics tab
UserLogData <- setDF(read.csv('./recorded_user_data/UserLog.csv', header = TRUE))
UserDates1 <- str_split_fixed(UserLogData$START_TIME, " ", 3)
colnames(UserDates1) <- c("Date1", "Time1", "TimeZone")
UserLogData <- cbind(UserLogData, UserDates1)
UserLogData$CWID <- as.factor(UserLogData$CWID)
UserLogData$Date1 <- as.Date(UserLogData$Date1, format = "%Y-%m-%d")

#Daily unique Visits
DailyVisits <- aggregate(UserLogData$CWID ~ Date1, data=UserLogData, FUN=function(x) length(unique(x)))
colnames(DailyVisits) <- c("Date", "Unique_Users")

#Overall total number of hits to the site
TotalVisits <- aggregate(UserLogData$CWID ~ Date1, data=UserLogData, FUN=function(x) length(x))
colnames(TotalVisits) <- c("Date", "Total_Users")



#Read in user log to get times of usage
UserLogDataFinal <- setDF(read.csv('./recorded_user_data/UserLogFinal.csv', header = TRUE))

#break up date/time string into separate parts
UserDates1 <- str_split_fixed(UserLogDataFinal$START_TIME, " ", 3)
colnames(UserDates1) <- c("Date1", "Time1", "TimeZone")
UserLogDataFinal <- cbind(UserLogDataFinal, UserDates1)

#break up date/time string into separate parts
UserDates2 <- str_split_fixed(UserLogDataFinal$END_TIME, " ", 3)
colnames(UserDates2) <- c("Date2", "Time2", "TimeZone")
UserLogDataFinal <- cbind(UserLogDataFinal, UserDates2)

UserTimes <- str_split_fixed(UserLogDataFinal$TOTAL_TIME, " ", 2)

UserTimes2 <- str_split_fixed(UserTimes[,2], ":", 3)

#Convert days hours minutes seconds to all minutes
UserTimesMinutes <- cbind(as.numeric(UserTimes[,1])*86400, as.numeric(UserTimes2[,1])*60, as.numeric(UserTimes2[,2]), as.numeric(UserTimes2[,3])/60)
UserTimesMinutes <- as.data.frame(UserTimesMinutes)

#Sum all UserTimesMinutes
TotalTime <-rowSums(UserTimesMinutes)


UserLogDataFinal <- cbind(UserLogDataFinal, TotalTime)

DailyTotalTime <- aggregate(UserLogDataFinal$TotalTime ~ Date1, data=UserLogDataFinal, FUN=sum)
colnames(DailyTotalTime) <- c("Date", "Total_Time")

DailyMeanTime <- aggregate(UserLogDataFinal$TotalTime ~ Date1, data=UserLogDataFinal, FUN=mean)
colnames(DailyMeanTime) <- c("Date", "Mean_Time")

TotalSDTime <- aggregate(UserLogDataFinal$TotalTime ~ Date1, data=UserLogDataFinal, FUN=sd)
colnames(TotalSDTime) <- c("Date", "SD_Time")
TotalSDTime[is.na(TotalSDTime)] <- 0
DailyMeanTime <- merge(x = DailyMeanTime, y = TotalSDTime, by = "Date", all.x = TRUE)

jscode <- "shinyjs.closeWindow = function() { window.close(); }"


 },
error=function(e){
  err_id <- sample(1000000000:9999999999, 1) #create error id
  write_error(e,err_id) #write error to txt file
}
)

# UI =================================================================================================
ui <- fluidPage(
  # UI inactivity functionality
  tags$head(
    HTML(
      "
      <script>
      var socket_timeout_interval
      var n = 0
      $(document).on('shiny:connected', function(event) {
      socket_timeout_interval = setInterval(function(){
      Shiny.onInputChange('count', n++)
      }, 60000)
      });
      $(document).on('shiny:disconnected', function(event) {
      clearInterval(socket_timeout_interval)
      });
      </script>
      "
    )
  ),
  
  sidebarLayout(
  sidebarPanel(width = 3,
    ###Common Sidebar Panels
    tags$style(".well {background-color:#5f6670;}"), # background color of sidebar
    
    # selectInput for FSR name -- common
    get_dropdown('fsr_name', "<p><span style='color: #C3CE44'>Select FSR Name <br/>| FSR Territory ID</span></p>",
                 choices_fsr, selected_fsr),
    # selectInput for seedsman ID -- common
    get_dropdown('seedsman_name', "<p><span style='color: #C3CE44'>Seedsman Account Name <br/>| Seedsman ID</span></p>",
                 choices_seedsman, selected_seedsman),
    
    # add choices for customer name -- common
    get_dropdown("cust_name", "<p><span style='color: #C3CE44'>Customer Account Name <br/>| Customer ID</span></p>",
                 choices_cust, ""),
    
    # Drop-down button for add new customer -- common
    fluidRow(align = "center", column(12, dropdownButton(inputId = 'add_new_customer',
                   label = 'New Customer Location',
                   icon = icon("map-marked-alt"),
                   circle=FALSE,
                   get_dropdown("state", "<p><span style='color: #000000'>State</span></p>",
                                choices_newcust, ""),
                   get_dropdown("county", "<p><span style='color: #000000'>County</span></p>",
                                NULL, "")
    ))),
    tags$style(type='text/css', "#add_new_customer { color: black; width: 100%; font-size: 12px;}"),
    br(),
    conditionalPanel(condition="input.tabselected==1",
                     # action button for pre-populating portfolio -- corn
                     fluidRow(align = "center", column(12,  actionButton(inputId = 'populate_cart',
                                                                         label = 'Pre-Populate Portfolio',
                                                                         icon = icon("shopping-basket")))),
                     tags$style(type='text/css', "#populate_cart { color: black; width: 100%; font-size: 12px;}"),
                     br(),
                     # Select Product name
                     get_dropdown("product_name", "<p><span style='color: #C3CE44'>Product Name</span></p>",
                                  choices_product, ""),
                     # numeric input for quantity -- corn
                     numericInput(inputId = "product_quantity",
                                  shiny::HTML("<p><span style='color: #C3CE44'>Quantity</span></p>"),
                                  value = 0,
                                  min = 0),
                     # action button for adding/clear product -- corn
                     fluidRow(column(6,  actionButton(inputId = "add_to_cart",
                                                                    label = "Add to Portfolio",
                                                                    icon = icon("plus-square"))),
                              column(6,  actionButton(inputId = "clear_cart",
                                                       label = "Clear Portfolio",
                                                       icon = icon("minus-square")))
                              ),
                     tags$style(type='text/css', "#add_to_cart { color: black; width: 100%; font-size: 12px;}"),
                     tags$style(type='text/css', "#clear_cart { color: black; width: 100%; font-size: 12px;}"),
                     br(),
                     #fluidRow(align = 'center', passwordInput("pass_corn", "Enter password to see KPI Metrics")),
                     passwordInput("pass_corn", shiny::HTML("<p><span style='color: #C3CE44'>Enter password to see KPI Metrics</span></p>")),
                     fluidRow(align = "center", column(12, actionButton(inputId = "enter_corn", 
                                                                        label = "Enter",
                                                                        icon = icon("table")))),
                     tags$style(type='text/css', "#enter_corn { color: black; width: 100%; font-size: 12px;}")
    ),
    conditionalPanel(condition="input.tabselected==2",
                     # select input for product -- soy
                     get_dropdown("product_name_soy", "<p><span style='color: #C3CE44'>Product Name</span></p>",
                                  choices_product_soy, ""),
                     # select input for quantity -- soy
                     numericInput(inputId = "product_quantity_soy",
                                  shiny::HTML("<p><span style='color: #C3CE44'>Quantity</span></p>"),
                                  value = 0,
                                  min = 0),
                     
                     # action button for adding product -- soy
                     fluidRow(align = "center", column(12, actionButton(inputId = "add_to_cart_soy",
                                                                    label = "Add to Portfolio",
                                                                    icon = icon("plus-square")))),
                     tags$style(type='text/css', "#add_to_cart_soy { color: black; width: 100%; font-size: 12px;}"),
                     br(),
                     # action button for clearing cart -- soy
                     fluidRow(align = "center", column(12, actionButton(inputId = "clear_cart_soy",
                                                                    label = "Clear Portfolio",
                                                                    icon = icon("minus-square")))),
                     tags$style(type='text/css', "#clear_cart_soy { color: black; width: 100%; font-size: 12px;}"),
                     br(),
                     # action button for pre populating portfolio--soy
                     fluidRow(align = "center", column(12, actionButton(inputId = 'populate_cart_soy',
                                                                    label = 'Pre-Populate Portfolio',
                                                                    icon = icon("shopping-basket")))),
                     tags$style(type='text/css', "#populate_cart_soy { color: black; width: 100%; font-size: 12px;}"),
                     br(),
                     # action button for recommend products -- soy
                     fluidRow(align = "center", column(12,  actionButton(inputId = 'recommend_prod_soy',
                                                                         label = 'Recommend Products',
                                                                         icon = icon("shopping-basket")))),
                     tags$style(type='text/css', "#populate_cart { color: black; width: 100%; font-size: 12px;}"),
                     
                     br(),
                     #fluidRow(align = "center", passwordInput("pass_soy", "Enter password to see KPI Metrics")),
                     passwordInput("pass_soy", shiny::HTML("<p><span style='color: #C3CE44'>Enter password to see KPI Metrics</span></p>")),
                     #tags$style(type='text/css', "#pass_soy { color: black; width: 90%; font-size: 12px;}"),
                     fluidRow(align = "center", column(12, actionButton(inputId = "enter_soy", 
                                                                        label = "Enter",
                                                                        icon = icon("table")))),
                     tags$style(type='text/css', "#enter_soy { color: black; width: 100%; font-size: 12px;}")
    ),
    conditionalPanel(condition = "input.tabselected==3",
                     width = 3, conpanelKPIUI("ConditionalPanel", DailyVisits))
  ),
  mainPanel(width = 9, useShinyjs(),useShinyalert(), extendShinyjs(text = jscode, functions = c("closeWindow")),
            navbarPage(#title = #"",
              # mainPanel( navbarPage(id = "navbarui",
              "MARS Market Funding Recommender System",
              header = tagList(
                useShinydashboard(),  # allows use of shinydashboard css to render info boxes
                shinyjs::useShinyjs(),
                tags$style(HTML(css)),
                tags$style(HTML(HTML_style)
                ),
                # tags$style(HTML("
                #               .navbar-default .navbar-brand {color: white;}
                #               .navbar-default .navbar-brand:hover {color: white;}
                #               .navbar { background-color: #5f6670;}
                #               .navbar-default .navbar-nav > li > a {color:black;}
                #               .navbar-default .navbar-nav > .active > a,
                #               .navbar-default .navbar-nav > .active > a:focus,
                #               .navbar-default .navbar-nav > .active > a:hover {color: white;background-color: #000000;}
                #               .navbar-default .navbar-nav > li > a:hover {color: black;background-color:white; font-weight: bold}
                #               .navbar-default .navbar-nav > li > a[data-value='Corn'] {color: #C3CE44; font-size: 20px;}
                #               .navbar-default .navbar-nav > li > a[data-value='Soy'] {color: #C3CE44; font-size: 20px;}
                #               .navbar-default .navbar-nav > li > a[data-value='Feedback'] {color: #C3CE44; font-size: 20px;}
                #               ")
                # )
                tags$style(HTML("
                                .navbar-default .navbar-nav > li > a[data-value='Feedback'] {background-color: #C3CE44}
                                .navbar-default .navbar-nav {float: none !important;}
                                .navbar-default .navbar-nav > li:nth-child(4) {float: right;}
                "))
              ),
              
              # CORN ---------------------------------------------------------------------------------------------
              tabPanel("Corn", value=1, 
                       # main display panel on the right -- corn
                       #               mainPanel(
                       # add busy spinner for when server is going
                       add_busy_spinner(spin='fading-circle'),
                       #infoboxes along the top of the page -- corn
                       fluidRow(
                         # infoBoxOutput("Total.Retail.Amount"),
                         # tags$style("#Total.Retail.Amount {width:33%;}"),
                         infoBoxOutput("Rec.LCR.Unit"),
                         tags$style("#Rec.LCR.Unit {width:50%;}"),
                         infoBoxOutput("Rec.LCR.Pct"),
                         tags$style("#Rec.LCR.Pct {width:50%;}")
                         #             )
                       ),
                       fluidRow(
                         #tabs in the main panel
                         tabBox(width = NULL,
                                #LCR information tab
                                get_FSR_LCR_Tab("corn"),
                                get_Seedsman_Tab("corn"),
                                get_Cust_Info_Tab("corn"),
                                get_Cust_Portfolio_Tab("corn"),
                                get_Prodoct_Recommend_Tab("corn"),
                                #get_Model_Summary_Tab("corn"),
                                tabPanel("Model Summary", 
                                        featuresUI("corn_features", "Show Additional Predictors", "Hide Additional Predictors"),
                                ),
                                tabPanel("About", 
                                         includeHTML("www/mars_instructions.html"))
                                #get_Feedback_Tab("corn")
                         )
                         
                       ),
              ),
              tabPanel("Soy", value=2, 
                       # main display panel on the right -- corn
                       #               mainPanel(
                       # add busy spinner for when server is going
                       add_busy_spinner(spin='fading-circle'),
                       #infoboxes along the top of the page -- corn
                       fluidRow(
                         # infoBoxOutput("Total.Retail.Amount_soy"),
                         # tags$style("#Total.Retail.Amount_soy {width:33%;}"),
                         infoBoxOutput("Rec.LCR.Unit_soy"),
                         tags$style("#Rec.LCR.Unit_soy {width:50%;}"),
                         infoBoxOutput("Rec.LCR.Pct_soy"),
                         tags$style("#Rec.LCR.Pct_soy {width:50%;}")
                       ),
                       
                       fluidRow(
                         #tabs in the main panel
                         tabBox(width = NULL,
                                #LCR information tab -- soy
                                get_FSR_LCR_Tab("soy"),
                                get_Seedsman_Tab("soy"),
                                get_Cust_Info_Tab("soy"),
                                get_Cust_Portfolio_Tab("soy"),
                                #get_Prodoct_Recommend_Tab("soy"),
                                tabPanel("Model Summary", 
                                         featuresUI("soy_features", "Show Additional Predictors", "Hide Additional Predictors"),
                                ),                                
                                tabPanel("About", 
                                         includeHTML("www/mars_instructions.html"))
                                #get_Feedback_Tab("soy")
                         ) #end tabBox
                       ), # end fluid row
                       
              ), 
              shinyjs::hidden(div(id= 'hide_close',
                                  tabPanel(actionLink("close"," Close", icon = icon("close"))),
              )),
              tabPanel("Feedback", 
                       # main display panel on the right -- corn
                       #               mainPanel(
                       # add busy spinner for when server is going
                       add_busy_spinner(spin='fading-circle'),
                       #infoboxes along the top of the page -- corn
                       # fluidRow(
                       #   # infoBoxOutput("Total.Retail.Amount"),
                       #   # tags$style("#Total.Retail.Amount {width:33%;}"),
                       #   infoBoxOutput("Rec.LCR.Unit"),
                       #   tags$style("#Rec.LCR.Unit {width:50%;}"),
                       #   infoBoxOutput("Rec.LCR.Pct"),
                       #   tags$style("#Rec.LCR.Pct {width:50%;}")
                       #   #             )
                       # ),
                       fluidRow(
                         #tabs in the main panel
                         tabBox(width = NULL,
                                get_Feedback_Tab("corn")
                         )
                         
                       ),
                       span(textOutput("keepAlive"), style="color:white")
                       
              ),
              id = "tabselected"
            )
  )))
# Server =============================================================================================
server <- function(input, output, session) {
  
  # Server end timeout functionality -----------------------------------------------------------------
  output$keepAlive <- renderText({
    req(input$count)
    paste(input$count)
  })
  
  zz <- file(paste0("./recorded_user_data/error_logs/err_report_", Sys.getpid(), '_', session$token,".Rout"), open = "wt")
  sink(file = zz, append = TRUE, type = "message",
       split = FALSE)
  
  dataModal <- function(failed = FALSE) {
    modalDialog(h4('Thank you for using MARS. We\'d like to hear from you. Please take a moment to rate your current level of satisfaction with the app ...', align = 'center'),
                shinyUI(bootstrapPage(
                  h3(ratingInput("feedback1", label="", dataStop=5, dataFractions=2), style = "color:#3399ff;padding-left:30px;", align = 'center')
                )),
                h4('and a few minutes to complete a brief survey.', align = 'center'),
                h3(actionButton("modal_feedback", "Go to survey"),align = 'center'),
                h4('Your feedback will be used to improve the MARS tool.', align = 'center'),
                br(),
                h4('Kind regards,'),
                br(),
                #h4('Charles Yeh (chiawei.yeh.ext@bayer.com)'),
                h4('MARS technical support'),
                
                footer = tagList(
                  modalButton("Cancel"),
                  actionButton("modal_logout", "Close")
                )
    )
  }
  
  dataModal_starsonly <- function(failed = FALSE) {
    modalDialog(h4('Thank you for using MARS. Please take a moment to rate your current level of satisfaction with the app.', align = 'center'),
                shinyUI(bootstrapPage(
                  h3(ratingInput("feedback1", label="", dataStop=5, dataFractions=2), style = "color:#3399ff;padding-left:30px;", align = 'center')
                )),
                br(),
                h4('Note: you can click over to the Feedback tab at any time to tell us about your experience with the app. We\'d like to hear (e.g.) about any issues you\'ve encountered or ideas you have for improving MARS.', align = 'center'),
                br(),
                img(src='feedback_screenshot.png', width="100%", style="display: block; margin-left: auto; margin-right: auto;"),
                br(),
                h4('Kind regards,'),
                br(),
                #h4('Charles Yeh (chiawei.yeh.ext@bayer.com)'),
                h4('MARS technical support'),
                
                footer = tagList(
                  modalButton("Cancel"),
                  actionButton("modal_logout", "Close")
                )
    )
  }

  dailyuserfp <- paste0('./recorded_user_data/feedback/daily_users/users_',Sys.Date(), '.csv')
  if(file.exists(dailyuserfp) == FALSE) {
    df_dailyuser <- data.frame(user=character())
    write.csv(df_dailyuser, dailyuserfp, row.names = FALSE)
  } else {
    df_dailyuser <- read.csv(dailyuserfp)
  }
  #df_userfeedback <- read.csv('./recorded_user_data/feedback/user_feedback_list.csv')
  if(paste0('user_', isolate(session$user)) %in% df_dailyuser$user) {
    #do nothing
  } else{
    write.table(paste0('user_',isolate(session$user)),
                file= dailyuserfp,
                append = T,
                sep='\n',
                row.names=F,
                col.names=F )
    write.table( paste0('user_', isolate(session$user)),  
                 file="./recorded_user_data/feedback/user_feedback_list.csv",
                 append = T,
                 sep='\n',
                 row.names=F,
                 col.names=F )
    #if(paste0('user_', isolate(session$user)) %in% df_userfeedback$user) {
    df_userfeedback <- read.csv('./recorded_user_data/feedback/user_feedback_list.csv')
    nvisits <- length(df_userfeedback[which(df_userfeedback$user == paste0('user_', isolate(session$user))),])
    if(nvisits %in% c(2,6,11)) {
      showModal(dataModal())
    } else if (nvisits > 1) {
      showModal(dataModal_starsonly())
    }
  }
  
  # observeEvent(input$close, {
  #   df_userfeedback <- read.csv('./recorded_user_data/feedback/user_feedback_list.csv')
  #   # print(df_userfeedback$user)
  #   # print(paste0('user_', isolate(session$user)) %in% df_userfeedback$user)
  #   if(paste0('user_', isolate(session$user)) %in% df_userfeedback$user) {
  #     #if(TRUE) {
  #     if(feedback_tab_generated == FALSE) {
  #       showModal(dataModal_starsonly())
  #     } else {
  #       js$closeWindow()
  #       stopApp()
  #     }
  #   } else {
  #     write.table( paste0('user_', isolate(session$user)),  
  #                  file="./recorded_user_data/feedback/user_feedback_list.csv", 
  #                  append = T, 
  #                  sep='\n', 
  #                  row.names=F, 
  #                  col.names=F )
  #     showModal(dataModal())
  #   }
  # })

  observeEvent(input$modal_logout, {
    f1 <- paste0('Q1__', input$feedback1)
    resps <- c(f1)
    
    rs <- c()
    qs <- c()
    for (i in 1:length(resps)) {
      r <- substr(resps[i],5,nchar(resps[i]))
      q <- gsub("_","",substr(resps[i],1,3))
      rs <- c(rs,r)
      qs <- c(qs,q)
    }
    df_feedback <- data.frame(question = qs, response = rs)
    sys_time <- gsub(" ", "_", Sys.time())
    write.csv(df_feedback, paste0('./recorded_user_data/feedback/user_',isolate(session$user), '_',sys_time,'.csv'),row.names = FALSE)
    removeModal()
    # js$closeWindow()
    # stopApp()
  })
  
  observeEvent(input$modal_feedback, {
    # appendTab(inputId = "tabselected",
    #           get_Feedback_Tab('corn'),
    #           select=TRUE)
    updateNavbarPage(session, "tabselected",
                      selected = "Feedback"
    )
    removeModal()
  })
  
  
  #What happens when submit button is clicked on Feedback tab
  ## CORN =====================================
  observeEvent(input$feedback_button, {
    f1 <- paste0('Q1__', input$feedback1)
    f2a <- paste0('Q2a_', input$feedback2a)
    f2b <- paste0('Q2b_', input$feedback2b)
    f3 <- paste0('Q3__', input$feedback3)
    f4 <- paste0('Q4__', input$feedback4)
    f5 <- paste0('Q5__', input$feedback5)
    f6a <- paste0('Q6a_', input$feedback6a)
    f6b <- paste0('Q6b_', input$feedback6b)
    f7a <- paste0('Q7a_', input$feedback7a)
    f7a2 <- paste0('Q7a_', input$feedback7a2)
    f7b <- paste0('Q7b_', input$feedback7b)
    f8 <- paste0('Q8__', input$feedback8)
    resps <- c(f1, f2a, f2b, f3, f4, f5, f6a, f6b, f7a, f7a2, f7b, f8)
    
    rs <- c()
    qs <- c()
    for (i in 1:length(resps)) {
      r <- substr(resps[i],5,nchar(resps[i]))
      q <- gsub("_","",substr(resps[i],1,3))
      rs <- c(rs,r)
      qs <- c(qs,q)
    }
    df_feedback <- data.frame(question = qs, response = rs)
    sys_time <- gsub(" ", "_", Sys.time())
    write.csv(df_feedback, paste0('./recorded_user_data/feedback/user_',isolate(session$user), '_',sys_time,'.csv'),row.names = FALSE)
    
    shinyjs::hide(id = "show_hide_feedback")
    shinyjs::hide(id = "show_hide_feedback_soy")
    shinyjs::show(id = "show_hide_thanks")
    shinyjs::show(id = "show_hide_thanks_soy")
    
  })
  ## SOY =====================================  
  observeEvent(input$feedback_button_soy, {
    f1 <- paste0('Q1__', input$feedback1_soy)
    f2a <- paste0('Q2a_', input$feedback2a_soy)
    f2b <- paste0('Q2b_', input$feedback2b_soy)
    f3 <- paste0('Q3__', input$feedback3_soy)
    f4 <- paste0('Q4__', input$feedback4_soy)
    f5 <- paste0('Q5__', input$feedback5_soy)
    f6a <- paste0('Q6a_', input$feedback6a_soy)
    f6b <- paste0('Q6b_', input$feedback6b_soy)
    f7a <- paste0('Q7a_', input$feedback7a_soy)
    f7b <- paste0('Q7b_', input$feedback7b_soy)
    f8 <- paste0('Q8__', input$feedback8_soy)
    resps <- c(f1, f2a, f2b, f3, f4, f5, f6a, f6b, f7a, f7b, f8)
    
    rs <- c()
    qs <- c()
    for (i in 1:length(resps)) {
      r <- substr(resps[i],5,nchar(resps[i]))
      q <- gsub("_","",substr(resps[i],1,3))
      rs <- c(rs,r)
      qs <- c(qs,q)
    }
    df_feedback <- data.frame(question = qs, response = rs)
    sys_time <- gsub(" ", "_", Sys.time())
    write.csv(df_feedback, paste0('./recorded_user_data/feedback/user_',isolate(session$user), '_',sys_time,'.csv'),row.names = FALSE)
    
    shinyjs::hide(id = "show_hide_feedback")
    shinyjs::hide(id = "show_hide_feedback_soy")
    shinyjs::show(id = "show_hide_thanks")
    shinyjs::show(id = "show_hide_thanks_soy")
    
  })
  
  #session$user <- "ELWYQ_v"
  CWID <- gsub("\\_v$", "", session$user)
  
  #Code to check for CWID permission
  #if(grepl(paste0("\\b", CWID, "\\b"), User_List, ignore.case=TRUE) == FALSE){ #abstract 964
  #  showNotification(paste(CWID ,"NOT ALLOWED, CONTACT", User_List_Access_Email,"FOR ACCESS", sep = ' '), action = NULL, type = "warning", duration = NULL)
  #  stopApp()
  #}
  
  #Get current time in Central time zone
  Time1 <- .POSIXct(Sys.time(), TimeZone)
  
  #Create row of log data
  userDataRow <- data.frame(t(c(CWID, 
                                paste(as.character(Time1), TimeZone, sep = ' '), 
                                isolate(session$clientData$url_pathname), 
                                isolate(session$clientData$url_search) )))
  
  #append row of log data to csv
  file.lock.UserLog <- flock::lock("./recorded_user_data/UserLog.csv")
  
  #file.lock.UserLog = lock("./recorded_user_data/UserLog.csv.lock")
  
  write.table(userDataRow, file = "./recorded_user_data/UserLog.csv", sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE, eol = "\n") #lock file, pathname should not be hardcoded
  #if(is.locked(file.lock.UserLog)) {
  #  print("Got the lock!")
  #}
  
  tryCatch(flock::unlock(file.lock.UserLog), finally = NULL)
  
  #unlock(file.lock.UserLog) #add tryfinally
  #rm(lockFile)
  passwordEntry <- reactive({
    list(input$enter_corn, input$enter_soy)
  })
  
  observeEvent(passwordEntry(), {
    if (input$pass_corn == "password123" | input$pass_soy == "password123") {
      appendTab(inputId = "tabselected", tab = 
                  tabPanel("KPI METRICS", value = 3,
                           mainPanel(width = 12,
                                     #The commented-out code here and below (ctrl + f: checkspec) 
                                     #allows the user to download the contents of user-specifed folders. 
                                     #The user specifications are made via checkboxes on the Downloads tab.
                                     #Those checkboxes however are currently 'commented out'.
                                     #dirx <-list.dirs("./",recursive=FALSE),
                                     #checkboxGroupInput("dir_boxes", tags$b("Select directories to download:"),
                                     #                   choices = gsub(".//","",list.dirs("./",recursive=FALSE))),
                                     br(), 
                                     fluidRow(
                                       tabsetPanel(type = "tabs",
                                                   tabPanel("Daily Unique Users", 
                                                            dailyvisitsKPIUI("DailyVisitsMetrics")
                                                   ),
                                                   
                                                   tabPanel("Daily Total Users", 
                                                           dailytotalusersKPIUI("DailyTotalUsers")
                                                   ),
                                                   
                                                   tabPanel("Daily Minutes Used", 
                                                          dailytotalminsKPIUI("DailyTotalMins")
                                                   ),
                                                   
                                                   tabPanel("Average Minutes Used", 
                                                         dailyavgminsKPIUI("DailyAvgMins")
                                                            
                                                   ),
                                                   
                                                   tabPanel("User Log", 
                                                           userlogKPIUI("DailyVisitsMetrics")
                                                            
                                                   )
                                       )
                                     )
                                     
                           )
                  ),
                
      )
    }
  })

  
  # KRIS FUNCTIONALITY FOR SEEDSMAN TAB ##############################################################
  # CORN ---------------------------------------------------------------------------------------------
  #append grower_report with some additional colums for seedsman info tab
  grower_report_updated_1 <- reactive({ 
    grower_report_updated_1_raw(fsrString(), seedsmanString(), grower_report) 
  })
  
  #aggregate seedsman for individual FSRs from grower_report
  grower_report_updated_2 <- reactive({ 
    grower_report_updated_2_raw(fsrString(), grower_report) 
  })
  
  #calculate grower_report stats
  grower_report_stats <- reactive({ 
    grower_report_stats_raw(grower_report) 
  })
  
  #create table to display seedsman info
  output$seedsmanInfo1 <- renderDT({ 
    seedsmanInfo1function(grower_report_updated_1(),fsrString()) 
  })
  
  
  #create table to display seedsman info
  output$seedsmanInfo2 <- renderDT({
    seedsmanInfo2function(grower_report_updated_2(),grower_report_stats(),fsrString())
  })
  
  #grab fsr as string
  fsrString <- callModule(get_fsrStringServer_app, NULL)
  fsrString_soy <- callModule(get_fsrStringServer_app, NULL)
  
  #grab seedsman as string
  seedsmanString <- callModule(get_seedsmanStringServer_app, NULL)
  seedsmanString_soy <- callModule(get_seedsmanStringServer_app, NULL)
  
  #compute budget run rate for corn given current fsr and attach it to output$runrate_corn
  #This output is not currently displayed anywhere in the app, but is anticipated to be deployed in v2
  budgetrunrate_corn <- callModule(get_budgetRunRateServer, NULL, runrate_df_corn, fsrString)
  budgetrunrate_soy <- callModule(get_budgetRunRateServer, NULL, runrate_df_soy, fsrString_soy)
  
  
  #define budget runrate output
  output$runrate_corn <- renderText({
    budgetrunrate_corn()
  })
  
  #append grower_report for plots (LCRTotals); then define outputs for plots tied to LCRTotals
  #these plots are plots that appear when UI loads
  #when UI is initially loaded, LCRTotals is read into global environment from cache
  #when FSR is updated, LCRTotals is read into session environment from cache if hash key is recognized
  #otherwise it is created from scratch
  observeEvent(fsrString(), {
    if(fsrString() == selected_fsr_init) {
      
      LCRTotals <- reactive({ LCRTotals0 })
      
      #Plot of "Chart A-Team"
      output$LCRPlot3 <- renderPlotly({ 
        plotlyLCRPlot3(LCRTotals()) 
      })
      
      
      #Plot of "Chart A-Team" part 2
      output$LCRPlot4 <- renderPlotly({ 
        plotlyLCRPlot4(LCRTotals()) 
      })
      
    }
    else {
      
      LCRTotals <- reactive({
        LCRTotals_raw_args <- list(fsrString(), grower_report, 'corn')
        wrapMemoise(LCRTotals_raw0, LCRTotals_raw, LCRTotals_raw_args)
      })
      
      #Plot of "Chart A-Team"
      output$LCRPlot3 <- renderPlotly({ 
        plotlyLCRPlot3(LCRTotals()) 
      })
      
      
      #Plot of "Chart A-Team" part 2
      output$LCRPlot4 <- renderPlotly({ 
        plotlyLCRPlot4(LCRTotals()) 
      })
      
    }
  },ignoreInit = FALSE)
  
  #append grower_report for plots
  LCRTotals3 <- reactive({ 
    LCRTotals3_raw_args <- list(fsrString(), grower_report, 'corn')
    wrapMemoise(LCRTotals3_raw0, LCRTotals3_raw, LCRTotals3_raw_args)
    #LCRTotals3_raw(fsrString(), grower_report) 
  })
  
  #append grower_report for plots
  LCRTotals4 <- reactive({ 
    LCRTotals4_raw_args <- list(fsrString(), grower_report, 'corn')
    wrapMemoise(LCRTotals4_raw0, LCRTotals4_raw, LCRTotals4_raw_args)
    #LCRTotals4_raw(fsrString(), grower_report) 
  })
  
  
  #append grower_report for plots  
  LCRTotals5 <- reactive({ 
    LCRTotals5_raw_args <- list(fsrString(), grower_report, 'corn')
    wrapMemoise(LCRTotals5_raw0, LCRTotals5_raw, LCRTotals5_raw_args)
    #LCRTotals5_raw(fsrString(), grower_report,'corn') 
  })
  
  
  #append grower_report for plots
  LCRTotals5B <- reactive({ 
    LCRTotals5B_raw_args <- list(fsrString(), grower_report, 'corn')
    wrapMemoise(LCRTotals5B_raw0, LCRTotals5B_raw, LCRTotals5B_raw_args)
    #LCRTotals5B_raw(fsrString(), grower_report,'corn') 
  })
  
  
  
  #append grower_report for plots
  LCRTotals6 <- reactive({ 
    LCRTotals6_raw_args <- list(fsrString(), grower_report, 'corn')
    wrapMemoise(LCRTotals6_raw0, LCRTotals6_raw, LCRTotals6_raw_args)
    #LCRTotals6_raw(fsrString(), grower_report) 
  })
  
  
  #append grower_report for plots  
  LCRTotals7 <- reactive({ 
    LCRTotals7_raw_args <- list(fsrString(), grower_report, 'corn')
    wrapMemoise(LCRTotals7_raw0, LCRTotals7_raw, LCRTotals7_raw_args)
    #LCRTotals7_raw(fsrString(), grower_report) 
  })
  
  
  #append grower_report for plots 
  LCRTotals8 <- reactive({ 
    LCRTotals8_raw_args <- list(fsrString(), grower_report, 'corn')
    wrapMemoise(LCRTotals8_raw0, LCRTotals8_raw, LCRTotals8_raw_args)
    #LCRTotals8_raw(fsrString(), grower_report) 
  })
  
  
  #create table to display seedsman info
  output$LCRDF <- renderDT({ 
    dtLCRDF_args <- list(fsrString(), LCRTotals3(), 'CORN')
    wrapMemoise(dtLCRDF_raw, dtLCRDF, dtLCRDF_args)
    #dtLCRDF(fsrString(), LCRTotals3()) 
  })
  
  
  #create table to display seedsman info 
  output$LCRDF4 <- renderDT({ 
    dtLCRDF4_args <- list(fsrString(), LCRTotals4(), 'CORN')
    wrapMemoise(dtLCRDF4_raw, dtLCRDF4, dtLCRDF4_args)
    #dtLCRDF4(fsrString(), LCRTotals4()) 
  })
  
  
  #create table to display seedsman info
  output$LCRDF5 <- renderDT({ 
    dtLCRDF5_args <- list(fsrString(), LCRTotals5B(), 'CORN')
    wrapMemoise(dtLCRDF5_raw, dtLCRDF5, dtLCRDF5_args)
    #dtLCRDF5(fsrString(), LCRTotals5B()) 
  })
  
  
  output$LCRDF6 <- renderDT({ 
    dtLCRDF6_args <- list(fsrString(), LCRTotals6(), 'CORN')
    wrapMemoise(dtLCRDF6_raw, dtLCRDF6, dtLCRDF6_args)
    #dtLCRDF6(fsrString(), LCRTotals6()) 
  })
  
  
  output$LCRDF7 <- renderDT({ 
    dtLCRDF7_args <- list(fsrString(), LCRTotals7(), 'CORN')
    wrapMemoise(dtLCRDF7_raw, dtLCRDF7, dtLCRDF7_args)
    #dtLCRDF7(fsrString(), LCRTotals7()) 
  })
  
  
  output$LCRDF8 <- renderDT({
    dtLCRDF8_args <- list(fsrString(), LCRTotals8(), 'CORN')
    wrapMemoise(dtLCRDF8_raw, dtLCRDF8, dtLCRDF8_args)
    #dtLCRDF8(fsrString(), LCRTotals8()) 
  })
  
  
  #Plot of "Chart B-Team"
  output$LCRPlot5 <- renderPlotly({
    plotlyLCRPlot5(LCRTotals3())
  })
  
  
  #Plot of "Chart C-Team"
  output$LCRPlot6 <- renderPlotly({ 
    plotlyLCRPlot6(LCRTotals4()) 
  })
  
  
  
  #Plot of "Chart D-Team"
  output$LCRPlot7 <- renderPlotly({ 
    plotlyLCRPlot7(LCRTotals5())
  })
  
  
#Code Modules for KPI Metrics tab
  DateRange <- callModule(conpanelKPIServer, "ConditionalPanel") #Get dates from date dropdown
  callModule(dailyvisitsKPIServer, "DailyVisitsMetrics", DailyVisits, UserLogData, DateRange)
  callModule(dailytotalusersKPIServer, "DailyTotalUsers", TotalVisits, DateRange)
  callModule(dailytotalminsKPIServer, "DailyTotalMins", DailyTotalTime, DateRange)
  callModule(dailyavgminsKPIServer, "DailyAvgMins", DailyMeanTime, DateRange)
  
  # SOY ----------------------------------------------------------------------------------------------
  # This is the functionality duplicated above for the soy tab, suffixed accordingly with '_soy'
  
  #append grower_report with some additional colums for seedsman info tab
  grower_report_updated_1_soy <- reactive({ 
    grower_report_updated_1_raw(fsrString_soy(), seedsmanString_soy(), grower_report_soy) 
  })
  
  #aggregate seedsman for individual FSRs from grower_report
  grower_report_updated_2_soy <- reactive({ 
    grower_report_updated_2_raw(fsrString_soy(), grower_report_soy) 
  })
  
  #calculate grower_report stats
  grower_report_stats_soy <- reactive({ 
    grower_report_stats_raw(grower_report_soy) 
  })
  
  #create table to display seedsman info
  output$seedsmanInfo1_soy <- renderDT({ 
    seedsmanInfo1function(grower_report_updated_1_soy(),fsrString_soy()) 
  })
  
  
  #create table to display seedsman info
  output$seedsmanInfo2_soy <- renderDT({
    seedsmanInfo2function(grower_report_updated_2_soy(),grower_report_stats_soy(),fsrString_soy())
  })
  

  #define budget runrate output
  output$runrate_soy <- renderText({budgetrunrate_soy()})
  
  
  #append grower_report for plots
  LCRTotals_soy <- reactive({
    LCRTotals_raw_args_soy <- list(fsrString_soy(), grower_report_soy, 'soy')
    wrapMemoise(LCRTotals_raw0, LCRTotals_raw, LCRTotals_raw_args_soy)
    #LCRTotals_raw(fsrString_soy(), grower_report_soy)
  })
  
  #append grower_report for plots
  LCRTotals3_soy <- reactive({ 
    LCRTotals3_raw_args_soy <- list(fsrString_soy(), grower_report_soy, 'soy')
    wrapMemoise(LCRTotals3_raw0, LCRTotals3_raw, LCRTotals3_raw_args_soy)
    #LCRTotals3_raw(fsrString_soy(), grower_report_soy) 
  })
  
  #append grower_report for plots
  LCRTotals4_soy <- reactive({ 
    LCRTotals4_raw_args_soy <- list(fsrString_soy(), grower_report_soy, 'soy')
    wrapMemoise(LCRTotals4_raw0, LCRTotals4_raw, LCRTotals4_raw_args_soy)
    #LCRTotals4_raw(fsrString_soy(), grower_report_soy) 
  })
  
  
  #append grower_report for plots  
  LCRTotals5_soy <- reactive({ 
    LCRTotals5_raw_args_soy <- list(fsrString_soy(), grower_report_soy, 'soy')
    wrapMemoise(LCRTotals5_raw0, LCRTotals5_raw, LCRTotals5_raw_args_soy)
    #LCRTotals5_raw(fsrString_soy(), grower_report_soy,'soy') 
  })
  
  
  #append grower_report for plots
  LCRTotals5B_soy <- reactive({ 
    LCRTotals5B_raw_args_soy <- list(fsrString_soy(), grower_report_soy, 'soy')
    wrapMemoise(LCRTotals5B_raw0, LCRTotals5B_raw, LCRTotals5B_raw_args_soy)
    #LCRTotals5B_raw(fsrString_soy(), grower_report_soy,'soy') 
  })
  
  
  
  #append grower_report for plots
  LCRTotals6_soy <- reactive({
    LCRTotals6_raw_args_soy <- list(fsrString_soy(), grower_report_soy, 'soy')
    wrapMemoise(LCRTotals6_raw0, LCRTotals6_raw, LCRTotals6_raw_args_soy)
    #LCRTotals6_raw(fsrString_soy(), grower_report_soy) 
  })
  
  
  #append grower_report for plots  
  LCRTotals7_soy <- reactive({ 
    LCRTotals7_raw_args_soy <- list(fsrString_soy(), grower_report_soy, 'soy')
    wrapMemoise(LCRTotals7_raw0, LCRTotals7_raw, LCRTotals7_raw_args_soy)
    #LCRTotals7_raw(fsrString_soy(), grower_report_soy) 
  })
  
  
  #append grower_report for plots 
  LCRTotals8_soy <- reactive({ 
    LCRTotals8_raw_args_soy <- list(fsrString_soy(), grower_report_soy, 'soy')
    wrapMemoise(LCRTotals8_raw0, LCRTotals8_raw, LCRTotals8_raw_args_soy)
    #LCRTotals8_raw(fsrString_soy(), grower_report_soy) 
  })
  
  #create table to display seedsman info
  output$LCRDF_soy <- renderDT({ 
    dtLCRDF_args_soy <- list(fsrString_soy(), LCRTotals3_soy(), 'SOY')
    wrapMemoise(dtLCRDF_raw, dtLCRDF, dtLCRDF_args_soy)
    #dtLCRDF(fsrString(), LCRTotals3_soy()) 
  })
  
  
  #create table to display seedsman info 
  output$LCRDF4_soy <- renderDT({ 
    dtLCRDF4_args_soy <- list(fsrString_soy(), LCRTotals4_soy(), 'SOY')
    wrapMemoise(dtLCRDF4_raw, dtLCRDF4, dtLCRDF4_args_soy)
    #dtLCRDF4(fsrString(), LCRTotals4_soy()) 
  })
  
  
  #create table to display seedsman info
  output$LCRDF5_soy <- renderDT({ 
    dtLCRDF5_args_soy <- list(fsrString_soy(), LCRTotals5B_soy(), 'SOY')
    wrapMemoise(dtLCRDF5_raw, dtLCRDF5, dtLCRDF5_args_soy)
    #dtLCRDF5(fsrString(), LCRTotals5B_soy()) 
  })
  
  
  output$LCRDF6_soy <- renderDT({ 
    dtLCRDF6_args_soy <- list(fsrString_soy(), LCRTotals6_soy(), 'SOY')
    wrapMemoise(dtLCRDF6_raw, dtLCRDF6, dtLCRDF6_args_soy)
    #dtLCRDF6(fsrString(), LCRTotals6_soy()) 
  })
  
  
  output$LCRDF7_soy <- renderDT({ 
    dtLCRDF7_args_soy <- list(fsrString_soy(), LCRTotals7_soy(), 'SOY')
    wrapMemoise(dtLCRDF7_raw, dtLCRDF7, dtLCRDF7_args_soy)
    #dtLCRDF7(fsrString(), LCRTotals7_soy()) 
  })
  
  
  output$LCRDF8_soy <- renderDT({ 
    dtLCRDF8_args_soy <- list(fsrString_soy(), LCRTotals8_soy(), 'SOY')
    wrapMemoise(dtLCRDF8_raw, dtLCRDF8, dtLCRDF8_args_soy)
    #dtLCRDF8(fsrString(), LCRTotals8_soy()) 
  })
  
  
  #Plot of "Chart A-Team"
  output$LCRPlot3_soy <- renderPlotly({
    plotlyLCRPlot3(LCRTotals_soy())
  })
  
  
  #Plot of "Chart A-Team" part 2
  output$LCRPlot4_soy <- renderPlotly({
    plotlyLCRPlot4(LCRTotals_soy())
  })
  
  
  #Plot of "Chart B-Team"
  output$LCRPlot5_soy <- renderPlotly({ 
    plotlyLCRPlot5(LCRTotals3_soy()) 
  })
  
  
  
  #Plot of "Chart C-Team"
  output$LCRPlot6_soy <- renderPlotly({ 
    plotlyLCRPlot6(LCRTotals4_soy()) 
  })
  
  
  
  #Plot of "Chart D-Team"
  output$LCRPlot7_soy <- renderPlotly({ 
    plotlyLCRPlot7(LCRTotals5_soy()) 
  })
  
  
  # download handler for grower report ---------------------------------------------------------------
  output$download_grower_report_SOY <- downloadHandler(
    filename = function() {
      paste('MARSgrowerReportSOY-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(grower_report_soy, con)
    }
  )
  
  # download customer history for selected FSR (CORN) ---------------------------------------------------------------
  cust_hist_features <- c('FSR', 'TERR_ID', 'FIPS',	'Year',	'GrowerSAPID',	'SeedsmanSAPID',	'REGION',	'REGION_ID',	'GrowerAcctNm',	'SeedsmanAcctNm',	'TEAM_ID',	'ORDERED_QTY',	'RETAIL_AMT',	'AVGPRICE_RETAIL',	'OFFERED_DISC',	'BRAND_DISC',	'NET_AMT_WO_CASH',	'AVGPRICE_WO_CASH',	'CASH_DISC',	'NET_AMT_INCL_CASH',	'AVG_FARMGATE_PRICE',	'NEW_CUST',	'RBD',	'ABM',	'ABM.ID')
  rv <- reactiveValues(download_flag = 0)
  output$download_cust_hist <- downloadHandler(
    filename = function() {
      paste('MARScustomerhistoryCORN-', str_replace(isolate(fsrString()),', ','_'), '_', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(cust_df[which(cust_df[[fsrColumn]] == fsrString()), cust_hist_features], con)
      rv$download_flag <- rv$download_flag + 1
    }
  )
  
  # download customer history for selected FSR (SOY) ---------------------------------------------------------------
  cust_hist_features_soy <- c('FSR', 'TERR_ID', 'FIPS',	'MKT_YR',	'GRWR_SAP_ID',	'DLR_SAP_ID',	'REGION',	'REGION_ID',	'GRWR_ACCT_NAME',	'DLR_NAME',	'TEAM_ID',	'ORDERED_QTY',	'RETAIL_AMT',	'AVGPRICE_RETAIL',	'OFFERED_DISC',	'BRAND_DISC',	'NET_AMT_WO_CASH',	'AVGPRICE_WO_CASH',	'CASH_DISC',	'NET_AMT_INCL_CASH',	'AVG_FARMGATE_PRICE',	'NEW_CUST',	'RBD',	'ABM',	'ABM.ID')
  rv_soy <- reactiveValues(download_flag = 0)
  output$download_cust_hist_soy <- downloadHandler(
    filename = function() {
      paste('MARScustomerhistorySOY-', str_replace(isolate(fsrString_soy()),', ','_'), '_', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(cust_df_soy[which(cust_df_soy[[fsrColumnSoy]] == fsrString_soy()), cust_hist_features_soy], con)
      rv_soy$download_flag <- rv_soy$download_flag + 1
    }
  )
  
  #record when user cwid and fsrname when user downloads customer data (CORN)
  observeEvent(rv$download_flag,{
    if(rv$download_flag > 0) {
      write.table( paste0('User ', isolate(session$user), ' downloaded historical CORN data tied to FSR ', isolate(fsrString())),
                   file="./recorded_user_data/fsr_download_history.csv",
                   append = T,
                   sep='\n',
                   row.names=F,
                   col.names=F )
    }
  })
  
  #record when user cwid and fsrname when user downloads customer data (SOY)
  observeEvent(rv_soy$download_flag,{
    if(rv_soy$download_flag > 0) {
      write.table( paste0('User ', isolate(session$user), ' downloaded historical SOY data tied to FSR ', isolate(fsrString_soy())),
                   file="./recorded_user_data/fsr_download_history.csv",
                   append = T,
                   sep='\n',
                   row.names=F,
                   col.names=F )
    }
  })
  
  
  # Define initial empty table and messages in customer portfolio tab ################################
  # CORN ---------------------------------------------------------------------------------------------
  cart_df <- data.table()
  output$cart_ui <- renderText("Portfolio is empty.")
  output$prod_ui <- renderText("Prodoct Recommend is empty.")
  
  # SOY ----------------------------------------------------------------------------------------------
  cart_df_soy <- data.table()
  output$cart_ui_soy <- renderText("Portfolio is empty.")
  
  
  # download handler for grower report ---------------------------------------------------------------
  # this is commented out due to privacy concerns when the link goes live
  #output$download_grower_report <- downloadHandler(
  #filename = function() {
  #paste('MARSgrowerReport-', Sys.Date(), '.csv', sep='')
  #},
  #content = function(con) {
  #write.csv(grower_report, con)
  #}
  #)
  # Create feature importance tables for discount model ##############################################
  callModule(featuresServer, "corn_features", discount_table, rest_of_disc_features,
             ep_table, rest_of_ep_features)
  # SOY ----------------------------------------------------------------------------------------------
  callModule(featuresServer, "soy_features", discount_table_soy, rest_of_disc_features_soy,
             ep_table_soy, rest_of_ep_features_soy)  

    # update choices for selectInput based on selected terr ID #########################################
  observeEvent(input$fsr_name,{
    updateSelectInput(session, "seedsman_name",
                      #label = "Seedsman Account Name | Seedsman ID",
                      choices = cust_df_displaynames[cust_df_displaynames[[terrIDColumn]] == get_terr_id(input$fsr_name),
                                                     "Seedsman Account Name | Seedsman ID"],
                      selected = "")
    #selected = input$seedsman_name)
    updateSelectInput(session, "cust_name",
                      choices = NULL,
                      selected = "")
  },ignoreInit = TRUE)

  
  # update choices for selectInput based on selected seedsman ID ####################################
  observeEvent(input$seedsman_name,{
    updateSelectInput(session, "cust_name",
                      #label = "Customer Account Name | Customer ID",
                      choices = append(choices,
                                       cust_df_displaynames[cust_df_displaynames[[seedsmanIDColumn]] == get_id(input$seedsman_name),
                                                            'Customer Account Name | Customer ID'])
    )
  },ignoreInit = TRUE)
  

  
  # update choices in location drop down for new customer based on selected state ####################
  observeEvent(input$state, {
    updateSelectInput(session, "county",
                      label = "County",
                      choices = append("", stateCountyToFips[stateCountyToFips$statename==input$state,
                                                             'County']))
  },ignoreInit = TRUE)

  # What happens when state is changed ###############################################################
  # CORN ---------------------------------------------------------------------------------------------
  observeEvent(input$state, {
    cart_df <<- data.table()
    prod_df <<- data.table()
    
    # render empty portfolio message
    output$cart_ui <- renderText("Portfolio is empty.")
    output$prod_ui <- renderText("Prodoct Recommend is empty.")
    
    # remove download success message
    output$download_success <- NULL
    
    # render other relevant info null
    output$order_table <- NULL
    output$cust_plot <- NULL
    output$comp_info <- NULL
    output$dist_plot <- NULL
  }, ignoreInit=TRUE)
  # SOY ----------------------------------------------------------------------------------------------
  observeEvent(input$state, {
    cart_df_soy <<- data.table()
    
    # render empty portfolio message
    output$cart_ui_soy <- renderText("Portfolio is empty.")
    
    # remove download success message
    output$download_success_soy <- NULL
    
    # render other relevant info null
    output$order_table_soy <- NULL
    output$cust_plot_soy <- NULL
    output$dist_plot_soy <- NULL
    output$comp_info_soy <- NULL
  }, ignoreInit=TRUE)
  
  # What happens when county is changed ##############################################################
  # CORN ---------------------------------------------------------------------------------------------
  observeEvent(input$county, {
    cart_df <<- data.table()
    prod_df <<- data.table()
    
    # render empty portfolio message
    output$cart_ui <- renderText("Portfolio is empty.")
    output$prod_ui <- renderText("Prodoct Recommend is empty.")
    
    # remove download success message
    output$download_success <- NULL
    
    # render other relevant info null
    output$order_table <- NULL
    output$cust_plot <- NULL
    output$comp_info <- NULL
    output$dist_plot <- NULL
  }, ignoreInit=TRUE)
  
  # SOY ----------------------------------------------------------------------------------------------
  observeEvent(input$county, {
    cart_df_soy <<- data.table()
    
    # render empty portfolio message
    output$cart_ui_soy <- renderText("Portfolio is empty.")
    
    # remove download success message
    output$download_success_soy <- NULL
    
    # render other relevant info null
    output$order_table_soy <- NULL
    output$cust_plot_soy <- NULL
    output$dist_plot_soy <- NULL
    output$comp_info_soy <- NULL
  }, ignoreInit=TRUE)
  
  
  # What happens when customer name is changed #######################################################
  # CORN ---------------------------------------------------------------------------------------------
  observeEvent(input$cust_name, {
    
    # clear customer portfolio
    cart_df <<- data.table()
    prod_df <<- data.table()
    # render empty portfolio message
    output$cart_ui <- renderText("Portfolio is empty.")
    output$prod_ui <- renderText("Prodoct Recommend is empty.")
    
    # remove plot
    output$dist_plot <- NULL
    
    # remove download success message
    output$download_success <- NULL
    
    # remove order info table
    output$order_table <- NULL
    
    # make info boxes null
    #output$Total.Retail.Amount <- NULL
    output$Rec.LCR.Unit <- NULL
    output$Rec.LCR.Pct <- NULL
    
    # hide record info workflow
    shinyjs::hide(id = "show_hide")
    shinyjs::hide(id = "show_hide_downloadlink")
    
    # update select input for history plotting to default initial choice
    updateSelectInput(session, "plot_var",
                      selected = "")
    updateSelectInput(session, 'state',
                      selected = "")
    updateSelectInput(session, 'county',
                      selected = "")
    
    # if customer is not new customer and initial choice not selected, display appropiate tables in
    #  customer information tab
    if (input$cust_name != 'New Customer' & input$cust_name != "" ) {
      # get customer id
      cust_id <- get_id(input$cust_name)
      
      #get seedsmanID
      seedsmanID <- get_id(input$seedsman_name)
      
      # get fips code of customer for displaying competitive info table
      # take max of row of the same value (repeated fips code)
      fips_code_comp <- as.numeric(as.character(unlist(cust_df[cust_df[[custIDColumn]] == cust_id, fipsCodeColumn])))[1]
      
      # create proper products_df from customer zone (zone pricing is for corn only)
      # get customer zone from lookup table
      cust_zone <- data.frame(zoneLookUp)[zoneLookUp$fips == fips_code_comp, 'Corn_AZR']
      
      # make correct products_df
      recent_zone_pricing <- recentPricing[recentPricing$`Zone Label` == cust_zone, c('Product', 'Price')]
      
      products_df <<- make_products_df_zone(recent_zone_pricing)
      
      # update product choices accordingly
      updateSelectInput(session, 'product_name',
                        choices = products_df$Product)
      
      # create customer information table
      grower_table <- get_grower_characs(cust_id,
                                         cust_df,
                                         custFeats,
                                         custFeatNames,
                                         custFeatKeys,
                                         sharedFarms,
                                         custIDColumn,
                                         yearColumn,
                                         custFeatOrder)
      # render table in UI
      output$grower_table <- renderTable({grower_table},
                                         caption = 'Customer Characteristics',
                                         caption.placement = getOption("xtable.caption.placement",
                                                                       "top"))

      # do same for competitive info table
      comp_table <- get_value_prop_UI(valueProp,
                                      fips_code_comp,
                                      stateCountyToFips,
                                      compTableNamesChannel,
                                      compTableNamesOther)
      output$comp_info <- renderTable({comp_table},
                                      caption = 'Customer County Competitive Information',
                                      caption.placement = getOption("xtable.caption.placement",
                                                                    "top"))
      # do the same for purchase history
      purch_table <- get_customer_products(cust_id,
                                           cust_df,
                                           products_df,
                                           custIDColumn,
                                           custPurchaseNames,
                                           formattedPurchaseNames,
                                           custPurchaseKeys,
                                           orderPurchaseNames,
                                           EffPrice,
                                           seedsmanIDColumn,
                                           seedsmanID,
                                           yearColumn,
                                           yearView = FALSE)
      output$purchase_hist <- renderTable({purch_table},
                                          caption='Customer Purchase History',
                                          caption.placement = getOption("xtable.caption.placement",
                                                                        "top"))
      ###################Prodc Recommend########################
      
      prod_df <- tail(purch_table, 1)
      #print(prod_df)
      # # make new row data table from user input
      # new_row <- data.table(Product = input$product_name,
      #                       Quantity = input$product_quantity)
      
      # Create prod table with buttons
      prod_ui_obj <- make_prod_ui(prod_df, new_ids = TRUE)
      output$prod_ui <- prod_ui_obj$ui
      
      # Create COMPARISON table
      COMPARISON <- data.table(
        Selected = c(919.85,1046.04), 
        Recommended = c(943.51,1048.85),
        Difference = c(23.66,2.81)
      )
      rownames(COMPARISON) <- c("Downside Profit per acre","Upside Profit per acre")
      output$COMPARISONtable <- renderTable(cbind(Comparison = rownames(COMPARISON), COMPARISON),width = "100%")
      #output$COMPARISONtable <- renderTable(COMPARISON, rownames = TRUE)
      
      # what happens when switch to year view button is clicked
      observeEvent(input$year_view, {
        purch_table <- get_customer_products(cust_id,
                                             cust_df,
                                             products_df,
                                             custIDColumn,
                                             custPurchaseNames,
                                             formattedPurchaseNames,
                                             custPurchaseKeys,
                                             orderPurchaseNames,
                                             EffPrice,
                                             seedsmanIDColumn,
                                             seedsmanID,
                                             yearColumn,
                                             yearView = TRUE)
        # render table in UI
        output$purchase_hist <- renderTable({purch_table},
                                            caption='Customer Purchase History',
                                            caption.placement = getOption("xtable.caption.placement",
                                                                          "top"))
      })
      
      # what happens when switch back to product view is clicked
      observeEvent(input$product_view, {
        purch_table <- get_customer_products(cust_id,
                                             cust_df,
                                             products_df,
                                             custIDColumn,
                                             custPurchaseNames,
                                             formattedPurchaseNames,
                                             custPurchaseKeys,
                                             orderPurchaseNames,
                                             EffPrice,
                                             seedsmanIDColumn,
                                             seedsmanID,
                                             yearColumn,
                                             yearView = FALSE)
        # render table in UI
        output$purchase_hist <- renderTable({purch_table},
                                            caption='Customer Purchase History',
                                            caption.placement = getOption("xtable.caption.placement",
                                                                          "top"))
      }, ignoreInit=TRUE)
      
      # what should happen when customer selects variables to plot from drop down in customer information tab
      observeEvent(input$plot_var, {
        # if not initial choice plot selected variable
        if (input$plot_var != ""){
          cust_plot <- make_customer_plot(cust_df,
                                          cust_id,
                                          custIDColumn,
                                          input$plot_var,
                                          custFeaturesToPlot,
                                          featsToPlotCleanNames,
                                          seedsmanIDColumn,
                                          seedsmanID,
                                          discountColumn,
                                          priceColumn)
          output$cust_plot<- renderPlot({cust_plot})
        } else {  # when var is reset to initial choice, plot is cleared
          output$cust_plot <- NULL
        }
      }, ignoreInit=TRUE)
      ################################################ import data ###################################
      # Load map log/lat data
      # source: https://opendata.socrata.com/dataset/Airport-Codes-mapped-to-Latitude-Longitude-in-the-/rxrh-4cxm
      filnam <- paste(fips_code_comp,"_longlat_area.csv", sep = "", collapse = NULL)
      airports <- read.csv(paste('./Data/tif/',filnam, sep = "", collapse = NULL))
      #airports <- read.csv('./Data/tif/26087_longlat_area.csv')
      airports$Longitude <- airports$long
      airports$Latitude <- airports$lat
      airports$locationID <- airports$field_id
      airports <- airports[,c('Longitude','Latitude', 'locationID')]

      filnam_ec <- paste(fips_code_comp,"_longlat_area_ec.csv", sep = "", collapse = NULL)
      airports_ec <- read.csv(paste('./Data/tif_ec/',filnam_ec, sep = "", collapse = NULL))
      #airports_ec <- read.csv('26087_longlat_area_ec.csv')
      airports_ec$long <- as.numeric(substr(airports_ec$field_id, 2, 6))/100
      airports_ec$lat <- as.numeric(substr(airports_ec$field_id, 7, 10))/100
      
      # generate second set of unique location IDs for second layer of selected locations
      airports$secondLocationID <- paste(as.character(airports$locationID), "_selectedLayer", sep="")
      coordinates <- SpatialPointsDataFrame(airports[,c('Longitude', 'Latitude')] , airports)
      
      #where to save location file
      filnam_l <- paste(cust_id,"_location.csv", sep = "", collapse = NULL)
      file_l <- paste('./recorded_user_data/cut_location/',filnam_l, sep = "", collapse = NULL)
      filnam_rds <- paste(cust_id,"_location.Rds", sep = "", collapse = NULL)
      file_rds <- paste('./recorded_user_data/cut_location/',filnam_rds, sep = "", collapse = NULL)

      
      ################################################# section one #################################################
      # list to store the selections for tracking
      
      data_of_click <- reactiveValues(clickedMarker = list())
      
      ################################################# section two #################################################
      # base map_corn
      output$mymap <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addScaleBar() %>%
          setView(lng = mean(airports$Longitude), lat = mean(airports$Latitude),  zoom = 12) %>%
          addCircles(data = airports,
                     radius = 60,
                     lat = airports$Latitude,
                     lng = airports$Longitude,
                     fillColor = "white",
                     fillOpacity = 1,
                     color = "Gold",
                     weight = 3,
                     stroke = T,
                     layerId = as.character(airports$locationID),
                     highlightOptions = highlightOptions(color = "mediumseagreen",
                                                         opacity = 1.0,
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
            editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions())) 
          
      })
      
      ## base map_soy
      output$mymap_soy <- renderLeaflet({
        leaflet() %>%
          addTiles() %>%
          addScaleBar() %>%
          setView(lng = mean(airports$Longitude), lat = mean(airports$Latitude),  zoom = 12) %>%
          addCircles(data = airports,
                     radius = 60,
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
                                                                              ,color = 'blue'
                                                                              ,weight = 2)),
            rectangleOptions = drawRectangleOptions(shapeOptions=drawShapeOptions(fillOpacity = 0
                                                                                  ,color = 'blue'
                                                                                  ,weight = 2)),
            circleOptions = drawCircleOptions(shapeOptions = drawShapeOptions(fillOpacity = 0
                                                                              ,color = 'blue'
                                                                              ,weight = 2)),
            editOptions = editToolbarOptions(edit = FALSE, selectedPathOptions = selectedPathOptions()))
      })
      
      ################################################# section 2.5 ###################3
      # history draw file -download
      
      raw <- NULL
      if (file.exists(file_rds)) {
        #s3readRDS(bucket = aws_bucket_name, object = filnam_rds)    # bucket and object specified separately
        read.all <- readRDS(file_rds)
        raw <- do.call(rbind,read.all$geometry$coordinates[[1]])
        raw <- apply(raw, 2,as.numeric)
        #print(raw)
        proxy <- leafletProxy("mymap", data = raw)
        proxy %>% addPolylines(data = raw, weight = 2,color = 'blue', layerId = as.character(cust_id)) #draw polylines
        #clear historical polygon
        # observeEvent(input$showP,{
        #   if (!is.null(raw) & nrow(raw)!=0) {
        #     print(cust_id)
        #     proxy %>% removeShape(layerId = as.character(cust_id))
        #     file.remove(file_rds) #delete from local
        #     delete_object(object = filnam_rds, bucket = aws_bucket_name, quiet = TRUE) #delete from aws
        #   }
        # })
      }
      observeEvent(input$showP,{
        if (file.exists(file_rds)) {
          print(cust_id)
          proxy %>% removeShape(layerId = as.character(cust_id))
          file.remove(file_rds) #delete from local
          delete_object(object = filnam_rds, bucket = aws_bucket_name, quiet = TRUE) #delete from aws
        }
      })
      #input$showP == 0
      ############################################### section three #################################################
      observeEvent(input$mymap_draw_new_feature,{
          #input$mymap_draw_new_feature <- read.all  
          #Only add new layers for bounded locations
          found_in_bounds <- findLocations(shape =input$mymap_draw_new_feature
                                           , location_coordinates = coordinates
                                           , location_id_colname = "locationID")
          #raw2 <- do.call(rbind,input$mymap_draw_new_feature$geometry$coordinates[[1]])
          #raw2 <- apply(raw2, 2,as.numeric)
          #print(raw2)
          for(id in found_in_bounds){
            if(id %in% data_of_click$clickedMarker){
              # don't add id
            } else {
              # add id
              data_of_click$clickedMarker<-append(data_of_click$clickedMarker, id, 0)
            }
          }
          
          # look up airports by ids found
          selected_l <- subset(airports, locationID %in% data_of_click$clickedMarker)
          #print(selected_l)
          proxy <- leafletProxy("mymap")
          proxy %>% addCircles(data = selected_l,
                               radius = 30,
                               lat = selected_l$Latitude,
                               lng = selected_l$Longitude,
                               fillColor = "wheat",
                               fillOpacity = 1,
                               color = "mediumseagreen",
                               weight = 3,
                               stroke = T,
                               layerId = as.character(selected_l$secondLocationID),
                               highlightOptions = highlightOptions(color = "Gold",
                                                                   opacity = 1.0,
                                                                   weight = 2,
                                                                   bringToFront = TRUE))
  
        
          # if selected_l null, save
          if (nrow(selected_l)!=0) {
            #write.table(selected_l, file = file_l, sep = ",",row.names = FALSE, col.names = TRUE, eol = "\n") #save point location file
            saveRDS(input$mymap_draw_new_feature, file = file_rds) # save polynomial coordinate
            
            #save data on aws
            #put_object(file = file_l, object = filnam_l, bucket = aws_bucket_name) #save point location file
            put_object(file = file_rds, object = filnam_rds, bucket = aws_bucket_name) # save polynomial coordinate
            
            #direct save to aws
            #s3saveRDS(nput$mymap_draw_new_feature, bucket = aws_bucket_name, object = filnam_rds)
            
            #double check the file on aws
            #data.table::rbindlist(get_bucket(bucket = aws_bucket_name))
          }
      })
      
      selectedLocations <- reactive({
        selectedLocations <- subset(airports, locationID %in% data_of_click$clickedMarker)
        # return this output
        selectedLocations <- selectedLocations[,c('Longitude','Latitude')]
        selectedLocations
      })
      
      output$mytable2 <- renderDataTable({
        datatable(selectedLocations())
        })
      

      output$mytable_soy <- renderDataTable({
       datatable(selectedLocations())
      })
      
      # data <- eventReactive(input$show2,{
      #   datatable(selectedLocations())
      # })
      # 
      # output$mytable <- renderDataTable({
      #   data()
      # })
      
      ########################################### section 3.5 yield #################
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
            
            usda_county <- usda_data[which(usda_data$fips_code == fips_code_comp),]
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
          }
          
          #if nrow(df_ec_relevant) == 0, replace with zvecj county-level env class proportions
          #grab usda, PI data for the county
          #combine zvecj with county-level data
          #feed same into yield, value prop models
          #print distributions
        } else{}
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

      
      
    } else if (input$cust_name == 'New Customer') {
      # if customer is new customer and no state/county is selected,
      #  make all tables in Customer Info tab blank
      toggleDropdownButton('add_new_customer')
      output$grower_table <- renderText("")
      output$purchase_hist <- renderText("")
      output$comp_info <- renderText("")
      output$cust_plot <- renderText("")
      #output$mytable <- renderText("")
      output$mymap <- NULL
      output$disty_plot_monyield <- NULL
      output$disty_plot_compyield <- NULL
      output$disty_plot_yieldadv <- NULL
      output$disty_plot_seedpricedif <- NULL
      output$disty_plot_valueprop <- NULL
    } else {
      # if "" (initial choice),  don't display anything
      output$grower_table <- renderText("")
      output$purchase_hist <- renderText("")
      output$comp_info <- renderText("")
      output$cust_plot <- renderText("")
      #output$mytable <- renderText("")
      output$mymap <- NULL
      output$disty_plot_monyield <- NULL
      output$disty_plot_compyield <- NULL
      output$disty_plot_yieldadv <- NULL
      output$disty_plot_seedpricedif <- NULL
      output$disty_plot_valueprop <- NULL
    }
  }, ignoreInit=TRUE)
  
  # SOY ----------------------------------------------------------------------------------------------
  observeEvent(input$cust_name, {
    #clear customer portfolio
    cart_df_soy <<- data.table()
    
    # render empty portfolio message
    output$cart_ui_soy <- renderText("Portfolio is empty.")
    
    # remove plot
    output$dist_plot_soy <- NULL
    
    # remove download success message
    output$download_success_soy <- NULL
    
    # remove order info table
    output$order_table_soy <- NULL
    
    # make info boxes null
    #output$Total.Retail.Amount_soy <- NULL
    output$Rec.LCR.Unit_soy <- NULL
    output$Rec.LCR.Pct_soy <- NULL
    
    # update select input for history plotting to default initial choice
    updateSelectInput(session, "plot_var_soy",
                      selected = "")
    updateSelectInput(session, 'state_soy',
                      selected = "")
    updateSelectInput(session, "county_soy",
                      selected = "")
    # if customer is not new customer and initial choice not selected, display appropiate tables in
    #  customer information tab
    if (input$cust_name != 'New Customer' & input$cust_name != "") {
      
      # get customer id
      cust_id_soy <- get_id(input$cust_name)
      
      # get seedsmanID
      seedsmanID_soy <- get_id(input$seedsman_name)
      
      # get fips code of customer for displaying competitive info table
      # take max of row of the same value (repeated fips code)
      fips_code_comp <- as.numeric(as.character(unlist(cust_df_soy[cust_df_soy[[custIDColumnSoy]] == cust_id_soy,
                                                                   fipsCodeColumnSoy])))[1]
      
      # create customer information table
      grower_table_soy <- get_grower_characs(cust_id_soy,
                                             cust_df_soy,
                                             custFeats,
                                             custFeatNames,
                                             custFeatKeys,
                                             sharedFarms,
                                             custIDColumnSoy,
                                             yearColumnSoy,
                                             custFeatOrder)
      # render table in UI
      output$grower_table_soy <- renderTable({grower_table_soy},
                                             caption = 'Customer Characteristics',
                                             caption.placement = getOption("xtable.caption.placement",
                                                                           "top"))
      # do same for competitive info table
      comp_table_soy <- makeCompTable(YieldAdvantageSoy,
                                      fips_code_comp,
                                      stateCountyToFips,
                                      compTableNamesChannel,
                                      compTableNamesOther)
      output$comp_info_soy <- renderTable({comp_table_soy},
                                          caption = 'Customer County Competitive Information',
                                          caption.placement = getOption("xtable.caption.placement",
                                                                        "top"))
      
      # do the same for purchase history
      purch_table_soy <- get_customer_products(cust_id_soy,
                                               cust_df_soy,
                                               products_df_soy,
                                               custIDColumnSoy,
                                               custPurchaseNamesSoy,
                                               formattedPurchaseNamesSoy,
                                               custPurchaseKeysSoy,
                                               orderPurchaseNamesSoy,
                                               EffPrice_soy,
                                               seedsmanIDColumnSoy,
                                               seedsmanID_soy,
                                               yearColumnSoy,
                                               yearView = FALSE)
      
      output$purchase_hist_soy <- renderTable({purch_table_soy},
                                              caption='Customer Purchase History',
                                              caption.placement = getOption("xtable.caption.placement",
                                                                            "top"))
      observeEvent(input$year_view_soy, {
        # what happens when switch to year view button is clicked
        purch_table_soy <- get_customer_products(cust_id_soy,
                                                 cust_df_soy,
                                                 products_df_soy,
                                                 custIDColumnSoy,
                                                 custPurchaseNamesSoy,
                                                 formattedPurchaseNamesSoy,
                                                 custPurchaseKeysSoy,
                                                 orderPurchaseNamesSoy,
                                                 EffPrice_soy,
                                                 seedsmanIDColumnSoy,
                                                 seedsmanID_soy,
                                                 yearColumnSoy,
                                                 yearView = TRUE)
        # render table in UI
        output$purchase_hist_soy <- renderTable({purch_table_soy},
                                                caption='Customer Purchase History',
                                                caption.placement = getOption("xtable.caption.placement",
                                                                              "top"))
      })
      observeEvent(input$product_view_soy, {
        # what happens when switch back to product view is clicked
        purch_table_soy <- get_customer_products(cust_id_soy,
                                                 cust_df_soy,
                                                 products_df_soy,
                                                 custIDColumnSoy,
                                                 custPurchaseNamesSoy,
                                                 formattedPurchaseNamesSoy,
                                                 custPurchaseKeysSoy,
                                                 orderPurchaseNamesSoy,
                                                 EffPrice_soy,
                                                 seedsmanIDColumnSoy,
                                                 seedsmanID_soy,
                                                 yearColumnSoy,
                                                 yearView = FALSE)
        # render table in UI
        output$purchase_hist_soy <- renderTable({purch_table_soy},
                                                caption='Customer Purchase History',
                                                caption.placement = getOption("xtable.caption.placement",
                                                                              "top"))
      })
      # what should happen when customer selects variables to plot from drop down in customer information tab
      observeEvent(input$plot_var_soy, {
        # if not initial choice plot selected variable
        if (input$plot_var_soy != ""){
          cust_plot_soy <- make_customer_plot(cust_df_soy,
                                              cust_id_soy,
                                              custIDColumnSoy,
                                              input$plot_var_soy,
                                              custFeaturesToPlotSoy,
                                              featsToPlotCleanNamesSoy,
                                              seedsmanIDColumnSoy,
                                              seedsmanID_soy,
                                              discountColumnSoy,
                                              priceColumnSoy)
          output$cust_plot_soy <- renderPlot({cust_plot_soy})
        }
      })
      
      
  ####################################################    
    } else if (input$cust_name == 'New Customer') {
      # if customer is new customer and no state/county is selected,
      #  make all tables in Customer Info tab blank
      toggleDropdownButton('add_new_customer_soy')
      output$grower_table_soy <- renderText("")
      output$purchase_hist_soy <- renderText("")
      output$cust_plot_soy <- renderText("")
      output$mytable_soy <- renderText("")
      output$mymap_soy <- renderText("")
    } else {
      # if "" (initial choice),  don't display anything
      output$grower_table_soy <- renderText("")
      output$purchase_hist_soy <- renderText("")
      output$cust_plot_soy <- renderText("")
      output$mytable_soy <- renderText("")
      output$mymap_soy <- renderText("")
    }
  }, ignoreInit=TRUE)
  
  # if county is not null and customer is new customer, try to show competitive information about
  #  that customer's county ##########################################################################
  # CORN ---------------------------------------------------------------------------------------------
  observeEvent(input$county, {
    if(!is.null(input$county) & input$county != "") {
      
      # get fips code
      fips_code_comp <- get_fips(input$state,
                                 input$county,
                                 stateCountyToFips)
      
      # create proper products_df from customer zone (zone pricing is for corn only)
      # get customer zone from lookup table
      cust_zone <- data.frame(zoneLookUp)[zoneLookUp$fips == fips_code_comp, 'Corn_AZR']
      
      # make correct products_df
      recent_zone_pricing <- recentPricing[recentPricing$`Zone Label` == cust_zone, c('Product', 'Price')]
      products_df <<- make_products_df_zone(recent_zone_pricing)
      
      # update product choices accordingly
      updateSelectInput(session, 'product_name',
                        choices = products_df$Product)
      
      # get competitive info table
      comp_table <- get_value_prop_UI(valueProp,
                                      fips_code_comp,
                                      stateCountyToFips,
                                      compTableNamesChannel,
                                      compTableNamesOther)
      # render table in UI
      output$comp_info <- renderTable({comp_table},
                                      caption = 'Customer County Competitive Information',
                                      caption.placement = getOption("xtable.caption.placement", "top"))
    }
  }, ignoreInit=TRUE)
  # SOY ----------------------------------------------------------------------------------------------
  observeEvent(input$county, {
    if(!is.null(input$county) & input$county != "") {
      
      # get fips code
      fips_code_comp <- get_fips(input$state,
                                 input$county,
                                 stateCountyToFips)
      
      # get competitive info table
      comp_table_soy <- makeCompTable(YieldAdvantageSoy,
                                      fips_code_comp,
                                      stateCountyToFips,
                                      compTableNamesChannel,
                                      compTableNamesOther)
      # render table in UI
      output$comp_info_soy <- renderTable({comp_table_soy},
                                          caption = 'Customer County Competitive Information',
                                          caption.placement = getOption("xtable.caption.placement", "top"))
    }
  }, ignoreInit=TRUE)
  
  # What happens when FSR name is changed  ###########################################################
  # CORN ---------------------------------------------------------------------------------------------
  observeEvent(input$fsr_name, {
    
    # make tables blank
    output$grower_table <- renderText("")
    output$purchase_hist <- renderText("")
    output$comp_info <- renderText("")
    #output$mytable <- renderText("")
    data_of_click <- NULL
    
    # make portfolio blank and render message
    cart_df <<- data.table()
    output$cart_ui <- renderText("Portfolio is empty.") 
    output$prod_ui <- renderText("Prodoct Recommend is empty.")
    # make plot blank
    output$dist_plot <- NULL
    output$disty_plot_monyield <- NULL
    output$disty_plot_compyield <- NULL
    output$disty_plot_yieldadv <- NULL
    output$disty_plot_seedpricedif <- NULL
    output$disty_plot_valueprop <- NULL
    
    # make download success message blank
    output$download_success <- NULL
    
    # make order info table blank
    output$order_table <- NULL
    
    #make map blank
    output$mymap <- NULL
    
    # make customer, seedsman historical plots empty
    output$cust_plot <- NULL
    output$seedsman_plot <- NULL
    
    # make info boxes null
    #output$Total.Retail.Amount <- NULL
    output$Rec.LCR.Unit <- NULL
    output$Rec.LCR.Pct <- NULL
    
    
    # update select input for history plotting to default initial choice
    updateSelectInput(session, "plot_var",
                      selected = "")
    # clear state and county selections for new customer
    updateSelectInput(session, 'state',
                      selected = "")
    updateSelectInput(session, 'county',
                      selected = "")
    
    if (input$fsr_name != "" ) {
      seedsmanID <- get_id(input$seedsman_name)
      seedsmanName <- seedsmanString()
      fsrName <- fsrString()
      # what should happen when users selects variables to plot from drop down on seedsman tab
      observeEvent(input$plot_var_seedsman, {
        # if not initial choice plot selected variable
        if (input$plot_var_seedsman != ""){
          seedsman_plot <- make_seedsman_plot(cust_df,
                                              custIDColumn,
                                              input$plot_var_seedsman,
                                              custFeaturesToPlot,
                                              featsToPlotCleanNames,
                                              seedsmanIDColumn,
                                              seedsmanNameColumn,
                                              seedsmanID,
                                              discountColumn,
                                              priceColumn,
                                              fsrColumn,
                                              fsrName,
                                              seedsmanName)
          output$seedsman_plot<- renderPlotly({seedsman_plot})
        } else {  # when var is reset to initial choice, plot is cleared
          output$seedsman_plot <- NULL
        }
      }, ignoreInit=FALSE)
    } else{
      output$seedsman_plot <- renderText("")
    }
    
    shinyjs::hide(id = "show_hide_downloadlink")
  }, ignoreInit=FALSE)
  
  # SOY ----------------------------------------------------------------------------------------------
  observeEvent(input$fsr_name, {
    # make tables blank
    output$grower_table_soy <- renderText("")
    output$purchase_hist_soy <- renderText("")
    output$comp_info_soy <- renderText("")
    #output$mytable_soy <- renderText("")
    output$mymap_soy <- renderText("")
    
    # make portfolio blank and render message
    cart_df_soy <<- data.table()
    output$cart_ui_soy <- renderText("Portfolio is empty.")
    
    # make plot blank
    output$dist_plot_soy <- NULL
    
    # make download success message blank
    output$download_success_soy <- NULL
    
    # make order info table blank
    output$order_table_soy <- NULL
    
    # make customer historical plots empty
    output$cust_plot_soy <- NULL
    output$seedsman_plot_soy <- NULL
    
    # make info boxes null
    #output$Total.Retail.Amount_soy <- NULL
    output$Rec.LCR.Unit_soy <- NULL
    output$Rec.LCR.Pct_soy <- NULL
    
    # update select input for history plotting to default initial choice
    updateSelectInput(session, "plot_var_soy",
                      selected = "")
    # clear state and county selections for new customer
    updateSelectInput(session, 'state_soy',
                      selected = "")
    updateSelectInput(session, 'county_soy',
                      selected = "")
    
    if (input$fsr_name != "" ) {
      seedsmanID_soy <- get_id(input$seedsman_name)
      seedsmanName_soy <- seedsmanString_soy()
      fsrName_soy <- fsrString_soy()
      # what should happen when users selects variables to plot from drop down on seedsman tab
      observeEvent(input$plot_var_seedsman_soy, {
        # if not initial choice plot selected variable
        if (input$plot_var_seedsman_soy != ""){
          seedsman_plot_soy <- make_seedsman_plot(cust_df_soy,
                                                  custIDColumnSoy,
                                                  input$plot_var_seedsman_soy,
                                                  custFeaturesToPlotSoy,
                                                  featsToPlotCleanNamesSoy,
                                                  seedsmanIDColumnSoy,
                                                  seedsmanNameColumnSoy,
                                                  seedsmanID_soy,
                                                  discountColumnSoy,
                                                  priceColumnSoy,
                                                  fsrColumnSoy,
                                                  fsrName_soy,
                                                  seedsmanName_soy)
          output$seedsman_plot_soy <- renderPlotly({seedsman_plot_soy})
        } else {  # when var is reset to initial choice, plot is cleared
          output$seedsman_plot_soy <- NULL
        }
      }, ignoreInit=FALSE)
    } else{
      output$seedsman_plot_soy <- renderText("")
    }
    
    
  }, ignoreInit=FALSE)
  
  # What happens when seedsman id is changed by user #################################################
  # CORN ---------------------------------------------------------------------------------------------
  observeEvent(input$seedsman_name, {
    
    # make tables blank
    output$grower_table <- renderText("")
    output$purchase_hist <- renderText("")
    output$comp_info <- renderText("")
    #output$mytable <- renderText("")
    data_of_click <- NULL
    
    # delete map information
    # selectedLocations <- NULL
    # poly_file <-NULL

    # make portfolio blank and render message
    cart_df <<- data.table()
    output$cart_ui <- renderText("Portfolio is empty.")
    
    # make plot blank
    output$dist_plot <- NULL
    
    # make download success message blank
    output$download_success <- NULL
    
    # make order info table blank
    output$order_table <- NULL
    
    # make customer historical plots empty
    output$cust_plot <- NULL
    output$disty_plot_monyield <- NULL
    output$disty_plot_compyield <- NULL
    output$disty_plot_yieldadv <- NULL
    output$disty_plot_seedpricedif <- NULL
    output$disty_plot_valueprop <- NULL
    
    # make info boxes null
    #output$Total.Retail.Amount <- NULL
    output$Rec.LCR.Unit <- NULL
    output$Rec.LCR.Pct <- NULL
    
    # make map blank
    output$mymap <- NULL
    
    # update select input for history plotting to default initial choice
    updateSelectInput(session, "plot_var",
                      selected = "")
    # clear state and county selections for new customer
    updateSelectInput(session, 'state',
                      selected = "")
    updateSelectInput(session, 'county',
                      selected = "")
    
    # if seedsman is not blank, display appropriate tables in
    #  seedsman tab
    # if (input$seedsman_name != "" ) {
    #   seedsmanID <- get_id(input$seedsman_name)
    #   seedsmanName <- seedsmanString()
    #   fsrName <- fsrString()
    #   # what should happen when users selects variables to plot from drop down on seedsman tab
    #   observeEvent(input$plot_var_seedsman, {
    #     # if not initial choice plot selected variable
    #     if (input$plot_var_seedsman != ""){
    #       seedsman_plot <- make_seedsman_plot(cust_df,
    #                                       custIDColumn,
    #                                       input$plot_var_seedsman,
    #                                       custFeaturesToPlot,
    #                                       featsToPlotCleanNames,
    #                                       seedsmanIDColumn,
    #                                       seedsmanNameColumn,
    #                                       seedsmanID,
    #                                       discountColumn,
    #                                       priceColumn,
    #                                       fsrColumn,
    #                                       fsrName,
    #                                       seedsmanName)
    #       output$seedsman_plot<- renderPlot({seedsman_plot})
    #     } else {  # when var is reset to initial choice, plot is cleared
    #       output$seedsman_plot <- NULL
    #     }
    #   }, ignoreInit=TRUE)
    # } else{
    #   output$seedsman_plot <- renderText("")
    # }
    
    shinyjs::hide(id = "show_hide_downloadlink")
    
  }, ignoreInit=TRUE)
  
  # SOY ----------------------------------------------------------------------------------------------
  observeEvent(input$seedsman_name, {
    
    # make tables blank
    output$grower_table_soy <- renderText("")
    output$purchase_hist_soy <- renderText("")
    output$comp_info_soy <- renderText("")
    output$mytable_soy <- renderText("")
    output$mymap_soy <- renderText("")
    
    # make portfolio blank and render message
    cart_df_soy <<- data.table()
    output$cart_ui_soy <- renderText("Portfolio is empty.")
    
    # make plot blank
    output$dist_plot_soy <- NULL
    
    # make download success message blank
    output$download_success_soy <- NULL
    
    # make order info table blank
    output$order_table_soy <- NULL
    
    # make customer, seedsman historical plots empty
    output$cust_plot_soy <- NULL
    #output$seedsman_plot_soy <- NULL
    
    # make info boxes null
    #output$Total.Retail.Amount_soy <- NULL
    output$Rec.LCR.Unit_soy <- NULL
    output$Rec.LCR.Pct_soy <- NULL
    
    # update select input for history plotting to default initial choice
    updateSelectInput(session, "plot_var_soy",
                      selected = "")
    # clear state and county selections for new customer
    updateSelectInput(session, 'state_soy',
                      selected = "")
    updateSelectInput(session, 'county_soy',
                      selected = "")
    
    # if seedsman is not blank, display appropriate tables in
    #  seedsman tab
    # if (input$seedsman_name_soy != "" ) {
    #   seedsmanID_soy <- get_id(input$seedsman_name_soy)
    #   # what should happen when users selects variables to plot from drop down on seedsman tab
    #   observeEvent(input$plot_var_seedsman_soy, {
    #     # if not initial choice plot selected variable
    #     if (input$plot_var_seedsman_soy != ""){
    #       seedsman_plot_soy <- make_seedsman_plot(cust_df_soy,
    #                                           custIDColumnSoy,
    #                                           input$plot_var_seedsman_soy,
    #                                           custFeaturesToPlotSoy,
    #                                           featsToPlotCleanNamesSoy,
    #                                           seedsmanIDColumnSoy,
    #                                           seedsmanID_soy,
    #                                           discountColumnSoy,
    #                                           priceColumnSoy)
    #       output$seedsman_plot_soy <- renderPlot({seedsman_plot_soy})
    #     } else {  # when var is reset to initial choice, plot is cleared
    #       output$seedsman_plot_soy <- NULL
    #     }
    #   }, ignoreInit=TRUE)
    # } else{
    #   output$seedsman_plot_soy <- renderText("")
    # }
    
    
  }, ignoreInit=TRUE)
  
  # All things that should happen when "Add to Portfolio' is clicked #################################
  # CORN ---------------------------------------------------------------------------------------------
  observeEvent(input$add_to_cart, {
    req(input$seedsman_name)
    req(input$cust_name)
    req(input$product_name)
    req(input$product_quantity)
      
    shinyjs::hide(id = "show_hide_downloadlink")
    
    # if products have been previously added to cart and deleted through portfolio prepopulation functionality,
    # prior to adding product to cart, use correct cart
    if (exists('cart_df_check')){ 
      if(!is.null(cart_df_check)){
        cart_df <<- cart_df_check
        cart_df_check <<- NULL
      }
    }
    
    # clear download success message
    output$download_success <- NULL
    
    # make new row data table from user input
    new_row <- data.table(Product = input$product_name,
                          Quantity = input$product_quantity)
    # add list price
    new_row_tmp <- merge(new_row, products_df, by = "Product")
    
    # track product for use later in predict functions
    product <- unlist(new_row_tmp[, 'Product'])
    
    # combine with existing cart
    new_port_tmp <- rbind(cart_df, new_row_tmp, use.names = TRUE, fill = TRUE)
    
    # clean $ from price column to merge
    new_port_tmp[, 'Price'] <- as.numeric(gsub("[\\$,]", "", new_port_tmp$Price))
    
    # extract whole portfolio before subsetting by product to correctly make basket level effective price predicitons
    new_port <- new_port_tmp[, .(Quantity = sum(Quantity),
                                 Price = mean(Price)),
                             by = Product]
    # get order level stats to use as memoisation keys that feed into predict discount function, can't just use new_port
    # otherwise memoisation will not work correctly, all keys must be UNIQUE
    avgPrice <- mean(new_port$Price)
    totalQuantity <- sum(new_port$Quantity)
    
    # collapse by product
    tmp <- new_port_tmp[new_port_tmp$Product == unlist(product), ]
    
    # get correct quantity for product added to cart to use in discount prediction later
    new_row_tmp <- tmp[, .(Quantity = sum(Quantity),
                           Price = mean(Price)),
                       by = Product]
    quantity <- unlist(new_row_tmp[, 'Quantity'])
    
    # if customer is a new customer and state/county isn't null, update cart with predictions
    #  and show correct graph
    if (input$cust_name == 'New Customer' & !is.null(input$state) & !is.null(input$county)){
      # get fips code
      fips_code <- get_fips(input$state,
                            input$county,
                            stateCountyToFips)
      # get seedsman ID
      seedsmanID <- get_id(input$seedsman_name)
      
      # predict discounts associated with new row
      predict_discount_new_cust_args <- list(product,
                                             quantity,
                                             fips_code, 
                                             logTransformDiscount,
                                             quantityColumn,
                                             priceColumn,
                                             productColumn,
                                             features,
                                             countyFeatures,
                                             cust_df,
                                             discountModel,
                                             pllel,
                                             avgPrice,
                                             totalQuantity,
                                             seedsmanIDColumn,
                                             seedsmanID,
                                             fipsCodeColumn)
      discounts_new_cust <- wrapMemoise(predict_discount_new_cust_raw, predict_discount_new_cust, predict_discount_new_cust_args)
      
      # # predict prices associated with new_port
      # prices_new_cust <- predict_ep_new_cust(new_port,
      #                                        fips_code,
      #                                        logTransformEP,
      #                                        features_effP,
      #                                        EffPrice,
      #                                        epAvgPriceColumn,
      #                                        epTotalQColumn,
      #                                        epFipsCodeColumn,
      #                                        countyFeatures,
      #                                        epModel,
      #                                        marketYear,
      #                                        epSeedsmanIDColumn,
      #                                        seedsmanID)
      
      # update cart and table accordingly
      cart_df <<- update_cart_new_cust(discounts_new_cust,
                                       cart_df,
                                       new_row,
                                       fips_code,
                                       product,
                                       products_df)
      
      # if more than one item in customer's cart, call predict discount functions for all rows 
      #  where product is not the most recently added to correctly update product level discounts 
      #  now that order level stats have changed 
      if (nrow(cart_df) > 1) {
        tmp <- c()
        # only do for products that are not in the most recently added row
        tmp_df <- cart_df[cart_df$Product != product, ]
        for (i in 1:nrow(tmp_df)) {
          product_tmp <- unlist(tmp_df[i, 'Product'])
          quantity_tmp <- unlist(tmp_df[i, 'Quantity'])
          predict_discount_new_cust_args <- list(product_tmp,
                                                 quantity_tmp,
                                                 fips_code, 
                                                 logTransformDiscount,
                                                 quantityColumn,
                                                 priceColumn,
                                                 productColumn,
                                                 features,
                                                 countyFeatures,
                                                 cust_df,
                                                 discountModel,
                                                 pllel,
                                                 avgPrice,
                                                 totalQuantity,
                                                 seedsmanIDColumn,
                                                 seedsmanID,
                                                 fipsCodeColumn)
          discounts_tmp <- wrapMemoise(predict_discount_new_cust_raw, predict_discount_new_cust, predict_discount_new_cust_args)
          
          # get mean predicted disount as percentage of list price for relevant cart_df row
          discountPct <- paste0(format(round(as.numeric((mean(discounts_tmp)/as.numeric(gsub("[\\$,]", "", tmp_df[i, Price])) * 100)), 2), nsmall=2),
                                '%')
          
          # round mean of discount predicitons to 2 decimal places and place into 
          #   relevant row in cart_df
          cart_df[cart_df$Product == product_tmp, Discount := paste0('$', format(round(mean(discounts_tmp), 2), nsmall=2))]
          
          # put discount as percentage of list price into relevant cart_df row
          cart_df[cart_df$Product == product_tmp, DiscountPCT := discountPct ]
          
          # put updated discounts into recommended column
          cart_df[cart_df$Product == product_tmp, RecDiscountPCT := discountPct]
          cart_df[cart_df$Product == product_tmp, RecDiscount := cart_df[cart_df$Product == product_tmp, Discount]]
          
          # combine with previous row's discounts so portfolio average can be calculated
          tmp <-append(tmp, list(discounts_tmp))
        }
        
        # append those to discounts for new row so we have entire distribution for the order
        discounts_out <- unlist(append(tmp, list(discounts_new_cust)))
        
      } else if (nrow(cart_df) == 1) {  # if not more than one item in cart, make order total equal
        #  to the discount's new row
        discounts_out <- discounts_new_cust
      }
      
      # generate plot for entire order
      cart_plt <- make_plot_cart_order_new_cust(discounts_out,
                                                stdN,
                                                cart_df)
      cart_plt <- ggplotly(cart_plt)
      # render plot
      output$dist_plot <- renderPlotly({cart_plt})
      
      # create observe event so plot can be easily referenced when "show portfolio total graph" button is
      #  clicked
      observeEvent(input$get_total_plot, {
        output$dist_plot <- renderPlotly({cart_plt})
      })
      
      
    } else { # if customer is not new customer
      
      # get customer id
      cust_id <- get_id(input$cust_name)
      # get seedsman ID
      seedsmanID <- get_id(input$seedsman_name)
      # predict discounts for product associated with new row added to cart and 
      #  updated portfolio (new_port)
      predict_discount_args <- list(product,
                                    quantity,
                                    cust_id,
                                    cust_df,
                                    logTransformDiscount,
                                    quantityColumn,
                                    priceColumn,
                                    productColumn,
                                    custIDColumn,
                                    cartDropCols,
                                    features,
                                    discountModel,
                                    pllel,
                                    avgPrice,
                                    totalQuantity,
                                    yearColumn,
                                    seedsmanIDColumn,
                                    seedsmanID,
                                    countyFeatures,
                                    fipsCodeColumn)
      discounts <- wrapMemoise(predict_discount_raw, predict_discount, predict_discount_args)
      
      # # predict prices for product associated with updated portfolio (new_port) # CURRENTLY DEPRECATED PER REQUEST OF BUSINESS
      # prices <- predict_ep(new_port,
      #                      cust_id,
      #                      EffPrice,
      #                      logTransformEP,
      #                      epCustIDColumn,
      #                      epYearColumn,
      #                      epCartDropCols,
      #                      features_effP,
      #                      epAvgPriceColumn,
      #                      epTotalQColumn,
      #                      epModel,
      #                      marketYear,
      #                      epSeedsmanIDColumn,
      #                      seedsmanID,
      #                      epCountyFeatures,
      #                      epFipsCodeColumn)
      
      
      # update cart and table accordingly--discounts updated for most recent row predicted
      cart_df <<- update_cart(discounts, 
                              cart_df, 
                              new_row, 
                              input$cust_name, 
                              cust_df,
                              input$product_name,
                              EffPrice,
                              custIDColumn,
                              epCustIDColumn,
                              productColumn,
                              yearColumn,
                              discountColumn,
                              effectivePriceColumn,
                              seedsmanIDColumn,
                              epSeedsmanIDColumn,
                              seedsmanID,
                              products_df,
                              epYearColumn,
                              quantityColumn,
                              priceColumn)
      # if more than one item in customer's cart, call predict discount functions for all rows 
      #  where product is not the most recently added to correctly update product level discounts 
      #  now that order level stats have changed
      if (nrow(cart_df) > 1) {
        tmp <- list()
        # only do for products that are not in most recently added row
        tmp_df <- cart_df[cart_df$Product != product, ]
        for (i in 1:nrow(tmp_df)) {
          product_tmp <- unlist(tmp_df[i, 'Product'])
          quantity_tmp <- unlist(tmp_df[i, 'Quantity'])
          predict_discount_args <- list(product_tmp,
                                        quantity_tmp,
                                        cust_id,
                                        cust_df,
                                        logTransformDiscount,
                                        quantityColumn,
                                        priceColumn,
                                        productColumn,
                                        custIDColumn,
                                        cartDropCols,
                                        features,
                                        discountModel,
                                        pllel,
                                        avgPrice,
                                        totalQuantity,
                                        yearColumn,
                                        seedsmanIDColumn,
                                        seedsmanID,
                                        countyFeatures,
                                        fipsCodeColumn)
          discounts_tmp <- wrapMemoise(predict_discount_raw, predict_discount, predict_discount_args)
          
          # get mean predicted disount as percentage of list price for relevant cart_df row
          discountPct <- paste0(format(round(as.numeric((mean(discounts_tmp)/as.numeric(gsub("[\\$,]", "", tmp_df[i, Price])) * 100)), 2), nsmall=2),
                                '%')
          
          # round mean of discount predicitons to 2 decimal places and place into
          #  relevant cart_df row
          cart_df[cart_df$Product == product_tmp, Discount := paste0('$', format(round(mean(discounts_tmp), 2), nsmall=2))]
          
          # put discount as percentage of list price into relevant cart_df row
          cart_df[cart_df$Product == product_tmp, DiscountPCT := discountPct ]
          
          # get min of predicted/last allocated discount, place into relevant cart_df row 
          if (cart_df[cart_df$Product == product_tmp, LastUnitDiscount] == "NA"){
            cart_df[cart_df$Product == product_tmp, RecDiscount := cart_df[cart_df$Product == product_tmp, Discount]]
            cart_df[cart_df$Product == product_tmp, RecDiscountPCT := cart_df[cart_df$Product == product_tmp, DiscountPCT]]
          } else {
            checkDiscounts <- list(as.numeric(gsub("[\\$,]", "", cart_df[cart_df$Product == product_tmp, Discount])), 
                                   as.numeric(gsub("[\\$,]", "", cart_df[cart_df$Product == product_tmp, LastUnitDiscount])))
            checkDiscountsPCT <- list(as.numeric(gsub("[\\%,]", "", cart_df[cart_df$Product == product_tmp, DiscountPCT])), 
                                      as.numeric(gsub("[\\%,]", "", cart_df[cart_df$Product == product_tmp, LastUnitDiscountPCT])))
            cart_df[cart_df$Product == product_tmp, RecDiscount := paste0('$', format(round(min(unlist(checkDiscounts)), 2), nsmall=2))]
            cart_df[cart_df$Product == product_tmp, RecDiscountPCT := paste0(format(round(min(unlist(checkDiscountsPCT)), 2), nsmall=2), '%')]
          }
          
          # combine with previous row's discounts
          tmp <-append(tmp, list(discounts_tmp))
        }
        
        discounts_out <- unlist(append(tmp, list(discounts)))
      }
      else if (nrow(cart_df) == 1) { # if not more than one item in cart, make portfolio equal
        #  to the discount's new row
        discounts_out <- discounts
      }
      # generate plot for entire order
      cart_plt <-  make_plot_cart_order(discounts_out,
                                        stdN,
                                        cart_df,
                                        cust_id,
                                        cust_df,
                                        custIDColumn,
                                        discountColumn,
                                        yearColumn,
                                        seedsmanID,
                                        seedsmanIDColumn,
                                        priceColumn)
      cart_plt <- ggplotly(cart_plt)
      # render plot
      output$dist_plot <- renderPlotly({cart_plt})
      # create observe event so plot can be easily referenced when "show portfolio total graph" button is clicked
      observeEvent(input$get_total_plot, {
        output$dist_plot <- renderPlotly({cart_plt})
      })
    }
    
    # update order table for total order
    # if the customer's cart is empty, don't show anything
    if (nrow(cart_df) == 0) {
      renderText("")
    } else { # if not, calculate order info for display in UI
      # correct function for existing customer
      if (input$cust_name != 'New Customer') {
        cust_id <- get_id(input$cust_name)
        order_tab <- data.table()
        order_tab <- update_order_info_table(order_tab,
                                             cart_df,
                                             cust_df,
                                             cust_id,
                                             seedsmanID,
                                             custIDColumn,
                                             orderTableNames,
                                             seedsmanIDColumn,
                                             discountColumn,
                                             yearColumn,
                                             priceColumn,
                                             effectivePriceColumn,
                                             initial=TRUE,
                                             new_cust=FALSE)
        output$order_table <- renderTable({order_tab},
                                          caption='Portfolio Information',
                                          caption.placement = getOption("xtable.caption.placement", "top"))
        
        # #infobox 1 that goes along the top of the main panel
        # output$Total.Retail.Amount <- renderInfoBox({
        #   x <- order_tab$`Total Retail Amount`
        #   infoBox(
        #     tags$p(style = "font-size: 15px;", "Total Retail Amount"), tags$p(style = "font-size: 22px;", paste(x, sep = "")   ), icon = icon("dollar-sign"),
        #     color = "green", fill = TRUE
        #   )
        # })
        
        #infobox for lcr unit that goes along the top of the main panel
        output$Rec.LCR.Unit <- renderInfoBox({
          
          x <- order_tab$`Rec. LCR Unit`
          
          infoBox(
            tags$p(style = "font-size: 16px;", "Recommended LCR Unit"), 
            tags$p(style = "font-size: 22px;", paste(x, sep = "")   ),
            icon = icon("dollar-sign"),
            color = "green", fill = TRUE
          )
        })
        
        #infobox lcr % that goes along the top of the main panel
        output$Rec.LCR.Pct <- renderInfoBox({
          
          x <- order_tab$`Rec. LCR %`
          
          infoBox(
            tags$p(style = "font-size: 18px;", "Recommended LCR %"), 
            tags$p(style = "font-size: 22px;", x), 
            icon = icon("percent"),
            color = "green", fill = TRUE
          )
        })
        
        
      } else{
        # do for new customer
        order_tab <- data.table()
        order_tab <- update_order_info_table(order_tab,
                                             cart_df,
                                             cust_df,
                                             cust_id,
                                             seedsmanID,
                                             custIDColumn,
                                             orderTableNamesNewCust,
                                             seedsmanIDColumn,
                                             discountColumn,
                                             yearColumn,
                                             priceColumn,
                                             effectivePriceColumn,
                                             initial=TRUE,
                                             new_cust=TRUE)
        output$order_table <- renderTable({order_tab},
                                          caption='Portfolio Information',
                                          caption.placement = getOption("xtable.caption.placement", "top"))
        
        
        # #infobox 1 that goes along the top of the main panel
        # output$Total.Retail.Amount <- renderInfoBox({
        #   x <- order_tab$`Total Retail Amount`
        #   infoBox(
        #     tags$p(style = "font-size: 15px;", "Total Retail Amount"), tags$p(style = "font-size: 22px;", paste(x, sep = "")   ), icon = icon("dollar-sign"),
        #     color = "green", fill = TRUE
        #   )
        # })
        
        
        #infobox for lcr unit that goes along the top of the main panel
        output$Rec.LCR.Unit <- renderInfoBox({
          
          x <- order_tab$`Prevailing LCR Unit`
          
          infoBox(
            tags$p(style = "font-size: 16px;", "Recommended LCR Unit"), 
            tags$p(style = "font-size: 22px;", paste(x, sep = "")   ), 
            icon = icon("dollar-sign"),
            color = "green", fill = TRUE
          )
        })
        
        
        #infobox for lcr % that goes along the top of the main panel
        output$Rec.LCR.Pct <- renderInfoBox({
          
          x <- order_tab$`Prevailing LCR %`
          
          infoBox(
            tags$p(style = "font-size: 18px;", "Recommended LCR %"), 
            tags$p(style = "font-size: 22px;", x), 
            icon = icon("percent"),
            color = "green", fill = TRUE
          )
        })
        
        
        
      }
    }

    
    # Create cart table with buttons
    cart_ui_obj <- make_cart_ui(cart_df, new_ids = TRUE)
    output$cart_ui <- cart_ui_obj$ui
    # Create listeners for each delete button
    lapply(cart_ui_obj$ids, function(id) {
      observeEvent(input[[id]], {
        
        # delete product from cart_df
        cart_df <<- cart_df[button_id != id, ]
        
        # render download success message null
        output$download_success <- NULL
        
        # extract correct whole portfolio before subsetting by product to correctly update basket level
        # effective price predicitons
        new_port <- cart_df[, c('Product', 'Quantity', 'Price')]
        
        # clean price in whole portfolio
        new_port[, 'Price'] <-as.numeric(gsub("[\\$,]", "", new_port$Price))
        
        # get order level stats to use as memoisation keys that feed into predict discount function, can't just use new_port
        # otherwise memoisation will not work correctly, all keys must be UNIQUE
        avgPrice <- mean(new_port$Price)
        totalQuantity <- sum(new_port$Quantity)
        
        # Output cart ui or message if nothing left in cart and clear plots
        if (nrow(cart_df) == 0) {
          output$cart_ui <- renderText("Portfolio is empty.")
          output$dist_plot <- NULL
          output$order_table <- renderText("")
          # make info boxes null
          #output$Total.Retail.Amount <- NULL
          output$Rec.LCR.Unit <- NULL
          output$Rec.LCR.Pct <- NULL
        } else { # if cart not empty
          
          # get last product in cart
          tmp_cart <- tail(cart_df, 1)
          
          # create new row from last product to use in updating/plotting functions
          new_row <- tmp_cart[, c('Product', 'Quantity')]
          
          # track product and quantity for use later
          product <- unlist(new_row[, 'Product'])
          quantity <- unlist(new_row[, 'Quantity'])
          
          
          # if customer is new customer and state/county not null
          if (input$cust_name == 'New Customer' & !is.null(input$state) & !is.null(input$county)) {
            
            # get fips code for new customer's county
            fips_code <- get_fips(input$state,
                                  input$county,
                                  stateCountyToFips)
            
            # predict discounts associated for last item in cart and updated portfolio
            predict_discount_new_cust_args <- list(product,
                                                   quantity,
                                                   fips_code, 
                                                   logTransformDiscount,
                                                   quantityColumn,
                                                   priceColumn,
                                                   productColumn,
                                                   features,
                                                   countyFeatures,
                                                   cust_df,
                                                   discountModel,
                                                   pllel,
                                                   avgPrice,
                                                   totalQuantity,
                                                   seedsmanIDColumn,
                                                   seedsmanID,
                                                   fipsCodeColumn)
            discounts_new_cust <- wrapMemoise(predict_discount_new_cust_raw, predict_discount_new_cust, predict_discount_new_cust_args)
            
            # # predict prices associated with updated portfolio
            # prices_new_cust <- predict_ep_new_cust(new_port,
            #                                        fips_code,
            #                                        logTransformEP,
            #                                        features_effP,
            #                                        EffPrice,
            #                                        epAvgPriceColumn,
            #                                        epTotalQColumn,
            #                                        epFipsCodeColumn,
            #                                        countyFeatures,
            #                                        epModel,
            #                                        marketYear,
            #                                        epSeedsmanIDColumn,
            #                                        seedsmanID)
            # 
            # # update effective price with predictions for all rows in cart
            # prices_new_cust <- as.data.frame(prices_new_cust)
            # PredPrices <- unlist(lapply(cart_df$Product, function(i) prices_new_cust[prices_new_cust$Product == as.character(i),
            #                                                                          'PredictedProductEffectivePrice']))
            # cart_df[, EffectivePrice := paste0('$', format(round(PredPrices, 2), nsmall = 2))]
            
            # update last row of cart with newly predicted discounts
            cart_df[nrow(cart_df), Discount := paste0('$', format(round(mean(discounts_new_cust), 2), nsmall=2))]
            
            # discount pct
            discountPct <- paste0(format(round(as.numeric((mean(discounts_new_cust)/as.numeric(gsub("[\\$,]", "", cart_df[nrow(cart_df), Price])) * 100)), 2), nsmall=2),
                                  '%')
            cart_df[nrow(cart_df), DiscountPCT := discountPct]
            
            # put updated discounts into recommended column
            cart_df[nrow(cart_df), RecDiscountPCT := discountPct]
            cart_df[nrow(cart_df) == product_tmp, RecDiscount := cart_df[nrow(cart_df), Discount]]
            
            # update order total plot and order information table accordingly
            # for all rows that didn't get updated correctly, update discounts now that order level
            # stats have changed if more than one item in cart
            if (nrow(cart_df) > 1) {
              tmp <- c()
              for (i in 1:(nrow(cart_df)-1)) {
                product_tmp <- unlist(cart_df[i, 'Product'])
                quantity_tmp <- unlist(cart_df[i, 'Quantity'])
                predict_discount_new_cust_args <- list(product_tmp,
                                                       quantity_tmp,
                                                       fips_code, 
                                                       logTransformDiscount,
                                                       quantityColumn,
                                                       priceColumn,
                                                       productColumn,
                                                       features,
                                                       countyFeatures,
                                                       cust_df,
                                                       discountModel,
                                                       pllel,
                                                       avgPrice,
                                                       totalQuantity,
                                                       seedsmanIDColumn,
                                                       seedsmanID,
                                                       fipsCodeColumn)
                discounts_tmp <- wrapMemoise(predict_discount_new_cust_raw, predict_discount_new_cust, predict_discount_new_cust_args)
                
                # get mean predicted disount as percentage of list price for relevant cart_df row
                discountPct <- paste0(format(round(as.numeric((mean(discounts_tmp)/as.numeric(gsub("[\\$,]", "",
                                                                                                   cart_df[i, Price])) * 100)), 2), nsmall=2),
                                      '%')
                
                # round mean of discount predicitons to 2 decimal places and place into
                #  relevant cart_df row
                cart_df[i, Discount := paste0('$', format(round(mean(discounts_tmp), 2), nsmall=2))]
                
                # put discount as percentage of list price into relevant cart_df row
                cart_df[i, DiscountPCT := discountPct ]
                
                # put updated discounts into recommended column
                cart_df[i, RecDiscountPCT := discountPct]
                cart_df[i, RecDiscount := cart_df[i, Discount]]
                
                # store to average for order distribution
                tmp <-append(tmp, list(discounts_tmp))
              }
              
              discounts_out <- unlist(append(tmp, list(discounts_new_cust)))
              
            } else if (nrow(cart_df) == 1) {  # if only one item in cart...
              
              discounts_out <- discounts_new_cust
              
            }
            
            cart_plt <-  make_plot_cart_order_new_cust(discounts_out,
                                                       stdN,
                                                       cart_df)
            cart_plt <- ggplotly(cart_plt)
            
            order_tab <- update_order_info_table(order_tab, 
                                                 cart_df, 
                                                 cust_df, 
                                                 cust_id,
                                                 seedsmanID,
                                                 custIDColumn,
                                                 orderTableNamesNewCust,
                                                 seedsmanIDColumn,
                                                 discountColumn,
                                                 yearColumn,
                                                 priceColumn,
                                                 effectivePriceColumn,
                                                 initial=FALSE,
                                                 new_cust=TRUE)
            
            output$order_table <- renderTable({order_tab},
                                              caption='Portfolio Information',
                                              caption.placement = getOption("xtable.caption.placement", "top"))
            
            # #infobox 1 that goes along the top of the main panel
            # output$Total.Retail.Amount <- renderInfoBox({
            #   x <- order_tab$`Total Retail Amount`
            #   infoBox(
            #     tags$p(style = "font-size: 15px;", "Total Retail Amount"), tags$p(style = "font-size: 22px;", paste(x, sep = "")   ), icon = icon("dollar-sign"),
            #     color = "green", fill = TRUE
            #   )
            # })
            
            
            #infobox for lcr unit that goes along the top of the main panel
            output$Rec.LCR.Unit <- renderInfoBox({
              
              x <- order_tab$`Prevailing LCR Unit`
              
              infoBox(
                tags$p(style = "font-size: 16px;", "Recommended LCR Unit"), 
                tags$p(style = "font-size: 22px;", paste(x, sep = "")   ), 
                icon = icon("dollar-sign"),
                color = "green", fill = TRUE
              )
            })
            
            
            #infobox for lcr % that goes along the top of the main panel
            output$Rec.LCR.Pct <- renderInfoBox({
              
              x <- order_tab$`Prevailing LCR %`
              
              infoBox(
                tags$p(style = "font-size: 18px;", "Recommended LCR %"), 
                tags$p(style = "font-size: 22px;", x), 
                icon = icon("percent"),
                color = "green", fill = TRUE
              )
            })
            
            
          } else {  # if customer not a new customer...
            
            # get customer id
            cust_id <- get_id(input$cust_name)
            
            
            # predict discounts associated with new_row and new_port
            predict_discount_args <- list(product,
                                          quantity,
                                          cust_id,
                                          cust_df,
                                          logTransformDiscount,
                                          quantityColumn,
                                          priceColumn,
                                          productColumn,
                                          custIDColumn,
                                          cartDropCols,
                                          features,
                                          discountModel,
                                          pllel,
                                          avgPrice,
                                          totalQuantity,
                                          yearColumn,
                                          seedsmanIDColumn,
                                          seedsmanID,
                                          countyFeatures,
                                          fipsCodeColumn)
            discounts <- wrapMemoise(predict_discount_raw, predict_discount, predict_discount_args)
            
            # # update effective price
            # prices <- predict_ep(new_port,
            #                      cust_id,
            #                      EffPrice,
            #                      logTransformEP,
            #                      epCustIDColumn,
            #                      epYearColumn,
            #                      epCartDropCols,
            #                      features_effP,
            #                      epAvgPriceColumn,
            #                      epTotalQColumn,
            #                      epModel,
            #                      marketYear,
            #                      epSeedsmanIDColumn,
            #                      seedsmanID,
            #                      epCountyFeatures,
            #                      epFipsCodeColumn)
            
            # # update effective price for all products
            # prices <- as.data.frame(prices)
            # PredPrices <- unlist(lapply(cart_df$Product, function(i) prices[prices$Product == as.character(i),
            #                                                                 'PredictedProductEffectivePrice']))
            # cart_df[, EffectivePrice := paste0('$', format(round(PredPrices, 2), nsmall = 2))]
            
            # update last row of cart with newly predicted discounts
            cart_df[nrow(cart_df), Discount := paste0('$', format(round(mean(discounts), 2), nsmall=2))]
            
            # update discount pct
            discountPct <- paste0(format(round(as.numeric((mean(discounts)/as.numeric(gsub("[\\$,]", "", cart_df[nrow(cart_df), Price])) * 100)), 2), nsmall=2),
                                  '%')
            cart_df[nrow(cart_df), DiscountPCT := discountPct]
            
            # Take minimum of predicted/actual and make "recommended" in last row of cart_df
            # if NA...
            if (cart_df[nrow(cart_df), LastUnitDiscount] == "NA"){
              cart_df[nrow(cart_df), RecDiscount := cart_df[nrow(cart_df), Discount]]
              cart_df[nrow(cart_df), RecDiscountPCT := cart_df[nrow(cart_df), DiscountPCT]]
            } else {
              checkDiscounts <- list(as.numeric(gsub("[\\$,]", "", cart_df[nrow(cart_df), Discount])), 
                                     as.numeric(gsub("[\\$,]", "", cart_df[nrow(cart_df), LastUnitDiscount])))
              checkDiscountsPCT <- list(as.numeric(gsub("[\\%,]", "", cart_df[nrow(cart_df), DiscountPCT])), 
                                        as.numeric(gsub("[\\%,]", "", cart_df[nrow(cart_df), LastUnitDiscountPCT])))
              cart_df[nrow(cart_df), RecDiscount := paste0('$', format(round(min(unlist(checkDiscounts)), 2), nsmall=2))]
              cart_df[nrow(cart_df), RecDiscountPCT := paste0(format(round(min(unlist(checkDiscountsPCT)), 2), nsmall=2), '%')]
            }
            
            # if more than one item in cart, correctly update discounts and order totals now that order
            # level stats have changed for items that are not the last row in cart
            if (nrow(cart_df) > 1) {
              tmp <- list()
              for (i in 1:(nrow(cart_df)-1)) {
                product_tmp <- unlist(cart_df[i, 'Product'])
                quantity_tmp <- unlist(cart_df[i, 'Quantity'])
                predict_discount_args <- list(product_tmp,
                                              quantity_tmp,
                                              cust_id,
                                              cust_df,
                                              logTransformDiscount,
                                              quantityColumn,
                                              priceColumn,
                                              productColumn,
                                              custIDColumn,
                                              cartDropCols,
                                              features,
                                              discountModel,
                                              pllel,
                                              avgPrice,
                                              totalQuantity,
                                              yearColumn,
                                              seedsmanIDColumn,
                                              seedsmanID,
                                              countyFeatures,
                                              fipsCodeColumn)
                discounts_tmp <- wrapMemoise(predict_discount_raw, predict_discount, predict_discount_args)
                
                # get mean predicted disount as percentage of list price
                discountPct <- paste0(format(round(as.numeric((mean(discounts_tmp)/as.numeric(gsub("[\\$,]", "", cart_df[i, Price])) * 100)), 2), nsmall=2),
                                      '%')
                # round mean of discount predicitons to 2 decimal places and place into
                # relevant cart_df row
                cart_df[i, Discount := paste0('$', format(round(mean(discounts_tmp), 2), nsmall=2))]
                
                # put discount as percentage of list price into relevant cart_df row
                cart_df[i, DiscountPCT := discountPct ]
                
                # Take minimum of predicted/actual and make "recommended"
                # if NA...
                if (cart_df[i, LastUnitDiscount] == "NA"){
                  cart_df[i, RecDiscount := cart_df[i, Discount]]
                  cart_df[i, RecDiscountPCT := cart_df[i, DiscountPCT]]
                } else {
                  checkDiscounts <- list(as.numeric(gsub("[\\$,]", "", cart_df[i, Discount])), 
                                         as.numeric(gsub("[\\$,]", "", cart_df[i, LastUnitDiscount])))
                  checkDiscountsPCT <- list(as.numeric(gsub("[\\%,]", "", cart_df[i, DiscountPCT])), 
                                            as.numeric(gsub("[\\%,]", "", cart_df[i, LastUnitDiscountPCT])))
                  cart_df[i, RecDiscount := paste0('$', format(round(min(unlist(checkDiscounts)), 2), nsmall=2))]
                  cart_df[i, RecDiscountPCT := paste0(format(round(min(unlist(checkDiscountsPCT)), 2), nsmall=2), '%')]
                }
                
                # combine with previous row's discounts
                tmp <-append(tmp, list(discounts_tmp))
              }
              # combine new_row discounts and all other newly updated discounts
              discounts_out <- unlist(append(tmp, list(discounts)))
            }
            # if only one item in cart make discounts new_row
            else if (nrow(cart_df) == 1) {  # if one item in cart...
              
              # make discount to be plotted only the discounts for new_row
              discounts_out <- discounts
              
            }
            
            # make plot for whole order using discounts_out
            cart_plt <-  make_plot_cart_order(discounts_out,
                                              stdN,
                                              cart_df, 
                                              cust_id,
                                              cust_df,
                                              custIDColumn,
                                              discountColumn,
                                              yearColumn,
                                              seedsmanID,
                                              seedsmanIDColumn,
                                              priceColumn)
            cart_plt <- ggplotly(cart_plt)
            
            # update order information table using updated cart_df
            order_tab <- update_order_info_table(order_tab,
                                                 cart_df,
                                                 cust_df,
                                                 cust_id,
                                                 seedsmanID,
                                                 custIDColumn,
                                                 orderTableNames,
                                                 seedsmanIDColumn,
                                                 discountColumn,
                                                 yearColumn,
                                                 priceColumn,
                                                 effectivePriceColumn,
                                                 initial=FALSE,
                                                 new_cust=FALSE)
            # render table in UI
            output$order_table <- renderTable({order_tab},
                                              caption='Portfolio Information',
                                              caption.placement = getOption("xtable.caption.placement", "top"))
            
            
            # #infobox 1 that goes along the top of the main panel
            # output$Total.Retail.Amount <- renderInfoBox({
            #   x <- order_tab$`Total Retail Amount`
            #   infoBox(
            #     tags$p(style = "font-size: 15px;", "Total Retail Amount"), tags$p(style = "font-size: 22px;", paste(x, sep = "")   ), icon = icon("dollar-sign"),
            #     color = "green", fill = TRUE
            #   )
            # })
            # 
            # 
            
            #infobox for lcr unit that goes along the top of the main panel
            output$Rec.LCR.Unit <- renderInfoBox({
              
              x <- order_tab$`Rec. LCR Unit`
              
              infoBox(
                tags$p(style = "font-size: 16px;", "Recommended LCR Unit"), 
                tags$p(style = "font-size: 22px;", paste(x, sep = "")   ), 
                icon = icon("dollar-sign"),
                color = "green", fill = TRUE
              )
            })
            
            #infobox for lcr % that goes along the top of the main panel
            output$Rec.LCR.Pct <- renderInfoBox({
              
              x <- order_tab$`Rec. LCR %`
              
              infoBox(
                tags$p(style = "font-size: 18px;", "Recommended LCR %"), 
                tags$p(style = "font-size: 22px;", x), 
                icon = icon("percent"),
                color = "green", fill = TRUE
              )
            })
            
          }
          
          # update UI object to reflect updated cart
          cart_ui_obj <- make_cart_ui(cart_df, new_ids = FALSE)
          
          # render plot in UI
          output$dist_plot <- renderPlotly({cart_plt})
          
          # render UI
          output$cart_ui <- cart_ui_obj$ui
          
          # observe event for switching back and forth between portfolio and product view
          observeEvent(input$get_total_plot, {
            output$dist_plot <- renderPlotly({cart_plt})
          }, ignoreInit=TRUE)
        }
      }, ignoreInit=TRUE)
    })
    # Create listeners for each get discount button
    lapply(cart_ui_obj$ids, function(id) {
      id2 <- paste0(id, "_gd")
      
      # do the following for each button...
      observeEvent(input[[id2]], {
        
        # make copy of cart_df to extract price for order level stats calculation
        cart_df_tmp <- copy(cart_df)
        
        cart_df_tmp[, 'Price'] <-as.numeric(gsub("[\\$,]", "", cart_df_tmp$Price))
        
        # get order level stats to use as memoisation keys that feed into predict discount function, can't just use new_port
        #  otherwise memoisation will not work correctly, all keys must be UNIQUE
        #   Here we call the cart and not new_port because no product is being added to the cart
        avgPrice <- mean(cart_df_tmp$Price)
        totalQuantity <- sum(cart_df_tmp$Quantity)
        
        # if customer is new...
        if (input$cust_name == 'New Customer' & !is.null(input$state) & !is.null(input$county)) {
          
          # get fips code based on user inputted state and county
          fips_code <- get_fips(input$state, input$county, stateCountyToFips)
          
          # predict discounts
          predict_discount_new_cust_args <- list(unlist(cart_df[button_id == id, 'Product']),
                                                 unlist(cart_df[button_id == id, 'Quantity']),
                                                 fips_code, 
                                                 logTransformDiscount,
                                                 quantityColumn,
                                                 priceColumn,
                                                 productColumn,
                                                 features,
                                                 countyFeatures,
                                                 cust_df,
                                                 discountModel,
                                                 pllel,
                                                 avgPrice,
                                                 totalQuantity,
                                                 seedsmanIDColumn,
                                                 seedsmanID,
                                                 fipsCodeColumn)
          discounts_new_cust <- wrapMemoise(predict_discount_new_cust_raw, predict_discount_new_cust, predict_discount_new_cust_args)
        } else {  # if customer exists...
          # get customer id
          cust_id <- get_id(input$cust_name)
          
          # get seedsmanID
          seedsmanID <- get_id(input$seedsman_name)
          
          # get predicted discounts for product associated with button_id
          predict_discount_args <- list(unlist(cart_df[button_id == id, "Product"]),
                                        unlist(cart_df[button_id == id, "Quantity"]),
                                        cust_id,
                                        cust_df,
                                        logTransformDiscount,
                                        quantityColumn,
                                        priceColumn,
                                        productColumn,
                                        custIDColumn,
                                        cartDropCols,
                                        features,
                                        discountModel,
                                        pllel,
                                        avgPrice,
                                        totalQuantity,
                                        yearColumn,
                                        seedsmanIDColumn,
                                        seedsmanID,
                                        countyFeatures,
                                        fipsCodeColumn)
          discounts <- wrapMemoise(predict_discount_raw, predict_discount, predict_discount_args)
        }
        
        cart_ui_obj <- make_cart_ui(cart_df, new_ids = FALSE)
        
        # Output cart ui or message
        if (nrow(cart_df) == 0) {
          output$cart_ui <- renderText("Portfolio is empty.")
        } else {
          output$cart_ui <- cart_ui_obj$ui
          # The following code renders the plot of the distribution of predicted
          #  per-unit discount for each discount listener button, some customers will not have an
          #  observed last unit discount, thus need to plot accordingly
          # If new customer...
          if (input$cust_name == 'New Customer' & !is.null(input$state) & !is.null(input$county)) {
            
            # get correct list of predictions to be plotted (discounts_new_cust)
            plot_discounts <- discounts_new_cust
            
          } else {  # If not new customer...
            # generate list of predictions to be plotted (discounts)
            plot_discounts <- discounts
          }
          # If customer does have a last unit discount, make correct plot
          if (cart_df[button_id == id, LastUnitDiscount] != as.character("NA")) {
            button_plt <- make_plot_button(plot_discounts, cart_df, stdN, id, lastDisc=TRUE)
            button_plt <- ggplotly(button_plt)
            output$dist_plot <- renderPlotly({button_plt})
            
          } else { # Make plot that doesn't have vertical line, essentially the same code as above
            button_plt <- make_plot_button(plot_discounts, cart_df, stdN, id, lastDisc=FALSE)
            button_plt <- ggplotly(button_plt)
            output$dist_plot <- renderPlotly({button_plt})
            
          }
        }
      }, ignoreInit=TRUE)
    })
    
    # Reset input
    updateNumericInput(session, "product_quantity", value = 0)
    shinyjs::show(id = "show_hide")
  }, ignoreInit=TRUE)
  
  # download handler for grower report ---------------------------------------------------------------
  output$download_grower_report_CORN <- downloadHandler(
    filename = function() {
      paste('MARSgrowerReportCORN-', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(grower_report, con)
    }
  )
  
  # END CORN ADD TO PORTFOLIO -----------------------------------------------------------------------
  
  # SOY ----------------------------------------------------------------------------------------------
  observeEvent(input$add_to_cart_soy, {
    req(input$seedsman_name)
    req(input$cust_name)
    req(input$product_name_soy)
    req(input$product_quantity_soy)
    # clear download success message
    output$download_success_soy <- NULL
    
    # deals with situation if prepopulate portfolio button has been pressed and then items deleted from cart
    if (exists('cart_df_check_soy')){ 
      if(!is.null(cart_df_check_soy)){
        cart_df_soy <<- cart_df_check_soy
        cart_df_check_soy <<- NULL
      }
    }
    
    # make new row data table from user input
    new_row <- data.table(Product = input$product_name_soy,
                          Quantity = input$product_quantity_soy)
    # add list price
    new_row_tmp <- merge(new_row,
                         products_df_soy,
                         by = "Product")
    
    # track product for use later in predict functions
    product <- unlist(new_row_tmp[, 'Product'])
    
    # combine with existing cart
    new_port_tmp <- rbind(cart_df_soy, new_row_tmp, use.names = TRUE, fill = TRUE)
    
    # clean $ from price column to merge
    new_port_tmp[, 'Price'] <- as.numeric(gsub("[\\$,]", "", new_port_tmp$Price))
    
    # extract whole portfolio before subsetting by product to correctly make basket level effective price predicitons
    new_port <- new_port_tmp[, .(Quantity = sum(Quantity),
                                 Price = mean(Price)),
                             by = Product]
    
    # get order level stats to use as memoisation keys that feed into predict discount function, can't just use new_port
    # otherwise memoisation will not work correctly, all keys must be UNIQUE
    avgPrice <- mean(new_port$Price)
    totalQuantity <- sum(new_port$Quantity)
    
    # collapse by product
    tmp <- new_port_tmp[new_port_tmp$Product == unlist(product), ]
    
    # get correct quantity for product added to cart to use in discount prediction later
    new_row_tmp <- tmp[, .(Quantity = sum(Quantity),
                           Price = mean(Price)),
                       by = Product]
    quantity <- unlist(new_row_tmp[, 'Quantity'])
    
    # if customer is a new customer and state/county isn't null, update cart with predictions
    #  and show correct graph
    if (input$cust_name == 'New Customer' & !is.null(input$state) & !is.null(input$county)){
      
      # get fips code
      fips_code <- get_fips(input$state,
                            input$county,
                            stateCountyToFips)
      # predict discounts associated with new row
      predict_discount_new_cust_soy_args <- list(product,
                                                 quantity,
                                                 fips_code,
                                                 logTransformDiscountSoy,
                                                 priceColumnSoy,
                                                 featuresSoy,
                                                 countyFeaturesSoy,
                                                 cust_df_soy,
                                                 discountModelSoy,
                                                 pllel,
                                                 avgPrice,
                                                 totalQuantity,
                                                 products_df_soy,
                                                 totalQColumnSoy,
                                                 avgPriceColumnSoy,
                                                 marketYear,
                                                 fipsCodeColumnSoy)
      discounts_new_cust <- wrapMemoise(predict_discount_raw_soy_new_cust, predict_discount_new_cust_soy, predict_discount_new_cust_soy_args)
      
      # # predict prices associated with udpated portfolio (new port)
      # prices_new_cust <- predict_ep_new_cust_soy(new_port,
      #                                            EffPrice_soy,
      #                                            fips_code,
      #                                            features_EffPSoy,
      #                                            epAvgPriceColumnSoy,
      #                                            epTotalQColumnSoy,
      #                                            epFipsCodeColumnSoy,
      #                                            epCountyFeaturesSoy,
      #                                            epModelSoy,
      #                                            marketYear,
      #                                            products_df_soy)
      
      # update cart and table accordingly most recent product gets updated and all effective
      #  prices get updated
      cart_df_soy <<- update_cart_new_cust(discounts_new_cust,
                                           cart_df_soy,
                                           new_row,
                                           fips_code,
                                           product,
                                           products_df_soy)
      
      # if more than one item in customer's cart, call predict_discount functions for all rows
      #  not equal to latest added product
      if (nrow(cart_df_soy) > 1) {
        tmp <- c()
        tmp_df <- cart_df_soy[cart_df_soy$Product != product, ]
        for (i in 1:nrow(tmp_df)) {
          product_tmp <- unlist(tmp_df[i, 'Product'])
          quantity_tmp <- unlist(tmp_df[i, 'Quantity'])
          predict_discount_new_cust_soy_args <- list(product_tmp,
                                                     quantity_tmp,
                                                     fips_code,
                                                     logTransformDiscountSoy,
                                                     priceColumnSoy,
                                                     featuresSoy,
                                                     countyFeaturesSoy,
                                                     cust_df_soy,
                                                     discountModelSoy,
                                                     pllel,
                                                     avgPrice,
                                                     totalQuantity,
                                                     products_df_soy,
                                                     totalQColumnSoy,
                                                     avgPriceColumnSoy,
                                                     marketYear,
                                                     fipsCodeColumnSoy)
          discounts_tmp <- wrapMemoise(predict_discount_raw_soy_new_cust, predict_discount_new_cust_soy, predict_discount_new_cust_soy_args)
          
          # get mean predicted disount as percentage of list price for relevant cart_df_soy row
          discountPct <- paste0(format(round(as.numeric((mean(discounts_tmp)/as.numeric(gsub("[\\$,]", "", tmp_df[i, Price])) * 100)), 2), nsmall=2),
                                '%')
          
          # round mean of discount predicitons to 2 decimal places and place into
          # relevant row in cart_df_soy
          cart_df_soy[cart_df_soy$Product == product_tmp, Discount := paste0('$', format(round(mean(discounts_tmp), 2), nsmall=2))]
          
          # put discount as percentage of list price into relevant cart_df_soy row
          cart_df_soy[cart_df_soy$Product == product_tmp, DiscountPCT := discountPct ]
          
          # put updated discounts into recommended column
          cart_df_soy[cart_df_soy$Product == product_tmp, RecDiscountPCT := discountPct]
          cart_df_soy[cart_df_soy$Product == product_tmp, RecDiscount := cart_df_soy[cart_df_soy$Product == product_tmp, Discount]]
          
          # combine with previous row's discounts
          tmp <-append(tmp, list(discounts_tmp))
        }
        
        # append those to discounts for new row
        discounts_out <- unlist(append(tmp, list(discounts_new_cust)))
        
      } else if (nrow(cart_df_soy) == 1) {  # if not more than one item in cart, make order total equal
        #  to the discount's new row
        discounts_out <- discounts_new_cust
      }
      
      # generate plot for entire order
      cart_plt_soy <- make_plot_cart_order_new_cust(discounts_out,
                                                    stdN,
                                                    cart_df_soy)
      cart_plt_soy <- ggplotly(cart_plt_soy)
      
      # render plot
      output$dist_plot_soy <- renderPlotly({cart_plt_soy})
      
      # create observe event so plot can be easily referenced when "show portfolio total graph" button is
      #  clicked
      observeEvent(input$get_total_plot_soy, {
        output$dist_plot_soy <- renderPlotly({cart_plt_soy})
      })
      
      
    } else { # if customer is not new customer
      
      # get customer id
      cust_id_soy <- get_id(input$cust_name)
      # get seedsmanID
      seedsmanID_soy <- get_id(input$seedsman_name)
      
      # predict discounts for product associated with most recent product and updated porftolio
      predict_discount_soy_args <- list(product,
                                        quantity,
                                        cust_id_soy,
                                        cust_df_soy,
                                        logTransformDiscountSoy,
                                        quantityColumnSoy,
                                        priceColumnSoy,
                                        productColumnSoy,
                                        custIDColumnSoy,
                                        cartDropColsSoy,
                                        featuresSoy,
                                        discountModelSoy,
                                        pllel,
                                        avgPrice,
                                        totalQuantity,
                                        yearColumnSoy,
                                        marketYear,
                                        products_df_soy,
                                        totalQColumnSoy,
                                        avgPriceColumnSoy,
                                        seedsmanIDColumnSoy,
                                        seedsmanID_soy,
                                        fipsCodeColumnSoy,
                                        countyFeaturesSoy)
      discounts <- wrapMemoise(predict_discount_raw_soy, predict_discount_soy, predict_discount_soy_args)
      
      
      # # predict prices for products associated with updated portfolio
      # prices <- predict_ep_soy(new_port,
      #                          cust_id_soy,
      #                          EffPrice_soy,
      #                          logTransformEPSoy,
      #                          epCustIDColumnSoy,
      #                          epYearColumnSoy,
      #                          epCartDropColsSoy,
      #                          features_EffPSoy,
      #                          epAvgPriceColumnSoy,
      #                          epTotalQColumnSoy,
      #                          epModelSoy,
      #                          marketYear,
      #                          products_df_soy,
      #                          epCountyFeaturesSoy,
      #                          epFipsCodeColumnSoy,
      #                          epSeedsmanIDColumnSoy,
      #                          seedsmanID_soy)
      
      
      # update cart and table accordingly--most recent addition is updated and all
      #  effective prices are updated
      cart_df_soy <<- update_cart(discounts,
                                  cart_df_soy,
                                  new_row,
                                  input$cust_name,
                                  cust_df_soy,
                                  input$product_name_soy,
                                  EffPrice_soy,
                                  custIDColumnSoy,
                                  epCustIDColumnSoy,
                                  productColumnSoy,
                                  yearColumnSoy,
                                  discountColumnSoy,
                                  effectivePriceColumnSoy,
                                  seedsmanIDColumnSoy,
                                  epSeedsmanIDColumnSoy,
                                  seedsmanID_soy,
                                  products_df_soy,
                                  epYearColumnSoy,
                                  quantityColumnSoy,
                                  priceColumnSoy)
      
      # if more than one item in customer's cart, call predict_discount functions
      #  to update product level discounts now that order level values have changed 
      if (nrow(cart_df_soy) > 1) {
        tmp <- list()
        tmp_df <- cart_df_soy[cart_df_soy$Product != product, ]
        for (i in 1:nrow(tmp_df)) {
          product_tmp <- unlist(tmp_df[i, 'Product'])
          quantity_tmp <- unlist(tmp_df[i, 'Quantity'])
          predict_discount_soy_args <- list(product_tmp,
                                            quantity_tmp,
                                            cust_id_soy,
                                            cust_df_soy,
                                            logTransformDiscountSoy,
                                            quantityColumnSoy,
                                            priceColumnSoy,
                                            productColumnSoy,
                                            custIDColumnSoy,
                                            cartDropColsSoy,
                                            featuresSoy,
                                            discountModelSoy,
                                            pllel,
                                            avgPrice,
                                            totalQuantity,
                                            yearColumnSoy,
                                            marketYear,
                                            products_df_soy,
                                            totalQColumnSoy,
                                            avgPriceColumnSoy,
                                            seedsmanIDColumnSoy,
                                            seedsmanID_soy,
                                            fipsCodeColumnSoy,
                                            countyFeaturesSoy)
          discounts_tmp <- wrapMemoise(predict_discount_raw_soy, predict_discount_soy, predict_discount_soy_args)
          
          # get mean predicted disount as percentage of list price for relevant cart_df_soy row
          discountPct <- paste0(format(round(as.numeric((mean(discounts_tmp)/as.numeric(gsub("[\\$,]", "", tmp_df[i, Price])) * 100)), 2), nsmall=2),
                                '%')
          
          # round mean of discount predicitons to 2 decimal places and place into
          #  relevant cart_df_soy row
          cart_df_soy[cart_df_soy$Product == product_tmp, Discount := paste0('$', format(round(mean(discounts_tmp), 2), nsmall=2))]
          
          # put discount as percentage of list price into relevant cart_df_soy row
          cart_df_soy[cart_df_soy$Product == product_tmp, DiscountPCT := discountPct ]
          
          # get min of predicted/last allocated discount, place into relevant cart_df_soy row 
          if (cart_df_soy[cart_df_soy$Product == product_tmp, LastUnitDiscount] == "NA"){
            cart_df_soy[cart_df_soy$Product == product_tmp, RecDiscount := cart_df_soy[cart_df_soy$Product == product_tmp, Discount]]
            cart_df_soy[cart_df_soy$Product == product_tmp, RecDiscountPCT := cart_df_soy[cart_df_soy$Product == product_tmp, DiscountPCT]]
          } else {
            checkDiscounts <- list(as.numeric(gsub("[\\$,]", "", cart_df_soy[cart_df_soy$Product == product_tmp, Discount])), 
                                   as.numeric(gsub("[\\$,]", "", cart_df_soy[cart_df_soy$Product == product_tmp, LastUnitDiscount])))
            checkDiscountsPCT <- list(as.numeric(gsub("[\\%,]", "", cart_df_soy[cart_df_soy$Product == product_tmp, DiscountPCT])), 
                                      as.numeric(gsub("[\\%,]", "", cart_df_soy[cart_df_soy$Product == product_tmp, LastUnitDiscountPCT])))
            cart_df_soy[cart_df_soy$Product == product_tmp, RecDiscount := paste0('$', format(round(min(unlist(checkDiscounts)), 2), nsmall=2))]
            cart_df_soy[cart_df_soy$Product == product_tmp, RecDiscountPCT := paste0(format(round(min(unlist(checkDiscountsPCT)), 2), nsmall=2), '%')]
          }
          
          # combine with previous row's discounts
          tmp <-append(tmp, list(discounts_tmp))
        }
        
        discounts_out <- unlist(append(tmp, list(discounts)))
      }
      else if (nrow(cart_df_soy) == 1) { # if not more than one item in cart, make portfolio equal
        #  to the discount's new row
        discounts_out <- discounts
      }
      
      # generate plot for entire order
      cart_plt_soy <-  make_plot_cart_order(discounts_out,
                                            stdN,
                                            cart_df_soy,
                                            cust_id_soy,
                                            cust_df_soy,
                                            custIDColumnSoy,
                                            discountColumnSoy,
                                            yearColumnSoy,
                                            seedsmanID_soy,
                                            seedsmanIDColumnSoy,
                                            priceColumnSoy)
      cart_plt_soy <- ggplotly(cart_plt_soy)
      # render plot
      output$dist_plot_soy <- renderPlotly({cart_plt_soy})
      
      # create observe event so plot can be easily referenced when "show portfolio total graph" button is clicked
      observeEvent(input$get_total_plot_soy, {
        output$dist_plot_soy <- renderPlotly({cart_plt_soy})
      })
    }
    # update order table for total order
    # if the customer's cart is empty, don't show anything
    if (nrow(cart_df_soy) == 0) {
      renderText("")
    } else { # if not, calculate order info for display in UI
      # correct function for existing customer
      if (input$cust_name != 'New Customer') {
        cust_id_soy <- get_id(input$cust_name)
        order_tab <- data.table()
        order_tab <- update_order_info_table(order_tab,
                                             cart_df_soy,
                                             cust_df_soy,
                                             cust_id_soy,
                                             seedsmanID_soy,
                                             custIDColumnSoy,
                                             orderTableNames,
                                             seedsmanIDColumnSoy,
                                             discountColumnSoy,
                                             yearColumnSoy,
                                             priceColumnSoy,
                                             effectivePriceColumnSoy,
                                             initial=TRUE,
                                             new_cust=FALSE)
        output$order_table_soy <- renderTable({order_tab},
                                              caption='Portfolio Information',
                                              caption.placement = getOption("xtable.caption.placement", "top"))
        
        # #infobox 1 that goes along the top of the main panel
        # output$Total.Retail.Amount_soy <- renderInfoBox({
        #   x <- order_tab$`Total Retail Amount`
        #   infoBox(
        #     tags$p(style = "font-size: 15px;", "Total Retail Amount"), tags$p(style = "font-size: 22px;", paste(x, sep = "")   ), icon = icon("dollar-sign"),
        #     color = "green", fill = TRUE
        #   )
        # })
        # 
        
        #infobox for lcr % that goes along the top of the main panel
        output$Rec.LCR.Unit_soy <- renderInfoBox({
          
          x <- order_tab$`Rec. LCR Unit`
          
          infoBox(
            tags$p(style = "font-size: 16px;", "Recommended LCR Unit"), 
            tags$p(style = "font-size: 22px;", paste(x, sep = "")   ), 
            icon = icon("dollar-sign"),
            color = "green", fill = TRUE
          )
        })
        
        #infobox for lcr % that goes along the top of the main panel
        output$Rec.LCR.Pct_soy <- renderInfoBox({
          
          x <- order_tab$`Rec. LCR %`
          
          infoBox(
            tags$p(style = "font-size: 18px;", "Recommended LCR %"), 
            tags$p(style = "font-size: 22px;", x), 
            icon = icon("percent"),
            color = "green", fill = TRUE
          )
        })
        
        
      } else{
        
        order_tab <- data.table()
        order_tab <- update_order_info_table(order_tab,
                                             cart_df_soy,
                                             cust_df_soy,
                                             cust_id_soy,
                                             seedsmanID_soy,
                                             custIDColumnSoy,
                                             orderTableNamesNewCust,
                                             seedsmanIDColumnSoy,
                                             discountColumnSoy,
                                             yearColumnSoy,
                                             priceColumnSoy,
                                             effectivePriceColumnSoy,
                                             initial=TRUE,
                                             new_cust=TRUE)
        output$order_table_soy <- renderTable({order_tab},
                                              caption='Portfolio Information',
                                              caption.placement = getOption("xtable.caption.placement", "top"))
        
        
        # #infobox 1 that goes along the top of the main panel
        # output$Total.Retail.Amount_soy <- renderInfoBox({
        #   x <- order_tab$`Total Retail Amount`
        #   infoBox(
        #     tags$p(style = "font-size: 15px;", "Total Retail Amount"), tags$p(style = "font-size: 22px;", paste(x, sep = "")   ), icon = icon("dollar-sign"),
        #     color = "green", fill = TRUE
        #   )
        # })
        
        
        #infobox for lcr unit that goes along the top of the main panel
        output$Rec.LCR.Unit_soy <- renderInfoBox({
          
          x <- order_tab$`Prevailing LCR Unit`
          
          infoBox(
            tags$p(style = "font-size: 16px;", "Recommended LCR Unit"), 
            tags$p(style = "font-size: 22px;", paste(x, sep = "")   ), 
            icon = icon("dollar-sign"),
            color = "green", fill = TRUE
          )
        })
        
        
        #infobox for lcr percent that goes along the top of the main panel
        output$Rec.LCR.Pct_soy <- renderInfoBox({
          
          x <- order_tab$`Prevailing LCR %`
          
          infoBox(
            tags$p(style = "font-size: 18px;", "Recommended LCR %"), 
            tags$p(style = "font-size: 22px;", x), 
            icon = icon("percent"),
            color = "green", fill = TRUE
          )
        })
        
        
        
      }
    }
    
    # Create cart table with buttons
    cart_ui_obj_soy <- make_cart_ui(cart_df_soy, new_ids = TRUE)
    output$cart_ui_soy <- cart_ui_obj_soy$ui
    # Create listeners for each delete button
    lapply(cart_ui_obj_soy$ids, function(id) {
      observeEvent(input[[id]], {
        
        # delete product from cart_df_soy
        cart_df_soy <<- cart_df_soy[button_id != id, ]
        
        # clear download success message
        output$download_success_soy <- NULL
        
        # extract correct whole portfolio before subsetting by product to correctly update basket level
        # effective price predicitons
        new_port <- cart_df_soy[, c('Product',
                                    'Quantity',
                                    'Price')]
        
        # clean price in whole portfolio
        new_port[, 'Price'] <-as.numeric(gsub("[\\$,]", "", new_port$Price))
        
        # get order level stats to use as memoisation keys that feed into predict discount function, can't just use new_port
        # otherwise memoisation will not work correctly, all keys must be UNIQUE
        avgPrice <- mean(new_port$Price)
        totalQuantity <- sum(new_port$Quantity)
        
        # Output cart ui or message if nothing left in cart and clear plots
        if (nrow(cart_df_soy) == 0) {
          output$cart_ui_soy <- renderText({'Portfolio is empty. '})
          output$dist_plot_soy <- NULL
          output$order_table_soy <- NULL
          # make info boxes null
          #output$Total.Retail.Amount_soy <- NULL
          output$Rec.LCR.Unit_soy <- NULL
          output$Rec.LCR.Pct_soy <- NULL
        } else { # if cart not empty
          
          # get last product in cart
          tmp_cart <- tail(cart_df_soy, 1)
          
          # create new row from last product to use in updating/plotting functions
          new_row <- tmp_cart[, c('Product', 'Quantity')]
          
          # track product and quantity for use later
          product <- unlist(new_row[, 'Product'])
          quantity <- unlist(new_row[, 'Quantity'])
          
          
          # if customer is new customer and state/county not null
          if (input$cust_name == 'New Customer' & !is.null(input$state) & !is.null(input$county)) {
            
            # get fips code for new customer's county
            fips_code <- get_fips(input$state,
                                  input$county,
                                  stateCountyToFips)
            
            # predict discounts associated with last item in cart and updated portfolio
            predict_discount_new_cust_soy_args <- list(product,
                                                       quantity,
                                                       fips_code,
                                                       logTransformDiscountSoy,
                                                       priceColumnSoy,
                                                       featuresSoy,
                                                       countyFeaturesSoy,
                                                       cust_df_soy,
                                                       discountModelSoy,
                                                       pllel,
                                                       avgPrice,
                                                       totalQuantity,
                                                       products_df_soy,
                                                       totalQColumnSoy,
                                                       avgPriceColumnSoy,
                                                       marketYear,
                                                       fipsCodeColumnSoy)
            discounts_new_cust <- wrapMemoise(predict_discount_raw_soy_new_cust, predict_discount_new_cust_soy, predict_discount_new_cust_soy_args)
            
            # # predict prices associated with new portfolio # CURRENTLY DEPRECATED PER REQUEST OF THE BUSINESS
            # prices_new_cust <- predict_ep_new_cust_soy(new_port,
            #                                            EffPrice_soy,
            #                                            fips_code,
            #                                            features_EffPSoy,
            #                                            epAvgPriceColumnSoy,
            #                                            epTotalQColumnSoy,
            #                                            epFipsCodeColumnSoy,
            #                                            epCountyFeaturesSoy,
            #                                            epModelSoy,
            #                                            marketYear,
            #                                            products_df_soy)
            # 
            # # update effective price with predictions for all products
            # prices_new_cust <- as.data.frame(prices_new_cust)
            # PredPrices <- unlist(lapply(cart_df_soy$Product,
            #                             function(i) prices_new_cust[prices_new_cust$Product == as.character(i),
            #                                                         'PredictedProductEffectivePrice']))
            # cart_df_soy[, EffectivePrice := paste0('$', format(round(PredPrices, 2), nsmall = 2))]
            
            # round mean of discount predicitons to 2 decimal places and place into
            #  relevant cart_df_soy row (last item in cart)
            cart_df_soy[nrow(cart_df_soy), Discount := paste0('$', format(round(mean(discounts_new_cust), 2), nsmall=2))]
            
            # get mean predicted disount as percentage of list price for relevant cart_df_soy row
            discountPct <- paste0(format(round(as.numeric((mean(discounts_new_cust)/as.numeric(gsub("[\\$,]", "", cart_df_soy[nrow(cart_df_soy), Price])) * 100)), 2), nsmall=2),
                                  '%')
            # put discount as percentage of list price into relevant cart_df_soy row
            cart_df_soy[nrow(cart_df_soy), DiscountPCT := discountPct ]
            
            # put updated discounts into recommended column
            cart_df_soy[nrow(cart_df_soy), RecDiscountPCT := discountPct]
            cart_df_soy[nrow(cart_df_soy), RecDiscount := cart_df_soy[nrow(cart_df_soy), Discount]]
            
            # update order total plot and order information table accordingly...
            # for all rows that didn't get updated correctly, update discounts now that order level
            # stats have changed if more than one item in cart
            if (nrow(cart_df_soy) > 1) {
              tmp <- c()
              for (i in 1:(nrow(cart_df_soy)-1)) {
                product_tmp <- unlist(cart_df_soy[i, 'Product'])
                quantity_tmp <- unlist(cart_df_soy[i, 'Quantity'])
                predict_discount_new_cust_soy_args <- list(product_tmp,
                                                           quantity_tmp,
                                                           fips_code,
                                                           logTransformDiscountSoy,
                                                           priceColumnSoy,
                                                           featuresSoy,
                                                           countyFeaturesSoy,
                                                           cust_df_soy,
                                                           discountModelSoy,
                                                           pllel,
                                                           avgPrice,
                                                           totalQuantity,
                                                           products_df_soy,
                                                           totalQColumnSoy,
                                                           avgPriceColumnSoy,
                                                           marketYear,
                                                           fipsCodeColumnSoy)
                discounts_tmp <- wrapMemoise(predict_discount_raw_soy_new_cust, predict_discount_new_cust_soy, predict_discount_new_cust_soy_args)
                
                
                # get mean predicted disount as percentage of list price for relevant cart_df_soy row
                discountPct <- paste0(format(round(as.numeric((mean(discounts_tmp)/as.numeric(gsub("[\\$,]", "", cart_df_soy[i, Price])) * 100)), 2), nsmall=2),
                                      '%')
                
                # round mean of discount predicitons to 2 decimal places and place into
                #  relevant cart_df_soy row
                cart_df_soy[i, Discount := paste0('$', format(round(mean(discounts_tmp), 2), nsmall=2))]
                
                # put discount as percentage of list price into relevant cart_df_soy row
                cart_df_soy[i, DiscountPCT := discountPct ]
                
                # put updated discounts into recommended column
                cart_df_soy[i, RecDiscountPCT := discountPct]
                cart_df_soy[i, RecDiscount := cart_df_soy[i, Discount]]
                
                # store to average for order distribution
                tmp <-append(tmp, list(discounts_tmp))
              }
              
              discounts_out <- unlist(append(tmp, list(discounts_new_cust)))
              
            } else if (nrow(cart_df_soy) == 1) {  # if only one item in cart...
              
              discounts_out <- discounts_new_cust
              
            }
            
            cart_plt_soy <-  make_plot_cart_order_new_cust(discounts_out,
                                                           stdN,
                                                           cart_df_soy)
            cart_plt_soy <- ggplotly(cart_plt_soy)
            
            order_tab <- update_order_info_table(order_tab,
                                                 cart_df_soy,
                                                 cust_df_soy,
                                                 cust_id_soy,
                                                 seedsmanID_soy,
                                                 custIDColumnSoy,
                                                 orderTableNames,
                                                 seedsmanIDColumnSoy,
                                                 discountColumnSoy,
                                                 yearColumnSoy,
                                                 priceColumnSoy,
                                                 effectivePriceColumnSoy,
                                                 initial=FALSE,
                                                 new_cust=TRUE)
            
            output$order_table_soy <- renderTable({order_tab},
                                                  caption='Portfolio Information',
                                                  caption.placement = getOption("xtable.caption.placement", "top"))
            
            # #infobox 1 that goes along the top of the main panel
            # output$Total.Retail.Amount_soy <- renderInfoBox({
            #   x <- order_tab$`Total Retail Amount`
            #   infoBox(
            #     tags$p(style = "font-size: 15px;", "Total Retail Amount"), tags$p(style = "font-size: 22px;", paste(x, sep = "")   ), icon = icon("dollar-sign"),
            #     color = "green", fill = TRUE
            #   )
            # })
            
            #infobox for lcr unit that goes along the top of the main panel
            output$Rec.LCR.Unit_soy <- renderInfoBox({
              
              x <- order_tab$`Prevailing LCR Unit`
              
              infoBox(
                tags$p(style = "font-size: 16px;", "Recommended LCR Unit"), 
                tags$p(style = "font-size: 22px;", paste(x, sep = "")   ), 
                icon = icon("dollar-sign"),
                color = "green", fill = TRUE
              )
            })
            
            #infobox for lcr % that goes along the top of the main panel
            output$Rec.LCR.Pct_soy <- renderInfoBox({
              
              x <- order_tab$`Prevailing LCR %`
              
              infoBox(
                tags$p(style = "font-size: 18px;", "Recommended LCR %"), 
                tags$p(style = "font-size: 22px;", x), 
                icon = icon("percent"),
                color = "green", fill = TRUE
              )
            })
            
            
          } else {  # if customer not a new customer...
            
            # get customer id
            cust_id_soy <- get_id(input$cust_name)
            # get seedsmanID
            seedsmanID_soy <- get_id(input$seedsman_name) 
            
            
            # predict discounts associated for last row in cart and updated portfolio
            predict_discount_soy_args <- list(product,
                                              quantity,
                                              cust_id_soy,
                                              cust_df_soy,
                                              logTransformDiscountSoy,
                                              quantityColumnSoy,
                                              priceColumnSoy,
                                              productColumnSoy,
                                              custIDColumnSoy,
                                              cartDropColsSoy,
                                              featuresSoy,
                                              discountModelSoy,
                                              pllel,
                                              avgPrice,
                                              totalQuantity,
                                              yearColumnSoy,
                                              marketYear,
                                              products_df_soy,
                                              totalQColumnSoy,
                                              avgPriceColumnSoy,
                                              seedsmanIDColumnSoy,
                                              seedsmanID_soy,
                                              fipsCodeColumnSoy,
                                              countyFeaturesSoy)
            discounts <- wrapMemoise(predict_discount_raw_soy, predict_discount_soy, predict_discount_soy_args)
            
            # # update effective price for all products
            # prices <- predict_ep_soy(new_port,
            #                          cust_id_soy,
            #                          EffPrice_soy,
            #                          logTransformEPSoy,
            #                          epCustIDColumnSoy,
            #                          epYearColumnSoy,
            #                          epCartDropColsSoy,
            #                          features_EffPSoy,
            #                          epAvgPriceColumnSoy,
            #                          epTotalQColumnSoy,
            #                          epModelSoy,
            #                          marketYear,
            #                          products_df_soy,
            #                          epCountyFeaturesSoy,
            #                          epFipsCodeColumnSoy,
            #                          epSeedsmanIDColumnSoy,
            #                          seedsmanID_soy)
            # 
            # prices <- as.data.frame(prices)
            # PredPrices <- unlist(lapply(cart_df_soy$Product, function(i) prices[prices$Product == as.character(i),
            #                                                                     'PredictedProductEffectivePrice']))
            # cart_df_soy[, EffectivePrice := paste0('$', format(round(PredPrices, 2), nsmall = 2))]
            
            # round mean of discount predicitons to 2 decimal places and place into
            #  relevant cart_df_soy row
            cart_df_soy[nrow(cart_df_soy), Discount := paste0('$', format(round(mean(discounts), 2), nsmall=2))]
            
            # get mean predicted disount as percentage of list price for relevant cart_df_soy row
            discountPct <- paste0(format(round(as.numeric((mean(discounts)/as.numeric(gsub("[\\$,]", "", cart_df_soy[nrow(cart_df_soy), Price])) * 100)), 2), nsmall=2),
                                  '%')
            # put discount as percentage of list price into relevant cart_df_soy row
            cart_df_soy[nrow(cart_df_soy), DiscountPCT := discountPct ]
            
            # Take minimum of predicted/actual and make "recommended" in last row of cart_df_soy
            # if NA...
            if (cart_df_soy[nrow(cart_df_soy), LastUnitDiscount] == "NA"){
              cart_df_soy[nrow(cart_df_soy), RecDiscount := cart_df_soy[nrow(cart_df_soy), Discount]]
              cart_df_soy[nrow(cart_df_soy), RecDiscountPCT := cart_df_soy[nrow(cart_df_soy), DiscountPCT]]
            } else {
              checkDiscounts <- list(as.numeric(gsub("[\\$,]", "", cart_df_soy[nrow(cart_df_soy), Discount])), 
                                     as.numeric(gsub("[\\$,]", "", cart_df_soy[nrow(cart_df_soy), LastUnitDiscount])))
              checkDiscountsPCT <- list(as.numeric(gsub("[\\%,]", "", cart_df_soy[nrow(cart_df_soy), DiscountPCT])), 
                                        as.numeric(gsub("[\\%,]", "", cart_df_soy[nrow(cart_df_soy), LastUnitDiscountPCT])))
              cart_df_soy[nrow(cart_df_soy), RecDiscount := paste0('$', format(round(min(unlist(checkDiscounts)), 2), nsmall=2))]
              cart_df_soy[nrow(cart_df_soy), RecDiscountPCT := paste0(format(round(min(unlist(checkDiscountsPCT)), 2), nsmall=2), '%')]
            }
            
            # if more than one item in cart, correctly update discounts and order totals now that order
            # level stats have changed
            if (nrow(cart_df_soy) > 1) {
              tmp <- list()
              for (i in 1:(nrow(cart_df_soy)-1)) {
                product_tmp <- unlist(cart_df_soy[i, 'Product'])
                quantity_tmp <- unlist(cart_df_soy[i, 'Quantity'])
                discounts_tmp <- predict_discount_soy(product_tmp,
                                                      quantity_tmp,
                                                      cust_id_soy,
                                                      cust_df_soy,
                                                      logTransformDiscountSoy,
                                                      quantityColumnSoy,
                                                      priceColumnSoy,
                                                      productColumnSoy,
                                                      custIDColumnSoy,
                                                      cartDropColsSoy,
                                                      featuresSoy,
                                                      discountModelSoy,
                                                      pllel,
                                                      avgPrice,
                                                      totalQuantity,
                                                      yearColumnSoy,
                                                      marketYear,
                                                      products_df_soy,
                                                      totalQColumnSoy,
                                                      avgPriceColumnSoy,
                                                      seedsmanIDColumnSoy,
                                                      seedsmanID_soy,
                                                      fipsCodeColumnSoy,
                                                      countyFeaturesSoy)
                
                # get mean predicted disount as percentage of list price
                discountPct <- paste0(format(round(as.numeric((mean(discounts_tmp)/as.numeric(gsub("[\\$,]", "", cart_df_soy[i, Price])) * 100)), 2), nsmall=2),
                                      '%')
                # round mean of discount predicitons to 2 decimal places and place into
                # relevant cart_df_soy row
                cart_df_soy[i, Discount := paste0('$', format(round(mean(discounts_tmp), 2), nsmall=2))]
                
                # put discount as percentage of list price into relevant cart_df_soy row
                cart_df_soy[i, DiscountPCT := discountPct ]
                
                # Take minimum of predicted/actual and make "recommended" in relevant cart_df_soy row 
                # if NA...
                if (cart_df_soy[i, LastUnitDiscount] == "NA"){
                  cart_df_soy[i, RecDiscount := cart_df_soy[i, Discount]]
                  cart_df_soy[i, RecDiscountPCT := cart_df_soy[i, DiscountPCT]]
                } else {
                  checkDiscounts <- list(as.numeric(gsub("[\\$,]", "", cart_df_soy[i, Discount])), 
                                         as.numeric(gsub("[\\$,]", "", cart_df_soy[i, LastUnitDiscount])))
                  checkDiscountsPCT <- list(as.numeric(gsub("[\\%,]", "", cart_df_soy[i, DiscountPCT])), 
                                            as.numeric(gsub("[\\%,]", "", cart_df_soy[i, LastUnitDiscountPCT])))
                  cart_df_soy[i, RecDiscount := paste0('$', format(round(min(unlist(checkDiscounts)), 2), nsmall=2))]
                  cart_df_soy[i, RecDiscountPCT := paste0(format(round(min(unlist(checkDiscountsPCT)), 2), nsmall=2), '%')]
                }
                
                # combine with previous row's discounts
                tmp <-append(tmp, list(discounts_tmp))
              }
              # combine new_row discounts and all other newly updated discounts
              discounts_out <- unlist(append(tmp, list(discounts)))
            }
            # if only one item in cart make discounts new_row
            else if (nrow(cart_df_soy) == 1) {  # if one item in cart...
              
              # make discount to be plotted only the discounts for  new_row
              discounts_out <- discounts
              
            }
            
            # make plot for whole order using discounts_out
            cart_plt_soy <-  make_plot_cart_order(discounts_out,
                                                  stdN,
                                                  cart_df_soy,
                                                  cust_id_soy,
                                                  cust_df_soy,
                                                  custIDColumnSoy,
                                                  discountColumnSoy,
                                                  yearColumnSoy,
                                                  seedsmanID_soy,
                                                  seedsmanIDColumnSoy,
                                                  priceColumnSoy)
            cart_plt_soy <- ggplotly(cart_plt_soy)
            
            # update order information table using updated cart_df_soy
            order_tab <- update_order_info_table(order_tab,
                                                 cart_df_soy,
                                                 cust_df_soy,
                                                 cust_id_soy,
                                                 seedsmanID_soy,
                                                 custIDColumnSoy,
                                                 orderTableNames,
                                                 seedsmanIDColumnSoy,
                                                 discountColumnSoy,
                                                 yearColumnSoy,
                                                 priceColumnSoy,
                                                 effectivePriceColumnSoy,
                                                 initial=FALSE,
                                                 new_cust=FALSE)
            # render table in UI
            output$order_table_soy <- renderTable({order_tab},
                                                  caption='Portfolio Information',
                                                  caption.placement = getOption("xtable.caption.placement", "top"))
            
            
            # #infobox 1 that goes along the top of the main panel
            # output$Total.Retail.Amount_soy <- renderInfoBox({
            #   x <- order_tab$`Total Retail Amount`
            #   infoBox(
            #     tags$p(style = "font-size: 15px;", "Total Retail Amount"), tags$p(style = "font-size: 22px;", paste(x, sep = "")   ), icon = icon("dollar-sign"),
            #     color = "green", fill = TRUE
            #   )
            # })
            
            #infobox for lcr unit that goes along the top of the main panel
            output$Rec.LCR.Unit_soy <- renderInfoBox({
              
              x <- order_tab$`Rec. LCR Unit`
              
              infoBox(
                tags$p(style = "font-size: 16px;", "Recommended LCR Unit"), 
                tags$p(style = "font-size: 22px;", paste(x, sep = "")   ), 
                icon = icon("dollar-sign"),
                color = "green", fill = TRUE
              )
            })
            
            
            #infobox for lcr % that goes along the top of the main panel
            output$Rec.LCR.Pct_soy <- renderInfoBox({
              
              x <- order_tab$`Rec. LCR %`
              
              infoBox(
                tags$p(style = "font-size: 18px;", "Recommended LCR %"), 
                tags$p(style = "font-size: 22px;", x), 
                icon = icon("percent"),
                color = "green", fill = TRUE
              )
            })
            
          }
          
          # update UI object to reflect updated cart
          cart_ui_obj_soy <- make_cart_ui(cart_df_soy, new_ids = FALSE)
          
          # render plot in UI
          output$dist_plot_soy <- renderPlotly({cart_plt_soy})
          
          # render UI object
          output$cart_ui_soy <- cart_ui_obj_soy$ui
          
          # observe event for switching back and forth between portfolio and product view
          observeEvent(input$get_total_plot, {
            output$dist_plot_soy <- renderPlotly({cart_plt_soy})
          })
        }
      })
    })
    # Create listeners for each get discount button
    lapply(cart_ui_obj_soy$ids, function(id) {
      id2 <- paste0(id, "_gd")
      
      # do the following for each button
      observeEvent(input[[id2]], {
        
        # make copy to extract price for order level stats calculation
        cart_df_tmp <- copy(cart_df_soy)
        
        cart_df_tmp[, 'Price'] <-as.numeric(gsub("[\\$,]", "", cart_df_tmp$Price))
        
        # get order level stats to use as memoisation keys that feed into predict discount function, can't just use new_port
        #  otherwise memoisation will not work correctly, all keys must be UNIQUE
        
        # here we call the cart and not new_port because no product is being added to the cart
        avgPrice <- mean(cart_df_tmp$Price)
        totalQuantity <- sum(cart_df_tmp$Quantity)
        
        # if customer is new...
        if (input$cust_name == 'New Customer' & !is.null(input$state) & !is.null(input$county)) {
          
          # get fips code based on user inputted state and county
          fips_code <- get_fips(input$state, 
                                input$county, 
                                stateCountyToFips)
          
          # predict discounts
          predict_discount_new_cust_soy_args <- list(unlist(cart_df_soy[button_id == id, 'Product']),
                                                     unlist(cart_df_soy[button_id == id, 'Quantity']),
                                                     fips_code,
                                                     logTransformDiscountSoy,
                                                     priceColumnSoy,
                                                     featuresSoy,
                                                     countyFeaturesSoy,
                                                     cust_df_soy,
                                                     discountModelSoy,
                                                     pllel,
                                                     avgPrice,
                                                     totalQuantity,
                                                     products_df_soy,
                                                     totalQColumnSoy,
                                                     avgPriceColumnSoy,
                                                     marketYear,
                                                     fipsCodeColumnSoy)
          discounts_new_cust <- wrapMemoise(predict_discount_raw_soy_new_cust, predict_discount_new_cust_soy, predict_discount_new_cust_soy_args)
          
        } else {  # if customer exists...
          # get customer id
          cust_id_soy <- get_id(input$cust_name)
          # get seedsmanID
          seedsmanID_soy <- get_id(input$seedsman_name)
          
          # get predicted discounts for product associated with button_id
          predict_discount_soy_args <- list(unlist(cart_df_soy[button_id == id, "Product"]),
                                            unlist(cart_df_soy[button_id == id, "Quantity"]),
                                            cust_id_soy,
                                            cust_df_soy,
                                            logTransformDiscountSoy,
                                            quantityColumnSoy,
                                            priceColumnSoy,
                                            productColumnSoy,
                                            custIDColumnSoy,
                                            cartDropColsSoy,
                                            featuresSoy,
                                            discountModelSoy,
                                            pllel,
                                            avgPrice,
                                            totalQuantity,
                                            yearColumnSoy,
                                            marketYear,
                                            products_df_soy,
                                            totalQColumnSoy,
                                            avgPriceColumnSoy,
                                            seedsmanIDColumnSoy,
                                            seedsmanID_soy,
                                            fipsCodeColumnSoy,
                                            countyFeaturesSoy)
          discounts <- wrapMemoise(predict_discount_raw_soy, predict_discount_soy, predict_discount_soy_args)
        }
        
        cart_ui_obj_soy <- make_cart_ui(cart_df_soy, new_ids = FALSE)
        
        # Output cart ui or message
        if (nrow(cart_df_soy) == 0) {
          output$cart_ui_soy <- renderText("Portfolio is empty.")
        } else {
          output$cart_ui_soy <- cart_ui_obj_soy$ui
          # The following code renders the plot of the distribution of predicted ---------------------
          #  per-unit discount for each discount listener button, some customers will not have an
          #  observed last unit discount, thus need to plot accordingly
          # If new customer...
          if (input$cust_name == 'New Customer' & !is.null(input$state) & !is.null(input$county)) {
            
            # get correct list of predictions to be plotted (discounts_new_cust)
            plot_discounts <- discounts_new_cust
            
          } else {  # If not new customer...
            # generate list of predictions to be plotted (discounts)
            plot_discounts <- discounts
          }
          # If customer does have a last unit discount, make correct plot ----------------------------
          if (cart_df_soy[button_id == id, LastUnitDiscount] != as.character("NA")) {
            button_plt <- make_plot_button(plot_discounts, cart_df_soy, stdN, id, lastDisc=TRUE)
            button_plt <- ggplotly(button_plt)
            output$dist_plot_soy <- renderPlotly({button_plt})
            
          } else { # Make plot that doesn't have vertical line, essentially the same code as above ----
            button_plt <- make_plot_button(plot_discounts, cart_df_soy, stdN, id, lastDisc=FALSE)
            button_plt <- ggplotly(button_plt)
            output$dist_plot_soy <- renderPlotly({button_plt})
            
          }
        }
      })
    })
    
    # Reset input
    updateNumericInput(session, "product_quantity_soy", value = 0)
    shinyjs::show(id = "show_hide_soy")
  }, ignoreInit=TRUE)
  # END SOY ADD TO PORTFOLIO -------------------------------------------------------------------------
  
  # Portfolio pre-population #########################################################################
  # CORN ---------------------------------------------------------------------------------------------
  observeEvent(input$populate_cart, {
    shinyjs::hide(id = "show_hide_downloadlink")
    # if customer is not a new customer...
    if (input$cust_name != 'New Customer' & input$cust_name != ""){
      # get customer and seedsman ids
      cust_id <- get_id(input$cust_name)
      seedsmanID <- get_id(input$seedsman_name)
      
      # clear cart_df
      cart_df <- data.table()
      # get cart_df and discounts to plot 
      prePopulatePortfolioCorn_args <- list(cust_id,
                                            seedsmanID,
                                            cust_df,
                                            EffPrice,
                                            discountModel,
                                            features,
                                            epModel,
                                            features_effP,
                                            custIDColumn,
                                            epCustIDColumn,
                                            seedsmanIDColumn,
                                            epSeedsmanIDColumn,
                                            yearColumn,
                                            epYearColumn,
                                            productColumn,
                                            priceColumn,
                                            quantityColumn,
                                            fipsCodeColumn,
                                            discountColumn,
                                            countyFeatures,
                                            cartDropCols,
                                            effectivePriceColumn,
                                            epCartDropCols,
                                            epAvgPriceColumn,
                                            epTotalQColumn,
                                            epCountyFeatures,
                                            epFipsCodeColumn,
                                            products_df,
                                            marketYear,
                                            logTransformDiscount,
                                            logTransformEP,
                                            pllel,
                                            predict_discount)
      portfolio_output <- wrapMemoise(prePopulatePortfolioCornRaw,prePopulatePortfolioCorn,prePopulatePortfolioCorn_args)
      
      # if all products customer has bought previously have been discontinuted, output appropiate message in UI
      # NOTE: function prePopulatePortfolioCorn will return NULL if this is the case
      if (is.null(portfolio_output)){
        output$cart_ui <-  renderText("ATTENTION: All of selected customer's previously purchased products have been discontinued. ")
        # hide record info workflow
        shinyjs::hide(id = "show_hide")
      } else {
        
        # extract output from prePopulatePortfolioCorn
        # cart_df
        cart_df <- portfolio_output[[1]]
        
        # discounts to plot
        discounts_out <- portfolio_output[[2]]
        
        # generate plot for entire order
        cart_plt <-  make_plot_cart_order(discounts_out,
                                          stdN,
                                          cart_df,
                                          cust_id,
                                          cust_df,
                                          custIDColumn,
                                          discountColumn,
                                          yearColumn,
                                          seedsmanID,
                                          seedsmanIDColumn,
                                          priceColumn)
        cart_plt <- ggplotly(cart_plt)
        # render plot
        output$dist_plot <- renderPlotly({cart_plt})
        
        # create observe event so plot can be easily referenced when "show portfolio total graph" button is clicked
        observeEvent(input$get_total_plot, {
          output$dist_plot <- renderPlotly({cart_plt})
        })
        
        # update order table for total order
        
        # if the customer's cart is empty, don't show anything
        if (nrow(cart_df) == 0) {
          renderText("")
        } else { # if not, calculate order info for display in UI
          cust_id <- get_id(input$cust_name)
          order_tab <- data.table()
          order_tab <- update_order_info_table(order_tab, 
                                               cart_df, 
                                               cust_df, 
                                               cust_id,
                                               seedsmanID,
                                               custIDColumn,
                                               orderTableNames,
                                               seedsmanIDColumn,
                                               discountColumn,
                                               yearColumn,
                                               priceColumn,
                                               effectivePriceColumn,
                                               initial=TRUE,
                                               new_cust=FALSE)
          output$order_table <- renderTable({order_tab}, 
                                            caption='Portfolio Information',
                                            caption.placement = getOption("xtable.caption.placement", "top"))
          
          # #infobox 1 that goes along the top of the main panel
          # output$Total.Retail.Amount <- renderInfoBox({
          #   x <- order_tab$`Total Retail Amount`
          #   infoBox(
          #     tags$p(style = "font-size: 15px;", "Total Retail Amount"), tags$p(style = "font-size: 22px;", paste(x, sep = "")   ), icon = icon("dollar-sign"),
          #     color = "green", fill = TRUE
          #   )
          # })
          
          #infobox for lcr unit that goes along the top of the main panel
          output$Rec.LCR.Unit <- renderInfoBox({
            
            x <- order_tab$`Rec. LCR Unit`
            
            infoBox(
              tags$p(style = "font-size: 16px;", "Recommended LCR Unit"), 
              tags$p(style = "font-size: 22px;", paste(x, sep = "")   ), 
              icon = icon("dollar-sign"),
              color = "green", fill = TRUE
            )
          })
          
          
          #infobox for lcr % that goes along the top of the main panel
          output$Rec.LCR.Pct <- renderInfoBox({
            
            x <- order_tab$`Rec. LCR %`
            
            infoBox(
              tags$p(style = "font-size: 18px;", "Recommended LCR %"), 
              tags$p(style = "font-size: 22px;", x), 
              icon = icon("percent"),
              color = "green", fill = TRUE
            )
          })
        }
        
        # Create cart table with buttons 
        cart_ui_obj <- make_cart_ui(cart_df, new_ids = TRUE)
        # get buttons in case function has already been called (memoised)
        buttons <- cart_ui_obj[[2]]
        # set button id column to deal with memoisation
        cart_df$button_id <- buttons
        output$cart_ui <- cart_ui_obj$ui
        # Create listeners for each delete button
        lapply(cart_ui_obj$ids, function(id) {
          observeEvent(input[[id]], {
            
            # delete product from cart_df
            cart_df <<- cart_df[button_id != id, ]
            cart_df_check <<- copy(cart_df)
            output$download_success <- NULL
            
            # extract correct whole portfolio before subsetting by product to correctly update basket level 
            # effective price predicitons
            new_port <- cart_df[, c('Product', 'Quantity', 'Price')]
            
            # clean price in whole portfolio
            new_port[, 'Price'] <-as.numeric(gsub("[\\$,]", "", new_port$Price))
            
            # get order level stats to use as memoisation keys that feed into predict discount function, can't just use new_port
            # otherwise memoisation will not work correctly, all keys must be UNIQUE
            avgPrice <- mean(new_port$Price)
            totalQuantity <- sum(new_port$Quantity)
            
            # Output cart ui or message if nothing left in cart and clear plots 
            if (nrow(cart_df) == 0) {
              output$cart_ui <- renderText("Portfolio is empty.")
              output$dist_plot <- NULL
              output$order_table <- renderText("")
              # make info boxes null
              #output$Total.Retail.Amount <- NULL
              output$Rec.LCR.Unit <- NULL
              output$Rec.LCR.Pct <- NULL
            } else { # if cart not empty
              
              # get last product in cart
              tmp_cart <- tail(cart_df, 1)
              
              # create new row from last product to use in updating/plotting functions
              new_row <- tmp_cart[, c('Product', 'Quantity')]
              
              # track product and quantity for use later
              product <- unlist(new_row[, 'Product'])
              quantity <- unlist(new_row[, 'Quantity'])
              
              # get customer id
              cust_id <- get_id(input$cust_name)
              
              
              # predict discounts associated with last item in cart and updated portfolio
              predict_discount_args <- list(product,
                                            quantity,
                                            cust_id,
                                            cust_df, 
                                            logTransformDiscount,
                                            quantityColumn, 
                                            priceColumn,
                                            productColumn,
                                            custIDColumn,
                                            cartDropCols,
                                            features,
                                            discountModel,
                                            pllel,
                                            avgPrice,
                                            totalQuantity,
                                            yearColumn,
                                            seedsmanIDColumn,
                                            seedsmanID,
                                            countyFeatures,
                                            fipsCodeColumn)
              discounts <- wrapMemoise(predict_discount_raw, predict_discount, predict_discount_args)
              
              # # update effective price for all products in cart CURRENTLY DEPRECATED PER REQUEST OF THE BUSINESS
              # prices <- predict_ep(new_port,
              #                      cust_id, 
              #                      EffPrice, 
              #                      logTransformEP,
              #                      epCustIDColumn,
              #                      epYearColumn,
              #                      epCartDropCols,
              #                      features_effP,
              #                      epAvgPriceColumn,
              #                      epTotalQColumn,
              #                      epModel,
              #                      marketYear,
              #                      epSeedsmanIDColumn,
              #                      seedsmanID,
              #                      epCountyFeatures,
              #                      epFipsCodeColumn)
              
              # prices <- as.data.frame(prices)
              # PredPrices <- unlist(lapply(cart_df$Product, function(i) prices[prices$Product == as.character(i), 
              #                                                                 'PredictedProductEffectivePrice']))
              # cart_df[, EffectivePrice := paste0('$', format(round(PredPrices, 2), nsmall = 2))]
              
              # update last row of cart with newly predicted discounts
              cart_df[nrow(cart_df), Discount := paste0('$', format(round(mean(discounts), 2), nsmall=2))]
              
              # update discount pct
              discountPct <- paste0(format(round(as.numeric((mean(discounts)/as.numeric(gsub("[\\$,]", "", cart_df[nrow(cart_df), Price])) * 100)), 2), nsmall=2),
                                    '%')
              cart_df[nrow(cart_df), DiscountPCT := discountPct]
              
              # Take minimum of predicted/actual and make "recommended" in last row of cart_df
              # if NA...
              if (cart_df[nrow(cart_df), LastUnitDiscount] == "NA"){
                cart_df[nrow(cart_df), RecDiscount := cart_df[nrow(cart_df), Discount]]
                cart_df[nrow(cart_df), RecDiscountPCT := cart_df[nrow(cart_df), DiscountPCT]]
              } else {
                checkDiscounts <- list(as.numeric(gsub("[\\$,]", "", cart_df[nrow(cart_df), Discount])), 
                                       as.numeric(gsub("[\\$,]", "", cart_df[nrow(cart_df), LastUnitDiscount])))
                checkDiscountsPCT <- list(as.numeric(gsub("[\\%,]", "", cart_df[nrow(cart_df), DiscountPCT])), 
                                          as.numeric(gsub("[\\%,]", "", cart_df[nrow(cart_df), LastUnitDiscountPCT])))
                cart_df[nrow(cart_df), RecDiscount := paste0('$', format(round(min(unlist(checkDiscounts)), 2), nsmall=2))]
                cart_df[nrow(cart_df), RecDiscountPCT := paste0(format(round(min(unlist(checkDiscountsPCT)), 2), nsmall=2), '%')]
              }
              
              # if more than one item in cart, correctly update discounts and order totals now that order
              # level stats have changed (every item except last row)
              if (nrow(cart_df) > 1) {
                tmp <- list()
                for (i in 1:(nrow(cart_df)-1)) {
                  product_tmp <- unlist(cart_df[i, 'Product'])
                  quantity_tmp <- unlist(cart_df[i, 'Quantity'])
                  predict_discount_args <- list(product_tmp,
                                                quantity_tmp,
                                                cust_id, 
                                                cust_df, 
                                                logTransformDiscount,
                                                quantityColumn, 
                                                priceColumn,
                                                productColumn,
                                                custIDColumn,
                                                cartDropCols,
                                                features,
                                                discountModel,
                                                pllel,
                                                avgPrice,
                                                totalQuantity,
                                                yearColumn,
                                                seedsmanIDColumn,
                                                seedsmanID,
                                                countyFeatures,
                                                fipsCodeColumn)
                  discounts_tmp <- wrapMemoise(predict_discount_raw, predict_discount, predict_discount_args)
                  
                  # get mean predicted disount as percentage of list price
                  discountPct <- paste0(format(round(as.numeric((mean(discounts_tmp)/as.numeric(gsub("[\\$,]", "", cart_df[i, Price])) * 100)), 2), nsmall=2),
                                        '%')
                  # round mean of discount predicitons to 2 decimal places and place into 
                  # relevant cart_df row
                  cart_df[i, Discount := paste0('$', format(round(mean(discounts_tmp), 2), nsmall=2))]
                  
                  # put discount as percentage of list price into relevant cart_df row 
                  cart_df[i, DiscountPCT := discountPct ]
                  
                  # Take minimum of predicted/actual and make "recommended"
                  # if NA...
                  if (cart_df[i, LastUnitDiscount] == "NA"){
                    cart_df[i, RecDiscount := cart_df[i, Discount]]
                    cart_df[i, RecDiscountPCT := cart_df[i, DiscountPCT]]
                  } else {
                    checkDiscounts <- list(as.numeric(gsub("[\\$,]", "", cart_df[i, Discount])), 
                                           as.numeric(gsub("[\\$,]", "", cart_df[i, LastUnitDiscount])))
                    checkDiscountsPCT <- list(as.numeric(gsub("[\\%,]", "", cart_df[i, DiscountPCT])), 
                                              as.numeric(gsub("[\\%,]", "", cart_df[i, LastUnitDiscountPCT])))
                    cart_df[i, RecDiscount := paste0('$', format(round(min(unlist(checkDiscounts)), 2), nsmall=2))]
                    cart_df[i, RecDiscountPCT := paste0(format(round(min(unlist(checkDiscountsPCT)), 2), nsmall=2), '%')]
                  }
                  
                  # combine with previous row's discounts
                  tmp <-append(tmp, list(discounts_tmp))
                }
                # combine new_row discounts and all other newly updated discounts 
                discounts_out <- unlist(append(tmp, list(discounts)))
              }
              # if only one item in cart make discounts new_row 
              else if (nrow(cart_df) == 1) {  # if one item in cart...
                
                # make discount to be plotted only the discounts for new_row 
                discounts_out <- discounts
                
              }
              
              # make plot for whole order using discounts_out
              cart_plt <-  make_plot_cart_order(discounts_out,
                                                stdN,
                                                cart_df,
                                                cust_id,
                                                cust_df,
                                                custIDColumn,
                                                discountColumn,
                                                yearColumn,
                                                seedsmanID,
                                                seedsmanIDColumn,
                                                priceColumn)
              cart_plt <- ggplotly(cart_plt)
              
              # update order information table using updated cart_df
              order_tab <- update_order_info_table(order_tab, 
                                                   cart_df, 
                                                   cust_df, 
                                                   cust_id,
                                                   seedsmanID,
                                                   custIDColumn,
                                                   orderTableNames,
                                                   seedsmanIDColumn,
                                                   discountColumn,
                                                   yearColumn,
                                                   priceColumn,
                                                   effectivePriceColumn,
                                                   initial=FALSE, 
                                                   new_cust=FALSE)
              # render table in UI
              output$order_table <- renderTable({order_tab}, 
                                                caption='Portfolio Information',
                                                caption.placement = getOption("xtable.caption.placement", "top"))
              
              
              # #infobox 1 that goes along the top of the main panel
              # output$Total.Retail.Amount <- renderInfoBox({
              #   x <- order_tab$`Total Retail Amount`
              #   infoBox(
              #     tags$p(style = "font-size: 15px;", "Total Retail Amount"), tags$p(style = "font-size: 22px;", paste(x, sep = "")   ), icon = icon("dollar-sign"),
              #     color = "green", fill = TRUE
              #   )
              # })
              
              
              #infobox 2 that goes along the top of the main panel
              output$Rec.LCR.Unit <- renderInfoBox({
                
                x <- order_tab$`Rec. LCR Unit`
                
                infoBox(
                  tags$p(style = "font-size: 16px;", "Recommended LCR Unit"), 
                  tags$p(style = "font-size: 22px;", paste(x, sep = "")   ), 
                  icon = icon("dollar-sign"),
                  color = "green", fill = TRUE
                )
              })
              
              
              #infobox 3 that goes along the top of the main panel
              output$Rec.LCR.Pct <- renderInfoBox({
                
                x <- order_tab$`Rec. LCR %`
                
                infoBox(
                  tags$p(style = "font-size: 18px;", "Recommended LCR %"), 
                  tags$p(style = "font-size: 22px;", x), 
                  icon = icon("percent"),
                  color = "green", fill = TRUE
                )
              })
              
              # update UI object to reflect updated cart 
              cart_ui_obj <- make_cart_ui(cart_df, new_ids = FALSE) 
              
              # render plot in UI
              output$dist_plot <- renderPlotly({cart_plt})
              
              # render UI
              output$cart_ui <- cart_ui_obj$ui
              
              # observe event for switching back and forth between portfolio and product view 
              observeEvent(input$get_total_plot, {
                output$dist_plot <- renderPlotly({cart_plt})
              })
            }
          })
        })
        # Create listeners for each get discount button
        lapply(cart_ui_obj$ids, function(id) {
          id2 <- paste0(id, "_gd")
          
          # do the following for each button...
          observeEvent(input[[id2]], {
            
            # make copy to extract price to calculate order level statistics 
            cart_df_tmp <- copy(cart_df)
            
            cart_df_tmp[, 'Price'] <-as.numeric(gsub("[\\$,]", "", cart_df_tmp$Price))
            
            # get order level stats to use as memoisation keys that feed into predict discount function, can't just use new_port
            #  otherwise memoisation will not work correctly, all keys must be UNIQUE
            # here we call the cart and not new_port because no product is being added to the cart
            avgPrice <- mean(cart_df_tmp$Price)
            totalQuantity <- sum(cart_df_tmp$Quantity)
            
            # get customer id
            cust_id <- get_id(input$cust_name)
            # get seedsmanID
            seedsmanID <- get_id(input$seedsman_name)
            
            # get predicted discounts for product associated with button_id
            predict_discount_args <- list(unlist(cart_df[button_id == id, "Product"]),
                                          unlist(cart_df[button_id == id, "Quantity"]),
                                          cust_id,
                                          cust_df,
                                          logTransformDiscount,
                                          quantityColumn,
                                          priceColumn,
                                          productColumn,
                                          custIDColumn,
                                          cartDropCols,
                                          features,
                                          discountModel,
                                          pllel,
                                          avgPrice,
                                          totalQuantity,
                                          yearColumn,
                                          seedsmanIDColumn,
                                          seedsmanID,
                                          countyFeatures,
                                          fipsCodeColumn)
            discounts <- wrapMemoise(predict_discount_raw, predict_discount, predict_discount_args)
            
            
            cart_ui_obj <- make_cart_ui(cart_df, new_ids = FALSE)
            
            # Output cart ui or message
            if (nrow(cart_df) == 0) {
              output$cart_ui <- renderText("Portfolio is empty.")
            } else {
              output$cart_ui <- cart_ui_obj$ui
              # The following code renders the plot of the distribution of predicted 
              #  per-unit discount for each discount listener button, some customers will not have an
              #  observed last unit discount, thus need to plot accordingly
              plot_discounts <- discounts
              # If customer does have a last unit discount, make correct plot
              if (cart_df[button_id == id, LastUnitDiscount] != as.character("NA")) {
                button_plt <- make_plot_button(plot_discounts, cart_df, stdN, id, lastDisc=TRUE)
                button_plt <- ggplotly(button_plt)
                output$dist_plot <- renderPlotly({button_plt})
                
              } else { # Make plot that doesn't have vertical line, essentially the same code as above
                button_plt <- make_plot_button(plot_discounts, cart_df, stdN, id, lastDisc=FALSE)
                button_plt <- ggplotly(button_plt)
                output$dist_plot <- renderPlotly({button_plt})
                
              }
            }
          })
        })
        # set globally
        cart_df <<- copy(cart_df)
        shinyjs::show(id = "show_hide")
      }
    }
  }, ignoreInit=TRUE)
  # END CORN PORTFOLIO PRE-POPULATION ---------------------------------------------------------------
  # SOY ---------------------------------------------------------------------------------------------
  observeEvent(input$populate_cart_soy, {
    # if customer not a new customer...
    if (input$cust_name != 'New Customer' & input$cust_name != ""){
      # get customer and seedsman ids
      cust_id_soy <- get_id(input$cust_name)
      seedsmanID_soy <- get_id(input$seedsman_name)
      # clear cart_df
      cart_df_soy <- data.table()
      # call prePopulatePortfolioSoy
      prePopulatePortfolioSoy_args <- list(cust_id_soy,
                                           seedsmanID_soy,
                                           cust_df_soy,
                                           EffPrice_soy,
                                           discountModelSoy,
                                           featuresSoy,
                                           epModelSoy,
                                           features_EffPSoy,
                                           custIDColumnSoy,
                                           epCustIDColumnSoy,
                                           seedsmanIDColumnSoy,
                                           epSeedsmanIDColumnSoy,
                                           yearColumnSoy,
                                           epYearColumnSoy,
                                           productColumnSoy,
                                           priceColumnSoy,
                                           quantityColumnSoy,
                                           fipsCodeColumnSoy,
                                           discountColumnSoy,
                                           countyFeaturesSoy,
                                           cartDropColsSoy,
                                           effectivePriceColumnSoy,
                                           epCartDropColsSoy,
                                           epAvgPriceColumnSoy,
                                           epTotalQColumnSoy,
                                           epCountyFeaturesSoy,
                                           epFipsCodeColumnSoy,
                                           products_df_soy,
                                           marketYear,
                                           logTransformDiscountSoy,
                                           logTransformEPSoy,
                                           pllel,
                                           predict_discount_soy)
      portfolio_output_soy <- wrapMemoise(prePopulatePortfolioSoyRaw,prePopulatePortfolioSoy,prePopulatePortfolioSoy_args)
      
      # if all products customer has recently purchased, output appropriate message in ui
      # NOTE: if this is the case, function prePopulatePortfolioSoy will return NULL
      if (is.null(portfolio_output_soy)){
        output$cart_ui_soy <- renderText("ATTENTION: All of selected customer's previously purchased products have been discontinued.")
        # hide record info workflow
        shinyjs::hide(id = "show_hide_soy")
      } else {
        
        # extract output from prePopulatePortfolioSoy
        cart_df_soy <- portfolio_output_soy[[1]]
        
        discounts_out <- portfolio_output_soy [[2]]
        
        # generate plot for entire order
        cart_plt_soy <-  make_plot_cart_order(discounts_out,
                                              stdN,
                                              cart_df_soy,
                                              cust_id_soy,
                                              cust_df_soy,
                                              custIDColumnSoy,
                                              discountColumnSoy,
                                              yearColumnSoy,
                                              seedsmanID_soy,
                                              seedsmanIDColumnSoy,
                                              priceColumnSoy)
        cart_plt_soy <- ggplotly(cart_plt_soy)
        
        # render plot
        output$dist_plot_soy <- renderPlotly({cart_plt_soy})
        
        # create observe event so plot can be easily referenced when "show portfolio total graph" button is clicked
        observeEvent(input$get_total_plot_soy, {
          output$dist_plot_soy <- renderPlotly({cart_plt_soy})
        })
        
        # update order table for total order
        # if the customer's cart is empty, don't show anything
        if (nrow(cart_df_soy) == 0) {
          renderText("")
        } else { # if not, calculate order info for display in UI
          # correct function for new customer
          cust_id_soy <- get_id(input$cust_name)
          order_tab <- data.table()
          order_tab <- update_order_info_table(order_tab,
                                               cart_df_soy,
                                               cust_df_soy,
                                               cust_id_soy,
                                               seedsmanID_soy,
                                               custIDColumnSoy,
                                               orderTableNames,
                                               seedsmanIDColumnSoy,
                                               discountColumnSoy,
                                               yearColumnSoy,
                                               priceColumnSoy,
                                               effectivePriceColumnSoy,
                                               initial=TRUE,
                                               new_cust=FALSE)
          output$order_table_soy <- renderTable({order_tab},
                                                caption='Portfolio Information',
                                                caption.placement = getOption("xtable.caption.placement", "top"))
          
          # #infobox 1 that goes along the top of the main panel
          # output$Total.Retail.Amount_soy <- renderInfoBox({
          #   x <- order_tab$`Total Retail Amount`
          #   infoBox(
          #     tags$p(style = "font-size: 15px;", "Total Retail Amount"), tags$p(style = "font-size: 22px;", paste(x, sep = "")   ), icon = icon("dollar-sign"),
          #     color = "green", fill = TRUE
          #   )
          # })
          
          #infobox for lcr unit that goes along the top of the main panel
          output$Rec.LCR.Unit_soy <- renderInfoBox({
            
            x <- order_tab$`Rec. LCR Unit`
            
            infoBox(
              tags$p(style = "font-size: 16px;", "Recommended LCR Unit"), 
              tags$p(style = "font-size: 22px;", paste(x, sep = "")   ), 
              icon = icon("dollar-sign"),
              color = "green", fill = TRUE
            )
          })
          
          #infobox for lcr percent that goes along the top of the main panel
          output$Rec.LCR.Pct_soy <- renderInfoBox({
            
            x <- order_tab$`Rec. LCR %`
            
            infoBox(
              tags$p(style = "font-size: 18px;", "Recommended LCR %"), 
              tags$p(style = "font-size: 22px;", x), 
              icon = icon("percent"),
              color = "green", fill = TRUE
            )
          })
        }
        
        # Create cart table with buttons
        cart_ui_obj_soy <- make_cart_ui(cart_df_soy, new_ids = TRUE)
        # save button_id's in case memoised version of function is called, otherwise buttons won't work
        buttons_soy <- cart_ui_obj_soy[[2]]
        # add button ids in case function is memoised 
        cart_df_soy$button_id <- buttons_soy
        output$cart_ui_soy <- cart_ui_obj_soy$ui
        # Create listeners for each delete button
        lapply(cart_ui_obj_soy$ids, function(id) {
          observeEvent(input[[id]], {
            # delete product from cart_df_soy
            cart_df_soy <<- cart_df_soy[button_id != id, ]
            cart_df_check_soy <<- copy(cart_df_soy)
            
            output$download_success_soy <- NULL
            
            # extract correct whole portfolio before subsetting by product to correctly update basket level
            # effective price predicitons
            new_port <- cart_df_soy[, c('Product', 
                                        'Quantity', 
                                        'Price')]
            
            # clean price in whole portfolio
            new_port[, 'Price'] <-as.numeric(gsub("[\\$,]", "", new_port$Price))
            
            # get order level stats to use as memoisation keys that feed into predict discount function, can't just use new_port
            # otherwise memoisation will not work correctly, all keys must be UNIQUE
            avgPrice <- mean(new_port$Price)
            totalQuantity <- sum(new_port$Quantity)
            
            # Output cart ui or message if nothing left in cart and clear plots
            if (nrow(cart_df_soy) == 0) {
              output$cart_ui_soy <- renderText({'Portfolio is empty. '})
              output$dist_plot_soy <- NULL
              output$order_table_soy <- NULL
              # make info boxes null
              #output$Total.Retail.Amount_soy <- NULL 
              output$Rec.LCR.Unit_soy <- NULL
              output$Rec.LCR.Pct_soy <- NULL
            } else { # if cart not empty
              
              # get last product in cart
              tmp_cart <- tail(cart_df_soy, 1)
              
              # create new row from last product to use in updating/plotting functions
              new_row <- tmp_cart[, c('Product', 'Quantity')]
              
              # track product and quantity for use later
              product <- unlist(new_row[, 'Product'])
              quantity <- unlist(new_row[, 'Quantity'])
              
              # get customer id
              cust_id_soy <- get_id(input$cust_name)
              # get seedsmanID
              seedsmanID_soy <- get_id(input$seedsman_name) 
              
              
              # predict discounts associated with last product in cart and updated portfolio
              predict_discount_soy_args <- list(product,
                                                quantity,
                                                cust_id_soy,
                                                cust_df_soy,
                                                logTransformDiscountSoy,
                                                quantityColumnSoy,
                                                priceColumnSoy,
                                                productColumnSoy,
                                                custIDColumnSoy,
                                                cartDropColsSoy,
                                                featuresSoy,
                                                discountModelSoy,
                                                pllel,
                                                avgPrice,
                                                totalQuantity,
                                                yearColumnSoy,
                                                marketYear,
                                                products_df_soy,
                                                totalQColumnSoy,
                                                avgPriceColumnSoy,
                                                seedsmanIDColumnSoy,
                                                seedsmanID_soy,
                                                fipsCodeColumnSoy,
                                                countyFeaturesSoy)
              discounts <- wrapMemoise(predict_discount_raw_soy, predict_discount_soy, predict_discount_soy_args)
              
              # # update effective price for all items in cart # CURRENTLY DEPRECATED PER REQUEST OF THE BUSINESS
              # prices <- predict_ep_soy(new_port,
              #                          cust_id_soy,
              #                          EffPrice_soy,
              #                          logTransformEPSoy,
              #                          epCustIDColumnSoy,
              #                          epYearColumnSoy,
              #                          epCartDropColsSoy,
              #                          features_EffPSoy,
              #                          epAvgPriceColumnSoy,
              #                          epTotalQColumnSoy,
              #                          epModelSoy,
              #                          marketYear,
              #                          products_df_soy,
              #                          epCountyFeaturesSoy,
              #                          epFipsCodeColumnSoy,
              #                          epSeedsmanIDColumnSoy,
              #                          seedsmanID_soy)
              # 
              # prices <- as.data.frame(prices)
              # PredPrices <- unlist(lapply(cart_df_soy$Product, function(i) prices[prices$Product == as.character(i),
              #                                                                     'PredictedProductEffectivePrice']))
              # cart_df_soy[, EffectivePrice := paste0('$', format(round(PredPrices, 2), nsmall = 2))]
              
              # round mean of discount predicitons to 2 decimal places and place into
              #  relevant cart_df_soy row (last row in cart)
              cart_df_soy[nrow(cart_df_soy), Discount := paste0('$', format(round(mean(discounts), 2), nsmall=2))]
              
              # get mean predicted disount as percentage of list price for relevant cart_df_soy row
              discountPct <- paste0(format(round(as.numeric((mean(discounts)/as.numeric(gsub("[\\$,]", "", cart_df_soy[nrow(cart_df_soy), Price])) * 100)), 2), nsmall=2),
                                    '%')
              # put discount as percentage of list price into relevant cart_df_soy row
              cart_df_soy[nrow(cart_df_soy), DiscountPCT := discountPct ]
              
              # Take minimum of predicted/actual and make "recommended" in last row of cart_df_soy
              # if NA...
              if (cart_df_soy[nrow(cart_df_soy), LastUnitDiscount] == "NA"){
                cart_df_soy[nrow(cart_df_soy), RecDiscount := cart_df_soy[nrow(cart_df_soy), Discount]]
                cart_df_soy[nrow(cart_df_soy), RecDiscountPCT := cart_df_soy[nrow(cart_df_soy), DiscountPCT]]
              } else {
                checkDiscounts <- list(as.numeric(gsub("[\\$,]", "", cart_df_soy[nrow(cart_df_soy), Discount])), 
                                       as.numeric(gsub("[\\$,]", "", cart_df_soy[nrow(cart_df_soy), LastUnitDiscount])))
                checkDiscountsPCT <- list(as.numeric(gsub("[\\%,]", "", cart_df_soy[nrow(cart_df_soy), DiscountPCT])), 
                                          as.numeric(gsub("[\\%,]", "", cart_df_soy[nrow(cart_df_soy), LastUnitDiscountPCT])))
                cart_df_soy[nrow(cart_df_soy), RecDiscount := paste0('$', format(round(min(unlist(checkDiscounts)), 2), nsmall=2))]
                cart_df_soy[nrow(cart_df_soy), RecDiscountPCT := paste0(format(round(min(unlist(checkDiscountsPCT)), 2), nsmall=2), '%')]
              }
              
              # if more than one item in cart, correctly update discounts and order totals now that order
              # level stats have changed
              if (nrow(cart_df_soy) > 1) {
                tmp <- list()
                for (i in 1:(nrow(cart_df_soy)-1)) {
                  product_tmp <- unlist(cart_df_soy[i, 'Product'])
                  quantity_tmp <- unlist(cart_df_soy[i, 'Quantity'])
                  predict_discount_soy_args <- list(product_tmp,
                                                    quantity_tmp,
                                                    cust_id_soy,
                                                    cust_df_soy,
                                                    logTransformDiscountSoy,
                                                    quantityColumnSoy,
                                                    priceColumnSoy,
                                                    productColumnSoy,
                                                    custIDColumnSoy,
                                                    cartDropColsSoy,
                                                    featuresSoy,
                                                    discountModelSoy,
                                                    pllel,
                                                    avgPrice,
                                                    totalQuantity,
                                                    yearColumnSoy,
                                                    marketYear,
                                                    products_df_soy,
                                                    totalQColumnSoy,
                                                    avgPriceColumnSoy,
                                                    seedsmanIDColumnSoy,
                                                    seedsmanID_soy,
                                                    fipsCodeColumnSoy,
                                                    countyFeaturesSoy)
                  discounts_tmp <- wrapMemoise(predict_discount_raw_soy, predict_discount_soy, predict_discount_soy_args)
                  
                  
                  # get mean predicted disount as percentage of list price
                  discountPct <- paste0(format(round(as.numeric((mean(discounts_tmp)/as.numeric(gsub("[\\$,]", "", cart_df_soy[i, Price])) * 100)), 2), nsmall=2),
                                        '%')
                  # round mean of discount predicitons to 2 decimal places and place into
                  # relevant cart_df_soy row
                  cart_df_soy[i, Discount := paste0('$', format(round(mean(discounts_tmp), 2), nsmall=2))]
                  
                  # put discount as percentage of list price into relevant cart_df_soy row
                  cart_df_soy[i, DiscountPCT := discountPct ]
                  
                  # Take minimum of predicted/actual and make "recommended" in relevant cart_df_soy row 
                  # if NA...
                  if (cart_df_soy[i, LastUnitDiscount] == "NA"){
                    cart_df_soy[i, RecDiscount := cart_df_soy[i, Discount]]
                    cart_df_soy[i, RecDiscountPCT := cart_df_soy[i, DiscountPCT]]
                  } else {
                    checkDiscounts <- list(as.numeric(gsub("[\\$,]", "", cart_df_soy[i, Discount])), 
                                           as.numeric(gsub("[\\$,]", "", cart_df_soy[i, LastUnitDiscount])))
                    checkDiscountsPCT <- list(as.numeric(gsub("[\\%,]", "", cart_df_soy[i, DiscountPCT])),
                                              as.numeric(gsub("[\\%,]", "", cart_df_soy[i, LastUnitDiscountPCT])))
                    cart_df_soy[i, RecDiscount := paste0('$', format(round(min(unlist(checkDiscounts)), 2), nsmall=2))]
                    cart_df_soy[i, RecDiscountPCT := paste0(format(round(min(unlist(checkDiscountsPCT)), 2), nsmall=2), '%')]
                  }
                  
                  # combine with previous row's discounts
                  tmp <-append(tmp, list(discounts_tmp))
                }
                # combine new_row discounts and all other newly updated discounts
                discounts_out <- unlist(append(tmp, list(discounts)))
              }
              # if only one item in cart make discounts new_row
              else if (nrow(cart_df_soy) == 1) {  # if one item in cart...
                
                # make discount to be plotted only the discounts for  new_row
                discounts_out <- discounts
                
              }
              
              # make plot for whole order using discounts_out
              cart_plt_soy <-  make_plot_cart_order(discounts_out,
                                                    stdN,
                                                    cart_df_soy,
                                                    cust_id_soy,
                                                    cust_df_soy,
                                                    custIDColumnSoy,
                                                    discountColumnSoy,
                                                    yearColumnSoy,
                                                    seedsmanID_soy,
                                                    seedsmanIDColumnSoy,
                                                    priceColumnSoy)
              cart_plt_soy <- ggplotly(cart_plt_soy)
              
              # update order information table using updated cart_df_soy
              order_tab <- update_order_info_table(order_tab,
                                                   cart_df_soy,
                                                   cust_df_soy,
                                                   cust_id_soy,
                                                   seedsmanID_soy,
                                                   custIDColumnSoy,
                                                   orderTableNames,
                                                   seedsmanIDColumnSoy,
                                                   discountColumnSoy,
                                                   yearColumnSoy,
                                                   priceColumnSoy,
                                                   effectivePriceColumnSoy,
                                                   initial=FALSE,
                                                   new_cust=FALSE)
              # render table in UI
              output$order_table_soy <- renderTable({order_tab},
                                                    caption='Portfolio Information',
                                                    caption.placement = getOption("xtable.caption.placement", "top"))
              
              
              # #infobox 1 that goes along the top of the main panel
              # output$Total.Retail.Amount_soy <- renderInfoBox({
              #   x <- order_tab$`Total Retail Amount`
              #   infoBox(
              #     tags$p(style = "font-size: 15px;", "Total Retail Amount"), tags$p(style = "font-size: 22px;", paste(x, sep = "")   ), icon = icon("dollar-sign"),
              #     color = "green", fill = TRUE
              #   )
              # })
              
              
              #infobox for lcr unit that goes along the top of the main panel
              output$Rec.LCR.Unit_soy <- renderInfoBox({
                
                x <- order_tab$`Rec. LCR Unit`
                
                infoBox(
                  tags$p(style = "font-size: 16px;", "Recommended LCR Unit"), 
                  tags$p(style = "font-size: 22px;", paste(x, sep = "")   ), 
                  icon = icon("dollar-sign"),
                  color = "green", fill = TRUE
                )
              })
              
              
              #infobox for lcr percvent that goes along the top of the main panel
              output$Rec.LCR.Pct_soy <- renderInfoBox({
                
                x <- order_tab$`Rec. LCR %`
                
                infoBox(
                  tags$p(style = "font-size: 18px;", "Recommended LCR %"), 
                  tags$p(style = "font-size: 22px;", x), 
                  icon = icon("percent"),
                  color = "green", fill = TRUE
                )
              })
              
              # update UI object to reflect updated cart
              cart_ui_obj_soy <- make_cart_ui(cart_df_soy, new_ids = FALSE)
              
              # render plot in UI
              output$dist_plot_soy <- renderPlotly({cart_plt_soy})
              
              # render UI object
              output$cart_ui_soy <- cart_ui_obj_soy$ui
              
              # observe event for switching back and forth between portfolio and product view
              observeEvent(input$get_total_plot, {
                output$dist_plot_soy <- renderPlotly({cart_plt_soy})
              })
            }
          })
        })
        
        # Create listeners for each get discount button
        lapply(cart_ui_obj_soy$ids, function(id) {
          id2 <- paste0(id, "_gd")
          
          # do the following for each button
          observeEvent(input[[id2]], {
            
            # make copy of cart_df_soy to extract price for order level stats calculation
            cart_df_tmp <- copy(cart_df_soy)
            
            cart_df_tmp[, 'Price'] <-as.numeric(gsub("[\\$,]", "", cart_df_tmp$Price))
            
            # get order level stats to use as memoisation keys that feed into predict discount function, can't just use new_port
            #  otherwise memoisation will not work correctly, all keys must be UNIQUE
            # here we call the cart and not new_port because no product is being added to the cart
            avgPrice <- mean(cart_df_tmp$Price)
            totalQuantity <- sum(cart_df_tmp$Quantity)
            
            # get customer id
            cust_id_soy <- get_id(input$cust_name)
            # get seedsmanID
            seedsmanID_soy <- get_id(input$seedsman_name)
            
            # get predicted discounts for product associated with button_id
            predict_discount_soy_args <- list(unlist(cart_df_soy[button_id == id, "Product"]),
                                              unlist(cart_df_soy[button_id == id, "Quantity"]),
                                              cust_id_soy,
                                              cust_df_soy,
                                              logTransformDiscountSoy,
                                              quantityColumnSoy,
                                              priceColumnSoy,
                                              productColumnSoy,
                                              custIDColumnSoy,
                                              cartDropColsSoy,
                                              featuresSoy,
                                              discountModelSoy,
                                              pllel,
                                              avgPrice,
                                              totalQuantity,
                                              yearColumnSoy,
                                              marketYear,
                                              products_df_soy,
                                              totalQColumnSoy,
                                              avgPriceColumnSoy,
                                              seedsmanIDColumnSoy,
                                              seedsmanID_soy,
                                              fipsCodeColumnSoy,
                                              countyFeaturesSoy)
            discounts <- wrapMemoise(predict_discount_raw_soy, predict_discount_soy, predict_discount_soy_args)
            
            
            cart_ui_obj_soy <- make_cart_ui(cart_df_soy, new_ids = FALSE)
            
            # Output cart ui or message
            if (nrow(cart_df_soy) == 0) {
              output$cart_ui_soy <- renderText("Portfolio is empty.")
            } else {
              output$cart_ui_soy <- cart_ui_obj_soy$ui
              # The following code renders the plot of the distribution of predicted ---------------------
              #  per-unit discount for each discount listener button, some customers will not have an
              #  observed last unit discount, thus need to plot accordingly
              # generate list of predictions to be plotted (discounts)
              plot_discounts <- discounts
              # If customer does have a last unit discount, make correct plot ----------------------------
              if (cart_df_soy[button_id == id, LastUnitDiscount] != as.character("NA")) {
                button_plt <- make_plot_button(plot_discounts, cart_df_soy, stdN, id, lastDisc=TRUE)
                button_plt <- ggplotly(button_plt)
                output$dist_plot_soy <- renderPlotly({button_plt})
                
              } else { # Make plot that doesn't have vertical line, essentially the same code as above ----
                button_plt <- make_plot_button(plot_discounts, cart_df_soy, stdN, id, lastDisc=FALSE)
                button_plt <- ggplotly(button_plt)
                output$dist_plot_soy <- renderPlotly({button_plt})
                
              }
            }
          })
        })
        # set cart_df globally
        cart_df_soy <<- copy(cart_df_soy)
        shinyjs::show(id = "show_hide_soy")
      }
    }
  }, ignoreInit=TRUE)
  # END SOY PORTFOLIO PRE-POPULATION -----------------------------------------------------------------
  
  # All the things that should happen when "Clear Portfolio" is clicked ##############################
  # CORN  --------------------------------------------------------------------------------------------
  clear_cart <- observeEvent(input$clear_cart, {
    cart_df <<- data.table()
    output$cart_ui <- renderText("Portfolio is empty.")
    output$dist_plot <- NULL
    output$download_success <- NULL
    output$order_table <- NULL
    # make info boxes null
    #output$Total.Retail.Amount <- NULL
    output$Rec.LCR.Unit <- NULL
    output$Rec.LCR.Pct <- NULL
    # if prepopulate portfolio button has been pressed, no items have been added to cart
    # and the delete button has been pressed--if the item that exists we use in add to cart 
    # in this scenario, make that object null when clear cart is pressed
    if (exists('cart_df_check')){
      cart_df_check <<- NULL
    }
    shinyjs::hide(id = "show_hide")
    updateNumericInput(session, "lcr_offer", value = 0)
    updateNumericInput(session, "comp_offer", value = 0)
    shinyjs::hide(id = "show_hide_downloadlink")
  }, ignoreInit=TRUE)
  # SOY ----------------------------------------------------------------------------------------------
  clear_cart_soy <- observeEvent(input$clear_cart_soy, {
    cart_df_soy <<- data.table()
    output$cart_ui_soy <- renderText("Portfolio is empty.")
    output$dist_plot_soy <- NULL
    output$download_success_soy <- NULL
    output$order_table_soy <- NULL
    # make info boxes null
    #output$Total.Retail.Amount_soy <- NULL
    output$Rec.LCR.Unit_soy <- NULL
    output$Rec.LCR.Pct_soy <- NULL
    # if prepopulate portfolio button has been pressed, no items have been added to cart
    # and the delete button has been pressed--if the item that exists we use in add to cart 
    # in this scenario, make that object null when clear cart is pressed
    if (exists('cart_df_check_soy')){
      cart_df_check_soy <<- NULL
    }
    shinyjs::hide(id = "show_hide_soy")
    updateNumericInput(session, "lcr_offer_soy", value = 0)
    updateNumericInput(session, "comp_offer_soy", value = 0)
  }, ignoreInit=TRUE)
  
  # All things that happen when "Record Discounts" is clicked ########################################
  # CORN ---------------------------------------------------------------------------------------------
  observeEvent(input$export_cart, {
    
    # deals with if prepopulate portfolio button has been pressed and delete buttons have been pressed,
    #  but no other products have been added to cart or portfolio hasn't been cleared
    if (exists('cart_df_check')){
      if (!is.null(cart_df_check)){
        cart_df <<- cart_df_check
      }
      
    }
    
    # if nothing to record don't do anything
    if (nrow(cart_df) == 0) {
      renderText("")
    } else {  # add user entered discount to appropiate row in cart_df
      for (i in 1:nrow(cart_df)){
        # get correct id to store recorded discretionary discount
        id <- cart_df[i, 'button_id']
        id3 <- paste0(id, "_od")
        cart_df[i, 'Offered Unit LCR'] <- paste0('$', input[[id3]])
      }
      
      # get customer id
      cust_id <- get_id(input$cust_name)
      
      # if customer id doesnt exist, label file with "NewCustomer"
      if (is.na(cust_id)) {
        cust_id <- 'NewCustomer'
      }
      
      # get whether deal was won or lost
      deal_lost <- input$deal_lost
      
      # get competing offer
      comp_offer <- input$comp_offer
      
      # get lcr offer
      lcr_offer <- input$lcr_offer
      
      # get dollar/percent designation tied to portfolio-level lcr offer (jcm)
      dollar_percent <- input$dollar_percent
      
      # get dollar/percent designation tied to lcr unit offer (jcm)
      dollar_percent_table <- input$dollar_percent_table
      
      # export cart (jcm)
      record_discount_df <- export_cart(cart_df, get_id(input$seedsman_name), deal_lost,
                                        comp_offer,lcr_offer, dollar_percent,dollar_percent_table)
      
      # get date to append to exported file
      date <- Sys.Date()
      
      # write to csv file in specified directory with structured file
      write.csv(record_discount_df, paste0(saveOfferFilePath, 'recordedDiscountsCorn_', date,
                                           '_', 'SeedsmanID',
                                           get_id(input$seedsman_name),
                                           '_', 'CustomerID', cust_id, '.csv'))
      
      
      # display success message
      output$download_success <- renderText('Discounts Successully Recorded!')
      
      output$download_recorded <- downloadHandler(
        filename = function() {
          paste('recorded_corn_data_', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
          write.csv(record_discount_df, con)
        }
      )
      
      shinyjs::show(id = "show_hide_downloadlink")
    }
    
  }, ignoreInit=TRUE)
  # SOY ----------------------------------------------------------------------------------------------
  observeEvent(input$export_cart_soy, {
    
    # deals with if prepopulate portfolio button has been pressed and delete buttons have been pressed,
    #  but no other products have been added to cart or portfolio hasn't been cleared
    if (exists('cart_df_check_soy')){
      if (!is.null(cart_df_check_soy)){
        cart_df_soy <<- cart_df_check_soy
      }
      
    }
    
    # if nothing to record don't do anything
    if (nrow(cart_df_soy) == 0) {
      renderText("")
    } else {  # add user entered discount to appropiate row in cart_df
      for (i in 1:nrow(cart_df_soy)){
        # get correct id to store recorded discretionary discount
        id <- cart_df_soy[i, 'button_id']
        id3 <- paste0(id, "_od")
        cart_df_soy[i, 'Offered Unit LCR'] <- paste0('$', input[[id3]])
      }
      
      # get customer id
      cust_id_soy <- get_id(input$cust_name)
      
      # if customer id doesnt exist, label file with "NewCustomer"
      if (is.na(cust_id_soy)) {
        cust_id_soy <- 'NewCustomer'
      }
      
      # get whether deal was won or lost
      deal_lost <- input$deal_lost_soy
      
      # get competing offer
      comp_offer <- input$comp_offer_soy
      
      # get lcr offer
      lcr_offer <- input$lcr_offer_soy
      
      # get dollar/percent designation tied to portfolio-level lcr offer
      dollar_percent <- input$dollar_percent_soy
      
      # get dollar/percent designation tied to lcr unit offer
      dollar_percent_table <- input$dollar_percent_table_soy
      
      # export cart (jcm)
      record_discount_df <- export_cart(cart_df_soy, get_id(input$seedsman_name), deal_lost,
                                        comp_offer, lcr_offer, dollar_percent,dollar_percent_table)
      
      # get date to append to exported file
      date <- Sys.Date()
      
      # write to csv file in specified directory with structured file
      write.csv(record_discount_df, paste0(saveOfferFilePath, 'recordedDiscountsSoy_', date,
                                           '_', 'SeedsmanID',
                                           get_id(input$seedsman_name),
                                           '_', 'CustomerID', cust_id_soy, '.csv'))
      # display success message
      output$download_success_soy <- renderText('Discounts Successully Recorded!')
    }
    
  }, ignoreInit=TRUE)
  
  
  # END EXPORT CART ##################################################################################
  
  # All the things that should happen when "Download" is clicked ##############################
  #The commented-out code here and above (ctrl + f: checkspec) 
  #allows the user to download the contents of user-specifed folders. 
  #The user specifications are made via checkboxes on the Downloads tab.
  #Those checkboxes however are currently 'commented out'.
  output$download_data <- downloadHandler(
    filename = function() {
      paste("MARSUI_data_", Sys.Date(),".zip", sep="")
    },
    content = function(fname) {
      data_dir <- saveOfferFilePath
      #dir_boxes <- input$dir_boxes
      #num_boxes <- sum(lengths(dir_boxes))
      #if(num_boxes>0) {
      paths <- c()
      #for (j in 1:num_boxes) {
      #fs <- paste0(dir_boxes[j],"//",list.files(dir_boxes[j],recursive = TRUE))
      fs <- paste0(data_dir,list.files(data_dir))
      paths <- c(paths, fs)
      #num_files <- lengths(list.files(data_dir))
      # for (i in 1:num_files) {
      #   path <- fs[i]
      #   #write(i*2, path)
      # }
      #}
      zip(zipfile=fname, files=paths)
      #}
    },
    contentType = "application/zip"
  )
  #session is here in case anything needs to happen upon app closing
  session$onSessionEnded(function() {
    Time2 <- .POSIXct(Sys.time(), TimeZone)
    TimeOnApp <- dhms(as.numeric(difftime(Time2, Time1, units=c("secs"))))
    userDataRow <- data.frame(t(c(CWID, 
                                  paste(as.character(Time1), TimeZone, sep = ' '),
                                  paste(as.character(Time2), TimeZone, sep = ' '), 
                                  TimeOnApp,
                                  isolate(session$clientData$url_pathname), 
                                  isolate(session$clientData$url_search)
    )))
    file.lock.UserLogFinal = flock::lock("./recorded_user_data/UserLogFinal.csv")
    write.table(userDataRow, file = "./recorded_user_data/UserLogFinal.csv", sep = ",", append = TRUE, row.names = FALSE, col.names = FALSE, eol = "\n") #lock file
    
    #if(is.locked(file.lock.UserLogFinal)) {
    #  print("Got the lock!")
    #}
    tryCatch(flock::unlock(file.lock.UserLogFinal), finally = NULL)
    #cat("Session stopped\n")  #lock 7026, use finally block
    
  })
  
}

# Run the application ================================================================================
shinyApp(ui = ui, server = server)
