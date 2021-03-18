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

# Load active seedsmen list
active_seedsmen <- setDF(read_csv(ActiveSeedsmenName))

# Load data. (The script above can be used to recreate csv files below when new runrate becomes available.)
runrate_df_corn <- setDF(read.csv('./Data/runrate_df_corn.csv',header = TRUE))
runrate_df_soy <- setDF(read.csv('./Data/runrate_df_soy.csv',header = TRUE))

#Code below has been deprecated
#Code was used to load and parse cust_df and EffPrice dataframes for corn + soy
#cust_df and EffPrice dataframes are now loaded in 'pre-parsed'
#if(TRUE) {

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



# Load customer and effective price dataframes (corn + soy)
# CORN -----------------------------------------------------------------------------------------------
cust_df_displaynames <- cust_df[ which(cust_df$SeedsmanSAPID %in% active_seedsmen$SAP_ID),] #Keeping only seedsmen in active seedsmen list for display
# SOY ------------------------------------------------------------------------------------------------
cust_df_soy_displaynames <- cust_df_soy[ which(cust_df_soy$DLR_SAP_ID %in% active_seedsmen$SAP_ID),] #Keeping only seedsmen in active seedsmen list for display



css <- ".shiny-input-container > label {margin-bottom: -10px;}"
feedback_tab_generated <- FALSE
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