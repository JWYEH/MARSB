#' This is the configurations file for the RScript app.R
#' These parameters are called in the back end of the MARS Shiny App
# ====================================================================================================
#' Parameters ----------------------------------------------------------------------------------------
# ====================================================================================================
# CORN Log transform #################################################################################
#'@param logTransformDiscount bool; should predictions for discount be transformed from log form 
#'before display in UI
logTransformDiscount <- TRUE
#'@param logTransformEP string; should predictions for effective price be transformed from log form 
#'before display in UI
logTransformEP <- FALSE
#-----------------------------------------------------------------------------------------------------
#'@param pllel bool; should app run in parallel (CORN + SOY)
pllel <- FALSE
# ----------------------------------------------------------------------------------------------------
#' @param trainDataPath string; directory where file histsorical customer records are located (CORN + SOY)
trainDataPath <- "./Data/"
# ----------------------------------------------------------------------------------------------------
# Data file naming  ##############################################################################
#' @param ActiveSeedsmenName string: file name of the active seedsmen list
ActiveSeedsmenName <- './Data/activeSeedsman_072020.csv'
# ----------------------------------------------------------------------------------------------------
# CORN Data file naming  ##############################################################################
#' @param trainDataName string; file name of historical customer records
trainDataName <- 'CleanedProductLevelData_fromLarrySwift_20201021.rds'
#' @param EffPriceName string; file name of data used to train effective price models at the basket level
#' (an aggregation to basket level of file trainDataPath + trainDataName)
EffPriceName <- 'CleanedData_fromLarrySwift_20200917.rds'
#' @param runRateName string; file name of data used to compute budget run rate
#' (an aggregation to basket level of file trainDataPath + runRateName)
runRateName <- 'budgetRunRate_corn.csv'
# ----------------------------------------------------------------------------------------------------
# CORN Historical customer discount record column name parameters (product-level data) ###############
#' @param quantityColumn string; column that measures product quantity
quantityColumn <- "PRODUCT_QTY"
#' @param priceColumn string; column that measures product list price
priceColumn <- "AVGRETAILPRICE"
#' @param productColumn string; column that mesaures product name
productColumn <- "VARIETY"
#' @param discountColumn string; column that measures per unit disount
discountColumn <- "PerUnitDiscount"
#' @param fipsCodeColumn string; column that measures customer fips code 
fipsCodeColumn <- "fips_code"
#' @param custIDColumn string; column that measures numeric customer id
custIDColumn <- "GrowerSAPID"
#' @param yearColumn string; column that measures purchase year
yearColumn <- "Year"
#' @param custNameColumn string; column that mesures customer name
custNameColumn <- "GRWR_ACCT_NAME"
#' @param seedsmanIDColumn string; column that measures numeric seedsman id
seedsmanIDColumn <- "SeedsmanSAPID"
#' @param seedsmanNameColumn stringl column that measures seedsman account name
seedsmanNameColumn <- 'DLR_NAME'
#' @param terrIDColumn string; column that measures terrID
terrIDColumn <- 'TERR_ID'
#' @param totalQColumn string; column that measures total order quantity
totalQColumn <- 'ORDERED_QTY'
#'@param avgPriceColumn string; column that measures order average unit retail price
avgPriceColumn <- 'AVGPRICE_RETAIL'
#'@param fsrColumn string; column that gives FSR name (last, first)
fsrColumn <- 'FSR'
# ----------------------------------------------------------------------------------------------------
# CORN Historical effective price (basket level) record column name parameters #######################
#' @param effectivePriceColumn string; column that measures effective price
effectivePriceColumn <- 'AVG_FARMGATE_PRICE' 
#' @param epCustIDColumn string; column that measures numeric customer id
epCustIDColumn <- 'GrowerSAPID'
#' @param epYearColumn string; column that measures purchase year
epYearColumn <- 'Year'
#' @param epAvgPriceColumn string; column that measures avg unit order price
epAvgPriceColumn <- 'AVGPRICE_RETAIL'
#' @param epTotalQColumn string; column that measures total order quantity
epTotalQColumn <- 'ORDERED_QTY'
#' @param epFipsCodeColumn string; column that measures fips code
epFipsCodeColumn <- 'fips_code'
#' @param epSeedsmanIDColumn string; column that measures numeric seedsman id
epSeedsmanIDColumn <- 'SeedsmanSAPID'
#'@param epSeedsmanNameColumn string; column that mesures seedsman name 
epSeedsmanNameColumn <- 'DLR_NAME'
# ----------------------------------------------------------------------------------------------------
# CORN budget run rate column name parameters #######################
#' @param teamID string; column that measures effective price
teamID <- 'TeamID'
#' @param  prevailingLCRPercentage string; column that measures effective price
prevailingLCRPercentage <- ' PrevailingLCRPercentage'
# ----------------------------------------------------------------------------------------------------
# CORN Features ######################################################################################
#' @param features vector; vector of features that enter the saved discount models used in UI
features <- readRDS(paste0("./R_Models/", 'features_to_keep_Discount'))
#' @param features_effP vector; vector of features that enter the saved effective price 
#' models used in UI
features_effP<-readRDS(paste0('./R_Models/features_to_keep_effP'))
#' @param countyFeatures
countyFeatures <- c("tmean_deviation_lag2", 
                    "tmean_deviation_lag1",
                    "ppt_lag1",
                    "ppt_lag2",
                    "tmean_lag1",
                    "tmean_lag2",
                    "fips_code")
#' @param epCountyFeatures
epCountyFeatures <- c("ppt_lag1", 
                      "fips_code")
# ----------------------------------------------------------------------------------------------------
# CORN Columns to drop for process cart functions that are determined by user input ##################
#' @param cartDropCols vector of strings; raw column names of input features that are determined 
#'  by user input and not historical customer records
cartDropCols <- c(totalQColumn, avgPriceColumn, priceColumn)
#' @param epCartDropCols
epCartDropCols <- c(epTotalQColumn, epAvgPriceColumn, "YearTimesLPBasket")
# ----------------------------------------------------------------------------------------------------
# Model path specification  (CORN + SOY) #############################################################
#' @param discountModelPath string; directory where the saved per-unit discount models used in UI
#' are located
discountModelPath <- "./R_Models/"
#' @param epModelPath string; directory where saved effective price models used in UI are located
epModelPath <- "./R_Models/"
# ----------------------------------------------------------------------------------------------------
# CORN Bootstrap model specification #################################################################
#' @param discountModelName string; file name of saved bootstrapped per-unit discount models used in UI
discountModelName <- 'PUDXGBoost_1.8.4_opt_40.4'
# ----------------------------------------------------------------------------------------------------
# Geo cross walk specification (CORN + SOY) ##########################################################
#' @param stateCountyToFips string; path + file name of state and county to fips crosswalk
stateCountyToFips <- read_csv("./Data/stateCountyToFips.csv")
# ----------------------------------------------------------------------------------------------------
# Standard deviation window specification (CORN + SOY) ###############################################
#' @param stdN numeric; standard deviation window to limit recommendations
stdN <- 0.5
# ----------------------------------------------------------------------------------------------------
# Features to show in grower characteristics table (CORN + SOY) ######################################
#' @param custFeats vector; vector of features to obtain regarding customer for Customer Information tab
custFeats <- c(#'YearsofRelationship', 
  'OPPORTUNITY_ACRES_Perc', 
  'TOTAL_CROP_AREA')
#'TopProducer')
#' @param custFeatNames vector; vector to rename features as in Customer Information tab
custFeatNames <- c(#'Relationship Years', 
  'Opportunity Acres (Pct. Total Acres)',
  'Total Crop Acres')
#'Top Producer')
#' @param custFeatKeys vector; vector of common keys to use between custFeats and custFeatNames
custFeatKeys <- c(#'relationshipYears',
  'oppAcres',
  'totalCropArea')
#'topProducer')
#' @param custFeatOrder vector; vector of formatted feature names ordered to display in UI
custFeatOrder <- c(#'Relationship Years', 
  #'Top Producer', 
  'Shared Farm', 
  'Total Crop Acres', 
  'Opportunity Acres (Pct. Total Acres)')
# ----------------------------------------------------------------------------------------------------
# CORN Point models #####################################################################
#' @param discountPointModelName string; name of point estimate discount model
discountPointModelName <- 'PUDXGBoostPoint_1.8.4'
#' @param epPointModelName string; name of point estimate effective price model
epPointModelName <- 'EPXGBoostPoint_1.8.4'
# ----------------------------------------------------------------------------------------------------
# Shared farms data (CORN + SOY) #############################################################
#'@param sharedFarms object; dataframe containing customer ids that own shared farms 
sharedFarms <- read_csv("./Data/sharedFarms.csv")
# ----------------------------------------------------------------------------------------------------
# Grower list (2015-2020) data #############################################################
#'@param growerList object; dataframe linking FSR to TeamID 
growerList <- read_csv("./Data/GrowerList2015_2020.csv")
#' @param abmID string; column corresponding to TeamID
abmID <- 'ABM ID'
#' @param fsr_byteam string; column corresponding to TeamID
fsr_byteam <- 'FSR'
# ----------------------------------------------------------------------------------------------------
# CORN Product data ##################################################################
#' @param recentPricing object; dataframe containing most recent Channel corn zone pricing data
recentPricing <- read_csv("./Data/fy21Pricing.csv")
#recentPricing <- read_csv("./Data/recentPricing.csv")
# source('functionsUI.R')
# # Use function to make customer selections
# products_df <- make_products_df(recentPricing)
# ----------------------------------------------------------------------------------------------------
# Feature dictionary specification and display options specification (CORN + SOY) ####################
#' @param featDict object; dataframe which maps formatted names and descriptions from raw feature names, 
#' currently this is file 'featureDict.csv'
featDict <- read_csv("./Data/featureDict.csv")
#' @param nFeaturesDisplay
nFeaturesDisplay <- 5
# ----------------------------------------------------------------------------------------------------
# CORN Grower report #################################################################################
#' @param reportAggCols vector; raw columns to include in historical grower recommendation report
#'  from discount product level data
reportAggCols <- c(priceColumn,
                   quantityColumn,
                   discountColumn,
                   yearColumn,
                   "REGION_ID",
                   "FSR_NAME",
                   #"ABM",
                   #"RBD",
                   #"FSR",
                   #"DSM",
                   "TEAM_ID",
                   "TERR_ID",
                   "CORN_ZONE_DESCR", 
                   custIDColumn,
                   custNameColumn,
                   productColumn,
                   fipsCodeColumn,
                   seedsmanIDColumn,
                   seedsmanNameColumn)
#' @param epReportAggCols vector; raw columns to include in historical grower report from effective
#'  price basket level data
epReportAggCols <- c(effectivePriceColumn, 
                     epCustIDColumn,
                     epSeedsmanIDColumn,
                     epYearColumn)
#' @param reportAggCleanNames vector; formatted column names to include in historical grower
#'  recommendation report
reportAggCleanNames <- c("List Price",
                         "Quantity",
                         "Observed Per-Unit LCR",
                         "Fiscal Year of Purchase",
                         "Sales Region",
                         #"ABM",
                         #"RBD",
                         "FSR",
                         #"DSM",
                         "Team ID",
                         "Territory ID",
                         "Corn Zone",
                         "Grower SAPID",
                         "Grower Account Name",
                         "Product",
                         "Fips Code",
                         'Seedsman SAPID',
                         'Seedsman Account Name',
                         #'District',
                         # Columns to store predicted and historical capped discount
                         "Predicted Per-Unit LCR",
                         "Capped Per-Unit LCR",
                         "Retail Total",
                         "Observed Total LCR",
                         "Predicted Total LCR",
                         "Capped Total LCR",
                         # Columns to store effective price 
                         'Observed Avg. Unit Farmgate Price',
                         'Predicted Unit Farmgate Price')
#' @param reportAggColOrder vector; order of formatted columns to include in historical grower
#' recommendation report 
reportAggColOrder <- c("Product",
                       "Quantity",
                       "List Price",
                       "Retail Total",
                       "Observed Per-Unit LCR", 
                       "Predicted Per-Unit LCR",
                       "Capped Per-Unit LCR",
                       "Observed Total LCR",
                       "Predicted Total LCR",
                       "Capped Total LCR",
                       "Observed Avg. Unit Farmgate Price",
                       "Predicted Unit Farmgate Price",
                       "Grower SAPID",
                       "Grower Account Name",
                       "Fiscal Year of Purchase",
                       "Sales Region",
                       "Corn Zone",
                       "Fips Code",
                       "Seedsman SAPID",
                       "Seedsman Account Name",
                       "Territory ID",
                       #"ABM",
                       #"RBD",
                       "FSR",
                       #"DSM",
                       "Team ID")
# ----------------------------------------------------------------------------------------------------
# Value prop (CORN + SOY) ############################################################################
#' @param compTableNamesChannel vector; formatted columns to use in county competitive information 
#'  table when Channel information is available
compTableNamesChannel <- c('County', 
                           'State', 
                           'Avg. Channel Yield (Bushels/Acre)', 
                           'Avg. Competitor Yield (Bushels/Acre)', 
                           'Avg. Value Proposition ($/Acre)')
#' @param compTableNameOther vector; formatted columns to use in county competitive information 
#'  table when Channel information is NOT available (i.e. use other Bayer hybrids)
compTableNamesOther <- c('County', 
                         'State', 
                         'Avg. Bayer Yield (Bushels/Acre)', 
                         'Avg. Competitor Yield (Bushels/Acre)', 
                         'Avg. Value Proposition ($/Acre)')
#' @param valueProp object; dataframe containing value proposition information 
valueProp <- read.csv('./Data/valuePropMARSUI.csv',header=TRUE)

#' @param YieldAdvSoy object; dataframe containing value propsition/yield advantage info for soy
YieldAdvantageSoy <- read.csv("./Data/YieldAdvantageSoy.csv",header=TRUE)

# ----------------------------------------------------------------------------------------------------
# CORN Purchase history table ########################################################################
#' @param custPurchaseNames vector; raw column names to include in customer purchase history table
custPurchaseNames <- c("PRODUCT_QTY",
                       "AVGRETAILPRICE",
                       yearColumn,
                       "VARIETY",
                       discountColumn,
                       effectivePriceColumn)

#' @param formattedPurchaseNames vector; formatted names to include in customer purchase history table
formattedPurchaseNames <- c('Quantity Purchased',
                            'Purchase List Price',
                            'Year of Purchase',
                            'Product',
                            'LCR Unit',
                            'Unit Farm Gate Price*')

#' @param custPurchaseKeys vector; common keys to use between custPurchaseNames and formattedPurchaseNames
custPurchaseKeys <- c('quantityPurchased',
                      'listPrice',
                      'purchaseYear',
                      'hybridName',
                      'perUnitLCR',
                      'avgFarmgatePrice')

#' @param orderPurchaseNames vector; order of formatted columns to include in customer purchase history
#' table
orderPurchaseNames <- c('Year of Purchase', 
                        'Product',
                        'Quantity Purchased',
                        'Purchase List Price',
                        'LCR Unit',
                        'LCR %',
                        'Unit Farm Gate Price*')
# ----------------------------------------------------------------------------------------------------
# CORN customer history plotting functionality #######################################################
#'@param custFeaturesToPlot raw names features to plot in customer information tab
custFeaturesToPlot <- c('Year',
                        'PerUnitDiscount', 
                        'PctDiscount',
                        'AVG_FARMGATE_PRICE')
#'@param featsToPlotCleanNames clean names of features to plot in customer information tab
featsToPlotCleanNames <- c('Year', 
                           'Avg. LCR Unit',
                           'Avg. LCR %',
                           'Avg. Farm Gate Price')
# ----------------------------------------------------------------------------------------------------
# SOY Log transform ##################################################################################
#'@param logTransformDiscountSoy bool; should predictions for discount be transformed from log form 
#'before display in UI
logTransformDiscountSoy <- TRUE
#'@param logTransformEPSoy string; should predictions for effective price be transformed from log form 
#'before display in UI
logTransformEPSoy <- FALSE
#-----------------------------------------------------------------------------------------------------
# SOY Data files #####################################################################################
#' @param trainDataNameSoy string; file name of historical customer records
trainDataNameSoy <- 'CleanedProductLevelSoyData_fromLarrySwift_20201021.rds'
#' @param EffPriceNameSoy string; file name of data used to train effective price models at the basket level
#' (an aggregation to basket level of file trainDataPath + trainDataName)
EffPriceNameSoy <- 'CleanedSoyData_fromLarrySwift_20200917.rds'
#' @param runRateNameSoy string; file name of data used to compute budget run rate
#' (an aggregation to basket level of file trainDataPath + runRateNameSoy)
runRateNameSoy <- 'budgetRunRate_soy.csv'
# ----------------------------------------------------------------------------------------------------
# SOY Historical customer discount record column name parameters #####################################
#' @param quantityColumnSoy string; column that measures product quantity
quantityColumnSoy <- "PRODUCT_QTY"
#' @param priceColumnSoy string; column that measures product list price
priceColumnSoy <- "AVGRETAILPRICE"
#' @param productColumnSoy string; column that mesaures product name
productColumnSoy <- "VARIETY"
#' @param discountColumnSoy string; column that measures per unit disount
discountColumnSoy <- "PerUnitDiscount"
#' @param fipsCodeColumnSoy string; column that measures customer fips code 
fipsCodeColumnSoy <- "FIPS"
#' @param custIDColumnSoy string; column that measures numeric customer id
custIDColumnSoy <- "GRWR_SAP_ID"
#' @param yearColumnSoy string; column that measures purchase year
yearColumnSoy <- "MKT_YR"
#' @param custNameColumnSoy string; column that mesures customer name
custNameColumnSoy <- "GRWR_ACCT_NAME"
#' @param seedsmanIDColumnSoy string; column that measures numeric seedsman id
seedsmanIDColumnSoy <- "DLR_SAP_ID"
#' @param seedsmanNameColumnSoy stringl column that measures seedsman account name
seedsmanNameColumnSoy <- 'DLR_NAME'
#' @param terrIDColumnSoy string; column that measures terrID
terrIDColumnSoy <- 'TERR_ID'
#' @param totalQColumnSoy string; column that measures total order quantity
totalQColumnSoy <- 'ORDERED_QTY'
#'@param avgPriceColumnSoy string; column that measures order average unit retail price
avgPriceColumnSoy <- 'AVGPRICE_RETAIL'
#'@param fsrColumnSoy string; column that gives FSR name (last, first)
fsrColumnSoy <- 'FSR'
# ----------------------------------------------------------------------------------------------------
# SOY Historical effective price (basket level) record column name parameters ########################
#' @param effectivePriceColumnSoy string; column that measures effective price
effectivePriceColumnSoy <- 'AVG_FARMGATE_PRICE' 
#' @param epCustIDColumnSoy string; column that measures numeric customer id
epCustIDColumnSoy <- 'GRWR_SAP_ID'
#' @param epYearColumnSoy string; column that measures purchase year
epYearColumnSoy <- 'MKT_YR'
#' @param epAvgPriceColumnSoy string; column that measures avg unit order price
epAvgPriceColumnSoy <- 'AVGPRICE_RETAIL'
#' @param epTotalQColumnSoy string; column that measures total order quantity
epTotalQColumnSoy <- 'ORDERED_QTY'
#' @param epFipsCodeColumnSoy string; column that measures fips code
epFipsCodeColumnSoy <- 'FIPS'
#' @param epSeedsmanIDColumnSoy string; column that measures numeric seedsman id
epSeedsmanIDColumnSoy <- 'DLR_SAP_ID'
#'@param epSeedsmanNameColumnSoy string; column that mesures seedsman name 
epSeedsmanNameColumnSoy <- 'DLR_NAME'
# ----------------------------------------------------------------------------------------------------
# SOY budget run rate column name parameters #######################
#' @param teamIDSoy string; column that measures effective price
teamIDSoy <- 'TeamID'
#' @param  prevailingLCRPercentageSoy string; column that measures effective price
prevailingLCRPercentageSoy <- ' PrevailingLCRPercentage'
# ----------------------------------------------------------------------------------------------------
# SOY Features #######################################################################################
#' @param featuresSoy vector; vector of features that enter the saved discount models used in UI
featuresSoy <- readRDS(paste0("./R_Models/", 'features_to_keep_SoyDiscount'))
#' @param features_EffPSoy vector; vector of features that enter the saved effective price 
#' models used in UI
features_EffPSoy<-readRDS(paste0('./R_Models/features_to_keep_SoyEffP'))
#' @param countyFeaturesSoy
countyFeaturesSoy <- c("tmean_deviation_lag2", 
                    "tmean_deviation_lag1",
                    "ppt_lag1",
                    "ppt_lag2",
                    "tmean_lag1",
                    "tmean_lag2",
                    "FIPS",
                    "usda_yield_lag1_Corn",
                    "usda_yield_lag1", 
                    "ppt_deviation_lag2",
                    "usda_harvestVplant_lag1_Corn",
                    "usda_yield_lag2",
                    "usda_harvestVplant_lag2_Corn", 
                    "ppt_deviation_lag1",
                    "usda_yield_lag2_Corn",
                    "usda_planted_lag1_Corn"
                    )
#' @param epCountyFeaturesSoy
epCountyFeaturesSoy <- c("FIPS")
# ----------------------------------------------------------------------------------------------------
# SOY Model inputs that are obtained directly from user ##############################################
#' @param cartDropColsSoy vector of strings; raw column names of input features that are determined 
#'  by user input and not historical customer records
cartDropColsSoy <- c(totalQColumnSoy, avgPriceColumnSoy, priceColumnSoy, "RETAIL_AMT")
#' @param epCartDropColsSoy vector of strings; raw column names of input features that are determined 
#'  by user input and not historical basket level records
epCartDropColsSoy <- c(epTotalQColumnSoy, epAvgPriceColumnSoy, "YearTimesLPBasket",
                       "RETAIL_AMT"
                       )
# ----------------------------------------------------------------------------------------------------
# SOY Bootstrap model ################################################################################
#' @param discountModelNameSoy string; file name of saved bootstrapped per-unit discount models used in UI
discountModelNameSoy <- 'SoyDiscountXGBoost_0.1.0_opt_40'
# ----------------------------------------------------------------------------------------------------
# SOY Point models ####################################################################################
#' @param discountPointModelNameSoy string; name of point estimate discount model
discountPointModelNameSoy <- 'SoyDiscountXGBoostPoint_0.1.0'
#' @param epPointModelNameSoy string; name of point estimate effective price model
epPointModelNameSoy <- 'SoyEPXGBoost_0.1.0'
# ----------------------------------------------------------------------------------------------------
# SOY Product data ###################################################################################
#' @param recentPricingSoy object; dataframe containing most recent Channel pricing data
recentPricingSoy <- read_csv("./Data/fy21PricingSoy.csv")
#recentPricingSoy <- read_csv("./Data/recentPricingSoy.csv")
source('functionsUI.R')
# Use function to make customer selections
products_df_soy <- make_products_df(recentPricingSoy)
# ----------------------------------------------------------------------------------------------------
# SOY Grower report ##################################################################################
#' @param reportAggColsSoy vector; raw columns to include in historical grower recommendation report
#'  from discount product level data
reportAggColsSoy <- c("AVGRETAILPRICE",
                      "PRODUCT_QTY",
                      "PerUnitDiscount",
                      "MKT_YR",
                      "REGION_ID", 
                      "FSR_NAME", 
                      "TEAM_ID",
                      "TERR_ID", 
                      "GRWR_SAP_ID", 
                      "GRWR_ACCT_NAME", 
                      "VARIETY",
                      "FIPS",
                      "DLR_SAP_ID",
                      "DLR_NAME")
#' @param epReportAggColsSoy vector; raw columns to include in historical grower report from effective
#'  price basket level data
epReportAggColsSoy <- c("AVG_FARMGATE_PRICE",
                     "GRWR_SAP_ID",
                     "DLR_SAP_ID",
                     "MKT_YR")
#' @param reportAggCleanNamesSoy vector; formatted column names to include in historical grower
#'  recommendation report
reportAggCleanNamesSoy <- c("List Price",
                            "Quantity",
                            "Observed Per-Unit LCR",
                            "Fiscal Year of Purchase",
                            "Sales Region",
                            "FSR",
                            "Team ID",
                            "Territory ID",
                            "Grower SAPID",
                            "Grower Account Name",
                            "Product",
                            "FIPS Code",
                            'Seedsman SAPID',
                            'Seedsman Account Name',
                            "Predicted Per-Unit LCR",
                            "Capped Per-Unit LCR",
                            "Retail Total",
                            "Observed Total LCR",
                            "Predicted Total LCR",
                            "Capped Total LCR",
                            'Observed Avg. Unit Farmgate Price',
                            'Predicted Unit Farmgate Price')
#' @param reportAggColOrderSoy vector; order of formatted columns to include in historical grower
#' recommendation report 
reportAggColOrderSoy <- c("Product",
                          "Quantity",
                          "List Price",
                          "Retail Total",
                          "Observed Per-Unit LCR", 
                          "Predicted Per-Unit LCR",
                          "Capped Per-Unit LCR",
                          "Observed Total LCR",
                          "Predicted Total LCR",
                          "Capped Total LCR",
                          "Observed Avg. Unit Farmgate Price",
                          "Predicted Unit Farmgate Price",
                          "Grower SAPID",
                          "Grower Account Name",
                          "Fiscal Year of Purchase",
                          "Sales Region",
                          "FIPS Code",
                          "Seedsman SAPID",
                          "Seedsman Account Name",
                          "Territory ID",
                          "FSR",
                          "Team ID")
# ----------------------------------------------------------------------------------------------------
# SOY Purchase history table #########################################################################
#' @param custPurchaseNamesSoy vector; raw column names to include in customer purchase history table
custPurchaseNamesSoy <- c("PRODUCT_QTY",
                        "AVGRETAILPRICE",
                        yearColumnSoy,
                        "VARIETY",
                       discountColumnSoy,
                       effectivePriceColumnSoy)

#' @param formattedPurchaseNamesSoy vector; formatted names to include in customer purchase history table
formattedPurchaseNamesSoy <- c('Quantity Purchased',
                             'Purchase List Price',
                             'Year of Purchase',
                             'Product',
                             'LCR Unit',
                             'Unit Farm Gate Price*')

#' @param custPurchaseKeysSoy vector; common keys to use between custPurchaseNames and formattedPurchaseNames
custPurchaseKeysSoy <- c('quantityPurchased',
                      'listPrice',
                      'purchaseYear',
                      'hybridName',
                      'perUnitLCR',
                      'avgFarmgatePrice')

#' @param orderPurchaseNamesSoy vector; order of formatted columns to include in customer purchase history
#' table
orderPurchaseNamesSoy <- c('Year of Purchase', 
                           'Product',
                           'Quantity Purchased',
                           'Purchase List Price',
                           'LCR Unit',
                           'LCR %',
                           'Unit Farm Gate Price*')
# ----------------------------------------------------------------------------------------------------
# SOY Customer history plotting functionality ########################################################
#'@param custFeaturesToPlotSoy raw names features to plot in customer information tab
custFeaturesToPlotSoy <- c('MKT_YR',
                        'PerUnitDiscount',
                        'PctDiscount',
                        'AVG_FARMGATE_PRICE')
#'@param featsToPlotCleanNamesSoy clean names of features to plot in customer information tab
featsToPlotCleanNamesSoy <- c('Year', 
                           'Avg. LCR Unit',
                           'Avg. LCR %',
                           'Avg. Farm Gate Price')
# ----------------------------------------------------------------------------------------------------
# Portfolio info table (CORN + SOY) ##################################################################
#'@param orderTableNames vector; what order to display formatted columns in portfolio info table
#' for existing customer
orderTableNames <- c('Actual LCR Unit*',
                     'Prevailing LCR Unit',
                     'Rec. LCR Unit',
                     'Actual LCR %',
                     'Prevailing LCR %',
                     'Rec. LCR %',
                     'Actual Farm Gate Price')
#'@param orderTableNamesNewCust vector;  what order to display formatted columns in portfolio info table
#' for new customer
orderTableNamesNewCust <- c('Prevailing LCR Unit',
                            'Prevailing LCR %')
# ----------------------------------------------------------------------------------------------------
#' @param saveOfferFilePath string; filepath to save exported customer portfolios to (CORN + SOY)
saveOfferFilePath <- './recorded_user_data/'
# ----------------------------------------------------------------------------------------------------
#' @param marketYear numeric; current market year (CORN + SOY)
marketYear <- 2020
# ----------------------------------------------------------------------------------------------------
#' @param zoneLookUp object; crosswalk of fips code to zone mapping
zoneLookUp <- read_csv('./Data/mappingall_processed.csv')

#' @param HTML_style Character string of nav-bar default settings
HTML_style <-c("
                              .navbar-default .navbar-brand {color: white;}
                              .navbar-default .navbar-brand:hover {color: white;}
                              .navbar { background-color: #5f6670;}
                              .navbar-default .navbar-nav > li > a {color:black;}
                              .navbar-default .navbar-nav > .active > a,
                              .navbar-default .navbar-nav > .active > a:focus,
                              .navbar-default .navbar-nav > .active > a:hover {color: white;background-color: #000000;}
                              .navbar-default .navbar-nav > li > a:hover {color: black;background-color:white; font-weight: bold}
                              .navbar-default .navbar-nav > li > a[data-value='Corn'] {color: #C3CE44; font-size: 20px;}
                              .navbar-default .navbar-nav > li > a[data-value='Soy'] {color: #C3CE44; font-size: 20px;}
                              .navbar-default .navbar-nav {float: none !important;}
                              .navbar-default .navbar-nav > li:nth-child(3) {float: right; bottom: 28px;}
                              ")
