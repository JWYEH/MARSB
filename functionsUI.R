# ====================================================================================================
# Functions ------------------------------------------------------------------------------------------
# ====================================================================================================
# channeldiscountpredict functions ===================================================================
# The following functions are from the custom R package 'channeldiscountpredict' 
#  and are used throughout the server portion of the Shiny App.
#  They are defined manually here to circumvent difficulties that may occurr when deploying 
#  with a custom package. 

#' Helper function for the vectorized function below this portion of code, predictXGBoost
#' 
#' @title getPredHelper
#' @param model object; model to predict with
#' @param testVars object; dataframe of test data to predict on
#' @param logTransform bool; should predictions be transformed from log form before saving
#' @returns pred, object; prediction from model
#'
getPredHelper <- function(model, testVars, logTransform=FALSE){
  
  pred_not_transformed <- predict(model, data.matrix(testVars))
  if (logTransform == TRUE){
    
    # transform if logTransform=TRUE
    pred_transformed <- exp(pred_not_transformed)
    pred <- as.numeric(pred_transformed)
  }
  
  else{
    pred <- as.numeric(pred_not_transformed)
  }
  
  return(pred)
}


#'
#' @title predictXGBoost
#' @description Generates a vector of predictions using saved trained XGBoost models
#' @param model_results 
#' @param test_df object; new data to use for prediction
#' @param features vector; features to predict with
#' @param logTransform bool; should predictions be transformed from log form before saving
#' @param pllel bool; should predictions be done in parallel
#' @return pred_list, vector; distribution of predictions from bootstrapped models
#'
predictXGBoost <- function(model_results, test_df, features, logTransform = FALSE,
                           pllel = FALSE) {
  
  testVars <- dplyr::select(data.frame(test_df),
                            all_of(features))
  if (pllel == TRUE) {
    
    pred_list <- unlist(mclapply(model_results, function(i) getPredHelper(i, testVars, logTransform = logTransform)))
    
    return(pred_list)
    
  } else {
    
    pred_list <- unlist(lapply(model_results, function(i) getPredHelper(i, testVars, logTransform = logTransform)))
    
    return(pred_list)
    
  }
  
}

# UI helper functions ================================================================================
# This section defines more functions that are used in the sever portion of the MARS Shiny App
#
#' Converts grower account names from all uppercase to all title case
#' 
#' @param df object; dataframe with data to convert
#' @param nameColumn string; name of column that tracks name to convert
#' @return df, dataframe with name column formatted in title case
#' 
convert_names <- function(df, nameColumn) {
  # convert from all capitals to title case
  df[, nameColumn] = stringr::str_to_title(df[[nameColumn]])

  # return df
  df
}


#' Function to make product choices
#'
#' @title make_products_df
#' @param recentPricing object; dataframe containing most recent pricing information for Channel's
#'  products--see file recentPricing.csv or recentPricingSoy.csv
#' @return products_df, object; datatable containing hybrids and prices for UI user to select from
#'
make_products_df <- function(recentPricing) {
  # group price by variety
  products_df <- aggregate(AVGRETAILPRICE ~ VARIETY, dat=recentPricing, mean)
  
  # rename columns
  colnames(products_df) <- c('Product', 'Price')
  
  # make prices integers
  products_df$Price <- as.integer(products_df$Price)
  
  data.table(products_df)
}

#' Function to make product choices accounting for zone
#' 
#' @title make_products_df_zone
#' @param recent_zone_pricing object; dataframe containing zone specific pricing information
#' @return products_df, object; datatable containing hybrids and prices for UI users to select from

make_products_df_zone <- function(recent_zone_pricing) {
  
  # group price by product
  products_df <- aggregate(Price ~ Product, dat=recent_zone_pricing, mean)
  
  # rename columns
  colnames(products_df) <- c('Product', 'Price')
  
  # make prices integers
  products_df$Price <- as.integer(products_df$Price)
  
  data.table(products_df)
}

#' Gets numeric id from string "name | id" combination in drop down menu in UI
#' 
#' @param name string; name to extract id from 
#' @return id, numeric; identifier
#' 
get_id <- function(name) {
  
  id <- as.numeric(unlist(strsplit(name, split='\\| '))[2])
  
  id
}

#' Gets alpha-numeric territory id from string "fsr name | terr id" combination in drop down menu in UI
#' 
#' @param name string; name to extract id from
#' @return terr_id, string; territory identifier 
get_terr_id <- function(name){
  id <- unlist(strsplit(name, split='\\| '))[2]
  
  id
}


#' Add row to cart for most recent product added to portfolio for existing
#'  customer for corn
#' 
#' @title update_cart
#' @param discounts vector; predicted discounts for row added to cart
#' @param cart_df object; datatable of customer's current portfolio
#' @param new_row object; datatable of product and quantity to add to customer's portfolio
#' @param cust_name string; customer account name
#' @param cust_df object; dataframe of historical customer product level records
#' @param product string; name of product discount prediction is for
#' @param EffPrice object; dataframe of historical customer basket level records
#' @param custIDColumn string; name of numeric customer identifier column in cust_df
#' @param epCustIDColumn string; name of numeric customer identifier column in EffPrice
#' @param productColumn string; name of column that tracks product name in cust_df
#' @param yearColumn string; name of column that tracks year in cust_df
#' @param effectivePriceColumn string; name of column that tracks effective price in EffPrice
#' @param seedsmanIDColumn string; name of column that tracks numeric seedsman id in cust_df
#' @param epSeedsmanIDColumn string; name of column that tracks numeric seedsman id in EffPrice
#' @param seedsmanID numeric; numeric seedsman identifier
#' @param products_df object; data frame of relevant products and prices
#' @param epYearColumn string; name of column that tracks year in basket level effective price data
#' @param quantityColumn string; name of column that tracks product quantity in product level customer records
#' @param priceColumn string; name of column that tracks list price in cust_df
#' @return cart_df object; updated customer portfolio
#' 
update_cart <- function(discounts, 
                        cart_df, 
                        new_row, 
                        cust_name, 
                        cust_df,
                        product,
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
                        priceColumn) {
  
  # create correct new row to add to
  # add list price
  new_row <- merge(new_row, products_df, by = "Product")
  
  # get customer id
  cust_id <- get_id(cust_name)
  
  cart_df[, 'Price'] <-  as.numeric(gsub("[\\$,]", "", cart_df$Price))
  
  # combine with existing cart and get only products that correspond to new row
  tmp <- rbind(cart_df, new_row, use.names = TRUE, fill = TRUE)
  tmp <- tmp[tmp$Product == unlist(product), ]
  
  # collapse by product
  new_row <- tmp[, .(Quantity = sum(Quantity),
                         Price = mean(Price)),
                     by = Product]
  
  # Get last quantity purchased if customer has previously purchased that product
  new_row <- new_row[, LastPurchasedQuantity := get_last_quantity(new_row,
                                                                  cust_id,
                                                                  cust_df,
                                                                  custIDColumn, 
                                                                  productColumn, 
                                                                  yearColumn,
                                                                  quantityColumn)]
  # # make empty effective price to merge
  # new_row[, EffectivePrice := as.character("NA")]
  
  # get mean predicted disount as percentage of list price
  discountPct <- paste0(format(round(as.numeric((mean(discounts)/new_row[,  'Price'] * 100)), 2), nsmall=2),
                        '%')

  # round mean of discount predicitons to 2 decimal places and place into 
  # new_row
  new_row <- new_row[, Discount := paste0('$', format(round(mean(discounts), 2), nsmall=2))]
  
  # put discount as percentage of list price into new_row
  new_row <- new_row[, DiscountPCT := discountPct ]

  # # Get last unit discount if customer has previously purchased that product
  # new_row <- new_row[, LastUnitDiscount :=  get_last_unit_discount(new_row,
  #                                                                  cust_id, 
  #                                                                  cust_df, 
  #                                                                  custIDColumn, 
  #                                                                  productColumn,
  #                                                                  yearColumn, 
  #                                                                  discountColumn,
  #                                                                  seedsmanID,
  #                                                                  seedsmanIDColumn)]
  
  
  new_row <- new_row[, LastUnitDiscountPCT := get_last_unit_discount_pct(new_row,
                                                                         cust_id,
                                                                         cust_df,
                                                                         custIDColumn,
                                                                         productColumn,
                                                                         yearColumn,
                                                                         discountColumn,
                                                                         seedsmanID,
                                                                         seedsmanIDColumn,
                                                                         priceColumn)]

  # last unit discount pro-forma calculation based on current list price and last discount percent
  if (new_row[, LastUnitDiscountPCT] == 'NA') {
    lastUnitDiscount <- 'NA'
  } else{
    lastUnitDiscount <- format(round((as.numeric(gsub("[\\%,]", "", 
                                                      new_row[, LastUnitDiscountPCT])) / 100) * as.numeric(gsub("[\\$,]", "", new_row[, Price])), 2), nsmall=2)
  }
  if (lastUnitDiscount == 'NA'){
    new_row[, LastUnitDiscount := lastUnitDiscount]
  } else{
    new_row[, LastUnitDiscount := paste0('$', lastUnitDiscount)]
  }
  
  # Take minimum of predicted/actual and make "recommended"
  # if NA...
  if (new_row[, LastUnitDiscount] == "NA"){
    new_row[, RecDiscount := new_row[, Discount]]
    new_row[, RecDiscountPCT := new_row[, DiscountPCT]]
  } else {
    checkDiscounts <- list(as.numeric(gsub("[\\$,]", "", new_row[, Discount])), 
                           as.numeric(gsub("[\\$,]", "", new_row[, LastUnitDiscount])))
    checkDiscountsPCT <- list(as.numeric(gsub("[\\%,]", "", new_row[, DiscountPCT])), 
                              as.numeric(gsub("[\\%,]", "", new_row[, LastUnitDiscountPCT])))
    new_row[, RecDiscount := paste0('$', format(round(min(unlist(checkDiscounts)), 2), nsmall=2))]
    new_row[, RecDiscountPCT := paste0(format(round(min(unlist(checkDiscountsPCT)), 2), nsmall=2), '%')]
  }
  
  
  # if nothing in cart, make cart new_row
  if (nrow(cart_df) == 0) {
    cart_df <- new_row
  # if product was not in cart already, rbind new_row and cart_df
  } else if (nrow(cart_df[cart_df$Product==unlist(product), ]) == 0) {
    
    cart_df <- rbind(cart_df, new_row, use.names = TRUE, fill = TRUE)
  
  } else {
    # add button_id column for merging
    new_row[, 'button_id'] <- cart_df[cart_df$Product==unlist(product), 'button_id']
    
    # order columns correctly
    colOrder <- names(cart_df[cart_df$Product == unlist(product), ])
    new_row <- new_row[, ..colOrder]
    
    # insert new row into where product is in cart_df
    cart_df[cart_df$Product==unlist(product), ] <- new_row
    
    
  }
  
  # # distribute predicted effective prices to appropiate rows
  # prices <- as.data.frame(prices)
  # PredPrices <- unlist(lapply(cart_df$Product, function(i) prices[prices$Product == as.character(i), 
  #                                                                 'PredictedProductEffectivePrice']))
  # # formatting predicted prices and list price
  # cart_df[, EffectivePrice := paste0('$', format(round(PredPrices, 2), nsmall=2))]
  
  # format list price 
  cart_df[, Price := paste0('$', format(cart_df$Price, nsmall=2))]
  
  return(cart_df)
}


#' Add row to cart for most recent product added to portfolio for new
#'  customer 
#' 
#' @title update_cart_new_cust
#' @param discounts vector; predicted discounts for row added to cart
#' @param cart_df object; datatable of customer's current portfolio
#' @param new_row object; datatable of product and quantity to add to customer's portfolio
#' @param fips_code numeric; fips code of new customer's county
#' @param product string; name of product discount prediction is for
#' @param products_df object; data frame of unique hybrid - price combinations (what goes into drop down menus)
#' @return cart_df object; updated customer portfolio
#' 
update_cart_new_cust <- function(discounts,
                                 cart_df, 
                                 new_row, 
                                 fips_code, 
                                 product,
                                 products_df) {
  # add list price
  new_row <- merge(new_row, products_df, by = "Product")
  
  # get rid of dollar sign for computation 
  cart_df[, 'Price'] <-  as.numeric(gsub("[\\$,]", "", cart_df$Price))
  
  # combine with existing cart and get only products that correspond to new row
  tmp <- rbind(cart_df, new_row, use.names = TRUE, fill = TRUE)
  tmp <- tmp[tmp$Product == unlist(product), ]

  # collapse by product
  new_row <- tmp[, .(Quantity = sum(Quantity),
                     Price = mean(Price)),
                 by = Product]

  # get mean predicted disount as percentage of list price
  discountPct <- paste0(format(round(as.numeric((mean(discounts)/new_row[,  'Price'] * 100)), 2),nsmall=2),
                        '%')
  
  
  # round mean of discount predicitons to 2 decimal places and place into 
  #  new_row
  new_row <- new_row[, Discount := paste0('$', format(round(mean(discounts), 2), nsmall=2))]
  
  # put discount as percentage of list price into new_row
  new_row <- new_row[, DiscountPCT := discountPct ]
  
  # new_row <- new_row[, EffectivePrice := as.character("NA")]
  
  # new customer won't have any previous purchases, fill accordingly
  new_row <- new_row[, LastUnitDiscount :=  as.character("NA")]
  new_row <- new_row[, LastUnitEP :=  as.character("NA")]
  new_row <- new_row[, LastPurchasedQuantity := as.character("NA")]
  new_row <- new_row[, LastUnitDiscountPCT := as.character ("NA")]
  
  # make recommended predicted because we have nothing to compare to
  new_row <- new_row[, RecDiscountPCT := discountPct]
  new_row <- new_row[, RecDiscount := new_row[, Discount]]
  
  # if nothing in cart, make cart new_row
  if (nrow(cart_df) == 0) {
    cart_df <- new_row
    # if product was not in cart already, rbind new_row and cart_df
  } else if (nrow(cart_df[cart_df$Product==unlist(product), ]) == 0) {
    
    cart_df <- rbind(cart_df, new_row, use.names = TRUE, fill = TRUE)
    
  } else {
    
    # add button_id column for merging
    new_row[, 'button_id'] <- cart_df[cart_df$Product==unlist(product), 'button_id']
    
    # order columns correctly
    colOrder <- names(cart_df[cart_df$Product == unlist(product), ])
    new_row <- new_row[, ..colOrder]
    
    # insert new_row into where product is in cart_df
    cart_df[cart_df$Product==unlist(product), ] <- new_row
    
  }
  
  # # distribute predicted effective prices to appropiate rows # CURRENTLY DEPRECATED PER REQUEST OF THE BUSINESS 
  # prices <- as.data.frame(prices)
  # PredPrices <- unlist(lapply(cart_df$Product, function(i) prices[prices$Product == as.character(i), 
  #                                                                 'PredictedProductEffectivePrice']))
  # # format effective and list price
  # cart_df[, EffectivePrice := paste0('$', format(round(PredPrices, 2), nsmall=2))]
  
  # format list price 
  cart_df[, Price := paste0('$', format(cart_df$Price, nsmall=2))]
  
  return(cart_df)
  
}


#' Processes input from user and creates data set necesscary to generate 
#'  predictions for discounts for corn
#' 
#' @title process_cart
#' @param product string; product name of most recent item added to cart
#' @param quantity numeric; product quantity of most recent item added to cart
#' @param cart_df object; datatable of customer's current portfolio
#' @param cust_id numeric; customer identifier
#' @param cust_df object; dataframe containing historical customer product records
#' @param quantityColumn string; name of column that tracks product quantity in cust_df
#' @param priceColumn string; name of column that tracks product list price in cust_df
#' @param productColumn string; name of column that tracks product name in cust_df 
#' @param custIDColumn string; name of column that tracks numeric customer id in cust_df
#' @param cartDropCols vector; columns that are derived directly from user input and do not
#'  need extraction from cust_df
#' @param features vector; vector of features that enter the saved discount models used in UI
#' @param avgPrice numeric; average retail price of customer order (including other products)
#' @param totalQuantity numeric; total quantity in customer order (including other products)
#' @param yearColumn string; name of column that tracks year in cust_df
#' @param seedsmanIDColumn string; name of column that tracks snumeric seedsman id in cust_df
#' @param seedsmanID numeric; seedsman identifier
#' @param countyFeatures vector; vector of strings identifying features that are associated with customer's fips_code
#' @param fipsCodeColumn string; name of column that tracks fips code in cust_df
#' @return obs_df, dataframe containing all necesscary customer information to generate predictions
#' 
process_cart <- function(product,
                         quantity,
                         cust_id, 
                         cust_df, 
                         quantityColumn, 
                         priceColumn,
                         productColumn,
                         custIDColumn,
                         cartDropCols,
                         features,
                         avgPrice,
                         totalQuantity,
                         yearColumn,
                         seedsmanIDColumn,
                         seedsmanID,
                         countyFeatures,
                         fipsCodeColumn){
  # tracking product and quantity

  df <- data.table()
  df[, "Product"] <- product
  df[, "Quantity"] <- quantity
  
  # Set customer ID as numeric to ensure merging goes smooth
  df[, custIDColumn := as.numeric(cust_id)]
  
  # Generate predictors for customer that are a direct function of user inputs
  #  and do not manipulate the ones that aren't...
  # Get customer-specific features from training data
  cust_df <- setDF(cust_df)
  tmp_cust <- cust_df[cust_df[[custIDColumn]]==cust_id & cust_df[[seedsmanIDColumn]] == seedsmanID, ]
  # make copy to refernce when making correct lags
  tmp_cust_ref <- copy(tmp_cust)
  # shift all purchase history lags so we use most recent data available
  # Farmgate price
  tmp_cust$AVG_FARMGATE_PRICE_lag2 <- tmp_cust_ref$AVG_FARMGATE_PRICE_lag1
  tmp_cust$AVG_FARMGATE_PRICE_lag1 <- tmp_cust_ref$AVG_FARMGATE_PRICE

  # LCR
  # Corn
  tmp_cust$AvgPerUnitDiscount_lag2 <- tmp_cust_ref$AvgPerUnitDiscount_lag1
  tmp_cust$AvgPerUnitDiscount_lag1 <- tmp_cust_ref$AvgPerUnitDiscount

  # Soy
  tmp_cust$AvgPerUnitDiscount_SOY_lag2 <- tmp_cust_ref$AvgPerUnitDiscount_SOY_lag1
  tmp_cust$AvgPerUnitDiscount_SOY_lag1 <- tmp_cust_ref$AvgPerUnitDiscount_Soy
 
  # get correct seedsman experience
  seedsmanDF <- cust_df[cust_df[[seedsmanIDColumn]] == seedsmanID, ]
  seedsmanExp <- max(cust_df$YearsOfService, na.rm=TRUE)
  tmp_cust$YearsOfService <- seedsmanExp
   
  # order by year 
  tmp_cust <- tmp_cust[order(-tmp_cust[[yearColumn]]), features]
  
  # Remove columns we are going to create based on user input
  tmp_cust[ , cartDropCols] <- NULL
  # data.table for compatibility 
  tmp_cust <- data.table(tmp_cust)
  tmp_cust[, custIDColumn := as.numeric(cust_id)]
  
  # Take most recent row 
  tmp_cust <- tmp_cust[1, ]

  # Merge so a single row exists for each product
  obs_df <- merge(df, tmp_cust, by='custIDColumn')
  
  # rename custIDColumn appropiately
  names(obs_df)[names(obs_df) == 'custIDColumn'] <- custIDColumn

  # Get list price for each product
  UnitPrices <- unlist(lapply(obs_df$Product, function(i) products_df[products_df$Product == as.character(i), 'Price']))
  obs_df[, priceColumn] <- as.numeric(UnitPrices)
  
  # get corresponding order level variables 
  obs_df[, totalQColumn] <- totalQuantity
  obs_df[, avgPriceColumn] <- avgPrice
  
  # Use mean time from start of season the customer typically purchases
  obs_df$TimefromStartofSeason <- mean(tmp_cust_ref$TimefromStartofSeason, na.rm=TRUE)
  
  # temporary fix for weather and usda data, use averages for features tied to county until weather data is fixed
  tmp_county <- tmp_cust_ref[, countyFeatures]
  tmp_county <- tmp_county[,colSums(is.na(tmp_county))<nrow(tmp_county)]
  
  out <- aggregate(.~tmp_county[[fipsCodeColumn]], data=tmp_county, FUN=mean, na.rm=TRUE, na.action=na.pass)
  out[[fipsCodeColumn]] <- NULL
  names(out)[names(out) == "tmp_county[[fipsCodeColumn]]"] <- fipsCodeColumn
  obs_df[, countyFeatures] <- NULL
  obs_df <- cbind(obs_df, out)
  
  obs_df[, 'Product'] <- NULL
  obs_df[, 'Quantity'] <- NULL
 
  empty_cols <- features[which(!features %in% names(obs_df))]
  len_empty_cols <- length(empty_cols)
  if(len_empty_cols > 0) {
    obs_df[, empty_cols] <- NA
  }
  
  obs_df
}


#' Processes input from user and creates data set necesscary to generate 
#' predictions for discount for a new customer for corn
#' 
#' @title process_cart_new_cust
#' @param product string; name of product discount prediction is for
#' @param quantity numeric; quantity of product discount is for
#' @param fips_code numeric; fips associated with customer county
#' @param quantityColumn string; name of column that tracks product quantity in cust_df
#' @param priceColumn string; name of column that tracks product price in cust_df
#' @param features vector; vector of features that enter the saved discount models used in UI
#' @param countyFeatures vector; vector of features that are measured at county level that could be 
#'  used for new customer prediction based on fips code
#' @param cust_df object; historical customer product level records
#' @param avgPrice numeric; avg price of customer's portfolio after addition of new product
#' @param totalQuantity numeric; total quantity of customer's portfolio after addition of new product
#' @param seedsmanIDColumn string; name of column that tracks numeric seedsman identifier in cust_df
#' @param seedsmanID numeric; seedsman identifier
#' @param fipsCodeColumn string; name of column that tracks fips code in cust_df
#' @return obs_df, dataframe containing all necesscary customer information to generate predictions
#' 
process_cart_new_cust <- function(product,
                                  quantity,
                                  fips_code, 
                                  quantityColumn, 
                                  priceColumn,
                                  productColumn,
                                  features,
                                  countyFeatures,
                                  cust_df,
                                  avgPrice,
                                  totalQuantity,
                                  seedsmanIDColumn,
                                  seedsmanID,
                                  fipsCodeColumn) {
  df <- data.table()
  df[, "Product"] <- product
  df[, "Quantity"] <- quantity
  
  # copy to manipulate
  obs_df <- copy(df)
  
  # try to get features associated with fips code
  check_fips <- data.frame(cust_df[cust_df[[fipsCodeColumn]] == as.numeric(fips_code), countyFeatures])
  check_fips <- check_fips[,colSums(is.na(check_fips))<nrow(check_fips)]
  if (all(is.na(check_fips)) == FALSE & nrow(check_fips) != 0) {
    out <- aggregate(.~check_fips[[fipsCodeColumn]], data=check_fips, FUN=mean, na.rm=TRUE)
    out[[fipsCodeColumn]] <- NULL
    names(out)[names(out) == "check_fips[[fipsCodeColumn]]"] <- fipsCodeColumn
    obs_df <- cbind(obs_df, out)
  }
  
  # get list price for each product
  UnitPrices <- unlist(lapply(obs_df$Product, function(i) products_df[products_df$Product == as.character(i), 'Price']))
  obs_df[, priceColumn] <- as.numeric(UnitPrices)
  
  # calculate relevant order level stats
  obs_df[, totalQColumn] <- totalQuantity
  obs_df[, avgPriceColumn] <- avgPrice
  
  # most recvent iteration of model does not take these as predictors 
  obs_df[, 'Product'] <- NULL
  obs_df[, 'Quantity'] <- NULL
  
  # get correct seedsman experience
  seedsmanDF <- cust_df[cust_df[[seedsmanIDColumn]] == seedsmanID, ]
  seedsmanExp <- max(cust_df$YearsOfService, na.rm=TRUE)
  obs_df[, 'YearsOfService']<- seedsmanExp
  
  # make features not observed for new customer NA 
  empty_cols <- features[which(!features %in% names(obs_df))]
  len_empty_cols <- length(empty_cols)
  if(len_empty_cols > 0) {
    obs_df[, empty_cols] <- NA
  }
  
  obs_df
}


#' Generates a vector of boostrapped discount predictions for an existing customer
#'  and single product for corn 
#' 
#' @title predict_discount_raw
#' @param product string; name of product discount prediction is for
#' @param quantity numeric; quantity of product discount is for
#' @param cust_id numeric; customer identifier
#' @param cust_df object; dataframe containing historical customer product records
#' @param logTransformDiscount bool; should predictions be transformed from log form before display
#'  in UI
#' @param quantityColumn string; name of column that tracks quantity in cust_df
#' @param priceColumn string; name of column that tracks price in cust_df
#' @param productColumn string; name of colum that tracks product name in cust_df
#' @param custIDColumn string; name of column that tracks numeric customer_id in cust_df
#' @param cartDropCols vector of strings; raw column names of input features that are determined 
#'  by user input and not historical customer records
#' @param features vector; vector of features that enter the saved discount models used in UI
#' @param discountModel object; list of boostrapped models to use to generate discount predictions
#' @param pllel bool; should predictions be done in parallel
#' @param avgPrice numeric; average retail price of customer order
#' @param totalQuantity numeric; total quantity in customer order
#' @param yearColumn string; name of column that tracks year in cust_df
#' @param seedsmanIDColumn string; name of column that tracks numeric seedsman identifier in cust_df
#' @param seedsmanID numeric; seedsman identifier
#' @param countyFeatures vector; vector of strings identifying features that are tied to the customer's fips code
#' @param fipsCodeColumn string; name of column that tracks fips code in cust_df
#' @return discounts, vector; vector of boostrapped unit discount predictions 
#' 
predict_discount_raw <- function(product,
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
                                 fipsCodeColumn) {
  
  # use 'process_cart' to create test data set for prediction
  test_df <- process_cart(product,
                          quantity,
                          cust_id, 
                          cust_df, 
                          quantityColumn, 
                          priceColumn,
                          productColumn,
                          custIDColumn,
                          cartDropCols,
                          features,
                          avgPrice,
                          totalQuantity,
                          yearColumn,
                          seedsmanIDColumn,
                          seedsmanID,
                          countyFeatures,
                          fipsCodeColumn)
  # predict using that data
  discounts <- predictXGBoost(discountModel, 
                              test_df, 
                              features,
                              logTransform = logTransformDiscount,
                              pllel = pllel)
  # return vector of bootstrapped predictions
  discounts
}


#' Generates effective price predictions for an existing
#'  customer and associated portfolio for corn 
#' 
#' @title predict_ep
#' @param new_port object; updated customer portfolio of products and quantities after selection 
#'  of most recent product
#' @param cust_id numeric; customer identifier
#' @param EffPrice object; historical customer basket level records
#' @param logTransformEP bool; should predictions be transformed from log form before display
#'  in UI
#' @param epCustIDColumn string; name of column that tracks numeric customer id in effective price data
#' @param epYearColumn string; name of column that tracks purchase year in effective price data
#' @param epCartDropCols vector of strings; raw column names of input features that are determined 
#'  by user input and not historical effective price records
#' @param features_effP vector; vector of features that enter the saved effective price 
#' models used in UI
#' @param epAvgPriceColumn string; name of column that tracks retail price in effective price basket level data
#' @param epTotalQColumn string; name of column that tracks order total quantity in effective price basket level data
#' @param epModel object; model used to make effective price predictions
#' @param marketYear numeric; current year
#' @param epSeedsmanIDColumn string; name of column that tracks numeric seedsman identifier in EffPrice
#' @param seedsmanID numeric; seedsman identifier
#' @param epCountyFeatures vector; vector of strings identifying features tied to the customer's fips code in EffPrice
#' @param epFipsCodeColumn string; name of column tracking fips code in EffPrice
#' @return prices, vector of effective price predictions for all products in new_port 
#' 
predict_ep <- function(new_port,
                       cust_id, 
                       EffPrice, 
                       logTransformEP,
                       epCustIDColumn,
                       epYearColumn,
                       epCartDropCols,
                       features_effP,
                       epAvgPriceColumn,
                       epTotalQColumn,
                       epModel,
                       marketYear,
                       epSeedsmanIDColumn,
                       seedsmanID,
                       epCountyFeatures,
                       epFipsCodeColumn) {

  # use 'process_cart' to create test data set for prediction
  test_df <- process_cart_ep(new_port,
                             cust_id, 
                             EffPrice, 
                             epCustIDColumn,
                             epYearColumn,
                             epCartDropCols,
                             features_effP,
                             epAvgPriceColumn,
                             epTotalQColumn,
                             marketYear,
                             epSeedsmanIDColumn,
                             seedsmanID,
                             epCountyFeatures,
                             epFipsCodeColumn)
  
  # predict using that data with saved models
  model <- epModel
  prices <- predict(model, data.matrix(test_df))
  
  
  # convert basket level predictions to product level predictions
  ConversiontoProduct <- cbind(new_port, prices)
  ConversiontoProduct[, PercentageofBasket := Quantity / sum(Quantity)]
  UnitPrices <- unlist(lapply(ConversiontoProduct$Product, 
                              function(i) products_df[products_df$Product == as.character(i), 'Price']))
  ConversiontoProduct[, 'UnitPrice'] <- as.numeric(UnitPrices)
  ConversiontoProduct$alpha <- ConversiontoProduct$PercentageofBasket*ConversiontoProduct$UnitPrice
  ConversiontoProduct[, Percent := prices / sum(alpha)]
  ConversiontoProduct$PredictedProductEffectivePrice <- ConversiontoProduct$Percent * ConversiontoProduct$UnitPrice
  
  ConversiontoProduct
}


#' Generates a vector of boostrapped discount predictions for a new customer
#'  and single product for corn
#' 
#' @title predict_discount_new_cust_raw
#' @param product string; name of product discount prediction is for
#' @param quantity numeric; quantity of product discount is for
#' @param fips_code numeric; fips associated with customer county
#' @param logTransformDiscount bool; should predictions be transformed from log form before display in UI
#' @param quantityColumn string; name of column that tracks product quantity in cust_df 
#' @param priceColumn string; name of column that tracks product price in cust_df
#' @param productColumn string; name of colum that tracks product name in cust_df
#' @param features vector; vector of features that enter the saved discount models used in UI
#' @param countyFeatures vector; vector of features that are measured at county level that could be 
#'  used for new customer prediction based on zip code
#' @param cust_df object; dataframe containing historical customer product records
#' @param discountModel object; list of boostrapped models to use to generate discount predictions
#' @param pllel bool; should predictions be done in parallel 
#' @param avgPrice numeric; avg price of customer's portfolio after addition of new product
#' @param totalQuantity numeric; total quantity of customer's portfolio after addition of new product 
#' @param seedsmanIDColumn string; name of colum that tracks seedsman identifier in cust_df
#' @param seedsmanID numeric; seedsman identifier
#' @param fipsCodeColumn string; name of column that tracks fips code in cust_df
#' @return discounts, vector of bootstrapped unit discount predictions
#' 
predict_discount_new_cust_raw <- function(product,
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
                                          fipsCodeColumn) {
  
  test_df <- process_cart_new_cust(product,
                                   quantity,
                                   fips_code, 
                                   quantityColumn, 
                                   priceColumn,
                                   productColumn,
                                   features,
                                   countyFeatures,
                                   cust_df,
                                   avgPrice,
                                   totalQuantity,
                                   seedsmanIDColumn,
                                   seedsmanID,
                                   fipsCodeColumn)
  
  discounts <- predictXGBoost(discountModel,
                              test_df, 
                              features, 
                              logTransform = logTransformDiscount,
                              pllel=pllel)
  
  discounts
}


#' Generates a vector of effective price predictions for a new customer
#'  and associated portfolio
#' 
#' @title predict_ep_new_cust
#' @param new_port object; updated customer portfolio of products and quantities after selection of most recent product
#' @param fips_code string; fips associated with customer county
#' @param logTransformEP bool; should predictions be transformed from log form before display
#'  in UI
#' @param features_effP vector; vector of features that enter the saved effective price 
#'  models used in UI
#' @param EffPrice object; historical customer basket level records
#' @param epAvgPriceColumn string; name of column that tracks retail price in EffPrice
#' @param epTotalQColumn string; name of column that tracks order total quantity in EffPrice
#' @param countyFeatures vector; vector of features that are measured at county level that could be 
#'  used for new customer prediction based on fips code
#' @param epModel object; model used to make effective price predictions
#' @param marketYear numeric; current market year
#' @param epSeedsmanIDColumn string; name of column that tracks seedsman id in EffPrice
#' @param seedsmanID numeric; seedsman identifier
#' @return prices, vector of effective price predictions for all products in new_port 
#'
predict_ep_new_cust <- function(new_port,
                                fips_code, 
                                logTransformEP,
                                features_effP,
                                EffPrice,
                                epAvgPriceColumn,
                                epTotalQColumn,
                                epFipsCodeColumn,
                                epCountyFeatures,
                                epModel,
                                marketYear,
                                epSeedsmanIDColumn,
                                seedsmanID) {

  test_df <- process_cart_ep_new_cust(new_port,
                                      EffPrice, 
                                      fips_code, 
                                      features_effP,
                                      epAvgPriceColumn,
                                      epTotalQColumn,
                                      epFipsCodeColumn,
                                      epCountyFeatures,
                                      marketYear,
                                      epSeedsmanIDColumn,
                                      seedsmanID)
  
  # predict using that data with saved models
  model <- epModel
  prices <- predict(model, data.matrix(test_df))
  
  # Convert basket level predictions to product level predictions
  ConversiontoProduct <- cbind(new_port, prices)
  ConversiontoProduct[, PercentageofBasket := Quantity / sum(Quantity)]
  UnitPrices <- unlist(lapply(ConversiontoProduct$Product, 
                              function(i) products_df[products_df$Product == as.character(i), 'Price']))
  ConversiontoProduct[, 'UnitPrice'] <- as.numeric(UnitPrices)
  ConversiontoProduct$alpha <- ConversiontoProduct$PercentageofBasket*ConversiontoProduct$UnitPrice
  ConversiontoProduct[, Percent := prices / sum(alpha)]
  ConversiontoProduct$PredictedProductEffectivePrice <- ConversiontoProduct$Percent * ConversiontoProduct$UnitPrice
  
  ConversiontoProduct
}


#' Gets the previous unit discount for an existing customer and product
#' 
#' @title get_last_unit_discount
#' @param cart_df object; datatable of customer's current portfolio
#' @param cust_id numeric; customer identifier
#' @param cust_df object; dataframe containing historical customer product records
#' @param custIDColumn string; name of column that tracks numeric customer id in cust_df
#' @param productColumn string; name of column that tracks product name in cust_df
#' @param yearColumn string; name of column that tracks year in cust_df
#' @param discountColumn string; name of column that tracks per-unit discount in cust_df
#' @param seedsmanID numeric; seedsman identifier
#' @param seedsmanIDColumn string; name of column that tracks seedsman identifier in cust_df
#' @return last_pud, object; customer's last observed unit discount if product previously purchased, NA if not
#' 
get_last_unit_discount <- function(cart_df, 
                                   cust_id, 
                                   cust_df, 
                                   custIDColumn, 
                                   productColumn,
                                   yearColumn, 
                                   discountColumn,
                                   seedsmanID,
                                   seedsmanIDColumn,
                                   priceColumn) {
  # get existing customer information from records
  tmp_cust <- cust_df[cust_df[[custIDColumn]] == cust_id & cust_df[[seedsmanIDColumn]] == seedsmanID, ]
  # for each product in customer's cart
  for (i in 1:nrow(cart_df)) {
    # check for last unit discount
    pud_df <- data.frame(tmp_cust[tmp_cust[[productColumn]] == cart_df$Product[i], 
                                  c(yearColumn, discountColumn, priceColumn)])
    # set order by year
    pud_df <- pud_df[order(-pud_df[[yearColumn]]), ]
    
    # if there is no last unit discount, make value NA
    if (nrow(pud_df) == 0 | all(is.na(pud_df))) {
      last_pud <- as.character("NA")
    } else {  # if there is, round to 2 decimal places and paste $ for display in UI
      # take first row (most recent date)
      last_pud <- pud_df[, discountColumn][1]
      last_pud <- paste0('$', format(round(last_pud, 2), nsmall=2))
    }
  }
  last_pud
}

#' Gets the previous unit discount for an existing customer and product
#' 
#' @title get_last_unit_discount_pct
#' @param cart_df object; datatable of customer's current portfolio
#' @param cust_id numeric; customer identifier
#' @param cust_df object; dataframe containing historical customer product records
#' @param custIDColumn string; name of column that tracks numeric customer id in cust_df
#' @param productColumn string; name of column that tracks product name in cust_df
#' @param yearColumn string; name of column that tracks year in cust_df
#' @param discountColumn string; name of column that tracks per-unit discount in cust_df
#' @param priceColumn string; name of column that tracks list price in cust_df
#' @param seedsmanID numeric; seedsman identifier
#' @param seedsmanIDColumn string; name of column that tracks seedsman identifier in cust_df
#' @return last_pud, object; customer's last observed unit discount if product previously purchased, NA if not
#' 
get_last_unit_discount_pct <- function(cart_df, 
                                       cust_id, 
                                       cust_df, 
                                       custIDColumn, 
                                       productColumn,
                                       yearColumn, 
                                       discountColumn,
                                       seedsmanID,
                                       seedsmanIDColumn,
                                       priceColumn) {
  # get existing customer information from records
  tmp_cust <- cust_df[cust_df[[custIDColumn]] == cust_id & cust_df[[seedsmanIDColumn]] == seedsmanID, ]
  # for each product in customer's cart
  for (i in 1:nrow(cart_df)) {
    # check for last unit discount
    pud_df <- data.frame(tmp_cust[tmp_cust[[productColumn]] == cart_df$Product[i], 
                                  c(yearColumn, discountColumn, priceColumn)])
    # set order by year
    pud_df <- pud_df[order(-pud_df[[yearColumn]]), ]
    
    # if there is no last unit discount, make value NA
    if (nrow(pud_df) == 0 | all(is.na(pud_df))) {
      last_pud_pct <- as.character("NA")
    } else {  # if there is, round to 2 decimal places and paste % for display in UI
      # take first row (most recent date)
      last_pud_pct <- (pud_df[, discountColumn][1]/pud_df[, priceColumn][1]) * 100
      last_pud_pct <- paste0(format(round(last_pud_pct, 2), nsmall=2), '%')
    }
  }
  last_pud_pct
}


#' Gets the previous effective price for an existing customer and product
#' 
#' @title get_last_unit_ep
#' @param cart_df object; datatable of customer's current portfolio
#' @param cust_id numeric; customer identifier
#' @param EffPrice object; historical customer basket level records
#' @param epCustIDColumn string; name of column that tracks numeric customer id in EffPrice
#' @param epYearColumn string; name of column that tracks year in EffPrice
#' @param effectivePriceColumn string; name of column that tracks effective price in EffPrice
#' @param epSeedsmanIDColumn string; name of column that tracks numeric seedsman id in EffPrice
#' @param seedsmanID numeric; seedsman identifier 
#' @return last_ep, object; customer's last observed effective price if product previously purchased, NA if not
#' 
get_last_unit_ep <- function(cart_df, 
                             cust_id, 
                             EffPrice, 
                             epCustIDColumn, 
                             epYearColumn, 
                             effectivePriceColumn,
                             epSeedsmanIDColumn,
                             seedsmanID) {
  
  # get existing customer information from historical records
  EffPrice <- setDF(EffPrice)
  tmp_cust <- EffPrice[EffPrice[[epCustIDColumn]]== cust_id & EffPrice[[epSeedsmanIDColumn]] == seedsmanID, ]
  ep_df <- tmp_cust[order(-tmp_cust[[epYearColumn]]), ]
  # if no records exist, make value NA
  if (nrow(ep_df) == 0 | all(is.na(ep_df))) {
    last_ep <- as.character("NA")
    
  } else { # get correct ep (most recent) and format 
    last_ep <- ep_df[1, effectivePriceColumn]
    last_ep <- paste0('$', format(round(last_ep, 2), nsmall=2))
  }
  
  last_ep

}


#' Gets the previous quantity purchased for an existing customer and product
#' 
#' @title get_last_quantity
#' @param cart_df object; datatable of customer's current portfolio
#' @param cust_id numeric; customer identifier
#' @param cust_df object; dataframe containing historical customer information
#' @param custIDColumn string; name of column that tracks numeric customer id in cust_df
#' @param productColumn string; name of column that tracks product name in cust_df
#' @param yearColumn string; name of column that tracks year in cust_df
#' @param quantityColumn string; name of column that tracks quantity in cust_df
#' @return last_q, object; customer's last observed quantity if product previously purchased, NA if not purchased
#' 
get_last_quantity <- function(cart_df, 
                              cust_id, 
                              cust_df, 
                              custIDColumn, 
                              productColumn, 
                              yearColumn,
                              quantityColumn) {
  # get existing customer information from historical records
  tmp_cust <- cust_df[cust_df[[custIDColumn]] == cust_id, ]
  
  # for each item in cart
  for (i in 1:nrow(cart_df)) {
    
    # check for last purchased quantity
    q_df <- data.frame(tmp_cust[tmp_cust[[productColumn]] == cart_df$Product[i], 
                                c(yearColumn, quantityColumn)])
    
    # order by date
    q_df <- q_df[order(-q_df[[yearColumn]]), ]
    
    # if doesn't exist, make value NA
    if (nrow(q_df) == 0 | all(is.na(q_df))){
      last_q <- as.character("NA")
    } else {  # if does exist, round and display in UI
      # take first row, df is ordered by year
      last_q <- q_df[, quantityColumn][1]
      last_q <- format(as.character(as.integer(last_q), nsmall=2))
    }
  }
  last_q
}


#' Takes user input for state and county, then finds fips code associated with respective input
#' 
#' @title get_fips
#' @param state string; user inputted state
#' @param county string; user inputted county
#' @param stateCountyToFips object; crosswalk of state and county to fips codes
#' @return numeric fips code of county/state combination
#' 
get_fips <- function(state, county, stateCountyToFips) {
  fips <- as.numeric(stateCountyToFips[stateCountyToFips$statename == state &
                                                stateCountyToFips$County == county, 'fipsco'])
  fips
}


#' Extracts feature importance for a given model to display in UI; e.g. effective price or discount 
#' 
#' @title get_feature_importance
#' @param model object; model to get features for 
#' @param n numeric; how many features to extract
#' @return xgb_imp, object; dataframe of n important features plus empty column to add description
#' 
get_feature_importance <- function(model, n) {
  # get saved model
  model <- model 
  
  # extract feature importance from loaded model
  xgb_imp <- xgb.importance(feature_names = model$feature_names,
                            model = model)
  
  # subset feature importance to columns and rows we want
  xgb_imp <- xgb_imp[, c('Feature')][1:n]
  
  # rename columns for display in UI
  names(xgb_imp) <- c('Feature Name')
  
  # create blank column to enter feature descriptions later
  xgb_imp[, 'Description'] <- as.character("")
  
  xgb_imp
}


#' Gets customer characteristics to be displayed in UI 
#' 
#' @title get_grower_characs
#' @param cust_id numeric; customer identifier
#' @param cust_df object; dataframe containing historical customer information
#' @param custFeats vector; vector of features to obtain regarding customer
#' @param custFeatNames vector; names to rename features as
#' @param custFeatKeys vector; vector of common keys to use between custFeats and custFeatNames
#' @param sharedFarms object, dataframe containing customer ids that are associated with a shared farm
#' @param custIDColumn string; name of column containing numeric customer id in cust_df
#' @param yearColumn string; name of column that tracks year in cust_df
#' @param custFeatOrder vector; order of features to display in grower characteristics table 
#' @return df_out, dataframe containing formatted customer information
#' 
get_grower_characs <- function(cust_id, 
                               cust_df, 
                               custFeats, 
                               custFeatNames,
                               custFeatKeys,
                               sharedFarms,
                               custIDColumn,
                               yearColumn,
                               custFeatOrder) {
  
  # get data.table of customer specific features we want to display
  df <- data.table(cust_df[cust_df[[custIDColumn]] == cust_id, append(custFeats, yearColumn)])
  
  # order by year
  df <- df[order(-df[[yearColumn]])]
  
  # create data.frame to fill and export
  df_out <- data.frame(matrix(ncol=length(custFeatNames)))
  
  # rename that data.frame's columns with names we want to display in UI
  names(df_out) <- custFeatNames
  
  # add common keys to both raw and formatted feature names
  names(custFeats) <- custFeatKeys
  names(custFeatNames) <- custFeatKeys
  
  # fill data.frame with correct features (each feature is indexed by common key defined in custFeatKeys)
  # years of relationship # CURRENTLY DEPRECATED BECAUSE WE DON'T HAVE THIS DATA
  #df_out[, unname(custFeatNames["relationshipYears"])] <- as.integer(max(df[, df[[unname(custFeats["relationshipYears"])]]], 
                                                                         #na.rm=TRUE) + 1)
  # total crop area
  df_out[, unname(custFeatNames["totalCropArea"])] <- as.integer(df[, df[[unname(custFeats["totalCropArea"])]]])[1]
  
  # get opportunity acres, make into whole percentage
  df_out[, unname(custFeatNames["oppAcres"])] <- as.integer(as.numeric((df[, df[[unname(custFeats["oppAcres"])]]])*100)[1])
  # if not NA add percentage sign
  if (!is.na(df_out[,  unname(custFeatNames["oppAcres"])])) {
    df_out[,  unname(custFeatNames["oppAcres"])] <- paste0(df_out[,  unname(custFeatNames["oppAcres"])], 
                                                               '%')
  }
  
  # get/format top producer column # CURRENTLY DEPRECATED BECAUSE WE DON'T HAVE THIS DATA
  #df_out[,  unname(custFeatNames["topProducer"])] <- df[, df[[unname(custFeats["topProducer"])]]][1]
  #df_out[, unname(custFeatNames["topProducer"])] <- ifelse(df_out[, unname(custFeatNames["topProducer"])] == 1, 
                                     #"Yes", 
                                     #"No")
  # flag for whether or not customer operates a shared farm -- doesn't have key because created in this function
  # extract cust_id's that exist in sharedFarms
  sharedFarms <- sharedFarms[match(cust_df[[custIDColumn]], sharedFarms$CUST_ID), ]
  sharedFarms <- sharedFarms[complete.cases(sharedFarms), ]
  
  # check if specific cust_id exists in that subset
  shared_farm_flag <- sharedFarms[sharedFarms$CUST_ID == cust_id, ]
  
  # if no, shared_flag = 'No'
  if (nrow(shared_farm_flag) == 0) {
    df_out[, 'Shared Farm'] <- 'No'
    
  } else {  # if it does, shared_flag = 'Yes'
    df_out[, 'Shared Farm'] <- 'Yes'
  }
  # return formatted dataframe with columns we want to display in UI
  df_out <- df_out[, custFeatOrder]
  
  df_out
}


#' Gets customer purchase history to be displayed in UI 
#' 
#' @title get_customer_products
#' @param cust_id numeric; customer identifier
#' @param cust_df object; dataframe containing historical customer information
#' @param products_df object; dataframe containing products and prices 
#' @param custIDColumn string; name of column that tracks numeric customer id in cust_df
#' @param custPurchaseNames vector; raw names of columns to include in purchase history table
#' @param formattedPurchaseNames vector; formatted names of columns to include in purchase history table
#' @param custPurchaseKeys vector; common keys to use between custPurchaseNames and formattedPurchaseNames
#' @param orderPurchaseNames vector; order of formatted columns to display in purchase history table
#' @param EffPrice object; historical customer basket level records
#' @param seedsmanIDColumn string; name of column that tracks seedsman identifier in cust_df
#' @param seedsmanID numeric; seedsman identifier
#' @param yearColumn string; name of column that tracks year in cust_df
#' @param yearView bool; should table be displayed as all past purchases or annual averages 
#' @return df_out, dataframe containing formatted customer purchase history
#' 
get_customer_products <- function(cust_id, 
                                  cust_df, 
                                  product_df, 
                                  custIDColumn,
                                  custPurchaseNames, 
                                  formattedPurchaseNames,
                                  custPurchaseKeys,
                                  orderPurchaseNames,
                                  EffPrice,
                                  seedsmanIDColumn,
                                  seedsmanID,
                                  yearColumn,
                                  yearView = FALSE) {
  
  # get customer specific purchase history to display in UI
  product_df <- data.table(cust_df[cust_df[[custIDColumn]] == cust_id & cust_df[[seedsmanIDColumn]] == seedsmanID, 
                               c(custPurchaseNames)])
  product_df <- product_df[!is.na(product_df[[yearColumn]]), ]
  
  if (yearView == TRUE & nrow(product_df > 0)){
    year_q <- aggregate(PRODUCT_QTY ~ product_df[[yearColumn]], data = product_df, FUN=sum, na.action = na.pass)
    year_fgp <- aggregate(AVG_FARMGATE_PRICE ~ product_df[[yearColumn]], data = product_df, FUN=mean, na.action = na.pass)
    year_avg_rp <- aggregate(AVGRETAILPRICE ~ product_df[[yearColumn]], data = product_df, FUN=mean, na.action = na.pass)
    year_avg_discount <- aggregate(PerUnitDiscount ~ product_df[[yearColumn]], data = product_df, FUN=mean, na.action = na.pass)
    df_out <- suppressMessages(left_join(year_q, year_fgp))
    df_out <- suppressMessages(left_join(df_out, year_avg_rp))
    df_out <- suppressMessages(left_join(df_out, year_avg_discount))
    
    # TODO: parameterize?
    # sort by year
    df_out <- df_out[order(-df_out$`product_df[[yearColumn]]`), ]
    df_out[, 'pctListPrice'] <- (df_out$PerUnitDiscount / df_out$AVGRETAILPRICE) * 100
    # format
    df_out[, 'pctListPrice'] <- paste0(format(round(df_out$pctListPrice, 2), nsmall=2), '%')
    df_out[, 'product_df[[yearColumn]]'] <- as.integer(df_out$`product_df[[yearColumn]]`)
    df_out[, 'PRODUCT_QTY'] <- as.integer(df_out$PRODUCT_QTY)
    df_out[, 'PerUnitDiscount'] <- paste0("$", format(round(df_out$PerUnitDiscount, 2), nsmall=2))
    df_out[, 'AVGRETAILPRICE'] <- paste0("$", format(round(df_out$AVGRETAILPRICE, 2), nsmall=2))
    df_out[, 'AVG_FARMGATE_PRICE'] <- paste0("$", format(round(df_out$AVG_FARMGATE_PRICE, 2), nsmall=2))
    
    # rename columns
    names(df_out) <- c("Year of Purchase", 
                      "Total Quantity Purchased",
                      "Avg. Farm Gate Price",
                      "Avg. Purchase List Price",
                      "Avg. LCR Unit",
                      "Avg. LCR %")
    df_out <- df_out[, c('Year of Purchase', 'Total Quantity Purchased', 'Avg. Purchase List Price', 
                         'Avg. LCR Unit', 'Avg. LCR %', 'Avg. Farm Gate Price')]
    # temp fix for 2020 data mismatch
    #df_out[df_out$`Year of Purchase` == 2020, 'Avg. Unit Farmgate Price'] <- as.character("NA")
    #df_out[as.character(df_out$`Avg. Unit Farmgate Price`) == "$    NA", 'Avg. Unit Farmgate Price' ] <- as.character("NA")
    
    if(nrow(df_out) == 0){df_out <- data.frame('Year of Purchase' = " ", 'Total Quantity Purchased' = " ",
                                                'Avg. Purchase List Price' = " ", 'Avg. LCR Unit' = " ",
                                                'Avg. LCR %' = " ", 'Avg. Farm Gate Price' = " ")}
    return(df_out)
  } else {
    # rename columns for display in UI
    names(product_df) <- formattedPurchaseNames
  
    # add common keys
    names(formattedPurchaseNames) <- custPurchaseKeys
  
    # order by purchase date
    sortCol <- unname(formattedPurchaseNames["purchaseYear"])
    df_out <- product_df[order(-product_df[[sortCol]]), ]

    # format and extract values
    df_out[, unname(formattedPurchaseNames["purchaseYear"])] <- as.integer(df_out[, 
                                                                                df_out[[unname(formattedPurchaseNames["purchaseYear"])]]])
    df_out[, unname(formattedPurchaseNames["quantityPurchased"])] <- as.integer(df_out[, 
                                                                                     df_out[[unname(formattedPurchaseNames["quantityPurchased"])]]])
    df_out[, 'LCR %'] <- paste0(format(round((df_out[, 
                                                       df_out[[unname(formattedPurchaseNames["perUnitLCR"])]]]/df_out[, 
                                                                                                                      df_out[[unname(formattedPurchaseNames["listPrice"])]]]) * 100, 2), nsmall=2), '%')
    df_out[, unname(formattedPurchaseNames["listPrice"])] <- paste0('$', format(as.numeric(df_out[, 
                                                                   df_out[[unname(formattedPurchaseNames["listPrice"])]]]), nsmall=2))
    df_out[, unname(formattedPurchaseNames["perUnitLCR"])] <- paste0('$', format(round(as.numeric(df_out[, 
                                                                              df_out[[unname(formattedPurchaseNames["perUnitLCR"])]]]), 2), nsmall=2))
  # make implied farmgate price from list price for each year in customer purchase history
    for (i in 1:length(unique(df_out$`Year of Purchase`))) {
      tmp_year <- df_out[df_out$`Year of Purchase` == unique(df_out$`Year of Purchase`)[i], ]
      tmp_year[, PercentageofBasket := `Quantity Purchased` / sum(`Quantity Purchased`)]
      tmp_year$alpha <- tmp_year$PercentageofBasket*as.numeric(gsub("[\\$,]", "", tmp_year$`Purchase List Price`))
      tmp_year[, Percent := `Unit Farm Gate Price*`/ sum(alpha)]
      df_out[df_out$`Year of Purchase`== tmp_year$`Year of Purchase`[1], 
           'Unit Farm Gate Price*'] <- round(tmp_year$Percent * as.numeric(gsub("[\\$,]", "", tmp_year$`Purchase List Price`)), 2)
      }
  
    # format
    df_out$`Unit Farm Gate Price*` <- paste0('$', format(df_out$`Unit Farm Gate Price*`, nsmall=2))
  
    # return columns we want to display in UI
    df_out <- df_out[, orderPurchaseNames, with=FALSE]
  
    # remove rows that are duplicates in case something is going on with the data
    df_out <- df_out[!duplicated(df_out), ]


    if(sum(df_out$`Quantity Purchased`) == 0){df_out <- df_out[FALSE,]} #if no sales history, return empty dataframe  
    df_out
  }
}


#' Get rest of features not listed as 'Top 5' in UI
#' 
#' @title get_rest_of_features
#' @param model object; model to predict with 
#' @param n numeric; where to begin feature extraction
#' @return xgb_imp, object; dataframe of nrow n_features-n, important features plus column to add description
#' 
get_rest_of_features <- function(model, n) {
  # get saved model
  model <- model
  
  # extract feature importance
  xgb_imp <- xgb.importance(feature_names = model$feature_names,
                            model = model)
  # take columns and rows we want
  xgb_imp <- xgb_imp[, c('Feature')][n+1:nrow(xgb_imp)]
  
  # rename columns for display in UI
  names(xgb_imp) <- c('Feature Name')
  
  # create blank description column for use later
  xgb_imp[, 'Description'] <- as.character("")
  
  xgb_imp[complete.cases(xgb_imp)]
}


#' Populates and formats feature importance tables in UI
#'  
#' @title describe_features
#' @param feat_imp object; dataframe of feature importances i.e. output from functions
#'  get_feature_importance or get_rest_of_features
#' @param featDict object; dataframe which maps formatted names and descriptions from raw feature names, 
#'  currently this is file 'featureDict.csv'
#' @returns feat_imp, obejct; dataframe of feature importances with formatted names and descriptions
#'  
describe_features <- function(feat_imp, featDict) {
  
  #TODO: Look into merge
  for (i in 1:nrow(feat_imp)){
    formatted_name <- featDict[featDict$feat_name_raw == as.character(feat_imp$'Feature Name'[i]), 
                                'feature_name_formatted']
    feat_desc <- featDict[featDict$feat_name_raw == as.character(feat_imp$'Feature Name'[i]), 
                           'description']
    feat_imp[i, 'Feature Name'] <- formatted_name
    feat_imp[i, 'Description'] <- feat_desc
  }
  feat_imp
}


#' Modified version of Julie's function to generate reports of model 
#'  results for historical per-unit discount and effective price to be exported from UI
#' 
#' @title generate_report_raw
#' @param discountPointModel object; saved model to use to generate discount point predictions
#' @param cust_df object; dataframe of historical customer product level information
#' @param reportAggCols vector; raw names of columns to include in report output
#' @param reportAggCleanNames vector; formatted names of columns to include in report output
#' @param reportAggColOrder vector; order of columns to display in report output
#' @param epModel object; model to make point estimate projections for basket effective price
#' @param features_effP vector; vector of features that enter the saved effective price 
#'  models used in UI
#' @param EffPrice object; historical customer basket level records 
#' @param epReportAggCols vector; raw columns to include in historical grower report from effective
#'  price basket level data
#' @param priceColumn string; name of column that tracks price in cust_df
#' @param quantityColumn string; name of column that tracks quantity in cust_df
#' @returns projections_out, object; report to be dowloaded/exported 
#' 
generate_report_raw <- function(discountPointModel, 
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
                                quantityColumn) {
  
  xgboostmodel <- discountPointModel
  projections <- predict(xgboostmodel, data.matrix(cust_df[,features]))

  #TODO: Probably need to parameterize this functionality
  projections <- exp(projections)
  
  projections <- data.frame(cbind(cust_df[ , reportAggCols], projections))
  
  #The capped offer looks at the historical discount and the projected discount
  #Then it keeps whichever one is lower
  projections$CappedOffer <- ifelse(projections$PerUnitDiscount > projections$projections, 
                                    projections$projections, 
                                    projections$PerUnitDiscount)
  
  projections$RetailTotal <- projections[[priceColumn]] * projections[[quantityColumn]]
  
  projections$obsTotalDiscounts <- projections$PerUnitDiscount * projections[[quantityColumn]]
  projections$predTotalDiscounts <- projections$projections * projections[[quantityColumn]]
  projections$cappedTotalDiscounts <- projections$CappedOffer * projections[[quantityColumn]]
  
  epModel <- epModel
  projectionsEP <- predict(epModel, data.matrix(EffPrice[, features_effP]))
  
  projectionsEP <- data.frame(cbind(EffPrice[ , epReportAggCols], projectionsEP))
  
  projections_out <- suppressMessages(left_join(projections, projectionsEP))

  names(projections_out) <- reportAggCleanNames
  
  projections_out <- projections_out[, reportAggColOrder]
  
  projections_out
}


#' Gets value proposition and competitior vs county yields for UI County Competitive Info table
#' RIGHT NOW THIS IS FOR CORN ONLY
#' TODO: Make functionaity work for soy--need to get data
#' 
#' @title get_value_prop_UI
#' @param valueProp object; file valuePropMARSUI.csv -- see file makeValuePropMARSUI.R
#' @param fips_code numeric; fips code of customer's county
#' @param stateCountyToFips object; crosswalk of state and county to fips codes
#' @param compTableNamesChannel vector; formatted columns to use in county competitive information 
#'  table when Channel information is available
#' @param compTableNamesOther vector; formatted columns to use in county competitive information 
#'  table when Channel information is NOT available (i.e. use other Bayer hybrids)
#' @return countyTable, object; dataframe of value proposition associated with the customer's
#' county
#' 
get_value_prop_UI <- function(valueProp, 
                              fips_code, 
                              stateCountyToFips,
                              compTableNamesChannel,
                              compTableNamesOther) {
  
  # seperate to channel and non channel hybrids
  valuePropOther <- valueProp[valueProp$MonBrand != 'CHANNEL', ]
  valuePropChannel <- valueProp[valueProp$MonBrand == 'CHANNEL', ]
  
  # average value proposition for both channel and other
  valuePropTableOther <- aggregate(ValueDif ~ fips + STATE, dat=valuePropOther, mean)
  valuePropTableChannel <- aggregate(ValueDif ~ fips + STATE, dat=valuePropChannel, mean)
  
  # average yields for channel, other, and competitor
  otherYieldTable <- aggregate(MonYield ~ fips + STATE, dat=valuePropOther, mean)
  compYieldTableOther <- aggregate(CompYield ~ fips + STATE, dat=valuePropOther, mean)
  channelYieldTable <- aggregate(MonYield ~ fips + STATE, dat=valuePropChannel, mean)
  compYieldTableChannel <- aggregate(CompYield ~ fips + STATE, dat=valuePropChannel, mean)
  
  # join aggregated chistorical yields
  yieldTableOther <- suppressMessages(left_join(otherYieldTable, compYieldTableOther))
  yieldTableChannel <- suppressMessages(left_join(channelYieldTable, compYieldTableChannel))
  
  # join valueProp table with yieldTable
  outTableOther <- suppressMessages(left_join(yieldTableOther, valuePropTableOther))
  outTableChannel <- suppressMessages(left_join(yieldTableChannel, valuePropTableChannel))
  
  
  # subset table to customer's county
  countyTableOther <- outTableOther[outTableOther$fips == as.numeric(fips_code), ]
  countyTableChannel <- outTableChannel[outTableChannel$fips == as.numeric(fips_code), ]
  
  if(is.null(fips_code) | is.na(fips_code) | length(fips_code) == 0){ #making empty df for before cust_name is selected
    countyTable <- countyTableOther[FALSE,]
    names(countyTable) <- compTableNamesOther
  }else{
  
  # if no info exists on channel hybrids
  if(nrow(countyTableChannel) == 0) {
    # check bayer hybrids
    countyTable <- countyTableOther

    if (nrow(countyTableOther) == 0) {  # if those don't exist, show empty table
      countyTable <- countyTable[FALSE,]
      names(countyTable) <- compTableNamesOther
      
      } else {  # format to show bayer yield instead of channel hybrid yield
        # replace fips column with formatted county name 
        countyTable$fips <- unlist(stateCountyToFips[stateCountyToFips$fipsco == fips_code,
                                                     'County'])
        # format state to title case
        countyTable$STATE <- stringr::str_to_title(countyTable$STATE)
        
        # rename columns with correctly formatted names
        names(countyTable) <- compTableNamesOther
        
        countyTable[, compTableNamesOther[3]] <- as.integer(unlist(countyTable[, compTableNamesOther[3]]))
        countyTable[, compTableNamesOther[4]] <- as.integer(unlist(countyTable[, compTableNamesOther[4]]))
      }
  } else { # if we do have channel value prop info, format and return that information
    
    countyTable <- countyTableChannel
    
    # replace fips column with formatted county name 
    countyTable$fips <- unlist(stateCountyToFips[stateCountyToFips$fipsco == fips_code,
                                                        'County'])
    # format state to title case
    countyTable$STATE <- stringr::str_to_title(countyTable$STATE)
    
    # rename columns with correctly formatted names
    names(countyTable) <- compTableNamesChannel
    
    countyTable[, compTableNamesChannel[3]] <- as.integer(unlist(countyTable[, compTableNamesChannel[3]]))
    countyTable[, compTableNamesChannel[4]] <- as.integer(unlist(countyTable[, compTableNamesChannel[4]]))
  }
  if (nrow(countyTable) == 0) {
    return(countyTable[FALSE,])
  }
  else if (countyTable[1, 5] > 0) {
    countyTable[1, 'Competitive Position'] <- 'STRONG'
  } else if (countyTable[1, 5] < 0) { 
  countyTable[1, 'Competitive Position'] <- 'WEAK'
  }}
  countyTable[1, 5] <- format(round(countyTable[1, 5], 2), nsmall=2)
  countyTable
}


#' Exports customer portfolio to dataframe file
#' 
#' @title export_cart
#' @param cart_df object; datatable of customer's current portfolio
#' @param seedsman_id numeric; seedsman identifier
#' @param deal_lost bool; was deal successful
#' @param comp_offer numeric; competiting avg farmgate price offer 
#' @returns df_out, object; formatted dataframe cart_df for export
#' 
export_cart <- function(cart_df, seedsman_id,
                        deal_lost,
                        comp_offer,
                        lcr_offer,
                        dollar_percent,
                        dollar_percent_table){
  if (nrow(cart_df) == 0) { # if nothing in cart, return empty data.table
    return(data.table())
  } else {
    # take cart
    df_out <- cart_df
    
    # enter seedsman id
    df_out[, 'SeedsmanID'] <- seedsman_id
    
    # drop button id column
    df_out[, 'button_id'] <- NULL
    
    #update offered unit lcr depending if percent is selected
    if(dollar_percent_table == "percent_table") {
      df_out[, 'Price'] <-  as.numeric(gsub("[\\$,]", "", df_out$Price))
      df_out[, 'Offered Unit LCR'] <-  as.numeric(gsub("[\\$,]", "", df_out$'Offered Unit LCR'))
      df_out[,'Offered Unit LCR'] <- format(.01*df_out$'Offered Unit LCR'*df_out$Price, nsmall = 2)
      df_out[,'Price'] = paste0('$', format(df_out[,'Price'],nsmall=2))
      df_out[,'Offered Unit LCR'] = paste0('$', format(df_out[,'Offered Unit LCR'],nsmall=2))
    }
    
    # record whether deal was won or lost
    df_out[, 'DealLost'] <- deal_lost
    
    df_out[, 'CompOffer'] <- paste0('$', comp_offer)
    
    #add logic for handling portfolio-level lcr offer
    if(lcr_offer > 0){
      if(dollar_percent == "percent") {
        df_out[, 'Price'] <-  as.numeric(gsub("[\\$,]", "", df_out$Price))
        lcr_offer = format(.01*lcr_offer*df_out[,"Price"],nsmall = 2)
        df_out[,'Price'] = paste0('$', format(df_out[,'Price'],nsmall=2))
      } else {
        tot_quant = sum(df_out[,'Quantity'])
        lcr_offer = format(lcr_offer/tot_quant, nsmall=2,digits=2)
      }
      df_out[, 'OfferedPortfolioLevelLCR_PerProductUnit'] <- paste0('$', lcr_offer)
    } else {
      df_out[, 'OfferedPortfolioLevelLCR_PerProductUnit'] <- paste0('$', 0)
    }
    
    df_out
  }
}


#' Gets the weighted average per unit disccount for the entire customer order
#' 
#' @name get_weight_unit_discount
#' @param cart_df A data frame containing items added to the cart by users of 
#' the MARS UI.  Column names must include 'Quantity' and 'Discount'
#' @return The weighted average per unit disccount for the order in the cart
#'
get_weight_unit_discount <- function(cart_df) {
  weighted_avg <- weighted.mean(x = as.numeric(gsub("[\\$,]", "", cart_df$Discount)), 
                                w = as.numeric(cart_df$Quantity))
  return(weighted_avg)
}


#' Gets the total disccount for the entire customer order
#' 
#' @name get_total_discount
#' @param cart_df A data frame containing items added to the cart by users of 
#' the MARS UI.  Column names must include 'Quantity' and 'Discount'
#' @return The aggregate total disccount for the order in the cart
#'
get_total_discount <- function(cart_df) {
  total_discount <- sum(as.numeric(cart_df$Quantity) * as.numeric(gsub("[\\$,]", "", cart_df$Discount)))
  return(total_discount)
}


# Gets the retail total for the entire customer order
#' @name get_retail_total
#' @param cart_df A data frame containing items added to the cart by users of 
#' the MARS UI.  Column names must include 'Quantity' and 'Price'
#' @return The aggregate retail total for the order in the cart
#'
get_retail_total <- function(cart_df) {
  retail_total <- sum(as.numeric(cart_df$Quantity) *as.numeric(gsub("[\\$,]", "", cart_df$Price)))
  return(retail_total)
}


# Gets the average effective price for the entire customer order
#' @name get_avg_fg_price
#' @param cart_df A data frame containing items added to the cart by users of 
#' the MARS UI.
#' @return The average farmgate price for the order in the cart
#'
get_avg_fg_price <- function(cart_df) {
  avg_fg_price <- mean(as.numeric(gsub("[\\$,]", "", cart_df$EffectivePrice)))
  return(avg_fg_price)
}

#' Gets the weighted average retail price for the entire customer order
#' 
#' @name get_weight_unit_retail_price
#' @param cart_df A data frame containing items added to the cart by users of 
#' the MARS UI.  Column names must include 'Quantity' and 'Price'
#' @return The weighted average per unit retail price for the order in the cart
#'
get_weight_unit_retail_price <- function(cart_df) {
  weighted_avg <- weighted.mean(x = as.numeric(gsub("[\\$,]", "", cart_df$Price)), 
                                w = as.numeric(cart_df$Quantity))
  return(weighted_avg)
}

#' Renders plots for each get discount button in the customer portfolio
#' 
#' @title make_plot_button
#' @param discounts vector; vector of predicted discounts associated with get discount button
#' @param cart_df object; datatable of customer's current portfolio
#' @param stdN numeric; how many SD on each side of mean to limit discount range on plot
#' @param id numeric; id associated with get discount button
#' @param lastDisc bool; should plot have line for last unit discount
#' @return distPlot, object; plot of distributed discounts
#' 
make_plot_button <- function(discounts, 
                             cart_df, 
                             stdN, 
                             id, 
                             lastDisc=TRUE) {
  
  avg_retail_price <- get_weight_unit_retail_price(cart_df)
  
  # make x to correspond with previously written code
  x <- (discounts / avg_retail_price) * 100

  # calculate some stats needed to correctly fill area under the distribution curve
  mean_x <- mean(x)
  high_sd <- mean(x) + stdN*sd(x)
  low_sd <- mean(x) - stdN*sd(x)
  sd_x <- sd(x)
    
  # create data frame for density plotting and shading
  dens <- density(x)
  dd <- with(dens, data.frame(x, y))
  
  # use created data frame to render plot 
  distPlot <-  qplot(x,y,data=dd,geom="line") + # plot base
      
  # fill discount range
  geom_ribbon(data=subset(dd, x>=low_sd, x<high_sd),aes(ymax=y),ymin=0,
              fill="gold1",colour=NA,alpha=0.7) +
      
  # fill left of discount range
  geom_ribbon(data=subset(dd, x<=low_sd),aes(ymax=y),ymin=0,
              fill="forestgreen",colour=NA,alpha=0.7) + 
  
    # fill right of discount range
  geom_ribbon(data=subset(dd, x>=high_sd),aes(ymax=y),ymin=0,
              fill="red2",colour=NA,alpha=0.7) +
  
    # set correct y limits
  ylim(0, max(dd$y)) +
      
  # Put text for discount range on bottom of plot
  xlab(paste("Prevailing LCR % Range:", 
             paste0( 
                    format(round(mean_x-stdN*sd_x,2), nsmall=2), '%'), "to", paste0( 
                                                                               format(round(mean_x+stdN*sd_x,2), nsmall=2), '%'))) +
      
  # add vertical line at mean discount offer
  geom_vline(aes(xintercept = mean_x),col='grey50', linetype = 'dashed', size=1.25) +
      
  # set vertical lines at edge of recommended discount range
  geom_vline(xintercept = c(mean_x-stdN*sd(x), mean_x+stdN*sd_x), color = "grey50", size=1.25) +
      
  # Add minimial theme to match white background
  theme_minimal(base_size = 16)  +
      
  # Some styling...take labels off y-axis, title each plot by it's product name, 
  # and annotate with text the line for the customer's last unit discount
    theme(
        axis.text.y=element_blank())+ labs(y="") +
      ggtitle(paste(cart_df[button_id == id, 'Product'][1]))
      # if lastDisc=TRUE, put line for last unit discount on plot
      if (lastDisc == TRUE) {
        distPlot <- distPlot +  # add line for last unit discount 
          geom_vline(aes(xintercept=as.numeric(gsub("\\%", "", cart_df[button_id == id, LastUnitDiscountPCT])),
                         color='Actual LCR %'),
                     linetype='dotdash', size=1.25) + 
          scale_color_manual(name = "", values = c(`Actual LCR %` = 'black')) + 
          theme(legend.position='top')
       return(distPlot)
      } else {
      return(distPlot)
      }
}


#' Function that updates the portfolio information table in the UI
#' 
#' @param order_tab object; dataframe of current portfolio information table
#' @param cart_df object; datatable of customer's current portfolio
#' @param cust_df object; historical customer product level records
#' @param cust_id numeric; customer identifier
#' @param seedsmanID numeric; seedsman identifier
#' @param custIDColumn string; name of column in cust_df that contains numeric customer identifier
#' @param orderTableNames vector; order to display formatted column names in table
#' @param seedsmanIDColumn string; name of column that contains numeric seedsmanID in cust_df
#' @param discountColumn string; name of column that contains per-unit LCR in cust_df
#' @param yearColumn string; name of column that contains year in cust_df
#' @param priceColumn string; name of column that tracks year in cust_df
#' @param effectivePriceColumn string; name of columnt that tracks effective price (basket level) in cust_df/EffPrice
#' @param initial bool; has a portfolio table previously been rendered in the UI
#' @param new_cust bool; is customer new customer
#' @return order_tab, object; updated current portfolio information dataframe
#' 
update_order_info_table <- function(order_tab, 
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
                                    new_cust=FALSE) {
  if (initial==TRUE) {
    
    weighted_average <- get_weight_unit_discount(cart_df = cart_df)
    order_tab[, 'Prevailing LCR Unit'] <- paste0('$', format(round(weighted_average, 2), nsmall=2))

    prevailingLCRPct <- mean(as.numeric(gsub("[\\%,]", "", cart_df$DiscountPCT)))
    order_tab[, 'Prevailing LCR %'] <- paste0(format(round(prevailingLCRPct, 2), nsmall=2), '%')
    
    if (new_cust == FALSE){
      
      grower_tmp <- cust_df[cust_df[[custIDColumn]] == cust_id & cust_df[[seedsmanIDColumn]] == seedsmanID, ]
      
      actualFGprice_tmp <- aggregate(grower_tmp[[effectivePriceColumn]] ~ grower_tmp[[yearColumn]], FUN=mean)
      actualFGprice <- actualFGprice_tmp[actualFGprice_tmp$`grower_tmp[[yearColumn]]` == max(actualFGprice_tmp$`grower_tmp[[yearColumn]]`),
                                         'grower_tmp[[effectivePriceColumn]]']
      order_tab[, 'Actual Farm Gate Price'] <- paste0('$', format(round(actualFGprice, 2), nsmall=2))

      grower_pos_disc <- aggregate(grower_tmp[[discountColumn]] ~ grower_tmp[[yearColumn]], data=grower_tmp,
                              FUN=mean)
      grower_pos_price <- aggregate(grower_tmp[[priceColumn]] ~ grower_tmp[[yearColumn]], data=grower_tmp,
                                  FUN=mean)
      grower_pos <- merge(grower_pos_disc, grower_pos_price)
      
      grower_pos[, 'discountPct'] <- (grower_pos$`grower_tmp[[discountColumn]]` / grower_pos$`grower_tmp[[priceColumn]`) * 100
      position <- grower_pos[grower_pos$`grower_tmp[[yearColumn]]` == max(grower_pos$`grower_tmp[[yearColumn]]`),
                             'discountPct']
      
      order_tab[, 'Actual LCR %'] <- paste0(format(round((position), 2), nsmall=2), '%')

      weighted_price <- get_weight_unit_retail_price(cart_df = cart_df)
      order_tab[, 'Actual LCR Unit*'] <- paste0('$', format(round((as.numeric(gsub("[\\%,]", "", 
                                                                      order_tab[, 'Actual LCR %'])) / 100) * weighted_price, 2), 
                                                           nsmall=2))
      
      checkDiscounts <- list(as.numeric(gsub("[\\$,]", "", order_tab[, 'Actual LCR Unit*'])), 
                             as.numeric(gsub("[\\$,]", "", order_tab[, 'Prevailing LCR Unit'])))
      checkDiscountsPCT <- list(as.numeric(gsub("[\\%,]", "", order_tab[, 'Prevailing LCR %'])), 
                                as.numeric(gsub("[\\%,]", "", order_tab[, 'Actual LCR %'])))
      order_tab[, 'Rec. LCR Unit'] <-  paste0('$', format(min(unlist(checkDiscounts)), nsmall=2))
      order_tab[, 'Rec. LCR %'] <- paste0(format(min(unlist(checkDiscountsPCT)), nsmall=2), '%')
      
    }
    
  } else {
    # convert to string because prettyNum makes all columns factors instead of strings like
    #  they need to be
    i <- sapply(order_tab, is.factor)
    order_tab[i] <- lapply(order_tab[i], as.character)
    
    weighted_average <- get_weight_unit_discount(cart_df = cart_df)
    order_tab[1, 'Prevailing LCR Unit'] <- paste0('$', format(round(weighted_average, 2), nsmall=2))
    
    prevailingLCRPct <- mean(as.numeric(gsub("[\\%,]", "", cart_df$DiscountPCT)))
    order_tab[1, 'Prevailing LCR %'] <- paste0(format(round(prevailingLCRPct, 2), nsmall=2), '%')
    
    if (new_cust == FALSE){
      
      grower_tmp <- cust_df[cust_df[[custIDColumn]] == cust_id & cust_df[[seedsmanIDColumn]] == seedsmanID, ]
      
      actualFGprice_tmp <- aggregate(grower_tmp[[effectivePriceColumn]] ~ grower_tmp[[yearColumn]], FUN=mean)
      actualFGprice <- actualFGprice_tmp[actualFGprice_tmp$`grower_tmp[[yearColumn]]` == max(actualFGprice_tmp$`grower_tmp[[yearColumn]]`),
                                         'grower_tmp[[effectivePriceColumn]]']
      order_tab[1, 'Actual Farm Gate Price'] <- paste0('$', format(round(actualFGprice, 2), nsmall=2))
      
      grower_pos_disc <- aggregate(grower_tmp[[discountColumn]] ~ grower_tmp[[yearColumn]], data=grower_tmp,
                                   FUN=mean)
      grower_pos_price <- aggregate(grower_tmp[[priceColumn]] ~ grower_tmp[[yearColumn]], data=grower_tmp,
                                    FUN=mean)
      grower_pos <- merge(grower_pos_disc, grower_pos_price)
      
      grower_pos[, 'discountPct'] <- (grower_pos$`grower_tmp[[discountColumn]]` / grower_pos$`grower_tmp[[priceColumn]`) * 100
      position <- grower_pos[grower_pos$`grower_tmp[[yearColumn]]` == max(grower_pos$`grower_tmp[[yearColumn]]`),
                             'discountPct']
      
      order_tab[1, 'Actual LCR %'] <- paste0(format(round((position), 2), nsmall=2), '%')
      
      weighted_price <- get_weight_unit_retail_price(cart_df = cart_df)
      order_tab[1, 'Actual LCR Unit*'] <- paste0('$', format(round((as.numeric(gsub("[\\%,]", "", 
                                                                                  order_tab[1, 'Actual LCR %'])) / 100) * weighted_price, 2), 
                                                           nsmall=2))
      
      checkDiscounts <- list(as.numeric(gsub("[\\$,]", "", order_tab[1, 'Actual LCR Unit*'])), 
                             as.numeric(gsub("[\\$,]", "", order_tab[1, 'Prevailing LCR Unit'])))
      checkDiscountsPCT <- list(as.numeric(gsub("[\\%,]", "", order_tab[1, 'Prevailing LCR %'])), 
                                as.numeric(gsub("[\\%,]", "", order_tab[1, 'Actual LCR %'])))
      order_tab[1, 'Rec. LCR Unit'] <-  paste0('$', format(min(unlist(checkDiscounts)), nsmall=2))
      order_tab[1, 'Rec. LCR %'] <- paste0(format(min(unlist(checkDiscountsPCT)), nsmall=2), '%')
      
    }
  }
  
  order_tab <- data.frame(lapply(order_tab, FUN=function(x) prettyNum(x, big.mark=",")), check.names = FALSE)
  if (new_cust == TRUE){
    order_tab <- order_tab[, orderTableNamesNewCust]
  } else{
    order_tab <- order_tab[, orderTableNames]
  }
  return(order_tab)
}


#' Processes customer inputs to generate effective price predictions for existing customer
#' for corn
#'
#' @param new_port object; updated customer portfolio of products and quantities after selection 
#'  of most recent product
#' @param cust_id numeric; customer identifier
#' @param EffPrice object; historical customer basket level records
#' @param epCustIDColumn string; name of column that tracks numeric customer id in EffPrice
#' @param epYearColumn string; name of column that tracks purchase year in EffPrice
#' @param epCartDropCols vector of strings; raw column names of input features that are determined 
#'  by user input and not historical effective price records
#' @param features_effP vector; vector of features that enter the saved effective price 
#'  models used in UI
#' @param epAvgPriceColumn string; name of column that tracks retail price in EffPrice
#' @param epTotalQColumn string; name of column that tracks order total quantity in EffPrice
#' @param marketYear numeric; current market year
#' @param epSeedsmanIDColumn string; name of column tracking seedsman id in EffPrice
#' @param seedsmanID numeric; seedsman identifier
#' @param epCountyFeatures vector; vector of strings identifying features tied to the customer's county in EffPrice
#' @param epFipsCodeColumn string; name of column tracking fips code in EffPrice
#'@return obs_df object; dataframe to use to make effective price predictions
#'
process_cart_ep <- function(new_port,
                            cust_id, 
                            EffPrice, 
                            epCustIDColumn,
                            epYearColumn,
                            epCartDropCols,
                            features_effP,
                            epAvgPriceColumn,
                            epTotalQColumn,
                            marketYear,
                            epSeedsmanIDColumn,
                            seedsmanID,
                            epCountyFeatures,
                            epFipsCodeColumn) {
  df <- copy(new_port)
  
  # Set customer ID as numeric to ensure merging goes smooth
  df[, epCustIDColumn := as.numeric(cust_id)]
  
  # Generate predictors for customer that are a direct function of user inputs
  #  and do not manipulate the ones that aren't...
  # Get customer-specific features from training data
  EffPrice <- setDF(EffPrice)
  tmp_cust <- EffPrice[EffPrice[[epCustIDColumn]]==cust_id & EffPrice[[epSeedsmanIDColumn]] == seedsmanID, ]
  
  # make copy to refernce when making correct lags
  tmp_cust_ref <- copy(tmp_cust)
  # shift all purchase history lags so we use most recent data available
  # Farmgate price
  tmp_cust$AVG_FARMGATE_PRICE_lag2 <- tmp_cust_ref$AVG_FARMGATE_PRICE_lag1
  tmp_cust$AVG_FARMGATE_PRICE_lag1 <- tmp_cust_ref$AVG_FARMGATE_PRICE
  
  # LCR
  # Corn
  tmp_cust$AvgPerUnitDiscount_lag2 <- tmp_cust_ref$AvgPerUnitDiscount_lag1
  tmp_cust$AvgPerUnitDiscount_lag1 <- tmp_cust_ref$AvgPerUnitDiscount
  
  # Soy
  tmp_cust$AvgPerUnitDiscount_SOY_lag2 <- tmp_cust_ref$AvgPerUnitDiscount_SOY_lag1
  tmp_cust$AvgPerUnitDiscount_SOY_lag1 <- tmp_cust_ref$AvgPerUnitDiscount_Soy
  
  # get correct seedsman experience
  seedsmanDF <- EffPrice[EffPrice[[epSeedsmanIDColumn]] == seedsmanID, ]
  seedsmanExp <- max(EffPrice$YearsOfService, na.rm=TRUE)
  tmp_cust$YearsOfService <- seedsmanExp
  
  # order by year
  tmp_cust <- tmp_cust[order(-tmp_cust[[epYearColumn]]), features_effP]
  
  # Remove columns we are going to create based on user input
  tmp_cust[ , epCartDropCols] <- NULL
  # data.table for compatibility 
  tmp_cust <- data.table(tmp_cust)
  tmp_cust[, epCustIDColumn := as.numeric(cust_id)]
  
  # Take most recent row for customer characteristics
  tmp_cust <- tmp_cust[1, ]

  # Merge so a single row exists for each product
  obs_df <- merge(df, tmp_cust, by='epCustIDColumn')

  # rename custIDColumn appropiately
  names(obs_df)[names(obs_df) == 'epCustIDColumn'] <- epCustIDColumn
  
  # Get list price for each product
  UnitPrices <- unlist(lapply(obs_df$Product, function(i) products_df[products_df$Product == as.character(i), 'Price']))
  obs_df[, 'UnitPrice'] <- as.numeric(UnitPrices)
  
  # calculate order level total stats 
  avg_retail_price <- sum(obs_df$Quantity * obs_df$UnitPrice)/sum(obs_df$Quantity)
  ordered_quantity <- sum(obs_df$Quantity)
  
  tmp <- data.table()
  tmp[, epAvgPriceColumn] <- avg_retail_price
  tmp[, epTotalQColumn] <- ordered_quantity
  tmp[, 'YearTimesLPBasket'] <- marketYear * tmp[[epAvgPriceColumn]]
  
  obs_df <- cbind(tmp, obs_df[1, ])
  
  # Use mean time from start of season the customer typically purchases
  obs_df$TimefromStartofSeason <- mean(tmp_cust_ref$TimefromStartofSeason, na.rm=TRUE)
  
  # temporary fix for weather and usda data, use averages for features tied to county until weather data is fixed
  tmp_county <- tmp_cust_ref[, epCountyFeatures]
  tmp_county <- tmp_county[,colSums(is.na(tmp_county))<nrow(tmp_county)]
  
  out <- aggregate(.~tmp_county[[epFipsCodeColumn]], data=tmp_county, FUN=mean, na.rm=TRUE, na.action=na.pass)
  out[[epFipsCodeColumn]] <- NULL
  names(out)[names(out) == "tmp_county[[epFipsCodeColumn]]"] <- epFipsCodeColumn
  obs_df[, epCountyFeatures] <- NULL
  obs_df <- cbind(obs_df, out)
  
  # model does not take these columns as a predictor
  obs_df[, 'Product'] <- NULL
  obs_df[, 'Quantity'] <- NULL
  obs_df[, 'UnitPrice'] <- NULL
  obs_df[, epCustIDColumn] <- NULL
  
  # replace any missing features with NA
  empty_cols <- features_effP[which(!features_effP %in% names(obs_df))]
  len_empty_cols <- length(empty_cols)
  if(len_empty_cols > 0) {
    obs_df[, empty_cols] <- NA
  }
  obs_df <- obs_df[, ..features_effP]
  
  obs_df
}


#' Processes customer inputs for a new customer to make effective price predictions for corn
#'
#' @param new_port object; updated customer portfolio of products and quantities after selection 
#'  of most recent product
#' @param EffPrice object; historical customer basket level records
#' @param fips_code numeric; fips code of new customer's county
#' @param epCustIDColumn string; name of column that tracks numeric customer id in EffPrice
#' @param epYearColumn string; name of column that tracks purchase year in EffPrice
#' @param epCartDropCols vector of strings; raw column names of input features that are determined 
#'  by user input and not historical effective price records
#' @param features_effP vector; vector of features that enter the saved effective price 
#'  models used in UI
#' @param epAvgPriceColumn string; name of column that tracks retail price in EffPrice
#' @param epTotalQColumn string; name of column that tracks order total quantity in EffPrice
#' @param epFipsCodeColumn string; column that measures fips code in EffPrice
#' @param countyFeatures vector; vector of features that are measured at county level that could be 
#'  used for new customer prediction based on zip code
#' @param marketYear numeric; current market year
#' @param epSeedsmanIDColumn string; name of column tracking effective price in EffPrice
#' @param seedsmanID numeric; seedsman identifier
#' @return obs_df object; dataframe to use to make effective price predictions
#'
process_cart_ep_new_cust <- function(new_port,
                            EffPrice, 
                            fips_code, 
                            features_effP,
                            epAvgPriceColumn,
                            epTotalQColumn,
                            epFipsCodeColumn,
                            epCountyFeatures,
                            marketYear,
                            epSeedsmanIDColumn,
                            seedsmanID
                            ) {
  # copy to manipulate
  obs_df <- copy(new_port)

  # fill fips code
  obs_df[, c(epFipsCodeColumn)] <- as.numeric(fips_code)
  
  # try to get features associated with fips code for new customer
  EffPrice <- setDF(EffPrice)
  check_fips <- EffPrice[EffPrice[[epFipsCodeColumn]] == as.numeric(fips_code), epCountyFeatures]
  if (all(is.na(check_fips)) == FALSE & nrow(check_fips) != 0) {
    out <- aggregate(.~check_fips[[epFipsCodeColumn]], data=check_fips, FUN=mean, na.rm=TRUE)
    out[[epFipsCodeColumn]] <- NULL
    names(out)[names(out) == "check_fips[[epFipsCodeColumn]]"] <- epFipsCodeColumn
    obs_df <- cbind(obs_df, out)
  }
  
  # get list price for each product
  UnitPrices <- unlist(lapply(obs_df$Product, function(i) products_df[products_df$Product == as.character(i), 'Price']))
  obs_df[, 'UnitPrice'] <- as.numeric(UnitPrices)
  
  # calculate order level stats from product level information to feed into model 
  avg_retail_price <- sum(obs_df$Quantity * obs_df$UnitPrice)/sum(obs_df$Quantity)
  ordered_quantity <- sum(obs_df$Quantity)
  tmp <- data.table()
  tmp[, epAvgPriceColumn] <- avg_retail_price
  tmp[, epTotalQColumn] <- ordered_quantity
  tmp[, 'YearTimesLPBasket'] <- marketYear * tmp[[epAvgPriceColumn]]
  obs_df <- cbind(tmp, obs_df[1, ])
  
  # model does not take any product level information as a predictor
  obs_df[, 'Product'] <- NULL
  obs_df[, 'Quantity'] <- NULL
  obs_df[, 'UnitPrice'] <- NULL
  
  # get correct seedsman experience
  seedsmanDF <- EffPrice[EffPrice[[epSeedsmanIDColumn]] == seedsmanID, ]
  seedsmanExp <- max(EffPrice$YearsOfService, na.rm=TRUE)
  obs_df[, 'YearsOfService']<- seedsmanExp
  
  # make features not observed for new customer NA 
  empty_cols <- features_effP[which(!features_effP %in% names(obs_df))]
  len_empty_cols <- length(empty_cols)
  if(len_empty_cols > 0) {
    obs_df[, empty_cols] <- NA
  }
  
  # order features correctly 
  obs_df <- obs_df[, ..features_effP]
  
  obs_df
}


#' Plots annual averages for historical customer purchase history metrics
#'
#'@param cust_df object; dataframe of historical customer product level records
#'@param EffPrice object;  dataframe of historical customer basket level records
#'@param cust_id numeric; customer identifier
#'@param custIDColumn string; name of column that tracks customer identifier in cust_df
#'@param epCustIDColumn string; name of column that tracks customer identifier in EffPrice
#'@param varToPlot string; name of variable to plot
#'@param custFeatsToPlot vector; features of variables made available to plot
#'@param featsToPlotCleanNames object; vector of formatted names for variables made available to plot
#'@param seedsmanIDColumn string; name of seedsman id column in cust_df
#'@param seedsmanId numeric; seedsman identifier 
#'@param discountColumn string; name of per-unit LCR column in cust_df
#'@param priceColumn string; name of retail price column in cust_df
#'@return p, object; plot of varToPlot over time
#'
make_customer_plot <- function(cust_df, 
                               cust_id,
                               custIDColumn,
                               varToPlot,
                               custFeatsToPlot,
                               featsToPlotCleanNames,
                               seedsmanIDColumn,
                               seedsmanID,
                               discountColumn,
                               priceColumn){
  
  plot_df <- cust_df[cust_df[[custIDColumn]] == cust_id & cust_df[[seedsmanIDColumn]] == seedsmanID, ]

  plot_df[, 'PctDiscount'] <- plot_df[[discountColumn]] / plot_df[[priceColumn]] * 100

  if(all(is.na(c(plot_df[, 'PctDiscount'])))){
    p <- "No Customer History"
  }else{
  plot_df <- plot_df[, custFeatsToPlot]

  names(plot_df) <- featsToPlotCleanNames

  plot_df <- aggregate(plot_df[[varToPlot]] ~ Year, data=plot_df, FUN=mean)
  names(plot_df)[2] <- varToPlot
  
  scale_x <- seq(min(plot_df$Year), max(plot_df$Year), by=1)
  p <- ggplot(data=plot_df, aes(Year, .data[[varToPlot]])) + geom_line(linetype=2) + geom_point()
  p <- p + theme_minimal(base_size=14) + ylab(varToPlot) + xlab('Year') + 
    scale_x_continuous(breaks=scale_x)
  p}
}


make_seedsman_plot <- function(cust_df, 
                               custIDColumn,
                               varToPlot,
                               custFeatsToPlot,
                               featsToPlotCleanNames,
                               seedsmanIDColumn,
                               seedsmanNameColumn,
                               seedsmanID,
                               discountColumn,
                               priceColumn,
                               fsrColumn,
                               fsrName,
                               seedsmanName){

  plot_df <- cust_df[cust_df[[fsrColumn]] == fsrName, ]

  plot_df[, 'PctDiscount'] <- plot_df[[discountColumn]] / plot_df[[priceColumn]] * 100

  plot_df <- plot_df[, c(seedsmanNameColumn, custFeatsToPlot)]

  names(plot_df) <- c(seedsmanNameColumn, featsToPlotCleanNames)

  plot_df <- aggregate(plot_df[[varToPlot]], data=plot_df, by = list(Year = plot_df[,'Year'], Seedsman = plot_df[,seedsmanNameColumn]), FUN=mean)
  names(plot_df)[3] <- varToPlot
  plot_df[,varToPlot] <- round(plot_df[,varToPlot],2)

  scale_x <- seq(min(plot_df$Year), max(plot_df$Year), by=1)
  p <- ggplot(data=plot_df, aes(Year, .data[[varToPlot]], colour = Seedsman)) + geom_line(linetype=2) + geom_point()
  p <- p + theme_minimal(base_size=14) + ylab(varToPlot) + xlab('Year') + 
    scale_x_continuous(breaks=scale_x)
  p <- ggplotly(p)
  
  p <- p %>% config(displaylogo = FALSE)
  p <- p %>% config(modeBarButtonsToRemove = c('zoom2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'autoScale2d', 'resetScale2d', 'hoverClosestCartesian', 'hoverCompareCartesian', 'toggleSpikelines'))
  
  p
}



#' Function that generates plot of predicted discount distribution for entire order when product 
#'  is added to cart for an existing customer
#' 
#' @title make_plot_cart_order
#' @param discounts_out vector; all distributions for products in customer cart
#' @param stdN numeric; how many SD on each side of mean to limit discount range on plot
#' @param cart_df object; datatable of customer's current portfolio
#' @param cust_id numeric; customer identifier
#' @param cust_df object; dataframe of historical customer records
#' @param custIDColumn string; name of numeric customer identifier column in cust_df
#' @param discountColumn string; name of column that tracks per-unit discount in cust_df
#' @param yearColumn string; name of column that tracks year in cust_df
#' @param seedsmanID numeric; seedsman identifier
#' @param seeesmanIDColumn string; name of column that tracks seedsman identifier in cust_df
#' @param priceColumn string; name of column that tracks list price in cust_df
#' @return distPlot, object; plot of distributed discounts for customner's entire portfolio
#'
make_plot_cart_order <- function(discounts_out,
                                 stdN,
                                 cart_df,
                                 cust_id,
                                 cust_df,
                                 custIDColumn,
                                 discountColumn,
                                 yearColumn,
                                 seedsmanID,
                                 seedsmanIDColumn,
                                 priceColumn) {

  
  tmp <- cust_df[cust_df[[custIDColumn]] == cust_id & cust_df[[seedsmanIDColumn]] == seedsmanID, ]
  tmp_by_year_discount <- aggregate(tmp[[discountColumn]] ~ tmp[[yearColumn]], data=tmp, FUN=mean)
  tmp_by_year_price <- aggregate(tmp[[priceColumn]] ~ tmp[[yearColumn]], data=tmp, FUN=mean)
  tmp_by_year <- merge(tmp_by_year_discount, tmp_by_year_price)
  
  tmp_by_year[, 'discountPct'] <- (tmp_by_year$`tmp[[discountColumn]]` / tmp_by_year$`tmp[[priceColumn]]`) * 100
  
  # get weighted average unit retail price for cart
  avg_retail_price <- get_weight_unit_retail_price(cart_df)


  # make x to correspond with old code
  x <- (discounts_out / avg_retail_price) * 100
  
  # calculate some stats needed to correctly fill area under the distribution curve
  mean_x <- mean(x)
  high_sd <- mean(x) + stdN*sd(x)
  low_sd <- mean(x) - stdN*sd(x)
  sd_x <- sd(x)
  
  # create data frame for density plotting and shading
  dens <- density(x)
  dd <- with(dens, data.frame(x, y))

  # use created data frame to render plot 
  distPlot <-  qplot(x,y,data=dd,geom="line") + # plot base
    
    # fill discount range
    geom_ribbon(data=subset(dd, x>=low_sd, x<high_sd),aes(ymax=y),ymin=0,
                fill="gold1",colour=NA,alpha=0.7) +
    
    # fill left of discount range
    geom_ribbon(data=subset(dd, x<=low_sd),aes(ymax=y),ymin=0,
                fill="forestgreen",colour=NA,alpha=0.7) + 
    # fill right of discount range
    geom_ribbon(data=subset(dd, x>=high_sd),aes(ymax=y),ymin=0,
                fill="red2",colour=NA,alpha=0.7) + 
    # set correct y limits
    ylim(0, max(dd$y)) +

    # Put text for discount range on bottom of plot
    xlab(paste("Prevailing LCR % Range:",
               paste0(
                      format(round(mean_x-stdN*sd_x,2), nsmall=2), '%'), "to", paste0(format(round(mean_x+stdN*sd_x,2), nsmall=2), '%'))) +

    # add vertical line at mean discount offer
    geom_vline(aes(xintercept = mean_x),col='grey50', linetype = 'dashed', size=1.25) +

    # set vertical lines at edge of recommended discount range
    geom_vline(xintercept = c(mean_x-stdN*sd(x), mean_x+stdN*sd_x), color = "grey50", size=1.25) +

    # Add minimial theme to match white background
    theme_minimal(base_size = 16)  +

    # Some styling...take labels off y-axis, title each plot by it's product name,
    # and annotate with text the line for the customer's last unit discount
    theme(
      axis.text.y=element_blank())+ labs(y="") +
    ggtitle("Portfolio Total") +
      geom_vline(aes(xintercept=tmp_by_year[tmp_by_year$`tmp[[yearColumn]]` == max(tmp_by_year$`tmp[[yearColumn]]`),
                                            'discountPct'],
                     color='Actual LCR %'),
                 linetype='dotdash', size=1.25) +
      scale_color_manual(name = "", values = c('Actual LCR %' = 'black')) +
      theme(legend.position='top')
  distPlot
}





#' Function that generates a gauge plot of predicted discount distribution for entire order when product 
#'  is added to cart for an existing customer
#' 
#' @title make_plot_cart_order2
#' @param discounts_out vector; all distributions for products in customer cart
#' @param stdN numeric; how many SD on each side of mean to limit discount range on plot
#' @param cust_id numeric; customer identifier
#' @param cust_df object; dataframe of historical customer records
#' @param custIDColumn string; name of numeric customer identifier column in cust_df
#' @param discountColumn string; name of column that tracks per-unit discount in cust_df
#' @param yearColumn string; name of column that tracks year in cust_df
#' @param seedsmanID numeric; seedsman identifier
#' @param seedsmanIDColumn string; name of column thaat tracks seedsman ID in cust_df
#' @param cart_df object; datatable of customer's current portfolio
#' @return fig, object; gauge plot 
#'
make_plot_cart_order2 <- function(discounts_out,
                                 stdN, 
                                 cust_id,
                                 cust_df,
                                 custIDColumn,
                                 discountColumn,
                                 yearColumn,
                                 seedsmanID,
                                 seedsmanIDColumn,
                                 cart_df) {
  
  tmp <- cust_df[cust_df[[custIDColumn]] == cust_id & cust_df[[seedsmanIDColumn]] == seedsmanID, ]
  tmp_by_year <- aggregate(tmp[[discountColumn]] ~ tmp[[yearColumn]], data=tmp, FUN=mean)
  
  
  # make x to correspond with old code
  x <- discounts_out
  
  # calculate some stats needed to correctly fill area under the distribution curve
  max_x <- max(x)
  mean_x <- mean(x)
  high_sd <- mean(x) + stdN*sd(x)
  low_sd <- mean(x) - stdN*sd(x)
  sd_x <- sd(x)
  
  #create gauge plot
  fig <- plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = tmp_by_year[tmp_by_year$`tmp[[yearColumn]]` == max(tmp_by_year$`tmp[[yearColumn]]`),
                'tmp[[discountColumn]]'],
    number = list(valueformat = '$,.2f'),
    title = list(text = "Grower Position (Last Order Avg. Unit LCR)"),
    type = "indicator",
    mode = "gauge+number+delta",
    delta = list(reference = get_weight_unit_discount(cart_df), valueformat = '$,.2f', increasing = list(color = "#FE6100"), decreasing = list(color = "#7CFC00")),
    gauge = list(
      axis =list(range = list(NULL, max_x), tickformat = '$,.2f'), #max x
      bar = list(color = "#2F4F4F"),
      steps = list(
        list(range = c(0, low_sd), color = "#228B22"),
        list(range = c(low_sd, high_sd), color = "#7CFC00"),
        list(range = c(high_sd, max_x), color = "#FE6100")),
      threshold = list(
        line = list(color = "yellow", width = 4, hoverformat = '$,.2f'),
        thickness = 0.75,
        value = get_weight_unit_discount(cart_df))
      )) 
  
  fig <- fig %>%
    layout(autosize = TRUE ,margin = list(l=20,r=20, t = 80))
  
  fig
}





#' Generates plot of discount distribution for entire order for a new customer when product is 
#'  added to cart
#' 
#' @param discounts_out vector; all distributions for products in customer cart
#' @param stdN numeric; how many SD on each side of mean to limit discount range on plot
#' @param cart_df object; current customer portfolio 
#' distPlot, object; plot of distributed discounts for customner's entire portfolio
#'
make_plot_cart_order_new_cust <- function(discounts_out,
                                          stdN,
                                          cart_df) {
  
  avg_retail_price <- get_weight_unit_retail_price(cart_df)
  
  # make x to correspond with old code
  x <- (discounts_out / avg_retail_price) * 100
  
  # calculate some stats needed to correctly fill area under the distribution curve
  mean_x <- mean(x)
  high_sd <- mean(x) + stdN*sd(x)
  low_sd <- mean(x) - stdN*sd(x)
  sd_x <- sd(x)
  
  # create data frame for density plotting and shading
  dens <- density(x)
  dd <- with(dens, data.frame(x, y))
  
  # use created data frame to render plot 
  distPlot <-  qplot(x,y,data=dd,geom="line") + # plot base
    
    # fill discount range
    geom_ribbon(data=subset(dd, x>=low_sd, x<high_sd),aes(ymax=y),ymin=0,
                fill="gold1",colour=NA,alpha=0.7) +
    
    # fill left of discount range
    geom_ribbon(data=subset(dd, x<=low_sd),aes(ymax=y),ymin=0,
                fill="forestgreen",colour=NA,alpha=0.7) + 
    # fill right of discount range
    geom_ribbon(data=subset(dd, x>=high_sd),aes(ymax=y),ymin=0,
                fill="red2",colour=NA,alpha=0.7) +
    # set correct y limits
    ylim(0, max(dd$y)) +
    
    # Put text for discount range on bottom of plot
    xlab(paste("Prevailing Unit LCR % Range:", 
               paste0( 
                      format(round(mean_x-stdN*sd_x,2), nsmall=2), '%'), "to", paste0(format(round(mean_x+stdN*sd_x, 2), nsmall=2), '%'))) +
    
    # add vertical line at mean discount offer
    geom_vline(aes(xintercept = mean_x),col='grey50', linetype = 'dashed', size=1.25) +
    
    # set vertical lines at edge of recommended discount range
    geom_vline(xintercept = c(mean_x-stdN*sd(x), mean_x+stdN*sd_x), color = "grey50", size=1.25) +
    
    # Add minimial theme to match white background
    theme_minimal(base_size = 16)  +
    
    # Some styling...
    theme(
      axis.text.y=element_blank())+ labs(y="") + 
    ggtitle("Portfolio Total")

   distPlot
}






#' Generates gauge plot of discount distribution for entire order for a new customer when product is 
#'  added to cart
#' 
#' @param discounts_out vector; all distributions for products in customer cart
#' @param stdN numeric; how many SD on each side of mean to limit discount range on plot
#' distPlot, object; plot of distributed discounts for customner's entire portfolio
#' @param cart_df object; data frame of customer's current portfolio
#' @return fig, object; gauge plot 
#'
make_plot_cart_order_new_cust2 <- function(discounts_out,
                                          stdN,
                                          cart_df) {
  
  # make x to correspond with old code
  x <- discounts_out
  
  # calculate some stats needed to correctly fill area under the distribution curve
  max_x <- max(x)
  mean_x <- mean(x)
  high_sd <- mean(x) + stdN*sd(x)
  low_sd <- mean(x) - stdN*sd(x)
  sd_x <- sd(x)
  
  # create data frame for density plotting and shading
  dens <- density(x)
  dd <- with(dens, data.frame(x, y))
  
  # use created data frame to render plot 
  #create gauge plot
  fig <- plot_ly(
    domain = list(x = c(0, 1), y = c(0, 1)),
    value = get_weight_unit_discount(cart_df),
    number = list(valueformat = '$,.2f'),
    title = list(text = "Avg. Recommended Unit LCR"),
    type = "indicator",
    mode = "gauge+number+delta",
    delta = list(reference = get_weight_unit_discount(cart_df), valueformat = '$,.2f', increasing = list(color = "#FE6100"), decreasing = list(color = "#7CFC00")),
    gauge = list(
      axis =list(range = list(NULL, max_x), tickformat = '$,.2f'), #max x
      bar = list(color = "#2F4F4F"),
      steps = list(
        list(range = c(0, low_sd), color = "#228B22"),
        list(range = c(low_sd, high_sd), color = "#7CFC00"),
        list(range = c(high_sd, max_x), color = "#FE6100")),
      threshold = list(
        line = list(color = "yellow", width = 4, hoverformat = '$,.2f'),
        thickness = 0.75,
        value = get_weight_unit_discount(cart_df))
      
    )) 
  
  fig <- fig %>%
    layout(autosize = TRUE ,margin = list(l=20,r=20, t = 80))
  
  fig
}

#' Processes input from user and creates data set necesscary to generate 
#'  predictions for soy discounts
#'   
#' @title process_cart_soy
#' @param cart_df object; datatable of customer's current portfolio
#' @param cust_id numeric; customer identifier
#' @param cust_df object; dataframe containing historical customer product records
#' @param quantityColumn string; name of column that tracks product quantity in cust_df
#' @param priceColumn string; name of column that tracks product list price in cust_df
#' @param productColumn string; name of column that tracks product name in cust_df 
#' @param custIDColumn string; name of column that tracks numeric customer id in cust_df
#' @param cartDropCols vector; columns that are derived directly from user input and do not
#'  need extraction from cust_df
#' @param features vector; vector of features that enter the saved discount models used in UI
#' @param avgPrice numeric; average retail price of customer's order
#' @param totalQuantity numeric; total quantity purchased in customer order
#' @param yearColumn string; name of column that tracks year in cust_df
#' @param marketYear numeric; current year
#' @param products_df object; data frame unique hybrid-price combinations
#' @param totalQColumn string; name of column that tracks total quantity in cust_df
#' @param avgPriceColumn stirng; name of column that tracks average order price in cust_df
#' @param seedsmanIDColumn string; name of column that tracks seedsman identifier in cust_df
#' @param seedsmanID numeric; seedsman identifier
#' @param fipsCodeColumn string; name of column tracking fips code in cust_df
#' @param countyFeatures vector; vector of strings identifying features associated with customer's fips code in cust_df
#' @return obs_df, dataframe containing all necesscary customer information to generate predictions
#' 
process_cart_soy <- function(product,
                             quantity,
                             cust_id, 
                             cust_df, 
                             quantityColumn, 
                             priceColumn,
                             productColumn,
                             custIDColumn,
                             cartDropCols,
                             features,
                             avgPrice,
                             totalQuantity,
                             yearColumn,
                             marketYear,
                             products_df,
                             totalQColumn,
                             avgPriceColumn,
                             seedsmanIDColumn,
                             seedsmanID,
                             fipsCodeColumn,
                             countyFeatures){
  
  # tracking product and quantity 
  df <- data.table()
  df[, "Product"] <- product
  df[, "Quantity"] <- quantity
  
  # set customer ID as numeric to ensure merging goes smooth
  df[, custIDColumn := as.numeric(cust_id)]
  
  # generate predictors for customer that are a direct function of user inputs
  #  and do not manipulate the ones that aren't...
  # get customer-specific features from training data
  cust_df <- setDF(cust_df)
  tmp_cust <- cust_df[cust_df[[custIDColumn]]==cust_id & cust_df[seedsmanIDColumn] == seedsmanID, ]
  # make copy to refernce when making correct lags
  tmp_cust_ref <- copy(tmp_cust)
  
  # shift all purchase history lags possible so we use most recent data available
  # Farmgate price
  tmp_cust$AVG_FARMGATE_PRICE_lag2 <- tmp_cust_ref$AVG_FARMGATE_PRICE_lag1
  tmp_cust$AVG_FARMGATE_PRICE_lag1 <- tmp_cust_ref$AVG_FARMGATE_PRICE
  # LCR
  
  # soy
  tmp_cust$AvgPerUnitDiscount_lag2 <- tmp_cust_ref$AvgPerUnitDiscount_lag1
  tmp_cust$AvgPerUnitDiscount_lag1 <- tmp_cust_ref$AvgPerUnitDiscount
  
  # order by year
  tmp_cust <- tmp_cust[order(-tmp_cust[[yearColumn]]), features]
  
  # remove columns we are going to create based on user input
  tmp_cust[ , cartDropCols] <- NULL
  # data.table for compatibility 
  tmp_cust <- data.table(tmp_cust)
  tmp_cust[, custIDColumn := as.numeric(cust_id)]
  
  # take most recent row 
  tmp_cust <- tmp_cust[1, ]
  
  # merge so a single row exists for each product
  obs_df <- merge(df, tmp_cust, by='custIDColumn')
  
  # rename custIDColumn appropiately
  names(obs_df)[names(obs_df) == 'custIDColumn'] <- custIDColumn
  
  # get list price for each product
  UnitPrices <- unlist(lapply(obs_df$Product, function(i) products_df[products_df$Product == as.character(i), 'Price']))
  obs_df[, priceColumn] <- as.numeric(UnitPrices)
  
  
  # get corresponding order level variables 
  obs_df[, totalQColumn] <- totalQuantity
  obs_df[, avgPriceColumn] <- avgPrice
  obs_df[, 'RETAIL_AMT'] <- totalQuantity*avgPrice
  obs_df[, 'YearTimesLPProduct'] <- marketYear * obs_df[, ..priceColumn]
  
  # temporary fix for weather and usda data, use averages for features tied to county until weather data is fixed
  tmp_county <- tmp_cust_ref[, countyFeatures]
  tmp_county <- tmp_county[,colSums(is.na(tmp_county))<nrow(tmp_county)]
  out <- aggregate(.~tmp_county[[fipsCodeColumn]], data=tmp_county, FUN=mean, na.rm=TRUE, na.action=na.pass)
  # take only columns we have info on
  out[[fipsCodeColumn]] <- NULL
  names(out)[names(out) == "tmp_county[[fipsCodeColumn]]"] <- fipsCodeColumn
  obs_df[, countyFeatures] <- NULL
  obs_df <- cbind(obs_df, out)
  
  obs_df[, 'Product'] <- NULL
  obs_df[, 'Quantity'] <- NULL
  
  empty_cols <- features[which(!features %in% names(obs_df))]
  len_empty_cols <- length(empty_cols)
  if(len_empty_cols > 0) {
    obs_df[, empty_cols] <- NA
  }
  
  obs_df
}

#' Generates a vector of boostrapped discount predictions for an existing customer
#'  and single product 
#' 
#' @title predict_discount_raw_soy
#' @param product string; name of product discount prediction is for
#' @param quantity numeric; quantity of product discount is for
#' @param cust_id numeric; customer identifier
#' @param cust_df object; dataframe containing historical customer product records
#' @param logTransformDiscount bool; should predictions be transformed from log form before display
#'  in UI
#' @param quantityColumn string; name of column that tracks quantity in cust_df
#' @param priceColumn string; name of column that tracks price in cust_df
#' @param productColumn string; name of colum that tracks product name in cust_df
#' @param custIDColumn string; name of column that tracks numeric customer_id in cust_df
#' @param cartDropCols vector of strings; raw column names of input features that are determined 
#'  by user input and not historical customer records
#' @param features vector; vector of features that enter the saved discount models used in UI
#' @param discountModel object; list of boostrapped models to use to generate discount predictions
#' @param pllel bool; should predictions be done in parallel
#' @param avgPrice numeric; average retail price of customer order
#' @param totalQuantity numeric; total quantity in customer order
#' @param yearColumn string; name of column that tracks year in cust_df
#' @param marketYear numeric; current year
#' @param products_df objectl data frame of relevant products and prices
#' @param totalQColumn string; name of column that tracks total order quantity in cust_df
#' @param avgPriceColumn string; name of column that tracks order average price in cust_df
#' @param seedsmanIDColumn string; name of column that tracks seedsman identifier in cust_df
#' @param seedsmanID numeric; seedsman identifier
#' @param fipsCodeColumn string; name of column tracking fips code in cust_df
#' @param countyFeatures vector; vector of strings that identifies features associated with customer's fips code in cust_df
#' @return discounts, vector; vector of boostrapped unit discount predictions 
#' 
predict_discount_raw_soy <- function(product,
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
                                 marketYear,
                                 products_df,
                                 totalQColumn,
                                 avgPriceColumn,
                                 seedsmanIDColumn,
                                 seedsmanID,
                                 fipsCodeColumn,
                                 countyFeatures) {
  # use 'process_cart' to create test data set for prediction
  test_df <- process_cart_soy(product,
                              quantity,
                              cust_id, 
                              cust_df, 
                              quantityColumn, 
                              priceColumn,
                              productColumn,
                              custIDColumn,
                              cartDropCols,
                              features,
                              avgPrice,
                              totalQuantity,
                              yearColumn,
                              marketYear,
                              products_df,
                              totalQColumn,
                              avgPriceColumn,
                              seedsmanIDColumn,
                              seedsmanID,
                              fipsCodeColumn,
                              countyFeatures)
  # predict using that data
  discounts <- predictXGBoost(discountModel, 
                              test_df, 
                              features,
                              logTransform = logTransformDiscount,
                              pllel = pllel)
  
  # return vector of bootstrapped predictions
  discounts
}


#' Processes customer inputs to generate effective price predictions for existing customer for
#' soy 
#'
#' @param new_port object; updated customer portfolio of products and quantities after selection 
#'  of most recent product
#' @param cust_id numeric; customer identifier
#' @param EffPrice object; historical customer basket level records
#' @param epCustIDColumn string; name of column that tracks numeric customer id in EffPrice
#' @param epYearColumn string; name of column that tracks purchase year in EffPrice
#' @param epCartDropCols vector of strings; raw column names of input features that are determined 
#'  by user input and not historical effective price records
#' @param features_effP vector; vector of features that enter the saved effective price 
#'  models used in UI
#' @param epAvgPriceColumn string; name of column that tracks retail price in EffPrice
#' @param epTotalQColumn string; name of column that tracks order total quantity in EffPrice
#' @param marketYear numeric; current year
#' @param products_df object; data frame of relevant products and prices 
#' @param epCountyFeatures vector; vector of strings identifying features associated with customer's fips code in EffPrice
#' @param epFipsCodeColumn string; name of column identfying fips code in EffPrice
#' @param epSeedsmanIDColumn string; name of columnb tracking seedsman identifier in EffPrice
#' @param seedsmanID numeric; seedsman identifier
#'@return obs_df object; dataframe to use to make effective price predictions
#'
process_cart_ep_soy <- function(new_port,
                            cust_id, 
                            EffPrice, 
                            epCustIDColumn,
                            epYearColumn,
                            epCartDropCols,
                            features_effP,
                            epAvgPriceColumn,
                            epTotalQColumn,
                            marketYear,
                            products_df,
                            epCountyFeatures,
                            epFipsCodeColumn,
                            epSeedsmanIDColumn,
                            seedsmanID) {
  df <- copy(new_port)
  
  # Set customer ID as numeric to ensure merging goes smooth
  df[, epCustIDColumn := as.numeric(cust_id)]
  
  # generate predictors for customer that are a direct function of user inputs
  #  and do not manipulate the ones that aren't...
  # get customer-specific features from training data
  EffPrice <- setDF(EffPrice)
  tmp_cust <- EffPrice[EffPrice[[epCustIDColumn]]==cust_id & EffPrice[[epSeedsmanIDColumn]] == seedsmanID, ]
  
  # make copy to refernce when making correct lags
  tmp_cust_ref <- copy(tmp_cust)
  
  # shift all purchase history lags possible so we use most recent data available
  # farmgate price
  tmp_cust$AVG_FARMGATE_PRICE_lag2 <- tmp_cust_ref$AVG_FARMGATE_PRICE_lag1
  tmp_cust$AVG_FARMGATE_PRICE_lag1 <- tmp_cust_ref$AVG_FARMGATE_PRICE
  
  # LCR
  # soy
  tmp_cust$AvgPerUnitDiscount_lag2 <- tmp_cust_ref$AvgPerUnitDiscount_lag1
  tmp_cust$AvgPerUnitDiscount_lag1 <- tmp_cust_ref$AvgPerUnitDiscount
  
  # order by year
  tmp_cust <- tmp_cust[order(-tmp_cust[[epYearColumn]]), features_effP]
  
  # Remove columns we are going to create based on user input
  tmp_cust[ , epCartDropCols] <- NULL
  # data.table for compatibility 
  tmp_cust <- data.table(tmp_cust)
  tmp_cust[, epCustIDColumn := as.numeric(cust_id)]
  
  # take most recent row for customer characteristics
  tmp_cust <- tmp_cust[1, ]
  
  # merge so a single row exists for each product
  obs_df <- merge(df, tmp_cust, by='epCustIDColumn')
  
  # rename custIDColumn appropiately
  names(obs_df)[names(obs_df) == 'epCustIDColumn'] <- epCustIDColumn
  
  # get list price for each product
  UnitPrices <- unlist(lapply(obs_df$Product, function(i) products_df[products_df$Product == as.character(i), 'Price']))
  obs_df[, 'UnitPrice'] <- as.numeric(UnitPrices)
  
  # calculate order level total stats 
  avg_retail_price <- sum(obs_df$Quantity * obs_df$UnitPrice)/sum(obs_df$Quantity)
  ordered_quantity <- sum(obs_df$Quantity)
  retail_amt <- sum(obs_df$Quantity * obs_df$UnitPrice)
  
  tmp <- data.table()
  tmp[, epAvgPriceColumn] <- avg_retail_price
  tmp[, epTotalQColumn] <- ordered_quantity
  tmp[, 'YearTimesLPBasket'] <- marketYear * tmp[[epAvgPriceColumn]]
  tmp[, 'RETAIL_AMT'] <- retail_amt
  
  obs_df <- cbind(tmp, obs_df[1, ])
  
  # model does not take these columns as a predictor
  obs_df[, 'Product'] <- NULL
  obs_df[, 'Quantity'] <- NULL
  obs_df[, 'UnitPrice'] <- NULL
  obs_df[, epCustIDColumn] <- NULL

  obs_df <- obs_df[, ..features_effP]
  
  obs_df
  
}

#' Generates effective price predictions for an existing
#'  customer and associated portfolio for soy
#' 
#' @title predict_ep_soy
#' @param new_port object; updated customer portfolio of products and quantities after selection 
#'  of most recent product
#' @param cust_id numeric; customer identifier
#' @param EffPrice object; historical customer basket level records
#' @param logTransformEP bool; should predictions be transformed from log form before display
#'  in UI
#' @param epCustIDColumn string; name of column that tracks numeric customer id in effective price data
#' @param epYearColumn string; name of column that tracks purchase year in effective price data
#' @param epCartDropCols vector of strings; raw column names of input features that are determined 
#'  by user input and not historical effective price records
#' @param features_effP vector; vector of features that enter the saved effective price 
#' models used in UI
#' @param epAvgPriceColumn string; name of column that tracks retail price in effective price basket level data
#' @param epTotalQColumn string; name of column that tracks order total quantity in effective price basket level data
#' @param epModel object; model used to make effective price predictions
#' @param marketYear numeric; current year
#' @param products_df object; data frame of unique hybrid-price combinations
#' @param epCountyFeatures vector; vector of strings identifying features tied to the customer's fips code in EffPrice
#' @param epFipsCodeColumn string; name of column tracking fips code in EffPrice
#' @param epSeedsmanIDColumn string; name of column tracking seedsman identifier in EffPrice
#' @param seedsmanID numeric; seedsman identifier 
#' @return prices, vector of effective price predictions for all products in new_port 
#' 
predict_ep_soy <- function(new_port,
                           cust_id, 
                          EffPrice, 
                          logTransformEP,
                          epCustIDColumn,
                          epYearColumn,
                          epCartDropCols,
                          features_effP,
                          epAvgPriceColumn,
                          epTotalQColumn,
                          epModel,
                          marketYear,
                          products_df,
                          epCountyFeatures,
                          epFipsCodeColumn,
                          epSeedsmanIDColumn,
                          seedsmanID) {
  
  # use 'process_cart' to create test data set for prediction
  test_df <- process_cart_ep_soy(new_port,
                                cust_id, 
                                EffPrice, 
                                epCustIDColumn,
                                epYearColumn,
                                epCartDropCols,
                                features_effP,
                                epAvgPriceColumn,
                                epTotalQColumn,
                                marketYear,
                                products_df,
                                epCountyFeatures,
                                epFipsCodeColumn,
                                epSeedsmanIDColumn,
                                seedsmanID)
  
  # predict using that data with saved models
  model <- epModel
  prices <- predict(model, data.matrix(test_df))
  
  
  # convert basket level predictions to product level predictions
  ConversiontoProduct <- cbind(new_port, prices)
  ConversiontoProduct[, PercentageofBasket := Quantity / sum(Quantity)]
  UnitPrices <- unlist(lapply(ConversiontoProduct$Product, 
                              function(i) products_df[products_df$Product == as.character(i), 'Price']))
  ConversiontoProduct[, 'UnitPrice'] <- as.numeric(UnitPrices)
  ConversiontoProduct$alpha <- ConversiontoProduct$PercentageofBasket*ConversiontoProduct$UnitPrice
  ConversiontoProduct[, Percent := prices / sum(alpha)]
  ConversiontoProduct$PredictedProductEffectivePrice <- ConversiontoProduct$Percent * ConversiontoProduct$UnitPrice
  
  ConversiontoProduct
}

#' Processes input from user and creates data set necesscary to generate 
#'  predictions for soy discounts for new customers
#'   
#' @title process_cart_soy_new_cust
#' @param priceColumn string; name of column that tracks product list price in cust_df
#' @param productColumn string; name of column that tracks product name in cust_df 
#' @param features vector; vector of features that enter the saved discount models used in UI
#' @param countyFeatures vector; vector of features that correspond to county to get for new
#'   customer modeling 
#' @param cust_df object; data frame of historic product level records 
#' @param avgPrice numeric; average retail price of customer's order
#' @param totalQuantity numeric; total quantity purchased in customer order
#' @param products_df object; data frame of products and prices
#' @param totalQColumn string; name of column that tracks total order quantity in cust_df
#' @param avgPriceColumn stirng; name of column that tracks order avg price in cust_df
#' @param marketYear numeric; current market year
#' @param fipsCodeColumn string; name of column that tracks fips code in cust_df
#' @return obs_df, dataframe containing all necesscary customer information to generate predictions
#' 
process_cart_soy_new_cust <- function(product,
                                      quantity,
                                      fips_code, 
                                      priceColumn,
                                      features,
                                      countyFeatures,
                                      cust_df,
                                      avgPrice,
                                      totalQuantity,
                                      products_df,
                                      totalQColumn,
                                      avgPriceColumn,
                                      marketYear,
                                      fipsCodeColumn){
  
  df <- data.table()
  df[, "Product"] <- product
  df[, "Quantity"] <- quantity
  
  # copy to manipulate
  obs_df <- copy(df)
  
  # try to get features associated with fips code
  check_fips <- data.frame(cust_df[cust_df[[fipsCodeColumn]] == as.numeric(fips_code), countyFeatures])
  # take only columns we have info on
  check_fips <- check_fips[,colSums(is.na(check_fips))<nrow(check_fips)]
  if (all(is.na(check_fips)) == FALSE & nrow(check_fips) != 0) {
    out <- aggregate(.~check_fips[[fipsCodeColumn]], data=check_fips, FUN=mean, na.rm=TRUE)
    out[[fipsCodeColumn]] <- NULL
    names(out)[names(out) == "check_fips[[fipsCodeColumn]]"] <- fipsCodeColumn
    out[, 1] <- NULL
    obs_df <- cbind(obs_df, out)
  } 
  # get list price for each product
  UnitPrices <- unlist(lapply(obs_df$Product, function(i) products_df[products_df$Product == as.character(i), 'Price']))
  obs_df[, priceColumn] <- as.numeric(UnitPrices)
  
  # fill fips code
  obs_df[, c(fipsCodeColumn)] <- fips_code
  
  # get corresponding order level variables 
  obs_df[, totalQColumn] <- totalQuantity
  obs_df[, avgPriceColumn] <- avgPrice
  obs_df[, 'RETAIL_AMT'] <- totalQuantity*avgPrice
  obs_df[, 'YearTimesLPProduct'] <- marketYear * obs_df[, ..priceColumn]
  
  obs_df[, 'Product'] <- NULL
  obs_df[, 'Quantity'] <- NULL
  
  # make features not observed for new customer NA 
  empty_cols <- features[which(!features %in% names(obs_df))]
  len_empty_cols <- length(empty_cols)
  if(len_empty_cols > 0) {
    obs_df[, empty_cols] <- NA
  }
  
  obs_df
}

#' Generates a vector of boostrapped discount predictions for an existing customer
#'  and single product for soy 
#' 
#' @title predict_discount_raw_soy_new_cust
#' @param product string; name of product discount prediction is for
#' @param quantity numeric; quantity of product discount is for
#' @param logTransformDiscount bool; should predictions be transformed from log form before display
#'  in UI
#' @param priceColumn string; name of column that tracks price in cust_df
#' @param features vector; vector of features that enter the saved discount models used in UI
#' @param countyFeatures vector; vector of features that correspond to county to get for new
#'   customer modeling 
#' @param cust_df object; data frame of historical product level records 
#' @param discountModel object; list of boostrapped models to use to generate discount predictions
#' @param pllel bool; should predictions be done in parallel
#' @param avgPrice numeric; average retail price of customer order
#' @param totalQuantity numeric; total quantity in customer order
#' @param yearColumn string; name of column that tracks year in cust_df
#' @param products_df object; data frame of relevant products and prices
#' @param totalQColumn string; name of column that tracks total order quantity in cust_df
#' @param avgPriceColumn string; name of column that tracks order avg price in cust_df
#' @param marketYear numeric; current market year
#' @param fipsCodeColumn string; name of column that tracks fips code in cust_df
#' @return discounts, vector; vector of boostrapped unit discount predictions 
#' 
predict_discount_raw_soy_new_cust <- function(product,
                                              quantity,
                                              fips_code, 
                                              logTransformDiscount,
                                              priceColumn,
                                              features,
                                              countyFeatures,
                                              cust_df,
                                              discountModel,
                                              pllel,
                                              avgPrice,
                                              totalQuantity,
                                              products_df,
                                              totalQColumn,
                                              avgPriceColumn,
                                              marketYear,
                                              fipsCodeColumn) {
  # use 'process_cart' to create test data set for prediction
  test_df <- process_cart_soy_new_cust(product,
                                       quantity,
                                       fips_code, 
                                       priceColumn,
                                       features,
                                       countyFeatures,
                                       cust_df,
                                       avgPrice,
                                       totalQuantity,
                                       products_df,
                                       totalQColumn,
                                       avgPriceColumn,
                                       marketYear,
                                       fipsCodeColumn)
  # predict using that data
  discounts <- predictXGBoost(discountModel, 
                              test_df, 
                              features,
                              logTransform = logTransformDiscount,
                              pllel = pllel)
  
  # return vector of bootstrapped predictions
  discounts
}

#' Processes customer inputs to generate effective price predictions new customer for soy 
#'
#' @param new_port object; updated customer portfolio of products and quantities after selection 
#'  of most recent product
#' @param EffPrice object; historical customer basket level records
#' @param fips_code numeric; fips code of new customer's county
#' @param features_effP vector; vector of features that enter the saved effective price 
#'  models used in UI
#' @param epAvgPriceColumn string; name of column that tracks retail price in EffPrice
#' @param epTotalQColumn string; name of column that tracks order total quantity in EffPrice
#' @param epFipsCodeColumn string; column that measures fips code in EffPrice
#' @param epCountyFeatures vector; vector of features that are measured at county level that could be 
#'  used for new customer prediction based on zip code
#'  @param marketYear numeric; current year
#'  @param products_df object; data frame of unique hybrid-price combinations 
#' @return obs_df object; dataframe to use to make effective price predictions
#'
process_cart_ep_soy_new_cust <- function(new_port,
                                         EffPrice, 
                                         fips_code, 
                                         features_effP,
                                         epAvgPriceColumn,
                                         epTotalQColumn,
                                         epFipsCodeColumn,
                                         epCountyFeatures,
                                         marketYear,
                                         products_df
                                         ){
  # copy to manipulate
  obs_df <- copy(new_port)
  
  # fill fips code
  obs_df[, c(epFipsCodeColumn)] <- as.numeric(fips_code)
  
  # get list price for each product
  UnitPrices <- unlist(lapply(obs_df$Product, function(i) products_df[products_df$Product == as.character(i), 'Price']))
  obs_df[, 'UnitPrice'] <- as.numeric(UnitPrices)
  
  # calculate order level total stats 
  avg_retail_price <- sum(obs_df$Quantity * obs_df$UnitPrice)/sum(obs_df$Quantity)
  ordered_quantity <- sum(obs_df$Quantity)
  retail_amt <- sum(obs_df$Quantity * obs_df$UnitPrice)
  
  tmp <- data.table()
  tmp[, epAvgPriceColumn] <- avg_retail_price
  tmp[, epTotalQColumn] <- ordered_quantity
  tmp[, 'YearTimesLPBasket'] <- marketYear * tmp[[epAvgPriceColumn]]
  tmp[, 'RETAIL_AMT'] <- retail_amt
  
  obs_df <- cbind(tmp, obs_df[1, ])
  
  # model does not take these columns as a predictor
  obs_df[, 'Product'] <- NULL
  obs_df[, 'Quantity'] <- NULL
  obs_df[, 'UnitPrice'] <- NULL
  
  # make features not observed for new customer NA 
  empty_cols <- features_effP[which(!features_effP %in% names(obs_df))]
  len_empty_cols <- length(empty_cols)
  if(len_empty_cols > 0) {
    obs_df[, empty_cols] <- NA
  }
  
  # order features correctly 
  obs_df <- obs_df[, ..features_effP]
  
  obs_df
}

#' Generates a vector of effective price predictions for a new customer
#'  and associated portfolio for soy 
#' 
#' @title predict_ep_new_cust_soy
#' @param new_port object; updated customer portfolio of products and quantities after selection of most recent product
#' @param fips_code string; fips associated with customer county
#' @param logTransformEP bool; should predictions be transformed from log form before display
#'  in UI
#' @param features_effP vector; vector of features that enter the saved effective price 
#'  models used in UI
#' @param EffPrice object; historical customer basket level records
#' @param epAvgPriceColumn string; name of column that tracks retail price in EffPrice
#' @param epTotalQColumn string; name of column that tracks order total quantity in EffPrice
#' @param epCountyFeatures vector; vector of features that are measured at county level that could be 
#'  used for new customer prediction based on fips code
#' @param epModel object; model used to make effective price predictions
#' @param marketYear numeric; current year
#' @param products_df object; data frame of unique hybrid-price combinations
#' @return prices, vector of effective price predictions for all products in new_port 
#'
predict_ep_new_cust_soy <- function(new_port,
                                    EffPrice, 
                                    fips_code, 
                                    features_effP,
                                    epAvgPriceColumn,
                                    epTotalQColumn,
                                    epFipsCodeColumn,
                                    epCountyFeatures,
                                    epModel,
                                    marketYear,
                                    products_df) {
  
  test_df <- process_cart_ep_soy_new_cust(new_port,
                                          EffPrice, 
                                          fips_code, 
                                          features_effP,
                                          epAvgPriceColumn,
                                          epTotalQColumn,
                                          epFipsCodeColumn,
                                          epCountyFeatures,
                                          marketYear,
                                          products_df)
  
  # predict using that data with saved models
  model <- epModel
  prices <- predict(model, data.matrix(test_df))
  
  # Convert basket level predictions to product level predictions
  ConversiontoProduct <- cbind(new_port, prices)
  ConversiontoProduct[, PercentageofBasket := Quantity / sum(Quantity)]
  UnitPrices <- unlist(lapply(ConversiontoProduct$Product, 
                              function(i) products_df[products_df$Product == as.character(i), 'Price']))
  ConversiontoProduct[, 'UnitPrice'] <- as.numeric(UnitPrices)
  ConversiontoProduct$alpha <- ConversiontoProduct$PercentageofBasket*ConversiontoProduct$UnitPrice
  ConversiontoProduct[, Percent := prices / sum(alpha)]
  ConversiontoProduct$PredictedProductEffectivePrice <- ConversiontoProduct$Percent * ConversiontoProduct$UnitPrice
  
  ConversiontoProduct
}

#' Gets value proposition and competitior vs county yields for UI County Competitive Info table for soy tab
#' NOTE--Eventually this functionality will be generalizable to corn, value prop needs to be corrected for
#' corn, we are continuing to use the old functionality for corn because that data is correct
#' 
#' @title makeCompTable
#' @param YieldAdv object; data frame from file YieldAdvantageSoy.csv and (equivalent for corn in the future) 
#' @param fips_code numeric; fips code of customer's county
#' @param stateCountyToFips object; crosswalk of state and county to fips codes
#' @param compTableNamesChannel vector; formatted columns to use in county competitive information 
#'  table when Channel information is available
#' @param compTableNamesOther vector; formatted columns to use in county competitive information 
#'  table when Channel information is NOT available (i.e. use other Bayer hybrids)
#' @return comp_table, object; formatted competitive information table for rendering in MARS UI
#' 
makeCompTable <- function(YieldAdv,
                         fips_code,
                         stateCountyToFips,
                         compTableNamesChannel,
                         compTableNamesOther){
  
  # get yield advantage data corresponding to fips_code
  tmp <- YieldAdv[YieldAdv$fips == fips_code, ]

  # if nothing on fips code return empty table 
  if (is.null(fips_code) | is.na(fips_code) | length(fips_code) == 0){
    
    comp_table <- setNames(data.frame(matrix(ncol = length(compTableNamesChannel), nrow = 0)), 
                                 compTableNamesChannel)
    return(comp_table)
  } else if (nrow(tmp) == 0){
    comp_table <- setNames(data.frame(matrix(ncol = length(compTableNamesChannel), nrow = 0)), 
                           compTableNamesChannel)
    
    }else {
  
  # if fips does exist, get most recent year observed for county
  tmp_recent <- tmp[tmp$YEAR == max(tmp$YEAR), ]
  
  # check if Channel brand exists
  # if they don't exist, use other Bayer brands instead and correctly format table
  if (nrow(tmp_recent[tmp_recent$ValuePropDifferentiator == 'Channel', ])==0){
    
    # make new table to fill
    comp_table_other <- data.frame(matrix(ncol=length(compTableNamesOther),
                                          nrow=1))
    names(comp_table_other) <- compTableNamesOther
    # county
    comp_table_other[1, compTableNamesOther[1]] <- stringr::str_to_title(tmp_recent$COUNTY[1])
    # state
    comp_table_other[1, compTableNamesOther[2]] <- stringr::str_to_title(tmp_recent$STATE[1])
    # Bayer yield
    comp_table_other[1, compTableNamesOther[3]] <- as.integer(tmp_recent[tmp_recent$Brand == 'Bayer',
                                                                         'YieldbyBrand'])
    # competitor yield
    comp_table_other[1, compTableNamesOther[4]] <- as.integer(tmp_recent[tmp_recent$Brand == 'Competitor',
                                                                         'YieldbyBrand'])
    
    # value prop --showing value prop calculation based on actual price paid not retail price
    comp_table_other[1, compTableNamesOther[5]] <- format(round(tmp_recent$ValuePropPaid[1], 
                                                                2), 
                                                          nsmall=2)
    
    # competitive position
    if (comp_table_other[1, compTableNamesOther[5]] > 0) {
      comp_table_other[1, 'Competitive Position'] <- 'STRONG'
    } else if (comp_table_other[1, compTableNamesOther[5]] < 0) { 
      comp_table_other[1, 'Competitive Position'] <- 'WEAK'
    }
    comp_table <- comp_table_other
    names(comp_table) <- compTableNamesOther
    if(nrow(comp_table) == 0){
      comp_table <- setNames(data.frame(matrix(ncol = length(compTableNamesChannel), nrow = 0)), 
                             compTableNamesChannel)
    }
    return(comp_table)
  } else {
    
    # make new table to fill
    comp_table_channel <- data.frame(matrix(ncol=length(compTableNamesChannel),
                                            nrow=1))
    # name columns for output in UI
    names(comp_table_channel) <- compTableNamesChannel
    
    # county
    comp_table_channel[1, compTableNamesChannel[1]] <- stringr::str_to_title(tmp_recent$COUNTY[1])
    # state
    comp_table_channel[1, compTableNamesChannel[2]] <- stringr::str_to_title(tmp_recent$STATE[1])
    # Channel yield
    comp_table_channel[1, compTableNamesChannel[3]] <- as.integer(tmp_recent[tmp_recent$Brand == 'Channel',
                                                                             'YieldbyBrand'])
    # competitor yield
    comp_table_channel[1, compTableNamesChannel[4]] <- as.integer(tmp_recent[tmp_recent$Brand == 'Competitor',
                                                                             'YieldbyBrand'])
    # value prop --showing value prop calculation based on actual price paid not retail price
    comp_table_channel[1, compTableNamesChannel[5]] <- format(round(tmp_recent$ValuePropPaid[1], 
                                                                    2), 
                                                              nsmall=2)
    # competitve position
    if (comp_table_channel[1, compTableNamesChannel[5]] > 0) {
      comp_table_channel[1, 'Competitive Position'] <- 'STRONG'
    } else if (comp_table_channel[1, compTableNamesChannel[5]] < 0) { 
      comp_table_channel[1, 'Competitive Position'] <- 'WEAK'
    }
    comp_table <- comp_table_channel
    if(nrow(comp_table_channel) == 0){
      comp_table <- setNames(data.frame(matrix(ncol = length(compTableNamesChannel), nrow = 0)), 
                             compTableNamesChannel)
    }
    return(comp_table)
  }
}
}

#' Calculate number of days after August 1, the start of sales season
#'
#' @title CalculateTimefromStartofSeason
#' @param OrderDate Vector containing the date of order In this format: %m/%d/%y
#' @return A vector of how many days after August 1 an order was initiated
#' 
CalculateTimefromStartofSeason<-function(OrderDate){
  
  TimefromStartofSeason<-as.numeric(as.Date(format(OrderDate, format="%m-%d"),
                                            format = "%m-%d") - as.Date("08-01", format = "%m-%d"))
  TimefromStartofSeason<-ifelse(TimefromStartofSeason < 0,
                                365 + TimefromStartofSeason, TimefromStartofSeason)
  return(TimefromStartofSeason)
}

#' Pre-populates customer portfolio for corn
#' 
#'@title prePopulatePortfolioCornRaw
#'@param cust_id numeric; customer identifier
#'@param seedsmanID numeric; seedsman identifier
#'@param cust_df object; data frame of historical product level records for corn
#'@param EffPrice object; data frame of historical order level records for effective price for corn
#'@param discountModel object; list bootstrapped discount models to make predictions with
#'@param features vector; vector of features to make predictions using discountModel
#'@param epModel object; effective price model to make predictions with
#'@param features_effP vector; vector of features to make predictions using epModel
#'@param custIDColumn string; name of column tracking customer identifier in cust_df
#'@param epCustIDColumn string; name of column tracking customer identifier in EffPrice
#'@param seedsmanIDColumn string; name of column tracking seedsman identifier in cust_df
#'@param yearColumn string; name of column tracking year in cust_df
#'@param epYearColumn string; name of column tracking year in EffPrice
#'@param productColunn string; name of column tracking product in cust_df
#'@param priceColumn string; name of column tracking price in cust_df
#'@param quantityColumn string; name of column tracking quantity in cust_df
#'@param fipsCodeColumn string; name of column tracking fips code in cust_df
#'@param discountColumn string; name of column tracking per-unit discount in cust_df
#'@param countyFeatures vector; vector of strings identifying features associated with the customer's fips code in cust_df
#'@param cartDropCols vector; vector of strings identifying features that are a direct function of user input for discountModel
#'@param effectivePriceColumn string; name of column tracking effective/farmgate price in EffPrice
#'@param epCartDropCols vector; vector of strings identifying features that are a direct function of user input for epModel
#'@param epAvgPriceColumn string; name of column tracking average order retail price in EffPrice
#'@param epTotalQColumn string; name of column tracking order quantity in EffPrice
#'@param epCountyFeatures vector; vector of features identifying features that are associated with the customer's fips code in EffPrice
#'@param epFipsCodeColumn string; name of column tracking fips code in EffPrice
#'@param products_df object; list of unique hybird-price combinations
#'@param marketYear numeric; current market year
#'@param logTransformDiscount bool; should discount predictions be log transformed before display
#'@param logTransformEP bool; should effective price predictions be log transformed before display
#'@param pllel bool; should predictions be made in parallel
#'@return list; 1. formatted cart df for display in ui (includes discount and effective price predictions), 
#'        2. distribution of discounts for entire order to be plotted
prePopulatePortfolioCornRaw <- function(cust_id,
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
                                        predict_discount){
  
  # get most recent purchase behavior at the product level
  discountTmp <- cust_df[cust_df[[custIDColumn]] == cust_id & cust_df[[seedsmanIDColumn]] == seedsmanID, ]
  recentDiscount <-  discountTmp[discountTmp[[yearColumn]] == max(discountTmp[[yearColumn]]), ]
  
  # make data for predictions 
  port_tmp <- data.table(Product = recentDiscount[[productColumn]],
                         Quantity = as.integer(recentDiscount[[quantityColumn]]))
  port_tmp <- merge(port_tmp, 
                    products_df, 
                    by = "Product")
  # if no products customer previously bought are still being sold, return NULL
  if (nrow(port_tmp) == 0) {
    return(NULL)
  } else {
  
  # merge if same product was bought more than once in most recent purchase
  cart_df <- port_tmp[, .(Quantity = sum(Quantity),
                          Price = mean(Price)),
                      by = Product]
  
  # extract order level stats to make predictions
  avgPrice <- mean(cart_df$Price)
  totalQuantity <- sum(cart_df$Quantity)
  
  # # effective price # CURRENTLY DEPRECATED BASED ON REQUESTS OF BUSINESS 
  # prices <- predict_ep(cart_df,
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
  # # update all effective prices in cart
  # prices <- as.data.frame(prices)
  # PredPrices <- unlist(lapply(cart_df$Product, function(i) prices[prices$Product == as.character(i), 
  #                                                                 'PredictedProductEffectivePrice']))
  # cart_df[, EffectivePrice := paste0('$', format(round(PredPrices, 2), nsmall = 2))]
  
  # format price
  cart_df[, Price := paste0('$', format(cart_df$Price, nsmall=2))]
  
  # make predictions at the product level 
  tmp <- list()
  for (i in 1:nrow(cart_df)){
    product_tmp <- unlist(cart_df[i, 'Product'])
    quantity_tmp <- unlist(cart_df[i, 'Quantity'])
    new_row_tmp <- cart_df[i, c('Product', 'Quantity', 'Price')]
    discounts_tmp <- predict_discount(product_tmp,
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
    
    # get mean predicted disount as percentage of list price
    discountPct <- paste0(format(round(as.numeric((mean(discounts_tmp)/as.numeric(gsub("[\\$,]", "", cart_df[i, Price])) * 100)), 2), nsmall=2),
                          '%')
    # round mean of discount predicitons to 2 decimal places and place into 
    # relevant cart_df row
    cart_df[i, Discount := paste0('$', format(round(mean(discounts_tmp), 2), nsmall=2))]
    
    # put discount as percentage of list price into relevant cart_df row 
    cart_df[i, DiscountPCT := discountPct ]
    
    # # update purchase history rows in relevant cart_df row
    # # last unit discount
    # cart_df[i, LastUnitDiscount :=  get_last_unit_discount(new_row_tmp,
    #                                                        cust_id, 
    #                                                        cust_df, 
    #                                                        custIDColumn, 
    #                                                        productColumn,
    #                                                        yearColumn, 
    #                                                        discountColumn,
    #                                                        seedsmanID,
    #                                                        seedsmanIDColumn)]
    # last unit discount percent 
    cart_df[i, LastUnitDiscountPCT := get_last_unit_discount_pct(new_row_tmp,
                                                                cust_id,
                                                                cust_df,
                                                                custIDColumn,
                                                                productColumn,
                                                                yearColumn,
                                                                discountColumn,
                                                                seedsmanID,
                                                                seedsmanIDColumn,
                                                                priceColumn)]
    
    # last unit discount pro-forma calculation based on current list price and last discount percent 
    cart_df[i, LastUnitDiscount := paste0('$', format(round((as.numeric(gsub("[\\%,]", "", 
                                                  cart_df[i, LastUnitDiscountPCT])) / 100) * as.numeric(gsub("[\\$,]", "", cart_df[i, Price])), 2), 
                                                  nsmall=2))]
    
    # # last effective price CURRENLY DEPRECATED PER REQUEST OF THE BUSINESS
    # cart_df[i, LastUnitEP :=  get_last_unit_ep(new_row_tmp,
    #                                            cust_id,
    #                                            EffPrice,
    #                                            epCustIDColumn,
    #                                            epYearColumn,
    #                                            effectivePriceColumn,
    #                                            epSeedsmanIDColumn,
    #                                            seedsmanID)]
    
    # taking min of predicted and last unit discount for percent and dollar amount 
    checkDiscounts <- list(as.numeric(gsub("[\\$,]", "", cart_df[i, Discount])), as.numeric(gsub("[\\$,]", "", cart_df[i, LastUnitDiscount])))
    checkDiscountsPCT <- list(as.numeric(gsub("[\\%,]", "", cart_df[i, DiscountPCT])), as.numeric(sub("[\\%,]", "", cart_df[i, LastUnitDiscountPCT])))
    cart_df[i, RecDiscount := paste0('$', format(round(min(unlist(checkDiscounts)), 2), nsmall=2))]
    cart_df[i, RecDiscountPCT := paste0(format(round(min(unlist(checkDiscountsPCT)), 2), nsmall=2), '%')]
    
    # last purchased quantity
    cart_df[i, LastPurchasedQuantity := get_last_quantity(new_row_tmp,
                                                          cust_id,
                                                          cust_df,
                                                          custIDColumn, 
                                                          productColumn, 
                                                          yearColumn,
                                                          quantityColumn)]
    # save discounts to plot (entire order)
    tmp <-append(tmp, list(discounts_tmp))
  }
  
  # unlist for output
  discounts_out <- unlist(tmp)
  
  # return formatted cart_df, discount distribution for entire order
  list(cart_df, discounts_out)
  }
}

#'Pre-populates customer portfolio for soy
#'
#'@param cust_id numeric; customer identifier
#'@param seedsmanID numeric; seedsman identifier
#'@param cust_df object; data frame of historical product level records for soy
#'@param EffPrice object; data frame of historical order level records for effective price for soy
#'@param discountModel object; list bootstrapped discount models to make predictions with
#'@param features vector; vector of features to make predictions using discountModel
#'@param epModel object; effective price model to make predictions with
#'@param features_effP vector; vector of features to make predictions using epModel
#'@param custIDColumn string; name of column tracking customer identifier in cust_df
#'@param epCustIDColumn string; name of column tracking customer identifier in EffPrice
#'@param seedsmanIDColumn string; name of column tracking seedsman identifier in cust_df
#'@param yearColumn string; name of column tracking year in cust_df
#'@param epYearColumn string; name of column tracking year in EffPrice
#'@param productColunn string; name of column tracking product in cust_df
#'@param priceColumn string; name of column tracking price in cust_df
#'@param quantityColumn string; name of column tracking quantity in cust_df
#'@param fipsCodeColumn string; name of column tracking fips code in cust_df
#'@param discountColumn string; name of column tracking per-unit discount in cust_df
#'@param countyFeatures vector; vector of strings identifying features associated with the customer's fips code in cust_df
#'@param cartDropCols vector; vector of strings identifying features that are a direct function of user input for discountModel
#'@param effectivePriceColumn string; name of column tracking effective/farmgate price in EffPrice
#'@param epCartDropCols vector; vector of strings identifying features that are a direct function of user input for epModel
#'@param epAvgPriceColumn string; name of column tracking average order retail price in EffPrice
#'@param epTotalQColumn string; name of column tracking order quantity in EffPrice
#'@param epCountyFeatures vector; vector of features identifying features that are associated with the customer's fips code in EffPrice
#'@param epFipsCodeColumn string; name of column tracking fips code in EffPrice
#'@param products_df object; list of unique hybird-price combinations
#'@param marketYear numeric; current market year
#'@param logTransformDiscount bool; should discount predictions be log transformed before display
#'@param logTransformEP bool; should effective price predictions be log transformed before display
#'@param pllel bool; should predictions be made in parallel
#'@return list; 1. formatted cart df for display in ui (includes discount and effective price predictions), 
#'        2. distribution of discounts for entire order to be plotted
#'
prePopulatePortfolioSoyRaw <- function(cust_id,
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
                                       predict_discount_soy){
  
  # get most recent purchase behavior at the product level
  discountTmp <- cust_df[cust_df[[custIDColumn]] == cust_id & cust_df[[seedsmanIDColumn]] == seedsmanID, ]
  recentDiscount <-  discountTmp[discountTmp[[yearColumn]] == max(discountTmp[[yearColumn]]), ]
  
  # make data for predictions 
  port_tmp <- data.table(Product = recentDiscount[[productColumn]],
                         Quantity = as.integer(recentDiscount[[quantityColumn]]))
  port_tmp <- merge(port_tmp, 
                    products_df, 
                    by = "Product")
  # if all of products in customer's most recent order have been discontinued, function return NULL
  if (nrow(port_tmp) == 0){
    return(NULL)
  } else {
  
  # merge if same product was bought more than once in most recent purchase
  cart_df <- port_tmp[, .(Quantity = sum(Quantity),
                          Price = mean(Price)),
                      by = Product]
  # get order level stats
  avgPrice <- mean(cart_df$Price)
  totalQuantity <- sum(cart_df$Quantity)
  
  # # make effective price predictions CURRENTLY DEPRECATED PER REQUEST OF THE BUSINESS
  # prices <- predict_ep_soy(cart_df,
  #                          cust_id, 
  #                          EffPrice, 
  #                          logTransformEP,
  #                          epCustIDColumn,
  #                          epYearColumn,
  #                          epCartDropCols,
  #                          features_effP,
  #                          epAvgPriceColumn,
  #                          epTotalQColumn,
  #                          epModel,
  #                          marketYear,
  #                          products_df,
  #                          epCountyFeatures,
  #                          epFipsCodeColumn,
  #                          epSeedsmanIDColumn,
  #                          seedsmanID)
  # # update effective price for whole cart
  # prices <- as.data.frame(prices)
  # PredPrices <- unlist(lapply(cart_df$Product, function(i) prices[prices$Product == as.character(i), 
  #                                                                 'PredictedProductEffectivePrice']))
  # 
  # cart_df[, EffectivePrice := paste0('$', format(round(PredPrices, 2), nsmall = 2))]
  
  # format price
  cart_df[, Price := paste0('$', format(cart_df$Price, nsmall=2))]
  
  # apply predict discount function to each row in cart
  tmp <- list()
  for (i in 1:nrow(cart_df)){
    product_tmp <- unlist(cart_df[i, 'Product'])
    quantity_tmp <- unlist(cart_df[i, 'Quantity'])
    new_row_tmp <- cart_df[i, c('Product', 'Quantity', 'Price')]
    discounts_tmp <- predict_discount_soy(product_tmp,
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
                                          marketYear,
                                          products_df,
                                          totalQColumn,
                                          avgPriceColumn,
                                          seedsmanIDColumn,
                                          seedsmanID,
                                          fipsCodeColumn,
                                          countyFeatures)
    
    # get mean predicted disount as percentage of list price
    discountPct <- paste0(format(round(as.numeric((mean(discounts_tmp)/as.numeric(gsub("[\\$,]", "", cart_df[i, Price])) * 100)), 2), nsmall=2),
                          '%')
    # round mean of discount predicitons to 2 decimal places and place into 
    # relevant cart_df row
    cart_df[i, Discount := paste0('$', format(round(mean(discounts_tmp), 2), nsmall=2))]
    
    # put discount as percentage of list price into relevant cart_df row 
    cart_df[i, DiscountPCT := discountPct ]
    
    # update purchase history columns in cart_df
    # # unit discount
    # cart_df[i, LastUnitDiscount :=  get_last_unit_discount(new_row_tmp,
    #                                                        cust_id, 
    #                                                        cust_df, 
    #                                                        custIDColumn, 
    #                                                        productColumn,
    #                                                        yearColumn, 
    #                                                        discountColumn,
    #                                                        seedsmanID,
    #                                                        seedsmanIDColumn)]
    # last unit discount percent 
    cart_df[i, LastUnitDiscountPCT := get_last_unit_discount_pct(new_row_tmp,
                                                                 cust_id,
                                                                 cust_df,
                                                                 custIDColumn,
                                                                 productColumn,
                                                                 yearColumn,
                                                                 discountColumn,
                                                                 seedsmanID,
                                                                 seedsmanIDColumn,
                                                                 priceColumn)]
    
    # last unit discount pro-forma calculation based on current list price and last discount percent 
    cart_df[i, LastUnitDiscount := paste0('$', format(round((as.numeric(gsub("[\\%,]", "", 
                                                                             cart_df[i, LastUnitDiscountPCT])) / 100) * as.numeric(gsub("[\\$,]", "", cart_df[i, Price])), 2), 
                                                      nsmall=2))]
    
    # # effective price CURRENTLY DEPRECATED PER REQUEST OF BUSINESS
    # cart_df[i, LastUnitEP :=  get_last_unit_ep(new_row_tmp,
    #                                            cust_id, 
    #                                            EffPrice, 
    #                                            epCustIDColumn, 
    #                                            epYearColumn, 
    #                                            effectivePriceColumn,
    #                                            epSeedsmanIDColumn, 
    #                                            seedsmanID)]
    
    # taking min of predicted and last unit discount for percent and dollar amount 
    checkDiscounts <- list(as.numeric(gsub("[\\$,]", "", cart_df[i, Discount])), as.numeric(gsub("[\\$,]", "", cart_df[i, LastUnitDiscount])))
    checkDiscountsPCT <- list(as.numeric(gsub("[\\%,]", "", cart_df[i, DiscountPCT])), as.numeric(gsub("[\\%,]", "", cart_df[i, LastUnitDiscountPCT])))
    cart_df[i, RecDiscount := paste0('$', format(round(min(unlist(checkDiscounts)), 2), nsmall=2))]
    cart_df[i, RecDiscountPCT := paste0(format(round(min(unlist(checkDiscountsPCT)), 2), nsmall=2), '%')]
    
    # last purchased quantity
    cart_df[i, LastPurchasedQuantity := get_last_quantity(new_row_tmp,
                                                          cust_id,
                                                          cust_df,
                                                          custIDColumn, 
                                                          productColumn, 
                                                          yearColumn,
                                                          quantityColumn)]
    # save discounts for plotting entire order and other relevant statistics
    tmp <-append(tmp, list(discounts_tmp))
  }
  
  # unlist discount distribution
  discounts_out <- unlist(tmp)
  
  # returb formatted cart_df and distribution of discounts for entire order
  list(cart_df, discounts_out)
  }
}



grower_report_updated_1_raw0 <- function(fsr_as_string, seedsman_as_string, grower_report) {
  grower_report_updated <- grower_report
  grower_report_updated = filter(grower_report_updated ,FSR==fsr_as_string )
  grower_report_updated = filter(grower_report_updated , grower_report_updated[,"Seedsman SAPID"] == seedsman_as_string)
  grower_report_updated <- grower_report_updated %>% mutate(totalLCRdiff = grower_report_updated[,"Observed Total LCR"]  - grower_report_updated[,"Predicted Total LCR"] )
  grower_report_updated <- grower_report_updated[,c("Seedsman Account Name",
                                                    "Grower Account Name",
                                                    "Product",
                                                    "Fiscal Year of Purchase",
                                                    "totalLCRdiff")]
  grower_report_updated[,2] <- as.factor(grower_report_updated[,2]) #factorize Grower Names for filtering
  grower_report_updated <- grower_report_updated[order(-grower_report_updated[,5]),]
  colnames(grower_report_updated) <- c("Seedsman Accou nt Name",
                                       "Grower Account Name",
                                       "Product",
                                       "Year",
                                       "Total LCR Difference")

  recentYear <- max(grower_report_updated$Year)
  grower_report_updated <- grower_report_updated[ which(grower_report_updated$Year==recentYear),]
  grower_report_updated[,5] <- as.numeric(format(round(grower_report_updated[,5], 2), nsmall = 2))
  grower_report_updated
}



grower_report_updated_2_raw0 <- function(fsr_as_string,grower_report) {
  grower_report_updated <- grower_report
  grower_report_updated = filter(grower_report_updated ,FSR==fsr_as_string )
  grower_report_updated <- grower_report_updated %>% mutate(totalLCRdiff = grower_report_updated[,"Observed Total LCR"]  - grower_report_updated[,"Predicted Total LCR"] )
  grower_report_updated <- grower_report_updated[,c("Seedsman Account Name", "Fiscal Year of Purchase", "totalLCRdiff")]
  colnames(grower_report_updated) <- c("Seedsman Account Name", "Year", "Total LCR Difference")

  recentYear <- max(grower_report_updated$Year)
  grower_report_updated <- grower_report_updated[ which(grower_report_updated$Year==recentYear),]
  grower_report_updated <- aggregate(grower_report_updated[, 3], list(grower_report_updated[, 1]), mean)
  colnames(grower_report_updated) <- c("Seedsman Account Name", "Average Total LCR Difference")
  grower_report_updated <- grower_report_updated[order(-grower_report_updated[,2]),]
  grower_report_updated[,1] <- as.factor(grower_report_updated[,1])
  grower_report_updated[,2] <- as.numeric(format(round(grower_report_updated[,2], 2), nsmall = 2))
  grower_report_updated
}



grower_report_stats_raw0 <- function(grower_report) {
  grower_report_updated <- grower_report
  grower_report_stats_Mean <- mean(grower_report_updated[,"Observed Total LCR"]  - grower_report_updated[,"Predicted Total LCR"])
  grower_report_stats_StDev <- sd(grower_report_updated[,"Observed Total LCR"]  - grower_report_updated[, "Predicted Total LCR"])
  grower_report_stats <- data.frame(Mean = grower_report_stats_Mean, StDev = grower_report_stats_StDev)
}



seedsmanInfo1function_raw <- function(grower_report_updated_1,fsr_as_string) {
  datatable(grower_report_updated_1, rownames= FALSE, filter = "top", extensions = "Buttons",  options = list(lengthMenu = list( c(20, 50, 100, -1), c(20, 50, 100, "All")), scrollX=TRUE, pageLength = 25, dom = "Blfrtip", buttons = 
                                                                                                                list( list( extend = 'collection',
                                                                                                                            buttons = list(list(extend='excel', title = NULL,
                                                                                                                                                filename = paste("Your Seedsman CORN", "--", gsub(",", "", fsr_as_string) , "--", Sys.Date(), sep ='' ) ),
                                                                                                                                           list(extend='csv', title = NULL,
                                                                                                                                                filename = paste("Your Seedsman CORN", "--", gsub(",", "", fsr_as_string), "--", Sys.Date(), sep ='' ) ),
                                                                                                                                           list(extend='pdf', title = NULL,
                                                                                                                                                filename = paste("Your Seedsman CORN", "--", gsub(",", "", fsr_as_string) , "--", Sys.Date(), sep ='' ) )
                                                                                                                            ),
                                                                                                                            text = c('Download Selected Data')
                                                                                                                )),
                                                                                                              lengthMenu = list( c(10, 20, -1), c(10, 20, "All"))
  ))  %>%
    formatCurrency(c('Total LCR Difference')) #%>%
  #formatStyle('Total LCR Difference', target = "row", fontWeight = styleInterval(c(grower_report_stats()$Mean - grower_report_stats()$StDev/2, grower_report_stats()$Mean + grower_report_stats()$StDev/2), c('bold', 'normal', 'bold'))) %>%
  #formatStyle(
  # 'Total LCR Difference', target = "row",
  #  backgroundColor  = styleInterval(c(grower_report_stats()$Mean - grower_report_stats()$StDev/2, grower_report_stats()$Mean + grower_report_stats()$StDev/2) , c('#76AF74', '#FEE26F', '#F15B31'))
  #)
}



seedsmanInfo2function_raw <- function(grower_report_updated_2, grower_report_stats,fsrString) {
  
  datatable(grower_report_updated_2, rownames= FALSE, filter = "top", extensions = "Buttons", 
            options = list(lengthMenu = list( c(20, 50, 100, -1), c(20, 50, 100, "All")), scrollX=TRUE, 
                           pageLength = 25, dom = "Blfrtip",
                                                                                                             buttons = list( list( extend = 'collection',
                                                                                                                                   buttons = list(
                                                                                                                                     list(extend='excel', title = NULL,
                                                                                                                                          filename = paste("Compare Seedsman CORN", "--", gsub(",", "", fsrString) , "--", Sys.Date(), sep ='' ) ),
                                                                                                                                     list(extend='csv', title = NULL,
                                                                                                                                          filename = paste("Compare Seedsman CORN", "--", gsub(",", "", fsrString), "--", Sys.Date(), sep ='' ) ),
                                                                                                                                     list(extend='pdf', title = NULL,
                                                                                                                                          filename = paste("Compare Seedsman CORN", "--", gsub(",", "", fsrString), "--", Sys.Date(), sep ='' ) )
                                                                                                                                   ),
                                                                                                                                   text = c('Download Selected Data')
                                                                                                             ))
                                                                                                             
  ))  %>%
    formatCurrency(c('Average Total LCR Difference')) %>%
    formatStyle('Average Total LCR Difference', target = "row", fontWeight = styleInterval(c(grower_report_stats$Mean - grower_report_stats$StDev/2, grower_report_stats$Mean + grower_report_stats$StDev/2), c('bold', 'normal', 'bold'))) %>%
    formatStyle(
      'Average Total LCR Difference', target = "row",
      backgroundColor  = styleInterval(c(grower_report_stats$Mean - grower_report_stats$StDev/2, grower_report_stats$Mean + grower_report_stats$StDev/2) , c('#76AF74', '#FEE26F', '#F15B31'))
    )
  
}



LCRTotals_raw0 <- function(fsr_as_string, grower_report, corn_soy){
  grower_report_updated <- grower_report
  grower_report_updated = filter(grower_report_updated ,FSR==fsr_as_string )
  
  if(nrow(grower_report_updated) > 0){
  grower_report_updated <- grower_report_updated %>% mutate(totalLCRdiff = grower_report_updated[,8]  - grower_report_updated[,9] )
  grower_report_updated <- grower_report_updated %>% mutate(observedPercentLCR = grower_report_updated$`Observed Total LCR`/grower_report_updated$`Retail Total`)
  grower_report_updated <- grower_report_updated %>% mutate(PredictedPercentLCR = grower_report_updated$`Predicted Total LCR`/grower_report_updated$`Retail Total`)
  if(corn_soy == 'corn'){
    grower_report_updated <- grower_report_updated[,c(20, 14, 1, 15, ncol(grower_report_updated)-2, ncol(grower_report_updated)-1, ncol(grower_report_updated) )]
  } else {
    grower_report_updated <- grower_report_updated[,c(19, 14, 1, 15, ncol(grower_report_updated)-2, ncol(grower_report_updated)-1, ncol(grower_report_updated) )] 
  }
  grower_report_updated <- grower_report_updated %>% mutate(Over = if_else(grower_report_updated[,5] > 0 , 1, 0))
  grower_report_updated[,2] <- as.factor(grower_report_updated[,2]) #factorize Grower Names for filtering
  grower_report_updated <- grower_report_updated[order(-grower_report_updated[,5]),]
  colnames(grower_report_updated) <- c("Seedsman Account Name", "Grower Account Name","Product", "Year", "Total LCR Difference", "Observed LCR Pct", "Predicted LCR Pct", "Over")

  recentYear <- max(grower_report_updated$Year)
  grower_report_updated <- grower_report_updated[ which(grower_report_updated$Year==recentYear),]
  grower_report_updated[,5] <- as.numeric(format(round(grower_report_updated[,5], 2), nsmall = 2))
  
  LCROver  <- filter(grower_report_updated, Over == "1")
  LCRUnder <- filter(grower_report_updated, Over == "0")
  
  LCROverGrowers <- length(unique(LCROver$`Grower Account Name`))
  LCRUnderGrowers <- length(unique(LCRUnder$`Grower Account Name`))
  
  LCROverSeedsman <- length(unique(LCROver$`Seedsman Account Name`))
  LCRUnderSeedsman <- length(unique(LCRUnder$`Seedsman Account Name`))
  
  LCROverDollars <- -sum(LCROver$`Total LCR Difference`)/1000
  LCRUnderDollars <- -sum(LCRUnder$`Total LCR Difference`)/1000
  
  LCRTotals <- rbind(data.frame("Group" = "Over", "Grower_Count" = LCROverGrowers, "Seedsman_Count" = LCROverSeedsman, "Dollars" = LCROverDollars),
                     data.frame("Group" = "Under", "Grower_Count" = LCRUnderGrowers, "Seedsman_Count" = LCRUnderSeedsman, "Dollars" = abs(LCRUnderDollars)) )}
  else{
    LCRTotals <- NULL
  }

  LCRTotals
}



LCRTotals3_raw0 <- function(fsr_as_string, grower_report, corn_soy) {
  grower_report_updated <- grower_report
  grower_report_updated = filter(grower_report_updated ,FSR==fsr_as_string )
  grower_report_updated <- grower_report_updated %>% mutate(totalLCRdiff = grower_report_updated[,8]  - grower_report_updated[,9] )
  grower_report_updated <- grower_report_updated %>% mutate(observedPercentLCR = grower_report_updated$`Observed Total LCR`/grower_report_updated$`Retail Total`)
  grower_report_updated <- grower_report_updated %>% mutate(PredictedPercentLCR = grower_report_updated$`Predicted Total LCR`/grower_report_updated$`Retail Total`)
  if(corn_soy == 'corn'){
    grower_report_updated <- grower_report_updated[,c(20, 14, 1, 15, ncol(grower_report_updated)-2, ncol(grower_report_updated)-1, ncol(grower_report_updated) )]
  } else {
    grower_report_updated <- grower_report_updated[,c(19, 14, 1, 15, ncol(grower_report_updated)-2, ncol(grower_report_updated)-1, ncol(grower_report_updated) )]
  }
  
  grower_report_updated <- grower_report_updated %>% mutate(Over = if_else(grower_report_updated[,5] > 0 , 1, 0))
  grower_report_updated[,2] <- as.factor(grower_report_updated[,2]) #factorize Grower Names for filtering
  grower_report_updated <- grower_report_updated[order(-grower_report_updated[,5]),]
  colnames(grower_report_updated) <- c("Seedsman Account Name", "Grower Account Name","Product", "Year", "Total LCR Difference", "Observed LCR Pct", "Predicted LCR Pct", "Over")

  recentYear <- max(grower_report_updated$Year)
  grower_report_updated <- grower_report_updated[ which(grower_report_updated$Year==recentYear),]
  grower_report_updated[,5] <- as.numeric(format(round(grower_report_updated[,5], 2), nsmall = 2))
  
  LCROver  <- filter(grower_report_updated, Over == "1")
  
  cuts <- cut(LCROver$`Observed LCR Pct`, breaks = c(0, 0.05, .1, .15, .2, .25, 1.0), include.lowest = TRUE)
  LCROver <- cbind(LCROver, cuts)
  if(nrow(LCROver) > 0){
  LCRGrower <- aggregate(LCROver$`Grower Account Name` ~ cuts, data=LCROver, FUN=function(x) length(unique(x)) )
  LCROverSum <- aggregate(LCROver$`Total LCR Difference` ~ cuts, data=LCROver, FUN=sum)
  LCROverPct <- aggregate(abs(LCROver$`Observed LCR Pct` - LCROver$`Predicted LCR Pct`) ~ cuts, data=LCROver, FUN=mean)
  colnames(LCRGrower) <- c("Bins", "Growers")
  colnames(LCROverSum) <- c("Bins", "Dollars")
  colnames(LCROverPct) <- c("Bins", "Percent")
  LCROverSum$Dollars <- LCROverSum$Dollars/1000
  
  LCROverSum <- data.frame(Bins = c("[0%, 5%]", "(5%, 10%]", "(10%, 15%]", "(15%, 20%]", "(20%, 25%]", "(25%, 100%]"),
                           Dollars = c(-LCROverSum$Dollars[1], -LCROverSum$Dollars[2], -LCROverSum$Dollars[3], -LCROverSum$Dollars[4],  -LCROverSum$Dollars[5], -LCROverSum$Dollars[6]),
                           Percent = c(-LCROverPct$Percent[1], -LCROverPct$Percent[2], -LCROverPct$Percent[3], -LCROverPct$Percent[4],  -LCROverPct$Percent[5], -LCROverPct$Percent[6]),
                           Growers = c(LCRGrower$Growers[1], LCRGrower$Growers[2], LCRGrower$Growers[3], LCRGrower$Growers[4],  LCRGrower$Growers[5], LCRGrower$Growers[6])
  )
  
  LCROverSum$Bins <- factor(LCROverSum$Bins, levels = c("[0%, 5%]", "(5%, 10%]", "(10%, 15%]", "(15%, 20%]", "(20%, 25%]", "(25%, 100%]"))}
  else{
    LCROverSum <- NULL
  }

  LCROverSum
}




LCRTotals4_raw0 <- function(fsr_as_string, grower_report, corn_soy) {
  grower_report_updated <- grower_report
  grower_report_updated = filter(grower_report_updated ,FSR==fsr_as_string )
  grower_report_updated <- grower_report_updated %>% mutate(totalLCRdiff = grower_report_updated$`Observed Total LCR`  - grower_report_updated$`Predicted Total LCR` )
  grower_report_updated <- grower_report_updated %>% mutate(observedPercentLCR = grower_report_updated$`Observed Total LCR`/grower_report_updated$`Retail Total`)
  grower_report_updated <- grower_report_updated %>% mutate(PredictedPercentLCR = grower_report_updated$`Predicted Total LCR`/grower_report_updated$`Retail Total`)
  if(corn_soy == 'corn'){
    grower_report_updated <- grower_report_updated[,c(20, 14, 1, 15, ncol(grower_report_updated)-2, ncol(grower_report_updated)-1, ncol(grower_report_updated) )]
  } else {
    grower_report_updated <- grower_report_updated[,c(19, 14, 1, 15, ncol(grower_report_updated)-2, ncol(grower_report_updated)-1, ncol(grower_report_updated) )]
  }

  grower_report_updated <- grower_report_updated %>% mutate(Over = if_else(grower_report_updated[,5] > 0 , 1, 0))
  grower_report_updated[,2] <- as.factor(grower_report_updated[,2]) #factorize Grower Names for filtering
  grower_report_updated <- grower_report_updated[order(-grower_report_updated[,5]),]
  colnames(grower_report_updated) <- c("Seedsman Account Name", "Grower Account Name","Product", "Year", "Total LCR Difference", "Observed LCR Pct", "Predicted LCR Pct", "Over")

  recentYear <- max(grower_report_updated$Year)
  grower_report_updated <- grower_report_updated[ which(grower_report_updated$Year==recentYear),]
  grower_report_updated[,5] <- as.numeric(format(round(grower_report_updated[,5], 2), nsmall = 2))
  
  LCROver  <- filter(grower_report_updated, Over == "1")
  LCROver <- LCROver %>% mutate(totalLCRdiffPct = abs(LCROver$`Observed LCR Pct` - LCROver$`Predicted LCR Pct` ))
  colnames(LCROver) <- c("Seedsman Account Name", "Grower Account Name","Product", "Year", "Total LCR Difference", "Observed LCR Pct", "Predicted LCR Pct", "Over", "Total LCR Difference Pct")
  
  cuts <- cut(LCROver$`Total LCR Difference Pct`, breaks = c(0, 0.02, 0.04, 0.06, 0.08, 0.1, 0.12, 1.00), include.lowest = TRUE)
  
  LCROver <- cbind(LCROver, cuts)
  
  if(nrow(LCROver) > 0){
    
  LCRGrower <- aggregate(LCROver$`Grower Account Name` ~ cuts, data=LCROver, FUN=function(x) length(unique(x)) )
  LCROverSum <- aggregate(LCROver$`Total LCR Difference` ~ cuts, data=LCROver, FUN=sum)
  LCROverPct <- aggregate(abs(LCROver$`Observed LCR Pct` - LCROver$`Predicted LCR Pct`) ~ cuts, data=LCROver, FUN=mean)
  
  colnames(LCRGrower) <- c("Bins", "Growers")
  colnames(LCROverSum) <- c("Bins", "Dollars")
  colnames(LCROverPct) <- c("Bins", "Percent")
  LCROverSum$Dollars <- LCROverSum$Dollars/1000
  
  LCROverSum <- data.frame(Bins = c("[0%, 2%]", "(2%, 4%]", "(4%, 6%]", "(6%, 8%]", "(8%, 10%]", "(10%, 12%]", "(12%, 100%]"),
                           Dollars = c(-LCROverSum$Dollars[1], -LCROverSum$Dollars[2], -LCROverSum$Dollars[3], -LCROverSum$Dollars[4],  -LCROverSum$Dollars[5], -LCROverSum$Dollars[6], -LCROverSum$Dollars[7]),
                           Percent = c(-LCROverPct$Percent[1], -LCROverPct$Percent[2], -LCROverPct$Percent[3], -LCROverPct$Percent[4],  -LCROverPct$Percent[5], -LCROverPct$Percent[6], -LCROverPct$Percent[7]),
                           Growers = c(LCRGrower$Growers[1], LCRGrower$Growers[2], LCRGrower$Growers[3], LCRGrower$Growers[4],  LCRGrower$Growers[5], LCRGrower$Growers[6], LCRGrower$Growers[7])
  )
  
  
  LCROverSum$Bins <- factor(LCROverSum$Bins, levels = c("[0%, 2%]", "(2%, 4%]", "(4%, 6%]", "(6%, 8%]", "(8%, 10%]", "(10%, 12%]", "(12%, 100%]"))}
  else{
    LCROverSum <- NULL
  }
  
  return(LCROverSum)
}



LCRTotals5_raw0 <- function(fsr_as_string, grower_report,corn_soy) {
  grower_report_updated <- grower_report
  grower_report_updated = filter(grower_report_updated ,FSR==fsr_as_string )
  grower_report_updated <- grower_report_updated %>% mutate(totalLCRdiff = grower_report_updated[,8]  - grower_report_updated[,9] )
  grower_report_updated <- grower_report_updated %>% mutate(observedPercentLCR = grower_report_updated$`Observed Total LCR`/grower_report_updated$`Retail Total`)
  grower_report_updated <- grower_report_updated %>% mutate(PredictedPercentLCR = grower_report_updated$`Predicted Total LCR`/grower_report_updated$`Retail Total`)
  if(corn_soy == 'corn') {
    grower_report_updated <- grower_report_updated[,c(20, 14, 1, 15, ncol(grower_report_updated)-2, ncol(grower_report_updated)-1, ncol(grower_report_updated) )]
  } else{
    grower_report_updated <- grower_report_updated[,c(19, 14, 1, 15, ncol(grower_report_updated)-2, ncol(grower_report_updated)-1, ncol(grower_report_updated) )]
  }
  grower_report_updated <- grower_report_updated %>% mutate(Over = if_else(grower_report_updated[,5] > 0 , 1, 0))
  grower_report_updated[,2] <- as.factor(grower_report_updated[,2]) #factorize Grower Names for filtering
  grower_report_updated <- grower_report_updated[order(-grower_report_updated[,5]),]
  colnames(grower_report_updated) <- c("Seedsman Account Name", "Grower Account Name","Product", "Year", "Total LCR Difference", "Observed LCR Pct", "Predicted LCR Pct", "Over")

  recentYear <- max(grower_report_updated$Year)
  grower_report_updated <- grower_report_updated[ which(grower_report_updated$Year==recentYear),]
  grower_report_updated[,5] <- as.numeric(format(round(grower_report_updated[,5], 2), nsmall = 2))
 
  
  LCROver  <- filter(grower_report_updated, Over == "1")
  LCRUnder <- filter(grower_report_updated, Over == "0")
  
  if(nrow(LCROver) > 0 & nrow(LCRUnder) > 0){
    
  LCROverSum <- aggregate(LCROver$`Total LCR Difference` ~ `Seedsman Account Name`, data=LCROver, FUN=sum)
  LCROverPct <- aggregate(-abs(LCROver$`Observed LCR Pct` - LCROver$`Predicted LCR Pct`) ~ `Seedsman Account Name`, data=LCROver, FUN=mean)
  LCROverGrower <- aggregate(LCROver$`Grower Account Name` ~ `Seedsman Account Name`, data=LCROver, FUN=function(x) length(unique(x)) )
  
  colnames(LCROverSum) <- c("Seedsman", "DollarsOver")
  colnames(LCROverPct) <- c("Seedsman", "PercentOver")
  colnames(LCROverGrower) <- c("Seedsman", "GrowersOver")
  LCROverSum$DollarsOver <- -LCROverSum$DollarsOver/1000
  
  LCROverMerge <- merge(x = LCROverSum, y = LCROverPct, by = "Seedsman", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverGrower, by = "Seedsman", all.x = TRUE)
  
  LCRUnderSum <- aggregate(LCRUnder$`Total LCR Difference` ~ `Seedsman Account Name`, data=LCRUnder, FUN=sum)
  LCRUnderPct <- aggregate(abs(LCRUnder$`Observed LCR Pct` - LCRUnder$`Predicted LCR Pct`) ~ `Seedsman Account Name`, data=LCRUnder, FUN=mean)
  LCRUnderGrower <- aggregate(LCRUnder$`Grower Account Name` ~ `Seedsman Account Name`, data=LCRUnder, FUN=function(x) length(unique(x)) )
  
  colnames(LCRUnderSum) <- c("Seedsman", "DollarsUnder")
  colnames(LCRUnderPct) <- c("Seedsman", "PercentUnder")
  colnames(LCRUnderGrower) <- c("Seedsman", "GrowersUnder")
  LCRUnderSum$DollarsUnder <- -LCRUnderSum$DollarsUnder/1000
  
  LCRUnderMerge <- merge(x = LCRUnderSum, y = LCRUnderPct, by = "Seedsman", all.x = TRUE)
  LCRUnderMerge <- merge(x = LCRUnderMerge, y = LCRUnderGrower, by = "Seedsman", all.x = TRUE)
  
  LCRMerge <- merge(x = LCROverMerge, y = LCRUnderMerge, by = "Seedsman", all = TRUE)
  
  LCRMerge[is.na(LCRMerge)] <- 0}
  else{
    LCRMerge <- NULL
  }
  LCRMerge
}



LCRTotals5B_raw0 <- function(fsr_as_string, grower_report,corn_soy) {
  grower_report_updated <- grower_report
  grower_report_updated = filter(grower_report_updated ,FSR==fsr_as_string )
  grower_report_updated <- grower_report_updated %>% mutate(totalLCRdiff = grower_report_updated[,8]  - grower_report_updated[,9] )
  grower_report_updated <- grower_report_updated %>% mutate(observedPercentLCR = grower_report_updated$`Observed Total LCR`/grower_report_updated$`Retail Total`)
  grower_report_updated <- grower_report_updated %>% mutate(PredictedPercentLCR = grower_report_updated$`Predicted Total LCR`/grower_report_updated$`Retail Total`)
  if(corn_soy == 'corn') {
    grower_report_updated <- grower_report_updated[,c(20, 14, 1, 15, ncol(grower_report_updated)-2, ncol(grower_report_updated)-1, ncol(grower_report_updated) )]
  } else{
    grower_report_updated <- grower_report_updated[,c(19, 14, 1, 15, ncol(grower_report_updated)-2, ncol(grower_report_updated)-1, ncol(grower_report_updated) )]
  }
  grower_report_updated <- grower_report_updated %>% mutate(Over = if_else(grower_report_updated[,5] > 0 , 1, 0))
  grower_report_updated[,2] <- as.factor(grower_report_updated[,2]) #factorize Grower Names for filtering
  grower_report_updated <- grower_report_updated[order(-grower_report_updated[,5]),]
  colnames(grower_report_updated) <- c("Seedsman Account Name", "Grower Account Name","Product", "Year", "Total LCR Difference", "Observed LCR Pct", "Predicted LCR Pct", "Over")

  recentYear <- max(grower_report_updated$Year)
  grower_report_updated <- grower_report_updated[ which(grower_report_updated$Year==recentYear),]
  grower_report_updated[,5] <- as.numeric(format(round(grower_report_updated[,5], 2), nsmall = 2))
  
  LCROver  <- filter(grower_report_updated, Over == "1")
  LCRUnder <- filter(grower_report_updated, Over == "0")
  
  if(nrow(LCROver) > 0 & nrow(LCRUnder) > 0){
    
  LCROverSum <- aggregate(LCROver$`Total LCR Difference` ~ `Seedsman Account Name`, data=LCROver, FUN=sum)
  LCROverPct <- aggregate(-abs(LCROver$`Observed LCR Pct` - LCROver$`Predicted LCR Pct`) ~ `Seedsman Account Name`, data=LCROver, FUN=mean)
  LCROverGrower <- aggregate(LCROver$`Grower Account Name` ~ `Seedsman Account Name`, data=LCROver, FUN=function(x) length(unique(x)) )
  
  colnames(LCROverSum) <- c("Seedsman", "DollarsOver")
  colnames(LCROverPct) <- c("Seedsman", "PercentOver")
  colnames(LCROverGrower) <- c("Seedsman", "GrowersOver")
  LCROverSum$DollarsOver <- -LCROverSum$DollarsOver/1000
  
  LCROverMerge <- merge(x = LCROverSum, y = LCROverPct, by = "Seedsman", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverGrower, by = "Seedsman", all.x = TRUE)
  
  LCRUnderSum <- aggregate(LCRUnder$`Total LCR Difference` ~ `Seedsman Account Name`, data=LCRUnder, FUN=sum)
  LCRUnderPct <- aggregate(abs(LCRUnder$`Observed LCR Pct` - LCRUnder$`Predicted LCR Pct`) ~ `Seedsman Account Name`, data=LCRUnder, FUN=mean)
  LCRUnderGrower <- aggregate(LCRUnder$`Grower Account Name` ~ `Seedsman Account Name`, data=LCRUnder, FUN=function(x) length(unique(x)) )
  
  colnames(LCRUnderSum) <- c("Seedsman", "DollarsUnder")
  colnames(LCRUnderPct) <- c("Seedsman", "PercentUnder")
  colnames(LCRUnderGrower) <- c("Seedsman", "GrowersUnder")
  LCRUnderSum$DollarsUnder <- -LCRUnderSum$DollarsUnder/1000
  
  LCRUnderMerge <- merge(x = LCRUnderSum, y = LCRUnderPct, by = "Seedsman", all.x = TRUE)
  LCRUnderMerge <- merge(x = LCRUnderMerge, y = LCRUnderGrower, by = "Seedsman", all.x = TRUE)
  
  LCRMerge <- merge(x = LCROverMerge, y = LCRUnderMerge, by = "Seedsman", all = TRUE)
  
  LCRMerge[is.na(LCRMerge)] <- 0
  
  LCRMerge <- LCRMerge[c(1:4)]
  LCRMerge <- LCRMerge[order(LCRMerge$PercentOver),] }
  else{
    LCRMerge <- NULL
  }
  
  return(LCRMerge)
}



LCRTotals6_raw0 <- function(fsr_as_string, grower_report, corn_soy) {
  grower_report_updated <- grower_report
  grower_report_updated = filter(grower_report_updated ,FSR==fsr_as_string )
  grower_report_updated <- grower_report_updated %>% mutate(totalLCRdiff = grower_report_updated[,8]  - grower_report_updated[,9] )
  grower_report_updated <- grower_report_updated %>% mutate(observedPercentLCR = grower_report_updated$`Observed Total LCR`/grower_report_updated$`Retail Total`)
  grower_report_updated <- grower_report_updated %>% mutate(PredictedPercentLCR = grower_report_updated$`Predicted Total LCR`/grower_report_updated$`Retail Total`)
  if(corn_soy == 'corn') {
    grower_report_updated <- grower_report_updated[,c(20, 14, 1, 15, ncol(grower_report_updated)-2, ncol(grower_report_updated)-1, ncol(grower_report_updated), 2, 5, 6, 11, 12, 4, 8, 9)]
  } else{
    grower_report_updated <- grower_report_updated[,c(19, 14, 1, 15, ncol(grower_report_updated)-2, ncol(grower_report_updated)-1, ncol(grower_report_updated), 2, 5, 6, 11, 12, 4, 8, 9)]
  }
  grower_report_updated <- grower_report_updated %>% mutate(Over = if_else(grower_report_updated[,5] > 0 , 1, 0))
  grower_report_updated <- grower_report_updated %>% mutate(totalLCRdiff = if_else(grower_report_updated[,5] < 0 , 0.00, totalLCRdiff))
  grower_report_updated[,2] <- as.factor(grower_report_updated[,2]) #factorize Grower Names for filtering
  grower_report_updated[,1] <- as.factor(grower_report_updated[,1]) #factorize Seedsman Names for filtering
  grower_report_updated <- grower_report_updated[order(-grower_report_updated[,5]),]
  
  colnames(grower_report_updated) <- c("Seedsman Account Name", "Grower Account Name","Product", "Year", "Total LCR Difference", "Observed LCR Pct", "Predicted LCR Pct", "Quantity", "Observed Per Unit LCR", "Predicted Per Unit LCR", "Observed Avg Unit Farmgate Price",	"Predicted Unit Farmgate Price", "Retail Total", "Observed Total LCR", "Predicted Total LCR","Over")

  recentYear <- max(grower_report_updated$Year)
  grower_report_updated <- grower_report_updated[ which(grower_report_updated$Year==recentYear),]
  grower_report_updated[,5] <- as.numeric(format(round(grower_report_updated[,5], 2), nsmall = 2))
  LCROver <- grower_report_updated
  
  if(nrow(LCROver) > 0){
  LCROverSum <- aggregate(LCROver$`Total LCR Difference` ~ `Grower Account Name`, data=LCROver, FUN=sum)
  LCROverRetailTotal <- aggregate(LCROver$`Retail Total` ~ `Grower Account Name`, data=LCROver, FUN=sum)
  LCROverObsTotLCR <- aggregate(LCROver$`Observed Total LCR` ~ `Grower Account Name`, data=LCROver, FUN=sum)
  LCROverPredTotLCR <- aggregate(LCROver$`Predicted Total LCR` ~ `Grower Account Name`, data=LCROver, FUN=sum)
  
  LCROverPct <- aggregate(-abs(LCROver$`Observed LCR Pct` - LCROver$`Predicted LCR Pct`) ~ `Grower Account Name`, data=LCROver, FUN=mean)
  LCROverGrower <- aggregate(LCROver$`Seedsman Account Name` ~ `Grower Account Name`, data=LCROver, FUN=function(x) length(unique(x)) )
  LCROverSeedsmen <- aggregate(LCROver$`Seedsman Account Name` ~ `Grower Account Name`, data=LCROver, FUN=function(x) unique(x))
  LCROverQuantity <- aggregate(LCROver$`Quantity` ~ `Grower Account Name`, data=LCROver, FUN=sum)
  LCROverPctObserved <- aggregate(LCROver$`Observed LCR Pct` ~ `Grower Account Name`, data=LCROver, FUN=mean)
  LCROverPctPrevail <- aggregate(LCROver$`Predicted LCR Pct` ~ `Grower Account Name`, data=LCROver, FUN=mean)
  
  LCROverObservedLCRAve <- aggregate(LCROver$`Observed Per Unit LCR` ~ `Grower Account Name`, data=LCROver, FUN=mean)
  LCROverPrevailLCRAve <- aggregate(LCROver$`Predicted Per Unit LCR` ~ `Grower Account Name`, data=LCROver, FUN=mean)
  LCROverObservedFarmgateAve <- aggregate(LCROver$`Observed Avg Unit Farmgate Price` ~ `Grower Account Name`, data=LCROver, FUN=mean)
  LCROverPrevailFarmgateAve <- aggregate(LCROver$`Predicted Unit Farmgate Price` ~ `Grower Account Name`, data=LCROver, FUN=mean)
  
  colnames(LCROverSum) <- c("Grower", "DollarsOver")
  colnames(LCROverObsTotLCR) <- c("Grower", "ObsTotLCR")
  colnames(LCROverPredTotLCR) <- c("Grower", "PredTotLCR")
  colnames(LCROverRetailTotal) <- c("Grower", "RetailTotal")
  colnames(LCROverPct) <- c("Grower", "PercentOver")
  colnames(LCROverGrower) <- c("Grower", "SeedsmanOver")
  colnames(LCROverSeedsmen) <- c("Grower", "Seedsman")
  colnames(LCROverQuantity) <- c("Grower", "Quantity")
  colnames(LCROverPctObserved) <- c("Grower", "LCRPCTObserved")
  colnames(LCROverPctPrevail) <- c("Grower", "LCRPCTPrevail")
  colnames(LCROverObservedLCRAve) <- c("Grower", "LCROverObservedLCRAve")
  colnames(LCROverPrevailLCRAve) <- c("Grower", "LCROverPrevailLCRAve")
  colnames(LCROverObservedFarmgateAve) <- c("Grower", "LCROverObservedFarmgateAve")
  colnames(LCROverPrevailFarmgateAve) <- c("Grower", "LCROverPrevailFarmgateAve")
  
  LCROverSum$DollarsOver <- -LCROverSum$DollarsOver#/1000
  
  LCROverMerge <- merge(x = LCROverSum, y = LCROverPct, by = "Grower", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverObsTotLCR, by = "Grower", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverPredTotLCR, by = "Grower", all.x = TRUE)
  
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverRetailTotal, by = "Grower", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverGrower, by = "Grower", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverSeedsmen, by = "Grower", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverQuantity, by = "Grower", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverPctObserved, by = "Grower", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverPctPrevail, by = "Grower", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverObservedLCRAve, by = "Grower", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverPrevailLCRAve, by = "Grower", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverObservedFarmgateAve, by = "Grower", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverPrevailFarmgateAve, by = "Grower", all.x = TRUE)
  
  LCROverMerge <- LCROverMerge %>% mutate(ObsLCRPCT = LCROverMerge$ObsTotLCR/LCROverMerge$RetailTotal)
  LCROverMerge <- LCROverMerge %>% mutate(PreLCRPCT = LCROverMerge$PredTotLCR/LCROverMerge$RetailTotal)
  
  LCROverMerge <- LCROverMerge %>% mutate(RecommendedLCRPCT = if_else(LCROverMerge$ObsLCRPCT >= LCROverMerge$PreLCRPCT , LCROverMerge$PreLCRPCT, LCROverMerge$ObsLCRPCT))
  LCROverMerge <- LCROverMerge %>% mutate(RecommendedLCRAVE = if_else(LCROverMerge$LCROverObservedLCRAve >= LCROverMerge$LCROverPrevailLCRAve , LCROverMerge$LCROverPrevailLCRAve, LCROverMerge$LCROverObservedLCRAve))
  LCROverMerge <- LCROverMerge %>% mutate(RecommendedFarmgateAVE = LCROverMerge$LCROverObservedFarmgateAve + abs(LCROverMerge$DollarsOver/LCROverMerge$Quantity))
  
  LCRMerge <- LCROverMerge
  LCRMerge[is.na(LCRMerge)] <- 0
  LCRMerge <- LCRMerge[order(LCRMerge$DollarsOver),]
  LCRMerge$ObsLCRPCT <- round(LCRMerge$ObsLCRPCT, 3)
  LCRMerge$PreLCRPCT <- round(LCRMerge$PreLCRPCT, 3)
  col_order <- c("Grower", "Seedsman", "Quantity", "ObsLCRPCT", "PreLCRPCT", "RecommendedLCRPCT", "DollarsOver", "PredTotLCR", "LCROverObservedLCRAve", "LCROverPrevailLCRAve", "RecommendedLCRAVE", "LCROverObservedFarmgateAve", "RecommendedFarmgateAVE")
  LCRMerge <- LCRMerge[, col_order]}
  else{
    LCRMerge <- NULL
  }
  
  return(LCRMerge)
}



LCRTotals7_raw0 <- function(fsr_as_string, grower_report, corn_soy) {
  grower_report_updated <- grower_report
  grower_report_updated = filter(grower_report_updated ,FSR==fsr_as_string )
  grower_report_updated <- grower_report_updated %>% mutate(totalLCRdiff = grower_report_updated[,8]  - grower_report_updated[,9] )
  grower_report_updated <- grower_report_updated %>% mutate(observedPercentLCR = grower_report_updated$`Observed Total LCR`/grower_report_updated$`Retail Total`)
  grower_report_updated <- grower_report_updated %>% mutate(PredictedPercentLCR = grower_report_updated$`Predicted Total LCR`/grower_report_updated$`Retail Total`)
  if(corn_soy == 'corn') {
    grower_report_updated <- grower_report_updated[,c(20, 14, 1, 15, ncol(grower_report_updated)-2, ncol(grower_report_updated)-1, ncol(grower_report_updated), 2, 5, 6, 11, 12, 4, 8, 9)]
  } else{
    grower_report_updated <- grower_report_updated[,c(19, 14, 1, 15, ncol(grower_report_updated)-2, ncol(grower_report_updated)-1, ncol(grower_report_updated), 2, 5, 6, 11, 12, 4, 8, 9)]
  }
  grower_report_updated <- grower_report_updated %>% mutate(Over = if_else(grower_report_updated[,5] > 0 , 1, 0))
  grower_report_updated <- grower_report_updated %>% mutate(totalLCRdiff = if_else(grower_report_updated[,5] < 0 , 0.00, totalLCRdiff))
  grower_report_updated[,2] <- as.factor(grower_report_updated[,2]) #factorize Grower Names for filtering
  grower_report_updated[,1] <- as.factor(grower_report_updated[,1]) #factorize Seedsman Names for filtering
  grower_report_updated <- grower_report_updated[order(-grower_report_updated[,5]),]
  
  colnames(grower_report_updated) <- c("Seedsman Account Name", "Grower Account Name","Product", "Year", "Total LCR Difference", "Observed LCR Pct", "Predicted LCR Pct", "Quantity", "Observed Per Unit LCR", "Predicted Per Unit LCR", "Observed Avg Unit Farmgate Price",	"Predicted Unit Farmgate Price", "Retail Total", "Observed Total LCR", "Predicted Total LCR","Over")

  recentYear <- max(grower_report_updated$Year)
  grower_report_updated <- grower_report_updated[ which(grower_report_updated$Year==recentYear),]
  grower_report_updated[,5] <- as.numeric(format(round(grower_report_updated[,5], 2), nsmall = 2))
  LCROver <- grower_report_updated
  
  if(nrow(LCROver) > 0){
    
  LCROverSum <- aggregate(LCROver$`Total LCR Difference` ~ `Seedsman Account Name`, data=LCROver, FUN=sum)
  LCROverRetailTotal <- aggregate(LCROver$`Retail Total` ~ `Seedsman Account Name`, data=LCROver, FUN=sum)
  LCROverObsTotLCR <- aggregate(LCROver$`Observed Total LCR` ~ `Seedsman Account Name`, data=LCROver, FUN=sum)
  LCROverPredTotLCR <- aggregate(LCROver$`Predicted Total LCR` ~ `Seedsman Account Name`, data=LCROver, FUN=sum)
  
  LCROverPct <- aggregate(-abs(LCROver$`Observed LCR Pct` - LCROver$`Predicted LCR Pct`) ~ `Seedsman Account Name`, data=LCROver, FUN=mean)
  LCROverGrower <- aggregate(LCROver$`Grower Account Name` ~ `Seedsman Account Name`, data=LCROver, FUN=function(x) length(unique(x)) )
  #LCROverSeedsmen <- aggregate(LCROver$`Seedsman Account Name` ~ `Grower Account Name`, data=LCROver, FUN=function(x) unique(x))
  LCROverQuantity <- aggregate(LCROver$`Quantity` ~ `Seedsman Account Name`, data=LCROver, FUN=sum)
  LCROverPctObserved <- aggregate(LCROver$`Observed LCR Pct` ~ `Seedsman Account Name`, data=LCROver, FUN=mean)
  LCROverPctPrevail <- aggregate(LCROver$`Predicted LCR Pct` ~ `Seedsman Account Name`, data=LCROver, FUN=mean)
  
  LCROverObservedLCRAve <- aggregate(LCROver$`Observed Per Unit LCR` ~ `Seedsman Account Name`, data=LCROver, FUN=mean)
  LCROverPrevailLCRAve <- aggregate(LCROver$`Predicted Per Unit LCR` ~ `Seedsman Account Name`, data=LCROver, FUN=mean)
  LCROverObservedFarmgateAve <- aggregate(LCROver$`Observed Avg Unit Farmgate Price` ~ `Seedsman Account Name`, data=LCROver, FUN=mean)
  LCROverPrevailFarmgateAve <- aggregate(LCROver$`Predicted Unit Farmgate Price` ~ `Seedsman Account Name`, data=LCROver, FUN=mean)
  
  colnames(LCROverSum) <- c("Seedsman", "DollarsOver")
  colnames(LCROverObsTotLCR) <- c("Seedsman", "ObsTotLCR")
  colnames(LCROverPredTotLCR) <- c("Seedsman", "PredTotLCR")
  colnames(LCROverRetailTotal) <- c("Seedsman", "RetailTotal")
  colnames(LCROverPct) <- c("Seedsman", "PercentOver")
  colnames(LCROverGrower) <- c("Seedsman", "Grower")
  #colnames(LCROverSeedsmen) <- c("Grower", "Seedsman")
  colnames(LCROverQuantity) <- c("Seedsman", "Quantity")
  colnames(LCROverPctObserved) <- c("Seedsman", "LCRPCTObserved")
  colnames(LCROverPctPrevail) <- c("Seedsman", "LCRPCTPrevail")
  colnames(LCROverObservedLCRAve) <- c("Seedsman", "LCROverObservedLCRAve")
  colnames(LCROverPrevailLCRAve) <- c("Seedsman", "LCROverPrevailLCRAve")
  colnames(LCROverObservedFarmgateAve) <- c("Seedsman", "LCROverObservedFarmgateAve")
  colnames(LCROverPrevailFarmgateAve) <- c("Seedsman", "LCROverPrevailFarmgateAve")
  
  LCROverSum$DollarsOver <- -LCROverSum$DollarsOver#/1000
  
  LCROverMerge <- merge(x = LCROverSum, y = LCROverPct, by = "Seedsman", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverObsTotLCR, by = "Seedsman", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverPredTotLCR, by = "Seedsman", all.x = TRUE)
  
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverRetailTotal, by = "Seedsman", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverGrower, by = "Seedsman", all.x = TRUE)
  #LCROverMerge <- merge(x = LCROverMerge, y = LCROverSeedsmen, by = "Seedsman", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverQuantity, by = "Seedsman", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverPctObserved, by = "Seedsman", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverPctPrevail, by = "Seedsman", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverObservedLCRAve, by = "Seedsman", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverPrevailLCRAve, by = "Seedsman", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverObservedFarmgateAve, by = "Seedsman", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverPrevailFarmgateAve, by = "Seedsman", all.x = TRUE)
  
  LCROverMerge <- LCROverMerge %>% mutate(ObsLCRPCT = LCROverMerge$ObsTotLCR/LCROverMerge$RetailTotal)
  LCROverMerge <- LCROverMerge %>% mutate(PreLCRPCT = LCROverMerge$PredTotLCR/LCROverMerge$RetailTotal)
  
  LCROverMerge <- LCROverMerge %>% mutate(RecommendedLCRPCT = if_else(LCROverMerge$ObsLCRPCT >= LCROverMerge$PreLCRPCT , LCROverMerge$PreLCRPCT, LCROverMerge$ObsLCRPCT))
  LCROverMerge <- LCROverMerge %>% mutate(RecommendedLCRAVE = if_else(LCROverMerge$LCROverObservedLCRAve >= LCROverMerge$LCROverPrevailLCRAve , LCROverMerge$LCROverPrevailLCRAve, LCROverMerge$LCROverObservedLCRAve))
  LCROverMerge <- LCROverMerge %>% mutate(RecommendedFarmgateAVE = LCROverMerge$LCROverObservedFarmgateAve + abs(LCROverMerge$DollarsOver/LCROverMerge$Quantity))
  
  LCRMerge <- LCROverMerge
  LCRMerge[is.na(LCRMerge)] <- 0
  LCRMerge <- LCRMerge[order(LCRMerge$DollarsOver),]
  LCRMerge$ObsLCRPCT <- round(LCRMerge$ObsLCRPCT, 3)
  LCRMerge$PreLCRPCT <- round(LCRMerge$PreLCRPCT, 3)
  col_order <- c("Seedsman", "Quantity", "ObsLCRPCT", "PreLCRPCT", "RecommendedLCRPCT", "DollarsOver", "PredTotLCR", "LCROverObservedLCRAve", "LCROverPrevailLCRAve", "RecommendedLCRAVE", "LCROverObservedFarmgateAve", "RecommendedFarmgateAVE")
  LCRMerge <- LCRMerge[, col_order]}
  else{
    LCRMerge <- NULL
  }
  
  return(LCRMerge)
}



LCRTotals8_raw0 <- function(fsr_as_string, grower_report, corn_soy) {
  grower_report_updated <- grower_report
  grower_report_updated = filter(grower_report_updated ,FSR==fsr_as_string )
  grower_report_updated <- grower_report_updated %>% mutate(totalLCRdiff = grower_report_updated[,8]  - grower_report_updated[,9] )
  grower_report_updated <- grower_report_updated %>% mutate(observedPercentLCR = grower_report_updated$`Observed Total LCR`/grower_report_updated$`Retail Total`)
  grower_report_updated <- grower_report_updated %>% mutate(PredictedPercentLCR = grower_report_updated$`Predicted Total LCR`/grower_report_updated$`Retail Total`)
  if(corn_soy == 'corn'){
    grower_report_updated <- grower_report_updated[,c(20, 14, 1, 15, ncol(grower_report_updated)-2, ncol(grower_report_updated)-1, ncol(grower_report_updated), 2, 5, 6, 11, 12, 4, 8, 9)]
  } else {
    grower_report_updated <- grower_report_updated[,c(19, 14, 1, 15, ncol(grower_report_updated)-2, ncol(grower_report_updated)-1, ncol(grower_report_updated), 2, 5, 6, 11, 12, 4, 8, 9)]
  }
  grower_report_updated <- grower_report_updated %>% mutate(Over = if_else(grower_report_updated[,5] > 0 , 1, 0))
  grower_report_updated <- grower_report_updated %>% mutate(totalLCRdiff = if_else(grower_report_updated[,5] < 0 , 0.00, totalLCRdiff))
  grower_report_updated[,2] <- as.factor(grower_report_updated[,2]) #factorize Grower Names for filtering
  grower_report_updated[,1] <- as.factor(grower_report_updated[,1]) #factorize Seedsman Names for filtering
  grower_report_updated <- grower_report_updated[order(-grower_report_updated[,5]),]
  
  colnames(grower_report_updated) <- c("Seedsman Account Name", "Grower Account Name", "Product", "Year", "Total LCR Difference", "Observed LCR Pct", "Predicted LCR Pct", "Quantity", "Observed Per Unit LCR", "Predicted Per Unit LCR", "Observed Avg Unit Farmgate Price",	"Predicted Unit Farmgate Price", "Retail Total", "Observed Total LCR", "Predicted Total LCR", "Over")
  
  grower_report_updated$Product <- as.factor(grower_report_updated$Product) #factorize Product Names for filtering

  recentYear <- max(grower_report_updated$Year)
  grower_report_updated <- grower_report_updated[ which(grower_report_updated$Year==recentYear),]
  grower_report_updated[,5] <- as.numeric(format(round(grower_report_updated[,5], 2), nsmall = 2))
  LCROver <- grower_report_updated
  LCROver
  
  if(nrow(LCROver) > 0){
    
  LCROverSum <- aggregate(LCROver$`Total LCR Difference` ~ `Product`, data=LCROver, FUN=sum)
  LCROverRetailTotal <- aggregate(LCROver$`Retail Total` ~ `Product`, data=LCROver, FUN=sum)
  LCROverObsTotLCR <- aggregate(LCROver$`Observed Total LCR` ~ `Product`, data=LCROver, FUN=sum)
  LCROverPredTotLCR <- aggregate(LCROver$`Predicted Total LCR` ~ `Product`, data=LCROver, FUN=sum)
  
  LCROverPct <- aggregate(-abs(LCROver$`Observed LCR Pct` - LCROver$`Predicted LCR Pct`) ~ `Product`, data=LCROver, FUN=mean)
  LCROverGrower <- aggregate(LCROver$`Grower Account Name` ~ `Product`, data=LCROver, FUN=function(x) length(unique(x)) )
  LCROverSeedsmen <- aggregate(LCROver$`Seedsman Account Name` ~ `Product`, data=LCROver, FUN=function(x) unique(x))
  LCROverQuantity <- aggregate(LCROver$`Quantity` ~ `Product`, data=LCROver, FUN=sum)
  LCROverPctObserved <- aggregate(LCROver$`Observed LCR Pct` ~ `Product`, data=LCROver, FUN=mean)
  LCROverPctPrevail <- aggregate(LCROver$`Predicted LCR Pct` ~ `Product`, data=LCROver, FUN=mean)
  
  LCROverObservedLCRAve <- aggregate(LCROver$`Observed Per Unit LCR` ~ `Product`, data=LCROver, FUN=mean)
  LCROverPrevailLCRAve <- aggregate(LCROver$`Predicted Per Unit LCR` ~ `Product`, data=LCROver, FUN=mean)
  LCROverObservedFarmgateAve <- aggregate(LCROver$`Observed Avg Unit Farmgate Price` ~ `Product`, data=LCROver, FUN=mean)
  LCROverPrevailFarmgateAve <- aggregate(LCROver$`Predicted Unit Farmgate Price` ~ `Product`, data=LCROver, FUN=mean)
  
  colnames(LCROverSum) <- c("Product", "DollarsOver")
  colnames(LCROverObsTotLCR) <- c("Product", "ObsTotLCR")
  colnames(LCROverPredTotLCR) <- c("Product", "PredTotLCR")
  colnames(LCROverRetailTotal) <- c("Product", "RetailTotal")
  colnames(LCROverPct) <- c("Product", "PercentOver")
  colnames(LCROverGrower) <- c("Product", "Grower")
  colnames(LCROverSeedsmen) <- c("Grower", "Seedsman")
  colnames(LCROverQuantity) <- c("Product", "Quantity")
  colnames(LCROverPctObserved) <- c("Product", "LCRPCTObserved")
  colnames(LCROverPctPrevail) <- c("Product", "LCRPCTPrevail")
  colnames(LCROverObservedLCRAve) <- c("Product", "LCROverObservedLCRAve")
  colnames(LCROverPrevailLCRAve) <- c("Product", "LCROverPrevailLCRAve")
  colnames(LCROverObservedFarmgateAve) <- c("Product", "LCROverObservedFarmgateAve")
  colnames(LCROverPrevailFarmgateAve) <- c("Product", "LCROverPrevailFarmgateAve")
  
  LCROverSum$DollarsOver <- -LCROverSum$DollarsOver#/1000
  
  LCROverMerge <- merge(x = LCROverSum, y = LCROverPct, by = "Product", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverObsTotLCR, by = "Product", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverPredTotLCR, by = "Product", all.x = TRUE)
  
  
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverRetailTotal, by = "Product", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverGrower, by = "Product", all.x = TRUE)
  #LCROverMerge <- merge(x = LCROverMerge, y = LCROverSeedsmen, by = "Product", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverQuantity, by = "Product", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverPctObserved, by = "Product", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverPctPrevail, by = "Product", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverObservedLCRAve, by = "Product", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverPrevailLCRAve, by = "Product", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverObservedFarmgateAve, by = "Product", all.x = TRUE)
  LCROverMerge <- merge(x = LCROverMerge, y = LCROverPrevailFarmgateAve, by = "Product", all.x = TRUE)
  LCROverMerge
  
  LCROverMerge <- LCROverMerge %>% mutate(ObsLCRPCT = LCROverMerge$ObsTotLCR/LCROverMerge$RetailTotal)
  LCROverMerge <- LCROverMerge %>% mutate(PreLCRPCT = LCROverMerge$PredTotLCR/LCROverMerge$RetailTotal)
  
  LCROverMerge <- LCROverMerge %>% mutate(RecommendedLCRPCT = if_else(LCROverMerge$ObsLCRPCT >= LCROverMerge$PreLCRPCT , LCROverMerge$PreLCRPCT, LCROverMerge$ObsLCRPCT))
  LCROverMerge <- LCROverMerge %>% mutate(RecommendedLCRAVE = if_else(LCROverMerge$LCROverObservedLCRAve >= LCROverMerge$LCROverPrevailLCRAve , LCROverMerge$LCROverPrevailLCRAve, LCROverMerge$LCROverObservedLCRAve))
  LCROverMerge <- LCROverMerge %>% mutate(RecommendedFarmgateAVE = LCROverMerge$LCROverObservedFarmgateAve + abs(LCROverMerge$DollarsOver/LCROverMerge$Quantity))
  
  LCRMerge <- LCROverMerge
  LCRMerge[is.na(LCRMerge)] <- 0
  LCRMerge <- LCRMerge[order(LCRMerge$DollarsOver),]
  LCRMerge$ObsLCRPCT <- round(LCRMerge$ObsLCRPCT, 3)
  LCRMerge$PreLCRPCT <- round(LCRMerge$PreLCRPCT, 3)
  col_order <- c("Product", "Quantity", "ObsLCRPCT", "PreLCRPCT", "RecommendedLCRPCT", "DollarsOver", "PredTotLCR", "LCROverObservedLCRAve", "LCROverPrevailLCRAve", "RecommendedLCRAVE", "LCROverObservedFarmgateAve", "RecommendedFarmgateAVE")
  LCRMerge <- LCRMerge[, col_order]}
  else{
    LCRMerge <- NULL
  }
  
  LCRMerge
}



dtLCRDF_raw <- function(fsr_as_string, lcrtotals3, corn_soy){
  datatable(lcrtotals3, rownames= FALSE, extensions = "Buttons", 
            colnames=c("Bins of LCR as Percent", "Reallocation Potential", 
                       "Ave. Percent Delta to Target", "Number of Growers"), 
            filter = "top", options = list(lengthMenu = list( c(20, 50, 100, -1), c(20, 50, 100, "All")), 
                                           scrollX=TRUE, pageLength = 25,
                                                                                                                                                                                                                     dom = "Blfrtip",
                                                                                                                                                                                                                     buttons = list( list( extend = 'collection',
                                                                                                                                                                                                                                           buttons = list(
                                                                                                                                                                                                                                             list(extend='excel', title = NULL,
                                                                                                                                                                                                                                                  filename = paste("Current LCR Spend ", corn_soy, "--", gsub(",", "", fsr_as_string) , "--", Sys.Date(), sep ='' ) ),
                                                                                                                                                                                                                                             list(extend='csv', title = NULL,
                                                                                                                                                                                                                                                  filename = paste("Current LCR Spend ", corn_soy, "--", gsub(",", "", fsr_as_string), "--", Sys.Date(), sep ='' ) )
                                                                                                                                                                                                                                           ),
                                                                                                                                                                                                                                           text = c('Download Selected Data')
                                                                                                                                                                                                                     ))
  ))%>% 
    formatCurrency(c('Dollars'))%>% 
    formatPercentage(c('Percent'), 1)
}



dtLCRDF4_raw <- function(fsr_as_string, lcrtotals4, corn_soy){
  datatable(lcrtotals4, rownames= FALSE, extensions = "Buttons", colnames=c("Degree of Gross to Net % Delta to Target", "Reallocation Potential", "Ave. Percent Delta to Target", "Number of Growers"), filter = "top", options = list(lengthMenu = list( c(20, 50, 100, -1), c(20, 50, 100, "All")), scrollX=TRUE, pageLength = 25,
                                                                                                                                                                                                                                       dom = "Blfrtip",
                                                                                                                                                                                                                                       buttons = list( list( extend = 'collection',
                                                                                                                                                                                                                                                             buttons = list(
                                                                                                                                                                                                                                                               list(extend='excel', title = NULL,
                                                                                                                                                                                                                                                                    filename = paste("LCR Decrease to Rec ", corn_soy, "--", gsub(",", "", fsr_as_string) , "--", Sys.Date(), sep ='' ) ),
                                                                                                                                                                                                                                                               list(extend='csv', title = NULL,
                                                                                                                                                                                                                                                                    filename = paste("LCR Decrease to Rec ", corn_soy, "--", gsub(",", "", fsr_as_string), "--", Sys.Date(), sep ='' ) )
                                                                                                                                                                                                                                                             ),
                                                                                                                                                                                                                                                             text = c('Download Selected Data')
                                                                                                                                                                                                                                       ))
                                                                                                                                                                                                                                       
  ))%>% 
    formatCurrency(c('Dollars'))%>% 
    formatPercentage(c('Percent'), 1)
}



dtLCRDF5_raw <- function(fsr_as_string, lcrtotals5b, corn_soy){
  datatable(lcrtotals5b, rownames= FALSE, extensions = "Buttons", colnames=c("Seedsman", "LCR Realloc Pot. UNFAV", "Delta to LCR % Trgt UNFAV", "Count of Grower UNFAV"),
            filter = "top", options = list(lengthMenu = list( c(20, 50, 100, -1), c(20, 50, 100, "All")), scrollX=TRUE, pageLength = 25,
                                           dom = "Blfrtip",
                                           buttons = list( list( extend = 'collection',
                                                                 buttons = list(
                                                                   list(extend='excel', title = NULL,
                                                                        filename = paste("LCR Reallocation Potential by Seedsman ", corn_soy, "--", gsub(",", "", fsr_as_string) , "--", Sys.Date(), sep ='' ) ),
                                                                   list(extend='csv', title = NULL,
                                                                        filename = paste("LCR Reallocation Potential by Seedsman ", corn_soy, "--", gsub(",", "", fsr_as_string), "--", Sys.Date(), sep ='' ) )
                                                                 ),
                                                                 text = c('Download Selected Data')
                                           ))
            ))%>% 
    formatCurrency(c('DollarsOver'))%>%
    #formatCurrency(c('DollarsUnder'))%>%
    formatPercentage(c('PercentOver'), 1)
  #formatPercentage(c('PercentUnder'), 1)
}



dtLCRDF6_raw <- function(fsr_as_string, lcrtotals6, corn_soy){
  if(nrow(lcrtotals6 > 0)){
  datatable(lcrtotals6, rownames= FALSE, extensions = "Buttons", colnames=c("Grower", "Seedsman", "Order Quantity", "Actual LCR %", "Prevailing LCR %", "Recommended LCR %", "Reallocation Potential (Over)", "Recommended LCR Total", "Actual LCR Per Unit", "Prevailing LCR Per Unit", "Recommended LCR Per Unit", "Actual Farm Gate Price", "Recommended Farm Gate Price"),
            filter = "top", options = list(lengthMenu = list( c(20, 50, 100, -1), c(20, 50, 100, "All")), scrollX=TRUE, pageLength = 20,
                                           dom = "Blfrtip",
                                           buttons = list( list( extend = 'collection',
                                                                 buttons = list(
                                                                   list(extend='excel', title = NULL,
                                                                        filename = paste("LCR by Grower ", corn_soy, "--", gsub(",", "", fsr_as_string) , "--", Sys.Date(), sep ='' ) ),
                                                                   list(extend='csv', title = NULL,
                                                                        filename = paste("LCR by Grower ", corn_soy, "--", gsub(",", "", fsr_as_string), "--", Sys.Date(), sep ='' ) )
                                                                 ),
                                                                 text = c('Download Selected Data')
                                           ))
            ))%>% 
    formatCurrency(c("DollarsOver", "PredTotLCR", "LCROverObservedLCRAve", "LCROverPrevailLCRAve", "RecommendedLCRAVE", "LCROverObservedFarmgateAve", "RecommendedFarmgateAVE"))%>%
    formatPercentage(c("ObsLCRPCT", "PreLCRPCT", "RecommendedLCRPCT"), 1)}
  else{
    lcrtotals6 <- NULL
  }
}



dtLCRDF7_raw <- function(fsr_as_string, lcrtotals7, corn_soy){
  datatable(lcrtotals7, rownames= FALSE, extensions = "Buttons", colnames=c("Seedsman", "Order Quantity", "Actual LCR %", "Prevailing LCR %", "Rec. LCR %", "Reallocation Potential (Over)", "Rec. LCR Total", "Actual LCR Per Unit", "Prevailing LCR Per Unit", "Rec. LCR Per Unit", "Actual Farm Gate Price", "Rec. Farm Gate Price"),
            filter = "top", options = list(lengthMenu = list( c(20, 50, 100, -1), c(20, 50, 100, "All")), scrollX=TRUE, pageLength = 20,
                                           dom = "Blfrtip",
                                           buttons = list( list( extend = 'collection',
                                                                 buttons = list(
                                                                   list(extend='excel', title = NULL,
                                                                        filename = paste("LCR by Seedsman ", corn_soy, "--", gsub(",", "", fsr_as_string) , "--", Sys.Date(), sep ='' ) ),
                                                                   list(extend='csv', title = NULL,
                                                                        filename = paste("LCR by Seedsman ", corn_soy, "--", gsub(",", "", fsr_as_string), "--", Sys.Date(), sep ='' ) )
                                                                 ),
                                                                 text = c('Download Selected Data')
                                           ))
            ))%>% 
    formatCurrency(c("DollarsOver", "PredTotLCR", "LCROverObservedLCRAve", "LCROverPrevailLCRAve", "RecommendedLCRAVE", "LCROverObservedFarmgateAve", "RecommendedFarmgateAVE"))%>%
    formatPercentage(c("ObsLCRPCT", "PreLCRPCT", "RecommendedLCRPCT"), 1)
}



dtLCRDF8_raw <- function(fsr_as_string, lcrtotals8, corn_soy){
  if(nrow(lcrtotals8 > 0)){
  datatable(lcrtotals8, rownames= FALSE, extensions = "Buttons", colnames=c("Product", "Order Quantity", "Actual LCR %", "Prevailing LCR %", "Rec. LCR %", "Reallocation Potential (Over)", "Rec. LCR Total", "Actual LCR Per Unit", "Prevailing LCR Per Unit", "Rec. LCR Per Unit", "Actual Farm Gate Price", "Rec. Farm Gate Price"),
            filter = "top", options = list(lengthMenu = list( c(20, 50, 100, -1), c(20, 50, 100, "All")), scrollX=TRUE, pageLength = 20,
                                           dom = "Blfrtip",
                                           buttons = list( list( extend = 'collection',
                                                                 buttons = list(
                                                                   list(extend='excel', title = NULL,
                                                                        filename = paste("LCR by Seedsman ", corn_soy, "--", gsub(",", "", fsr_as_string) , "--", Sys.Date(), sep ='' ) ),
                                                                   list(extend='csv', title = NULL,
                                                                        filename = paste("LCR by Seedsman ", corn_soy, "--", gsub(",", "", fsr_as_string), "--", Sys.Date(), sep ='' ) )
                                                                 ),
                                                                 text = c('Download Selected Data')
                                           ))
            ))%>% 
    formatCurrency(c("DollarsOver", "PredTotLCR", "LCROverObservedLCRAve", "LCROverPrevailLCRAve", "RecommendedLCRAVE", "LCROverObservedFarmgateAve", "RecommendedFarmgateAVE"))%>%
    formatPercentage(c("ObsLCRPCT", "PreLCRPCT", "RecommendedLCRPCT"), 1)}
  else{
    lcrtotals8 <- NULL
  }
}



plotlyLCRPlot3 <- function(lcrtots){
  #renderPlotly({
  dat = lcrtots
  #Group <- dat$Group
  #Grower_Count <- dat$Grower_Count
  #Seedsman_Count <- dat$Seedsman_Count
  fig <- plot_ly(dat, x = ~Group, y = ~Grower_Count, type = 'bar', name = 'Number of Growers',
                 hoverinfo = 'text',
                 text = ~paste('Number of Growers: ', Grower_Count),
                 marker = list(color = '#A5A5A5', line = list(color = '#000000', width = 1.5))) 
  fig <- fig %>% add_trace(y = ~Seedsman_Count, name = 'Number of Seedsmen',
                           hoverinfo = 'text', text = ~paste('Number of Seedsmen: ', Seedsman_Count), 
                           marker = list(color = '#4169E1', line = list(color = '#000000', width = 1.5) ))
  fig <- fig %>% layout(title="Growers or Seedsmen Over/Under Recommended LCR", font = list(size = 10), yaxis = list(title = 'Count'), barmode = 'group')
  
  fig <- fig %>% config(displaylogo = FALSE)
  fig <- fig %>% config(modeBarButtonsToRemove = c('zoom2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'autoScale2d', 'resetScale2d', 'hoverClosestCartesian', 'hoverCompareCartesian', 'toggleSpikelines'))
  
  fig

  #})
}



plotlyLCRPlot4 <- function(lcrtots) {
  dat = lcrtots
  fig <- plot_ly(dat, x = ~Group, y = ~Dollars, type = 'bar', name = 'Number of Growers', marker = list(color = c('#BE0712', '#72AC4D'), line = list(color = '#000000', width = 1.5)),
                 hoverinfo = 'text',
                 text = ~paste('Reallocation Potential in Thousands $: ', dollar(abs(Dollars)))
  )
  fig <- fig %>% layout(title="LCR Over/Under the Recommendation", separators = '.,', yaxis = list(title = 'Dollars (Thousands)', tickformat = "$,.2", hoverformat = '$,.2'), barmode = 'group')
  
  fig <- fig %>% config(displaylogo = FALSE)
  fig <- fig %>% config(modeBarButtonsToRemove = c('zoom2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'autoScale2d', 'resetScale2d', 'hoverClosestCartesian', 'hoverCompareCartesian', 'toggleSpikelines'))

  fig
}



plotlyLCRPlot5 <- function(lcrtots3) {
  fig <- plot_ly()
  
  fig <- fig %>% add_bars(
    data = lcrtots3,
    x = ~Bins,
    y = ~Growers,
    base = c("[0%, 5%]", "(5%, 10%]", "(10%, 15%]", "(15%, 20%]", "(20%, 25%]", "(25%, 100%]"),
    marker = list(
      color = '#A5A5A5', line = list(color = '#000000', width = 1.5)
    ),
    hoverinfo = 'text',
    text = ~paste('Number of Growers: ', Growers),
    name = 'Number of Growers'
  ) 
  
  fig <- fig %>% add_bars(
    data = lcrtots3,
    x = ~Bins,
    y = ~Dollars,
    base = 0,
    title="Total Units and Dollars Over Predicted LCR",
    marker = list(
      color = '#BE0712', line = list(color = '#000000', width = 1.5)
    ),
    hoverinfo = 'text',
    text = ~paste('</br> Reallocation Potential in Thousands $: ', dollar(-Dollars)),
    name =  paste("Reallocation Potential $K", sep = " ")
  )
  
  fig <- fig %>% add_markers(symbol = I('triangle-up'),
                             data = lcrtots3, x = ~Bins, y = ~Percent, type = 'scatter', name = paste("% Difference From","Recommended LCR", sep = " "),
                             mode = "markers",
                             color = I('#FFA500'), marker = list(size = 20, line = list(color = '#000000', width = 1.5)), yaxis = "y2", xaxis = "x1",                     
                             hoverinfo = "text",
                             text = ~paste('Percent Delta to Target ', percent(Percent))) %>%
    layout(yaxis2 = list(automargin = T, tickformat = ".1%", overlaying = "y", side = "right") )
  
  fig <- fig %>% layout(title = 'LCR Spend % off SRP', xaxis = list(title = "Bins of LCR as Percent"), 
                        legend = list(orientation = "h",   # show entries horizontally
                                      xanchor = "center",  # use center of legend as anchor
                                      x = 0.5,
                                      y = -0.2),
                        yaxis = list(range = c(-1.25*max(-min(lcrtots3$Dollars), max(lcrtots3$Growers)), 1.25*max(-min(lcrtots3$Dollars), max(lcrtots3$Growers))), side = 'left', title = 'Totals', showgrid = TRUE, zeroline = TRUE),
                        yaxis2 = list(range = c(1.25*min(lcrtots3$Percent), 1.0), automargin = T, side = 'right', title = 'Percent', showgrid = FALSE, zeroline = FALSE))
  
  fig <- fig %>% config(displaylogo = FALSE)
  fig <- fig %>% config(modeBarButtonsToRemove = c('zoom2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'autoScale2d', 'resetScale2d', 'hoverClosestCartesian', 'hoverCompareCartesian', 'toggleSpikelines'))
  
  fig
}



plotlyLCRPlot6 <- function(lcrtots4) {
  fig <- plot_ly()
  
  fig <- fig %>% add_bars(
    data = lcrtots4,
    x = ~Bins,
    y = ~Growers,
    base = c("[0%, 2%]", "(2%, 4%]", "(4%, 6%]", "(6%, 8%]", "(8%, 10%]", "(10%, 12%]", "(12%, 100%]"),
    marker = list(
      color = '#A5A5A5', line = list(color = '#000000', width = 1.5)
    ),
    hoverinfo = 'text',
    text = ~paste('Number of Growers: ', Growers),
    name = 'Number of Growers'
  ) 
  
  fig <- fig %>% add_bars(
    data = lcrtots4,
    x = ~Bins,
    y = ~Dollars,
    base = 0,
    title="Degree of Gross to Net % Delta to Target",
    marker = list(
      color = '#BE0712', line = list(color = '#000000', width = 1.5)
    ),
    hoverinfo = 'text',
    text = ~paste('</br> Reallocation Potential in Thousands $: ', dollar(-Dollars)),
    name =  paste("Reallocation Potential $K", sep = " ")
  )
  
  fig <- fig %>% add_markers(symbol = I('triangle-up'),
                             data = lcrtots4, x = ~Bins, y = ~Percent, type = 'scatter', name = paste("% Difference From","Recommended LCR", sep = " "),
                             mode = "markers",
                             color = I('#FDBF2D'), marker = list(size = 20, line = list(color = '#000000', width = 1.5)), yaxis = "y2", xaxis = "x1",                     
                             hoverinfo = "text",
                             text = ~paste('Percent Delta to Target: ', percent(Percent))) %>%
    layout(yaxis2 = list(automargin = T, tickformat = ".1%", overlaying = "y", side = "right") )
  
  fig <- fig %>% layout(title = 'LCR Decrease to Recommendation', xaxis = list(title = "Bins of Degree of Gross to Net % Delta to Target"), 
                        legend = list(orientation = "h",   # show entries horizontally
                                      xanchor = "center",  # use center of legend as anchor
                                      x = 0.5,
                                      y = -0.2),
                        yaxis = list(range = c(-1.25*max(-min(lcrtots4$Dollars), max(lcrtots4$Growers)), 1.25*max(-min(lcrtots4$Dollars), max(lcrtots4$Growers))), side = 'left', title = 'Totals', showgrid = TRUE, zeroline = TRUE),
                        yaxis2 = list(range = c(1.25*min(lcrtots4$Percent), -1.25*min(lcrtots4$Percent)), automargin = T, side = 'right', title = 'Percent', showgrid = FALSE, zeroline = FALSE))
  
  fig <- fig %>% config(displaylogo = FALSE)
  fig <- fig %>% config(modeBarButtonsToRemove = c('zoom2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'autoScale2d', 'resetScale2d', 'hoverClosestCartesian', 'hoverCompareCartesian', 'toggleSpikelines'))
  
  fig
}



plotlyLCRPlot7 <- function(lcrtots5) {
  fig <- plot_ly()
  fig <- fig %>% add_bars(data = lcrtots5, 
                          x = ~Seedsman, 
                          y = ~GrowersOver, 
                          type = 'bar', 
                          name = 'LCR Reallocation Potential FAV',
                          marker = list(
                            color = '#A5A5A5', line = list(color = '#000000', width = 1.5)
                          ),
                          hoverinfo = 'text',
                          text = ~paste('Number of Growers: ', GrowersOver),
                          name = 'Number of Growers'
  )
  fig <- fig %>% add_bars(data = lcrtots5, 
                          x = ~Seedsman,
                          y = ~DollarsOver, 
                          name = 'LCR Reallocation Potential UNFAV',
                          marker = list(
                            color = '#BE0712', line = list(color = '#000000', width = 1.5)
                          ),
                          hoverinfo = 'text',
                          text = ~paste(Seedsman, '<br>LCR Reallocation Potential UNFAV: ', dollar(DollarsOver))
  )
  fig <- fig %>% layout(
    yaxis = list(title = 'Totals'),
    legend = list(orientation = "h",   # show entries horizontally
                  xanchor = "center",  # use center of legend as anchor
                  yanchor = "bottom",  # use bottom of legend as anchor
                  x = 0.5,
                  y = -0.75
    )
  )
  
  fig <- fig %>% add_markers(symbol = I('triangle-up'),
                             data = lcrtots5, x = ~Seedsman, y = ~PercentOver, type = 'scatter', name = paste("% Difference From","Recommended LCR", sep = " "),
                             mode = "markers",
                             color = I('#FDBF2D'), marker = list(size = 20, line = list(color = '#000000', width = 1.5)), yaxis = "y2", xaxis = "x1",                     
                             hoverinfo = "text",
                             text = ~paste('Percent Delta to Target: ', percent(PercentOver))) %>%
    layout(yaxis2 = list(automargin = T, tickformat = ".1%", overlaying = "y", side = "right") )
  
  
  fig <- fig %>% layout(list(title = 'LCR Reallocation Potential by Seedsman'),
                        yaxis = list(range = c(  -1.25*max( max(abs(lcrtots5$DollarsOver) ), max(lcrtots5$GrowersOver)) , 1.25*max( max(abs(lcrtots5$DollarsOver) ), max(lcrtots5$GrowersOver)) ), side = 'left', title = 'Totals', showgrid = TRUE, zeroline = TRUE),
                        yaxis2 = list(range = c(  -1.25*max( abs(lcrtots5$PercentOver), max(lcrtots5$PercentUnder)) , 1.25*max( abs(lcrtots5$PercentOver), max(lcrtots5$PercentUnder)) ), overlaying = 'y1', automargin = T, side = 'right', title = 'Percent', showgrid = FALSE, zeroline = FALSE))
  
  fig <- fig %>% config(displaylogo = FALSE)
  fig <- fig %>% config(modeBarButtonsToRemove = c('zoom2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'autoScale2d', 'resetScale2d', 'hoverClosestCartesian', 'hoverCompareCartesian', 'toggleSpikelines'))
  
  
  fig
}


# Generate UI elements for cart: table with buttons --------------------------------------------------
make_cart_ui <- function(cart_df, new_ids = FALSE) {
  if (new_ids) {
    id_base <- paste(sample(letters, 10), collapse = "")
    cart_df[, button_id := paste0(id_base, "_", 1:.N)]
  }
  cart_ui <- renderUI({
    cart_list <- split(cart_df, by = "button_id")
    table_row <- lapply(cart_list, function(row) {
      tags$tr(
        tags$td(actionButton(inputId = row$button_id,
                             label = "Delete")),
        tags$td(actionButton(inputId = paste0(row$button_id, "_gd"),
                             label = "Show")),
        tags$td(numericInput(inputId = paste0(row$button_id, "_od"),
                             label = NULL,
                             value = 0,
                             min = 0,
                             max = if("dollar_percent_table"== "percent_table") {
                               100
                             } else {
                               800
                             },
                             step = 0.01,
                             width = '75px')),
        tags$td(row$Product),
        tags$td(row$LastPurchasedQuantity),
        tags$td(row$Quantity),
        tags$td(row$Price),
        tags$td(row$LastUnitDiscountPCT),
        tags$td(row$DiscountPCT),
        tags$td(row$RecDiscountPCT),
        tags$td(row$LastUnitDiscount),
        tags$td(row$Discount),
        tags$td(row$RecDiscount),
      )
    })
    tagList(
      tags$table(
        class = "table table-hover table-striped",
        tags$th("Delete"),
        tags$th("Show Product Plot"),
        tags$th("Offered Unit LCR*"),
        tags$th("Product"),
        tags$th("Last Purch. Quantity"),
        tags$th("Quantity"),
        tags$th("List Price"),
        tags$th("Actual LCR %"),
        tags$th("Prevailing LCR %"),
        tags$th("Rec. LCR %"),
        tags$th("Actual LCR Unit*"),
        tags$th("Prevailing LCR Unit"),
        tags$th("Rec. LCR Unit"),
        table_row
      )
    )
  })
  output <- list(ui = cart_ui,
                 ids = cart_df$button_id)
  return(output)
}


wrapMemoise <- function(f,mf,args) {
  out <- tryCatch({
    do.call(mf,args)
  }, warning = function(w) {
    # warning-handler-code
    returnValue <- do.call(mf,args)
    return(returnValue)
  }, error = function(e) {
    # clear the memoise cache
    print('Error detected')
    print(e)
    if (grepl('Lock not acquired', e, fixed=TRUE) == FALSE) {
      print('Clearing cache')
      forget(mf)
    }
    returnValue <- do.call(f,args)
    return(returnValue)
  }, finally = {
    #cleanup-code
    # nothing to do here
  })
  return(out)
}

#calculate the time a user is on the app in Days Hours:Minutes:Seconds
dhms <- function(t){
  paste(t %/% (60*60*24) 
        ,paste(formatC(t %/% (60*60) %% 24, width = 2, format = "d", flag = "0")
               ,formatC(t %/% 60 %% 60, width = 2, format = "d", flag = "0")
               ,formatC(t %% 60, width = 2, format = "d", flag = "0")
               ,sep = ":"
        )
  )
}




#' Helper function to wrap up the dropdown menu construction
#' 
#' @title get_dropdown
#'
#' @param inputID character string
#' @param HTMLstring character string
#' @param choices vector
#' @param selected vector
#' @return dropdown menu


get_dropdown <- function(inputID, HTMLstring, choices, selected){
  selectInput(inputId = inputID,
              shiny::HTML(HTMLstring),
              #choices = unique(sort(cust_df$`FSR Name | FSR Territory ID`))
              choices = choices,
              selected = selected
  )
}

#' Creates LCR Over/Under the recommendation tab, including the plots
#' @title get_LCROverUnderTab
#'
#' @param Plot3 Growers or Seedsmen Over/Under Recommended LCR Plot
#' @param Plot4 LCR Over/Under the Recommendation Plot
#' @return The FSR LCR Analysis - LCR Over/Under the Rec. Tab

get_LCROverUnderTab <- function(Plot3, Plot4){
  tabPanel("LCR Over/Under the Rec.", 
           fluidRow(
             column(6,
                    plotlyOutput(outputId=Plot3)
             ),
             column(6,
                    plotlyOutput(outputId=Plot4)
             )
           )#, #end fluidRow
           #br(), br(),
           #splitLayout(cellWidths = c(350,60),
           #             h4("LCR Budget for FSR, as a percentage of gross: ",style="padding-top:1px;"),
           #             verbatimTextOutput("runrate_corn"))
           
  )}

#' Creates Current LCR Spend tab, including the plots
#' @title get_CurrentLCRTab
#'
#' @param Plot5 LCR Spend % off SRP plot
#' @param LCRDF dataframe containing fields: Bins of LCR as Percent, Reallocation Potential, Ave. Percent Delta to Target, etc
#' @return FSR LCR Analysis - Current LCR Spend Tab

get_CurrentLCRTab <- function(Plot5, LCRDF){
  tabPanel("Current LCR Spend", 
           fluidRow(
             column(
               plotlyOutput(outputId=Plot5), width = 12
             )
           ),
           fluidRow(
             br(),
             column(
               (DTOutput(LCRDF)), width = 12
             )
           ) 
  )
}

#' Creates LCR Decrease to Rec. tab
#' @title get_LCRDecreasetoRec
#'
#' @param Plot6 LCR Decrease to Recommendation Plot
#' @param LCRDF4 dataframe containing fields: Degree of Gross to Net % Delta to Target, Reallocation Potential, etc
#' @return FSR LCR Analysis - LCR Decrease to Rec.

get_LCRDecreasetoRec <- function(Plot6, LCRDF4){
  tabPanel("LCR Decrease to Rec.", fluidRow(
    column(
      plotlyOutput(outputId=Plot6), width = 12
    )
  ),
  fluidRow(
    br(),
    column(
      (DTOutput(LCRDF4)), width = 12
    )
  ) 
  )
}

#' Creates LCR by Seedsman tab
#' @title get_LCRbySeedsman
#'
#' @param Plot7 LCR Totals by Seedsman plot
#' @param LCRDF5 dataframe containing fields: Seedsman, LCR Realloc Pot. UNFAV, etc.
#' @return FSR LCR Analysis - LCR by Seedsman tab

get_LCRbySeedsman <- function(Plot7, LCRDF5){
  tabPanel("LCR by Seedsman", fluidRow(
    column(
      plotlyOutput(outputId=Plot7), width = 12
    )
  ),
  fluidRow(
    br(),
    column(
      (DTOutput(LCRDF5)), width = 12
    )
  ) 
  )
}

#' Creates LCR by Grower tab
#' @title get_LCRbyGrower
#'
#' @param LCRDF6 dataframe containing fields: Grower, Seedsman, Order Quantity, etc.
#' @return FSR LCR Analysis - LCR by Grower tab

get_LCRbyGrower <- function(LCRDF6){
  tabPanel("LCR by Grower", 
           fluidRow(
             column(
               (DTOutput(LCRDF6)), width = 12
             )
           ) 
  )
}

#' Creates LCR by Product tab
#' @title get_LCRbyProduct
#'
#' @param LCRDF8 dataframe containing fields: Product, Order Quantity, Actual LCR%, etc.
#' @return FSR LCR Analysis - LCR by Product tab

get_LCRbyProduct <- function(LCRDF8){
  tabPanel("LCR by Product",
           fluidRow(
             column(12,
                    (DTOutput(LCRDF8))
             )
           )
  )
}

#' Creates Seedsman - Seedsman View tab
#' @title get_SeedsmanView
#'
#' @param LCRDF7 dataframe containing fields: Seedsman, Order Quantity, Actual LCR%, etc.
#' @return Seedsman - Seedsman View tab

get_SeedsmanView <- function(LCRDF7){
  tabPanel("Seedsman View",
           fluidRow(
             column(12,
                    (DTOutput(LCRDF7))
             )
           )
  )
}

#' Creates Historical Seedsman Metrics tab
#' @title get_HistoricalSeedsmanMetrics
#'
#' @param plot_var_seedsman 
#' @param Title
#' @param Names names of seedsmen to include in dropdown
#' @param PlotName name of customer for the customer purchase history plot
#' @return Seedsman - Seedsman View tab
get_HistoricalSeedsmanMetrics <- function(plot_var_seedsman, Title, Names, PlotName, CROP, AppendCrop){
  tabPanel("Historical Seedsman Metrics",
           fluidRow(column(12,tags$b('Download Historical Seedsman Data'))),
           fluidRow(column(12,downloadButton(paste0("download_cust_hist", AppendCrop),
                                             label = paste0('Download - ', toupper(CROP))))),
           br(),
           br(),
           # # var to plot in customer plot
           get_dropdown(plot_var_seedsman, Title,
                        Names, ""),
           # customer purchase history plot
           plotlyOutput(outputId = PlotName)
  )
}

#' Creates Customer Portfolio tab
#' @title get_CustPortfolio
#'
#' @param AppendCrop character string of either "corn" or "soy". Generates records associated with that crop.
#' @return Customer Portfolio Tab

get_CustPortfolio <- function(AppendCrop){
  shinyjs::hidden(
    div(id= paste0("show_hide",AppendCrop),
        splitLayout(cellWidths = c(150,100),
                    tags$b("* Offered Unit LCR: "),
                    prettyRadioButtons(inputId = paste0("dollar_percent_table", AppendCrop),
                                       label = NULL,
                                       choices = c("$" = "dollar_table","%" = "percent_table"),
                                       selected = "percent_table",
                                       inline = TRUE)),
        br(),
        # show total portfolio plot -- corn
        actionButton(inputId = paste0('get_total_plot', AppendCrop),
                     label = HTML('Show Portfolio <br/> Total Plot')),
        fluidRow(column(12,h1("Portfolio Information",style="color:white;background-color:lightsteelblue;",align="center"))),
        # order info table -- corn
        tableOutput(outputId = paste0('order_table', AppendCrop)),
        #hr(),
        # main plot -- corn
        plotlyOutput(outputId=paste0("dist_plot", AppendCrop)),
        fluidRow(column(12,h1("FSRs Teach MARS: 4 Steps",style="color:white;background-color:lightsteelblue;",align="center"))),
        fluidRow(column(12, h4('Step 1 - Enter the average unit LCR offered for the Customer Portfolio above, either as a percentage or as a dollar amount. (If you have already entered offered unit LCR amounts into the Offered Unit LCR column of the Customer Portfolio, skip ahead to Step 2.)'))),
        br(),
        splitLayout(
          cellWidths = c(300,100),
          numericInput(inputId = paste0("lcr_offer", AppendCrop),
                       label = "Average Unit LCR Offered (Portfolio-Level)",
                       value = 0,
                       min = 0,
                       max = if(paste0("input$dollar_percent", AppendCrop) == "percent") {
                         100
                       } else {
                         800
                       },
                       step = 0.01,
                       width="250px"),
          prettyRadioButtons(inputId = paste0("dollar_percent", AppendCrop),
                             label = " ",
                             choices = c("$" = "dollar","%" = "percent"),
                             selected = "percent",
                             inline = TRUE)),
        br(),
        fluidRow(column(12, h4('Step 2 - Enter the average unit farmgate price that you were competing against, if known.'))),
        br(),
        numericInput(inputId = paste0("comp_offer", AppendCrop),
                     label = "Competing Avg. Unit Farmgate Price",
                     value = 0,
                     min = 0,
                     step = 0.01,
                     width="250px"),
        br(),
        fluidRow(column(12, h4('Step 3 - Check the box below if the deal was lost.'))),
        br(),
        prettyCheckbox(inputId = paste0('deal_lost', AppendCrop),
                       label = 'Deal Lost',
                       icon = icon("check"),
                       plain = TRUE,
                       inline = TRUE),
        br(),
        fluidRow(column(12,h4('Step 4 - Record your answers.'))),
        br(),
        # export cart -- corn
        actionButton(inputId = paste0('export_cart', AppendCrop),
                     label = HTML('Record <br/> Info')),
        # success message -- corn
        textOutput(outputId = paste0('download_success', AppendCrop)),
        br(),
        
        if(AppendCrop != "_soy"){
          shinyjs::hidden(
            div(id= "show_hide_downloadlink",
                'Click',
                downloadLink('download_recorded', 'here'),
                'to download recorded info.'))}
    ))
}


#' Creates FSR LCR Analysis tab
#' @title get_FSR_LCR_Tab
#'
#' @param CROP character string of either "corn" or "soy". Generates records associated with that crop.
#' @return FSR LCR Analysis tab
get_FSR_LCR_Tab <- function(CROP){
  
  if(CROP == "soy"){
    AppendCrop <- "_soy"
    ChoiceList <- append("", featsToPlotCleanNamesSoy[2:length(featsToPlotCleanNamesSoy)])
  }else{
    AppendCrop <- ""
    ChoiceList <- append("", featsToPlotCleanNames[2:length(featsToPlotCleanNames)])
  }
  
  tabPanel("FSR LCR Analysis",
           fluidRow(
             tabBox(
               title = tagList(shiny::icon("chart-bar"), "FSR LCR Analysis"),
               # The id lets us use input$tabset1 on the server to find the current tab
               id = "tabset1", width = NULL,
               get_LCROverUnderTab(paste0("LCRPlot3", AppendCrop), paste0("LCRPlot4", AppendCrop)),
               get_CurrentLCRTab(paste0("LCRPlot5", AppendCrop), paste0("LCRDF", AppendCrop)),
               get_LCRDecreasetoRec(paste0("LCRPlot6", AppendCrop), paste0("LCRDF4", AppendCrop)),
               get_LCRbySeedsman(paste0("LCRPlot7", AppendCrop), paste0("LCRDF5", AppendCrop)),
               get_LCRbyGrower(paste0("LCRDF6", AppendCrop)),
               get_LCRbyProduct(paste0("LCRDF8", AppendCrop)) 
             )#close TabBox
           ) #fluid row
  )
}

#' Creates Seedsman tab
#' @title get_Seedsman_Tab
#'
#' @param CROP character string of either "corn" or "soy". Generates records associated with that crop.
#' @return FSR LCR Analysis tab

get_Seedsman_Tab <- function(CROP){
  if(CROP == "soy"){
    AppendCrop <- "_soy"
    ChoiceList <- append("", featsToPlotCleanNamesSoy[2:length(featsToPlotCleanNamesSoy)])
  }else{
    AppendCrop <- ""
    ChoiceList <- append("", featsToPlotCleanNames[2:length(featsToPlotCleanNames)])
  }
  
  tabPanel("Seedsmen",
           fluidRow(
             tabBox(
               title = tagList(shiny::icon("clipboard-list"), "Seedsman Table"),
               # The id lets us use input$tabset1 on the server to find the current tab
               id = "tabset2", width = NULL,
               get_SeedsmanView(paste0("LCRDF7", AppendCrop)),
               get_HistoricalSeedsmanMetrics(paste0("plot_var_seedsman", AppendCrop), 
                                             "Plot Historical Seedsman Metrics", ChoiceList, paste0("seedsman_plot", AppendCrop), CROP, AppendCrop)
             )#close tabBox
           )#close fluidRow
  )
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
#' Creates Customer Information tab
#' @title get_Cust_Info_Tab
#'
#' @param CROP character string of either "corn" or "soy". Generates records associated with that crop.
#' @return Customer Information tab
get_Cust_Info_Tab <- function(CROP){
  if(CROP == "soy"){
    AppendCrop <- "_soy"
    ChoiceList <- append("", featsToPlotCleanNamesSoy[2:length(featsToPlotCleanNamesSoy)])
  }else{
    AppendCrop <- ""
    ChoiceList <- append("", featsToPlotCleanNames[2:length(featsToPlotCleanNames)])
  }
  # customer information tab -- corn
  tabPanel("Customer Information",
           tableOutput(outputId = paste0('grower_table', AppendCrop)),
           # competitive info
           tableOutput(outputId=paste0('comp_info', AppendCrop)),
           # purchase history
           tableOutput(outputId=paste0('purchase_hist', AppendCrop)),
           # switch to year view in purchase history table
           actionButton(inputId = paste0('year_view', AppendCrop),
                        label = 'Collapse Table to Order Level'),
           # switch to product view in purcahse history table
           actionButton(inputId = paste0('product_view', AppendCrop),
                        label = 'Expand Table to Product'),
           # var to plot in customer plot
           #get_dropdown(paste0('plot_var', AppendCrop), 'Plot Historical Customer Metrics',
           #             ChoiceList, ""),
           # customer purchase history plot
           #plotOutput(outputId = paste0('cust_plot', AppendCrop)),
           # customer location selection
           hr(),
           h4("Customer Crop Yield Advantage"),
           helpText("Please select customer crop location"),
           leafletOutput(outputId = paste0("mymap",AppendCrop)),
           #actionButton(inputId = paste0("show2", AppendCrop),label ="Populate yield advantage"),
           #checkboxInput("showP", "Clear historical polygon", FALSE), #v8_1
           actionButton(inputId = paste0('showP', AppendCrop),label = "Delete historical polygon"),
           #DT::dataTableOutput(outputId = paste0("mytable2",AppendCrop)),
           DT::dataTableOutput(outputId = paste0("mytable",AppendCrop)),
           plotlyOutput(outputId=paste0("disty_plot_monyield", AppendCrop)),
           plotlyOutput(outputId=paste0("disty_plot_compyield", AppendCrop)),
           plotlyOutput(outputId=paste0("disty_plot_yieldadv", AppendCrop)),
           plotlyOutput(outputId=paste0("disty_plot_seedpricedif", AppendCrop)),
           plotlyOutput(outputId=paste0("disty_plot_valueprop", AppendCrop)),
  )
}

#' Creates Customer Portfolio tab
#' @title get_Cust_Portfolio_Tab
#'
#' @param CROP character string of either "corn" or "soy". Generates records associated with that crop.
#' @return Customer Portfolio tab
get_Cust_Portfolio_Tab <- function(CROP){
  if(CROP == "soy"){
    AppendCrop <- "_soy"
  }else{
    AppendCrop <- ""
  }
  
  tabPanel(
    "Customer Portfolio", uiOutput(outputId = paste0('cart_ui', AppendCrop)),
    get_CustPortfolio(AppendCrop)
  )
}

#' Creates RecommenderUI tab
#' @title get_RecommenderUI
#'
#' @param AppendCrop character string of either "corn" or "soy". Generates records associated with that crop.
#' @return RecommenderUI Tab

get_RecommenderUI <- function(AppendCrop){
    div(id= "recommender", #paste0("recommender",AppendCrop),
        tags$style('#recommender {background-color: #5f6670;}'),
        #fluidRow(column(12,h1("Product Recommender",style="color:white;background-color:lightsteelblue;",align="center"))),
        #helpText("Please enter the information below for the Recommended Products base on the yield advantage."),
        #fluidRow(column(12, h4('Please enter the information below for the Recommended Products base on the yield advantage.'))),
        #br(),
        # numeric input for Downside Grower Profit Pctl -- corn
        fluidRow(
          column(3, numericInput(inputId = "Commodityprice", shiny::HTML("<p><span style='color: #C3CE44'>Avg Commodity Price (per bu)</span></p>"),
                                 value = 5)),
          column(3, numericInput(inputId = "Costperbag", shiny::HTML("<p><span style='color: #C3CE44'>Avg Cost per Bag</span></p>"),
                                 value = 200)),
          column(3, numericInput(inputId = "Seedsperbag", shiny::HTML("<p><span style='color: #C3CE44'>Seeds Per Bag</span></p>"),
                                 value = 80000)),
          column(3, numericInput(inputId = "Seedingrate", shiny::HTML("<p><span style='color: #C3CE44'>Default Seeding Rate (plants/ac)</span></p>"), 
                                 value = 37000)),
          column(3, selectInput(inputId = "OutcometoOptimize", shiny::HTML("<p><span style='color: #C3CE44'>Outcome to Optimize</span></p>"),
                                choices =  c("Profit","Yield"))),
          column(3, numericInput(inputId = "CropYieldGuarantee", shiny::HTML("<p><span style='color: #C3CE44'>Crop Ins. Yield Guarantee</span></p>"), 
                                 value = 200)), 
          column(3, numericInputIcon(inputId = "DownsideProfit", shiny::HTML("<p><span style='color: #C3CE44'>Downside Grower Profit Pctl</span></p>"), 
                                value = 5, min = 0, max =25, step = 5, icon = list(NULL, icon("percent")))),
          column(3, numericInputIcon(inputId = "UpsideProfit", shiny::HTML("<p><span style='color: #C3CE44'>Upside Grower Profit Pctl</span></p>"), 
                                value = 95, min = 75, max =100, step = 5, icon = list(NULL, icon("percent")))),
          # column(3, selectInput(inputId = "DownsideGrowerProfitPctl", shiny::HTML("<p><span style='color: #C3CE44'>Grower Profit Emphasis</span></p>"),
          #                       choices =  c("Downside Protection","Upside Potential", "Balance Downside and Upside")))
          # column(3,  actionButton(inputId = 'recommend_prod',
          #                                label = 'Recommend Products',
          #                                icon = icon("shopping-basket"))),
          # column(3,  actionButton(inputId = 'clear_order',
          #                                label = 'Clear Order',
          #                                icon = icon("minus-square")))
        ),

        # action buttons for recommend products/Clear Order -- corn
        fluidRow(column(4,  actionButton(inputId = 'recommend_prod',
                                         label = 'Recommend Products',
                                         icon = icon("shopping-basket"))),
                 column(4,  actionButton(inputId = 'clear_order',
                                         label = 'Clear Order',
                                         icon = icon("minus-square"))), responsive= TRUE),
        tags$style(type='text/css', "#recommend_prod { color: black; width: 100%; font-size: 12px;}"),
        tags$style(type='text/css', "#clear_order { color: black; width: 100%; font-size: 12px;}"),
        br()
        # if(AppendCrop != "_soy"){
        #     div(id= "show_hide_downloadlink",
        #         'Click',
        #         downloadLink('download_recorded', 'here'),
        #         'to download recorded info.')}
    )
}

# functionUI 4790 Generate UI elements for cart: table with buttons --------------------------------------------------
make_prod_ui <- function(prod_df, new_ids = FALSE) {
  if (new_ids) {
    id_base <- paste(sample(letters, 10), collapse = "")
    prod_df[, button_id := paste0(id_base, "_", 1:.N)]
  }
  prod_ui <- renderUI({
    cart_list <- split(prod_df, by = "button_id")
    table_row <- lapply(cart_list, function(row) {
      tags$tr(
        tags$td(actionButton(inputId = row$button_id,
                             label = "Delete")),
        #tags$td(row$Product),
        tags$td(selectInput("choose",label=NULL, choices = products_df, selected=row$Product),width = '75px'),
        tags$td(numericInput(inputId = paste0(row$button_id, "_od"),
                             label = NULL,
                             value = row[, 'Quantity Purchased'], 
                             #min = 0,
                             #max = 800,
                             #step = 0.01,
                             width = '75px'
        )),
        #tags$td(row[, 'Quantity Purchased']),
        # tags$td(numericInput(inputId = paste0(row$button_id, "_od"),
        #                      label = NULL,
        #                      value = 12,
        #                      width = '75px'
        #                      )),
        tags$td(numericInput(inputId = paste0(row$button_id, "_od"),
                             label = NULL,
                             value = 37000, 
                             #min = 0,
                             #max = 800,
                             #step = 0.01,
                             width = '75px'
                             )),
        #tags$tf(65),
        tags$td(numericInput(inputId = paste0(row$button_id, "_od"),
                             label = NULL,
                             value = 65,
                             width = '75px'
                             )),
        tags$td(checkboxInput("notdefine",label=NULL),width = '75px'),
        tags$td(row$Product),
      )
    })
    tagList(
      tags$table(
        class = "table table-hover table-striped",
        tags$th("Delete"),
        tags$th("Selected Product"),
        tags$th("Quantity (bags)"),
        tags$th("Seeding Rate (plants/ac)"),
        tags$th("Estimated Harvest Area (ac)"),
        tags$th("Lock Selected Product"),
        tags$th("Recommended Products"),
        table_row,
        table_row,
      )
    )
  })
  output <- list(ui = prod_ui,
                 ids = prod_df$button_id)
  return(output)
}

#' Creates Prodoct Recommend tab
#' @title get_Prodoct_Recommend_Tab
#'
#' @param CROP character string of either "corn" or "soy". Generates records associated with that crop.
#' @return Prodoct Recommend Tab #Customer Portfolio tab
get_Prodoct_Recommend_Tab <- function(CROP){
  if(CROP == "soy"){
    AppendCrop <- "_soy"
  }else{
    AppendCrop <- ""
  }
  tabPanel("Product Recommender", 
           #helpText("Please select product"),
           get_RecommenderUI(AppendCrop),
           br(),
           uiOutput(outputId = paste0('prod_ui', AppendCrop)),
           fluidRow(column(12,h3("OUTCOME COMPARISON",style="color:white;background-color:lightsteelblue;",align="center"))),
           # div(id= "COMPARISONtable", #paste0("COMPARISONtable",AppendCrop),
           #     tags$style('#COMPARISONtable {background-color: #5f6670;}'),
           #     fluidRow(column(12,tableOutput('COMPARISONtable')))
           # )
           fluidRow(column(12,tableOutput('COMPARISONtable')))
           #get_comparisonUI(AppendCrop)
  )
}

#' Creates Model Summary tab
#' @title get_Model_Summary_Tab
#'
#' @param CROP character string of either "corn" or "soy". Generates records associated with that crop.
#' @return Model Summary tab

get_Model_Summary_Tab <- function(CROP){
  if(CROP == "soy"){
    AppendCrop <- "_soy"
  }else{
    AppendCrop <- ""
  }
  # model summary tab -- corn
  tabPanel("Model Summary", tableOutput(outputId = paste0('discount_model_info', AppendCrop)),
           actionButton(inputId = paste0('discount_feat_button', AppendCrop),
                        label = 'Show Additional Predictors'),
           actionButton(inputId = paste0('hide_disct_features', AppendCrop),
                        label = 'Hide Additional Predictors'),
           tableOutput(outputId = paste0('discount_addl_info', AppendCrop)),
           tableOutput(outputId = paste0('ep_model_info', AppendCrop)),
           actionButton(inputId = paste0('ep_feat_button', AppendCrop),
                        label = 'Show Additional Predictors'),
           actionButton(inputId = paste0('hide_ep_features', AppendCrop),
                        label = 'Hide Additional Predictors'),
           tableOutput(outputId = paste0('ep_addl_info', AppendCrop))
  )
}

#' Creates About tab. Only created for consistency. About tab lives in the html file.
#' @title get_About_Tab
#'
#' @return About tab

get_About_Tab <- function(){
  tabPanel("About", includeHTML("www/mars_instructions.html"))
}

#' Creates Feedback tab
#' @title get_Feedback_Tab
#'
#' @param CROP character string of either "corn" or "soy". Generates records associated with that crop.
#' @return Feedback tab

get_Feedback_Tab <- function(CROP){
  if(CROP == "soy"){
    AppendCrop <- "_soy"
  }else{
    AppendCrop <- ""
  }
  tabPanel("Feedback",
           #shinyjs::hidden(
           div(id= "show_hide_feedback",
               fluidRow(tags$b('Please fill out the survey below. Your feedback will be used to improve the MARS tool. If you have an immediate concern, please contact Charles Yeh at chiawei.yeh.ext@bayer.com.')),
               br(),
               # fluidRow(column(12,h4('1. Rate your overall experience with the MARS tool.'))),
               # shinyUI(bootstrapPage(
               #   h3(ratingInput(paste0("feedback1", AppendCrop), label="", dataStop=5, dataFractions=2), style = "color:#3399ff;padding-left:30px;")
               # )),
               fluidRow(column(12,h4('1. What types of technical issues, if any, have you experienced while using the MARS tool? Select all that apply.'))),
               fluidRow(column(12, style='padding-left:30px;', checkboxGroupInput(paste0("feedback2a", AppendCrop),label = "",
                                                                                  choiceNames =
                                                                                    list('Frequent disconnections from server', 'App slowness and/or excessive load times', 'Page is not working or Cannot access page errors', 'Out-of-date or inaccurate SM or LCR data', 'Other', 'No technical issues experienced'),
                                                                                  choiceValues =
                                                                                    list("disconnections", "long_loadtimes", "cannot_access_page", "incorrect_data","other", "no_issues"),
                                                                                  width = '550px'
               ))),
               # if(TRUE){
               #   h4('ok, this worked..')
               # },
               #fluidRow(column,12,style='padding-left:30px;', checkboxInput("no_issues", "No significant technical issues experienced", FALSE)),
               fluidRow(column(6, style='padding-left:35px;padding-top:30px;', 'If you checked Other, please explain:'), column(6,textInput(paste0("feedback2b", AppendCrop), label = "", value = ""))),
               #fluidRow(column(12, style='padding-left:30px;', textInput("feedback6b", label = "", value = "", width = '550px', placeholder = NULL))),
               #fluidRow(column(12, verbatimTextOutput("feedback_value"))),
               conditionalPanel(
                 #condition = "length(grep('no_issues',paste0(input.feedback2a))) > 0",
                 condition = "input.feedback2a.includes('no_issues')== false",
                 fluidRow(column(12,h4('1b. Have you contacted Charles Yeh (chiawei.yeh.ext@bayer.com) on the MARS support team in the past about a technical issue? Select all that apply.'))),
                 fluidRow(column(12, style='padding-left:30px;', checkboxGroupInput(paste0("feedback3", AppendCrop),label = "",
                                                                                    choiceNames =
                                                                                      list('Yes - the issue was resolved.', 'Yes - but the issue was not resolved (or not resolved to my satisfaction).', 'No - I did not know who to contact.',  'No - I did not want to take the time.',  'No - the technical issue was minor.'),
                                                                                    choiceValues =
                                                                                      list("yes_resolved", "yes_unresolved", "no_nocontactinfo", "no_notime", "no_noissues"),
                                                                                    width = "550px"
                 )))
               ),
               fluidRow(column(12,h4('2. Rate the following statement: The MARS tool is easy to use.'))),
               fluidRow(column(12, style='padding-left:30px;', prettyRadioButtons(inputId = paste0("feedback4", AppendCrop),
                                                                                  label = " ",
                                                                                  choices = c("Strongly agree " = "strongly_agree","Agree " = "agree", "Neutral " = "neutral", "Disagree " = "disagree", "Strongly disagree " = "strongly_disagree"),
                                                                                  selected = character(0),
                                                                                  inline = TRUE))),
               fluidRow(column(12,h4('3. Rate the following statement: The MARS tool provides useful insights.'))),
               fluidRow(column(12, style='padding-left:30px;', prettyRadioButtons(inputId = paste0("feedback5", AppendCrop),
                                                                                  label = " ",
                                                                                  choices = c("Strongly agree" = "strongly_agree","Agree" = "agree", "Neutral" = "neutral", "Disagree" = "disagree", "Strongly disagree" = "strongly_disagree"),
                                                                                  selected = character(0),
                                                                                  inline = TRUE))),
               fluidRow(column(12,h4('4.	What types of positive feedback would you give or have you received about the MARS tool? Select all that apply.'))),
               fluidRow(column(12, style='padding-left:30px;', checkboxGroupInput(paste0("feedback6a", AppendCrop),label = "",
                                                                                  choiceNames =
                                                                                    list('Helps allocate market funding dollars more effectively and drives recommendations', 'Helps identify areas of opportunity/outlier spend', 'Provides (downloadable) historical account details', 'Provides useful information on Channel value proposition and grower-price sensitivity','Improves fairness of market funding allocations', 'Provides useful recommendations for determining specific discounts for specific grower orders', 'Other', 'None of the above'),
                                                                                  choiceValues =
                                                                                    list("improves_allocations_general", "identifies_opportunities", "provides_hxdata", "provides_valueprop", "provides_value_prop", "improves_allocations_specific", "other", "none"),
                                                                                  width = "550px"
               ))),
               #fluidRow(column(6, style='padding-left:35px;padding-top:30px;', 'If you checked Other, please explain:'), column(6,textInput("feedback6b", label = "", value = ""))),
               fluidRow(column(width = 12, style='padding-left:30px;', h4('If you checked other, please explain:'))),
               fluidRow(column(12, style='padding-left:30px;', textAreaInput(paste0("feedback6b", AppendCrop), label = "", value = "", width = '550px', height = '50px', placeholder = NULL))),
               fluidRow(column(12,h4('5. How likely are you to recommend the MARS tool to your team or others?'))),
               fluidRow(column(12, style='padding-left:30px;', sliderInput(paste0("feedback7a", AppendCrop), label = div(style='width:550px;', 
                                                                                                                         div(style='float:left;', 'not likely'), 
                                                                                                                         div(style='float:right;', 'very likely')),
                                                                           min = 0, max = 10,
                                                                           value = 5, width = '550px'))),
               conditionalPanel(
                 condition = "input.feedback7a < 8",
                 fluidRow(column(12, style='padding-left:30px;', h4('Why are you not more likely to recommend the MARS tool? Select the answer that best applies.'))),
                 fluidRow(column(12, style='padding-left:30px;', prettyRadioButtons(paste0("feedback7a2", AppendCrop), "",
                                                                                    c("I experienced technical issues." = "technical_issues",
                                                                                      "Data was inaccurate or out-of-date." = "inaccurate_data",
                                                                                      "MARS is complicated to use." = "too_complicated",
                                                                                      "My SM and I rely more on our local knowledge to make LCR discount decisions." = "rely_on_localknowledge",
                                                                                      "My SM and I use a run rate to manage our market funding." = "rely_on_runrate"), selected = character(0))))
               ),
               fluidRow(column(width = 12, style='padding-left:30px;', h4('Please provide any additional feedback related to question 5 in the space below:'))),
               fluidRow(column(12, style='padding-left:30px;', textAreaInput(paste0("feedback7b", AppendCrop), label = "", value = "", width = '550px', height = '150px', placeholder = NULL))),
               fluidRow(column(12, h4('6. Please provide any additional feedback about the MARS tool in the space below:'))),
               fluidRow(column(12, style='padding-left:30px;', textAreaInput(paste0("feedback8", AppendCrop), label = "", value = "", width = '550px', height = '200px', placeholder = NULL))),
               fluidRow(align = "center", column(12, actionButton('feedback_button', label = "Submit")))
           ), #end show_hide_feedback
           shinyjs::hidden(
             div(id= "show_hide_thanks",
                 fluidRow(align = "center", column(12,h3("Thank you!"))),
                 fluidRow(align = "center", column(12,h4("Your feedback has been recorded.")))
             ))
  )
  
}

#' Creates Daily Users Tab
#' @title get_DailyUniqueUsers_Tab
#'
#' @return Daily Unique Users Tab
#' 

get_DailyUniqueUsers_Tab <- function(){
  tabPanel("Daily Unique Users", 
           br(),
           br(),
           column(9,
                  fluidRow(
                    plotlyOutput(outputId="DailyVisitsPlot")
                  )
           ),
           column(3, tableOutput('DailyVisitsSubsetTable2'))
  )
}
#' Creates Daily Total Users Tab
#' @title get_DailyTotalUsers_Tab
#'
#' @return Daily Total Users Tab
#' 
get_DailyTotalUsers_Tab <- function(){
  tabPanel("Daily Total Users", 
           br(),
           br(),
           column(9,
                  fluidRow(
                    
                    plotlyOutput(outputId="DailyTotalVisitsPlot")
                  )
           ),
           column(3, tableOutput('TotalVisitsSubsetTable2'))
  )}
         

#' Creates Daily Minutes Tab
#' @title get_DailyMinutes_Tab
#'
#' @return Daily Minutes Tab
#' 
get_DailyMinutes_Tab <- function(){
  tabPanel("Daily Minutes Used", 
         br(),
         br(),
         column(9,
                fluidRow(
                  
                  plotlyOutput(outputId="DailyMinutesUsedPlot")
                )
         ),
         column(3, tableOutput('DailyTotalTimeSubsetTable2'))
)}

#' Creates Average Minutes Tab
#' @title get_AvgMinutes_Tab
#'
#' @return Average Minutes Tab
#'
get_AvgMinutes_Tab <- function(){
  tabPanel("Average Minutes Used", 
         br(),
         br(),
         column(9,
                fluidRow(
                  
                  plotlyOutput(outputId="DailyMeanMinutesUsedPlot")
                )
         ),
         
         column(3, tableOutput('DailyMeanTimeSubsetTable2'))
)}

#' Creates User Log Tab
#' @title get_UserLog_Tab
#'
#' @return User Log Tab
#'

get_UserLog_Tab <- function(){
  tabPanel("User Log", 
           br(),
           br(),
           
           fluidRow(
             DTOutput('UserLogDT')
             #plotlyOutput(outputId="DailyMeanMinutesUsedPlot")
           )
           
  )
  
}


write_error <- function(e, err_id) {
  err_report <- paste0('MARS error ', err_id, ': ', e, '; ', Sys.time(), ' ', Sys.timezone(), '; R process ID: ', Sys.getpid())
  err_report <- c(err_report,'', '', '', '# traceback() ##########################', '', as.character(traceback()), '', '', '', '# sys.calls() ##########################', '', as.character(sys.calls()))
  write.table(err_report, paste0('./recorded_user_data/error_logs/err_report_global_', err_id, '.txt'),row.names = FALSE, col.names = FALSE)
}

get_new_portfolio_row <- function(Product, Quantity, products_df, cart_df){
  # make new row data table from user input
  new_row <- data.table(Product = Product,
                        Quantity = Quantity)
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
  return(new_row_tmp)
}
