#' Produce Seedsmen - Seedsman View Tab
#'
#' @title seedsmanViewUI 
#' @author Julie Wisch (elzej)
#'
#' @description Generate the seedsman table view tab
#'
#' @param id app namespace
#'
#' @return A tab with a tabular display of seedsman order quantity, lcr & etc.
#'
#' @export
seedsmanViewUI <- function(id){
  ns <- NS(id)

  tagList(
           fluidRow(
             column(12,
                    (DTOutput(ns("LCRDF7"))
            )
           )
          )
  )
}



#' Server Side - Produce Seedsmen - Seedsman View Tab
#'
#' @title seedsmanViewServer
#' @author Julie Wisch (elzej)
#'
#' @description Generate the seedsman view table
#' 
#' @param fsrString string of fsr name
#' @param grower_repot dataframe either grower_report or grower_report_soy
#' @param CROP string of crop name
#' 
#' @return "LCRDF7" a table containing all the seedsman data
#'
#' @export
seedsmanViewServer <- function(input, output, session, fsrString, grower_report, LCRTotals7_raw0, LCRTotals7_raw, CROP) {
crop_upper <- toupper(CROP)
crop_lower <- tolower(CROP)
  
  LCRTotals7 <- reactive({ 
    LCRTotals7_raw_args <- list(fsrString(), grower_report, crop_lower)
    wrapMemoise(LCRTotals7_raw0, LCRTotals7_raw, LCRTotals7_raw_args)
  })
  
  output$LCRDF7 <- renderDT({ 
    dtLCRDF7_args <- list(fsrString(), LCRTotals7(), crop_upper)
    wrapMemoise(dtLCRDF7_raw, dtLCRDF7, dtLCRDF7_args)
  })  
  

}

#'Produce Seedsmen - Historic Seedsman Metrics Tab
#'
#' @title histSeedsmanMetricsUI
#' @author Julie Wisch (elzej)
#'
#' @description Generate historic seedsman metrics tab
#' 
#' @param id namespace id
#' @param CROP string of crop name
#' @param Title title of the plot_var_seedsman plot
#' @param Names names to display in the plot_var_seedsman plot
#' 
#' @return Historic Seedsman Metrics Tab
#'
#' @export
histSeedsmanMetricsUI <- function(id, CROP, Title, Names){
  ns <- NS(id)
  
  tagList(
    fluidRow(column(12,tags$b('Download Historical Seedsman Data'))),
    fluidRow(column(12,downloadButton(ns("download_cust_hist"),
                                      label = paste0('Download - ', toupper(CROP))))),
    br(),
    br(),
    # # var to plot in customer plot
    get_dropdown(ns('plot_var_seedsman'), Title,
                 Names, ""),
    # customer purchase history plot
    plotlyOutput(outputId = ns("seedsman_plot"))
  )
  
}

#'Server Side - Produce Seedsmen - Historic Seedsman Metrics Tab
#'
#' @title histSeedsmanMetricsServer
#' @author Julie Wisch (elzej)
#'
#' @description Generate historic seedsman metrics tab
#' 
#' @param id namespace id
#' @param dropdown_outputs list of inputs from the common sidebar selections
#' @param cust_df dataframe of customer information
#' @param fsrColumn character of the name of the column in cust_df that contains the fsr names
#' @param fsrString string of fsr name
#' @param seedsmanString string of seedsman name
#' @param CROP string of crop name
#' @param custIDColumn character of the name of the customer ID column in the cust_df
#' @param custFeaturestoPlot character of which customer feature to plot
#' @param featsToPlotCleanNames display names of which customer features to plot
#' @param seedsmanIDColumn character of the name of the seedsman ID column in the cust_df
#' @param seedsmanNameColumn character of the name of the seedsman name column in the cust_df
#' @param discountColumn character of the name of the discount name column in the cust_df
#' @param priceColumn character of the name of the price column in the cust_df
#' 
#' @return Historic Seedsman Metrics Tab
#'
#' @export
histSeedsmanMetricsServer <- function(input, output, session, dropdown_outputs, cust_df, fsrColumn, fsrString, 
                                      seedsmanString, CROP,
                                      custIDColumn, custFeaturesToPlot, featsToPlotCleanNames,
                                      seedsmanIDColumn, seedsmanNameColumn, discountColumn,
                                      priceColumn){
  if(CROP == "CORN"){
  cust_hist_features <- c('FSR', 'TERR_ID', 'FIPS',	'Year',	'GrowerSAPID',	'SeedsmanSAPID',
                          'REGION',	'REGION_ID',	'GrowerAcctNm',	'SeedsmanAcctNm',	'TEAM_ID',
                          'ORDERED_QTY',	'RETAIL_AMT',	'AVGPRICE_RETAIL',	'OFFERED_DISC',
                          'BRAND_DISC',	'NET_AMT_WO_CASH',	'AVGPRICE_WO_CASH',	'CASH_DISC',
                          'NET_AMT_INCL_CASH',	'AVG_FARMGATE_PRICE',	'NEW_CUST',	'RBD',	'ABM',	'ABM.ID')}else if(CROP == "SOY"){
                            cust_hist_features <- c('FSR', 'TERR_ID', 'FIPS',	'MKT_YR',	'GRWR_SAP_ID',	'DLR_SAP_ID',
                                                    'REGION',	'REGION_ID',	'GRWR_ACCT_NAME',	'DLR_NAME',	'TEAM_ID',
                                                    'ORDERED_QTY',	'RETAIL_AMT',	'AVGPRICE_RETAIL',	'OFFERED_DISC',
                                                    'BRAND_DISC',	'NET_AMT_WO_CASH',	'AVGPRICE_WO_CASH',	'CASH_DISC',
                                                    'NET_AMT_INCL_CASH',	'AVG_FARMGATE_PRICE',	'NEW_CUST',	'RBD',	'ABM',	'ABM.ID')

                          }

  rv <- reactiveValues(download_flag = 0)
  output$download_cust_hist <- downloadHandler(
    filename = function() {
      paste('MARScustomerhistory', '-', str_replace(isolate(fsrString()),', ','_'), '_', Sys.Date(), '.csv', sep='')
    },
    content = function(con) {
      write.csv(cust_df[which(cust_df[[fsrColumn]] == fsrString()), cust_hist_features], con)
      rv$download_flag <- rv$download_flag + 1
    }
  )

  #record when user cwid and fsrname when user downloads customer data (CORN)
  observeEvent(rv$download_flag,{
    if(rv$download_flag > 0) {
      write.table( paste0('User ', isolate(session$user), ' downloaded historical ', CROP, ' data tied to FSR ', isolate(fsrString())),
                   file="./recorded_user_data/fsr_download_history.csv",
                   append = T,
                   sep='\n',
                   row.names=F,
                   col.names=F )
    }
  })

  
    # what should happen when users selects variables to plot from drop down on seedsman tab
   observeEvent(input$plot_var_seedsman, {
      req(input$plot_var_seedsman)
      seedsman_id <- reactive({sub(".* | ", "", dropdown_outputs$seedsman())}) #extracts id number out of customer name dropdown
      # if not initial choice plot selected variable
      if (input$plot_var_seedsman != ""){
        seedsman_plot <- reactive({
                            req(dropdown_outputs$fsr())
                            make_seedsman_plot(cust_df,
                                            custIDColumn,
                                            input$plot_var_seedsman,
                                            custFeaturesToPlot,
                                            featsToPlotCleanNames,
                                            seedsmanIDColumn,
                                            seedsmanNameColumn,
                                            seedsman_id(),
                                            discountColumn,
                                            priceColumn,
                                            fsrColumn,
                                            fsrString(),
                                            seedsmanString()) })
        output$seedsman_plot<- renderPlotly(seedsman_plot())
  } else {  # when var is reset to initial choice, plot is cleared
    output$seedsman_plot <- NULL
  }
   }, ignoreInit=FALSE)

}
