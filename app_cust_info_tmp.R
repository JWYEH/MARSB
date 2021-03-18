# ====================================================================================================
# Market Funding Recommender System (MARS) Shiny App
# Version 2.0
# Corn + Soy
# ====================================================================================================

# tryCatch({
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

library(memoise)
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
library(ShinyRatingInput, lib.loc = "./Library")
library(shinyalert)

# Souce file configsUI.R and functionsUI.R ===========================================================
# Various helper functions and parameters used in the server potion of app.R are set here,
# see files for more information
source('functionsUI.r')
source('configsUI.r')
source('dollar.r')
source('percent.r')
source('GlobalLoad.r')
source('displayedfeatures.r') #Modularized feature display code
source('KPIMetricstab.r') #Modularized KPI Metrics tab
source('LCRAnalysisTab.r')
source('seedsmanTab.r') #Modularized Seedsman view tab
source('custInfotab.r') #Modularized customer information tab
source('CustPortfolioTab.r') #modularized portfolio tab


# UI =================================================================================================
ui <- fluidPage(sidebarLayout(
  sidebarPanel(width = 3,
               commonDropdownUI("SharedSidebar", choices_fsr, selected_fsr, choices_seedsman, selected_seedsman,
                                choices_cust, "", choices_newcust, "", NULL, ""),
               conditionalPanel(condition="input.tabselected==1",
                                cropSpecificDropdownUI("SharedSidebar1", choices_product, "")),
               conditionalPanel(condition="input.tabselected==2",
                                cropSpecificDropdownUI("SharedSidebar2", choices_product_soy, "")),
               commonPortfolioButtonsUI("SharedSidebar"),
               commonPasswordInputUI("PasswordInput"),
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
                )
              ),
              
              # CORN ---------------------------------------------------------------------------------------------
              tabPanel("Corn", value=1, 
                       add_busy_spinner(spin='fading-circle'),
                       fluidRow(
                         infoBoxOutput("Rec.LCR.Unit"),
                         tags$style("#Rec.LCR.Unit {width:50%;}"),
                         infoBoxOutput("Rec.LCR.Pct"),
                         tags$style("#Rec.LCR.Pct {width:50%;}")
                       ),
                       fluidRow(
                         #tabs in the main panel
                         tabBox(width = NULL,
                                tabPanel("FSR LCR Analysis",
                                         fluidRow(
                                           tabBox(width = NULL,
                                                  title = tagList(shiny::icon("chart-bar"), "FSR LCR Analysis"),
                                                  id = "tabset1", 
                                                  tabPanel("LCR Over/Under the Rec.",
                                                           LCROverUnderUI("Corn_tab")),
                                                  tabPanel("Current LCR Spend",
                                                           LCRPlotandTableUI("Corn_tab_Current")),
                                                  tabPanel("LCR Decrease to Rec.",
                                                           LCRPlotandTableUI("Corn_tab_Decrease")),
                                                  tabPanel("LCR by Seedsman",
                                                           LCRPlotandTableUI("Corn_tab_Seedsman")),
                                                  tabPanel("LCR by Grower",
                                                           LCRTablesUI("Corn_tab_Grower", "LCR by Grower")),
                                                  tabPanel("LCR by Product",
                                                           LCRTablesUI("Corn_tab_Product", "LCR by Product")))#end tab box
                                         ) #end fluid row
                                ),
                                tabPanel("Seedsmen",
                                         fluidRow(
                                           tabBox(title = tagList(shiny::icon("clipboard-list"), "Seedsman Table"),
                                                  id = "tabset2", width = NULL,
                                                  tabPanel("Seedsman View",
                                                           seedsmanViewUI("Corn_tab")),
                                                  tabPanel("Historical Seedsman Metrics",
                                                           histSeedsmanMetricsUI("Corn_tab", "CORN", "Plot Historical Seedsman Metrics",
                                                                                 append("", featsToPlotCleanNames[2:length(featsToPlotCleanNames)]))
                                                           )
                                                  ) #end tabBox
                                         ) #end fluid row
                                ),
                                tabPanel("Customer Information",
                                         custInfoUI("Corn_tab", append("", featsToPlotCleanNames[-1]))),
                                tabPanel("Customer Portfolio",
                                         custPortfolioUI("Corn_tab")
                                ),
                                tabPanel("Model Summary", 
                                         featuresUI("corn_features", "Show Additional Predictors", "Hide Additional Predictors")),
                                 tabPanel("About", 
                                          includeHTML("www/mars_instructions.html"))
                         ) #end tabBox
                       )#end fluid Row
              ), #end tabPanel Corn
              # SOY ---------------------------------------------------------------------------------------------
              tabPanel("Soy", value=2, 
                       add_busy_spinner(spin='fading-circle'),
                       fluidRow(
                         infoBoxOutput("Rec.LCR.Unit_soy"),
                         tags$style("#Rec.LCR.Unit_soy {width:50%;}"),
                         infoBoxOutput("Rec.LCR.Pct_soy"),
                         tags$style("#Rec.LCR.Pct_soy {width:50%;}")
                       ),
                       fluidRow(
                         #tabs in the main panel
                         tabBox(width = NULL,
                                tabPanel("FSR LCR Analysis",
                                         fluidRow(
                                           tabBox(width = NULL,
                                                  title = tagList(shiny::icon("chart-bar"), "FSR LCR Analysis"),
                                                  id = "tabset1", 
                                                  tabPanel("LCR Over/Under the Rec.",
                                                           LCROverUnderUI("Soy_tab")),
                                                  tabPanel("Current LCR Spend",
                                                           LCRPlotandTableUI("Soy_tab_Current")),
                                                  tabPanel("LCR Decrease to Rec.",
                                                           LCRPlotandTableUI("Soy_tab_Decrease")),
                                                  tabPanel("LCR by Seedsman",
                                                           LCRPlotandTableUI("Soy_tab_Seedsman")),
                                                  tabPanel("LCR by Grower",
                                                           LCRTablesUI("Soy_tab_Grower", "LCR by Grower")),
                                                  tabPanel("LCR by Product",
                                                           LCRTablesUI("Soy_tab_Product", "LCR by Product")))#end tab box
                                           ) #end fluid row
                                         ),
                                tabPanel("Seedsmen",
                                         fluidRow(
                                           tabBox(title = tagList(shiny::icon("clipboard-list"), "Seedsman Table"),
                                                  id = "tabset2", width = NULL,
                                                  tabPanel("Seedsman View",
                                                           seedsmanViewUI("Soy_tab")),
                                                  tabPanel("Historical Seedsman Metrics",
                                                          histSeedsmanMetricsUI("Soy_tab", "SOY", "Plot Historical Seedsman Metrics",
                                                                                append("", featsToPlotCleanNamesSoy[2:length(featsToPlotCleanNamesSoy)]))
                                                           )
                                                  ) #end tabBox
                                         ) #end fluid row
                                         ),
                                tabPanel("Customer Information",
                                         custInfoUI("Soy_tab", append("", featsToPlotCleanNamesSoy[-1]))),
                                tabPanel("Customer Portfolio",
                                         custPortfolioUI("Soy_tab")),
                                tabPanel("Model Summary", 
                                         featuresUI("soy_features", "Show Additional Predictors", "Hide Additional Predictors")),
                                tabPanel("About", 
                                         includeHTML("www/mars_instructions.html"))
                         ) #end tabBox
                       )#end fluid Row
              ), #end tabPanel soy                                        
              id = "tabselected"
    )
  )
))

server <- function(input, output, session) {
  #Getting inputs from the sidebars 
  dropdown_outputs <- callModule(commonDropdownServer, "SharedSidebar")
  dropdown_outputs_portfolio <- callModule(commonPortfolioButtonsServer, "SharedSidebar")
  
  #Updating inputs upon selection
  callModule(clearingDropdownServer, "SharedSidebar", 
             cust_df_displaynames, terrIDColumn, stateCountyToFips)
  
  #Handling selections
  #grab fsr as string
  fsrString <- callModule(get_fsrStringServer, NULL, dropdown_outputs)
  fsrString_soy <- callModule(get_fsrStringServer, NULL, dropdown_outputs)
  
  #grab seedsman as string
  seedsmanString <- callModule(get_seedsmanStringServer, NULL, dropdown_outputs)
  seedsmanString_soy <- callModule(get_seedsmanStringServer, NULL, dropdown_outputs)
  
  #compute budget run rate for corn given current fsr and attach it to output$runrate_corn
  #This output is not currently displayed anywhere in the app, but is anticipated to be deployed in v2
  budgetrunrate_corn <- callModule(get_budgetRunRateServer, NULL, runrate_df_corn, fsrString)
  budgetrunrate_soy <- callModule(get_budgetRunRateServer, NULL, runrate_df_soy, fsrString_soy)


  #FSR LCR Analysis Tab Modules ##############################################
  callModule(LCROverUnderServer, "Corn_tab", fsrString, selected_fsr_init, grower_report,
             LCRTotals_raw0, LCRTotals_raw, 'CORN')
  callModule(LCROverUnderServer, "Soy_tab", fsrString_soy, selected_fsr_init_soy, grower_report_soy,
              LCRTotals_raw0, LCRTotals_raw, 'SOY')
  callModule( LCRPlotandTableServer, "Corn_tab_Current", fsrString, 
              grower_report, plotlyLCRPlot5, LCRTotals3_raw0, LCRTotals3_raw, dtLCRDF_raw, dtLCRDF, "CORN") 
  callModule( LCRPlotandTableServer, "Soy_tab_Current", fsrString_soy, 
                grower_report_soy, plotlyLCRPlot5, LCRTotals3_raw0, LCRTotals3_raw, dtLCRDF_raw, dtLCRDF, "SOY")
  callModule( LCRPlotandTableServer, "Corn_tab_Decrease", fsrString, 
              grower_report, plotlyLCRPlot6, LCRTotals4_raw0, LCRTotals4_raw, dtLCRDF4_raw, dtLCRDF4, "CORN") 
  callModule( LCRPlotandTableServer, "Soy_tab_Decrease", fsrString_soy, 
              grower_report_soy, plotlyLCRPlot6, LCRTotals4_raw0, LCRTotals4_raw, dtLCRDF4_raw, dtLCRDF4, "SOY")
  callModule( LCRPlotandTableServer, "Corn_tab_Seedsman", fsrString, 
              grower_report, plotlyLCRPlot7, LCRTotals5B_raw0, LCRTotals5B_raw, dtLCRDF5_raw, dtLCRDF5, "CORN") 
  callModule( LCRPlotandTableServer, "Soy_tab_Seedsman", fsrString_soy, 
              grower_report_soy, plotlyLCRPlot7, LCRTotals5B_raw0, LCRTotals5B_raw, dtLCRDF5_raw, dtLCRDF5, "SOY")
  callModule(LCRTablesServer, "Corn_tab_Grower", fsrString, 
                                         grower_report, dtLCRDF6, LCRTotals6_raw0, LCRTotals6_raw, "CORN") 
  callModule(LCRTablesServer, "Soy_tab_Grower", fsrString, 
             grower_report_soy, dtLCRDF6, LCRTotals6_raw0, LCRTotals6_raw, "SOY") 
  callModule(LCRTablesServer, "Corn_tab_Product", fsrString, 
             grower_report, dtLCRDF8, LCRTotals8_raw0, LCRTotals8_raw, "CORN") 
  callModule(LCRTablesServer, "Soy_tab_Product", fsrString, 
             grower_report_soy, dtLCRDF8, LCRTotals8_raw0, LCRTotals8_raw, "SOY")
  #Seedsman View Tab Modules ##############################################
  
  callModule(seedsmanViewServer, "Corn_tab",fsrString, grower_report, LCRTotals7_raw0, LCRTotals7_raw, 'CORN')
  callModule(seedsmanViewServer, "Soy_tab",fsrString_soy, grower_report_soy, LCRTotals7_raw0, LCRTotals7_raw, 'SOY')
  callModule(histSeedsmanMetricsServer, "Corn_tab", dropdown_outputs, cust_df, fsrColumn, fsrString, seedsmanString, 'CORN',
             custIDColumn, custFeaturesToPlot, featsToPlotCleanNames,
             seedsmanIDColumn, seedsmanNameColumn, discountColumn,
             priceColumn)
  callModule(histSeedsmanMetricsServer, "Soy_tab", dropdown_outputs, cust_df_soy, fsrColumnSoy, fsrString_soy,
             seedsmanString_soy, 'SOY',
             custIDColumnSoy, custFeaturesToPlotSoy, featsToPlotCleanNamesSoy,
             seedsmanIDColumnSoy, seedsmanNameColumnSoy, discountColumnSoy,
             priceColumnSoy)
  
  
  #Customer Information Tab Modules ##############################################
  callModule(growerTableServer, "Corn_tab", dropdown_outputs, 
             cust_df, custIDColumn, yearColumn, products_df) 
  callModule(growerTableServer, "Soy_tab", dropdown_outputs, 
             cust_df_soy, custIDColumnSoy, yearColumnSoy, products_df_soy) 
  callModule(compInfoServer, "Corn_tab", dropdown_outputs, 
             cust_df, custIDColumn, fipsCodeColumn, valueProp, 
             stateCountyToFips, compTableNamesChannel, compTableNamesOther)
  callModule(compInfoServer_soy, "Soy_tab", dropdown_outputs,
             cust_df, custIDColumn, fipsCodeColumn, YieldAdvantageSoy,  
             stateCountyToFips, compTableNamesChannel, compTableNamesOther) 
  callModule(purchaseHistServer, "Corn_tab", dropdown_outputs, 
             cust_df, custIDColumn, custPurchaseNames, formattedPurchaseNames,
             EffPrice, seedsmanIDColumn, yearColumn, FALSE) 
  callModule(purchaseHist_collapseServer, "Corn_tab", dropdown_outputs, 
             cust_df, custIDColumn, custPurchaseNames, formattedPurchaseNames,
             EffPrice, seedsmanIDColumn, yearColumn, TRUE, products_df)
  callModule(purchaseHist_expandServer, "Corn_tab", dropdown_outputs, 
             cust_df, custIDColumn, custPurchaseNames, formattedPurchaseNames,
             EffPrice, seedsmanIDColumn, yearColumn, FALSE, products_df)
  callModule(purchaseHistServer, "Soy_tab", dropdown_outputs, 
             cust_df_soy, custIDColumnSoy, custPurchaseNamesSoy, formattedPurchaseNamesSoy,
             EffPrice_soy, seedsmanIDColumnSoy, yearColumnSoy, FALSE)
  callModule(purchaseHist_collapseServer, "Soy_tab", dropdown_outputs, 
             cust_df_soy, custIDColumnSoy, custPurchaseNamesSoy, formattedPurchaseNamesSoy,
             EffPrice_soy, seedsmanIDColumnSoy, yearColumnSoy, TRUE, products_df_soy)
  callModule(purchaseHist_expandServer, "Soy_tab", dropdown_outputs, 
             cust_df_soy, custIDColumnSoy, custPurchaseNamesSoy, formattedPurchaseNamesSoy,
             EffPrice_soy, seedsmanIDColumnSoy, yearColumnSoy, FALSE, products_df_soy)
  callModule(custInfoPlotServer, "Corn_tab", dropdown_outputs, 
                                 cust_df, custIDColumn, custFeaturesToPlot, featsToPlotCleanNames,
                                 seedsmanIDColumn, discountColumn, priceColumn)
  callModule(custInfoPlotServer, "Soy_tab", dropdown_outputs, 
             cust_df_soy, custIDColumnSoy, custFeaturesToPlotSoy, featsToPlotCleanNamesSoy,
             seedsmanIDColumnSoy, discountColumnSoy, priceColumnSoy)
  
  # Customer Portfolio Tab Modules ##############################################
    result_corn <- callModule(cropSpecificDropdownServer, "SharedSidebar1")
    result_soy <- callModule(cropSpecificDropdownServer, "SharedSidebar2")
  
    callModule(custPortfolioServer, "Corn_tab", dropdown_outputs, 
               dropdown_outputs_portfolio, reactive({result_corn$name()}), reactive({result_corn$quantity()}),
               cust_df, custIDColumn, orderTableNames,
               seedsmanIDColumn, discountColumn, yearColumn,
               priceColumn, effectivePriceColumn, products_df)
    callModule(custPortfolioServer, "Soy_tab", dropdown_outputs, 
               dropdown_outputs_portfolio, reactive({result_soy$name()}), reactive({result_soy$quantity()}),
               cust_df_soy, custIDColumnSoy, orderTableNames,
               seedsmanIDColumnSoy, discountColumnSoy, yearColumnSoy,
               priceColumnSoy, effectivePriceColumnSoy, products_df_soy) 

    
  # Model Summary Tab Modules ##############################################
  callModule(featuresServer, "corn_features", discount_table, rest_of_disc_features,
             ep_table, rest_of_ep_features)
  callModule(featuresServer, "soy_features", discount_table_soy, rest_of_disc_features_soy,
             ep_table_soy, rest_of_ep_features_soy)  
  
 # KPI Metrics tab Modules ##############################################
  KPI_password_input <- reactive({input$pass})
  #Conditionally make the KPI metrics stuff visible if the correct password is supplied
    observe({
          req(KPI_password_input() == "password123")
          DateRange <- callModule(conpanelKPIServer, "ConditionalPanel") #Get dates from date dropdown
          callModule(dailyvisitsKPIServer, "DailyVisitsMetrics", DailyVisits, UserLogData, DateRange)
          callModule(dailytotalusersKPIServer, "DailyTotalUsers", TotalVisits, DateRange)
          callModule(dailytotalminsKPIServer, "DailyTotalMins", DailyTotalTime, DateRange)
          callModule(dailyavgminsKPIServer, "DailyAvgMins", DailyMeanTime, DateRange)
          callModule(KPIMetricsUIServer, NULL, parent.session = session)
      })
    }


shinyApp(ui = ui, server = server)

