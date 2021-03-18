#' Produce LCR Analysis - LCR Over/Under the Rec. Tab
#'
#' @title LCROverUnderUI 
#' @author Julie Wisch (elzej)
#'
#' @description Generate the LCR Analysis - LCR Over/Under the Recommendation tab
#'
#' @param id app namespace
#'
#' @return A tab with two plots comparing lcr spend relative to recommendations
#'
#' @export
LCROverUnderUI <- function(id){
  ns <- NS(id)
  
  tagList(
    tabPanel("LCR Over/Under the Rec.", 
             fluidRow(
               column(6,
                      plotlyOutput(outputId=ns("LCRPlot3"))
               ),
               column(6,
                      plotlyOutput(outputId=ns("LCRPlot4"))
               )
             ) #end fluidRow
    )
  )
}



#' Server Side - Produce LCR Analysis - LCR Over/Under the Rec. Tab
#'
#' @title LCROverUnderServer
#' @author Julie Wisch (elzej)
#'
#' @description Generate the LCR Analysis - LCR Over/Under the Recommendation tab
#' 
#' @param fsrString string of fsr name
#' @param grower_repot dataframe either grower_report or grower_report_soy
#' @param LCRTotals_raw0
#' @param LCRTotals_raw
#' @param CROP string of crop name
#' 
#' @return "LCRPlot3" a plot of LCR spend
#' @return "LCRPlot4" a plot of LCR spend

#'
#' @export
LCROverUnderServer <- function(input, output, session, fsrString, 
                               selected_fsr_init, grower_report, LCRTotals_raw0, LCRTotals_raw, CROP) {
  crop_upper <- toupper(CROP)
  crop_lower <- tolower(CROP)
 
  observeEvent(fsrString(), {
    if(fsrString() == selected_fsr_init) {
      LCRTotals <- reactive({ LCRTotals0 })
    }else{
        LCRTotals <- reactive({
            LCRTotals_raw_args <- list(fsrString(), grower_report, crop_lower)
            wrapMemoise(LCRTotals_raw0, LCRTotals_raw, LCRTotals_raw_args)
        })
    }
      
  #Plot of "Chart A-Team"
  output$LCRPlot3 <- renderPlotly({ 
    plotlyLCRPlot3(LCRTotals()) 
  })
  
  
  #Plot of "Chart A-Team" part 2
  output$LCRPlot4 <- renderPlotly({ 
    plotlyLCRPlot4(LCRTotals()) 
  })
  
  },ignoreInit = FALSE)
  
}




#' Produce LCR Analysis - Current LCR Tab
#'
#' @title CurrentLCRUI

#' @author Julie Wisch (elzej)
#'
#' @description Generate the LCR Analysis - Current LCR tab
#'
#' @param id app namespace
#'
#' @return FSR LCR Analysis - Current LCR Spend Tab
#'
#' @export
LCRPlotandTableUI <- function(id){
  ns <- NS(id)
  
  tagList(
    tabPanel("Current LCR Spend", 
             fluidRow(
               column(
                 plotlyOutput(outputId=ns("LCRPlot5")), width = 12
               )
             ),
             fluidRow(
               br(),
               column(
                 (DTOutput(ns("LCRDF"))), width = 12
               )
             ) 
    )
 
  )
}



#' Server Side - Produce LCR Analysis - Current LCR Spend Tab
#'
#' @title CurrentLCRServer
#' @author Julie Wisch (elzej)
#'
#' @description Generate the LCR Analysis - Current LCR tab
#' 
#' @param fsrString string of fsr name
#' @param grower_repot dataframe either grower_report or grower_report_soy
#' @param LCRTotals_raw0
#' @param LCRTotals_raw
#' @param CROP string of crop name
#' 
#' @return "LCRDF7" a table containing all the seedsman data
#'
#' @export
LCRPlotandTableServer <- function(input, output, session, fsrString, 
                               grower_report, plotlyLCRPlot5, 
                               LCRTotals3_raw0, LCRTotals3_raw, dtLCRDF_raw, dtLCRDF, CROP) {
  crop_upper <- toupper(CROP)
  crop_lower <- tolower(CROP)
  
  observeEvent(fsrString(), {
    LCRTotals3 <- reactive({ 
      LCRTotals3_raw_args <- list(fsrString(), grower_report, crop_lower)
      wrapMemoise(LCRTotals3_raw0, LCRTotals3_raw, LCRTotals3_raw_args)
    })
    
    output$LCRPlot5 <- renderPlotly({
      validate(need(!is.null(LCRTotals3()), "No historical sales records exist"))
      plotlyLCRPlot5(LCRTotals3())
    })
    
    #create table to display seedsman info
    output$LCRDF <- renderDT({ 
      validate(need(!is.null(LCRTotals3()), " "))
      dtLCRDF_args <- list(fsrString(), LCRTotals3(), crop_upper)
      wrapMemoise(dtLCRDF_raw, dtLCRDF, dtLCRDF_args)
    })
  },ignoreInit = FALSE)

}


#' Produce LCR Analysis - Tabular Tabs
#'
#' @title LCRTablesUI

#' @author Julie Wisch (elzej)
#'
#' @description Generate the LCR Analysis - LCR by Grower & LCR by Products tab
#'
#' @param id app namespace
#'
#' @return FSR LCR Analysis - Tab with a Tabular Display
#'
#' @export
LCRTablesUI <- function(id, tabTitle){
  ns <- NS(id)
  
  tagList(
    tabPanel(tabTitle, 
             fluidRow(
               column(
                 (DTOutput(ns("LCR_table"))), width = 12
               )
             ) 
    )
  )
}



#' Server Side - Produce LCR Analysis - Tabular Tabs
#'
#' @title LCRTablesServer
#' @author Julie Wisch (elzej)
#'
#' @description Generate the LCR Analysis - LCR by Grower & LCR by Products tab
#' 
#' @param fsrString string of fsr name
#' @param grower_repot dataframe either grower_report or grower_report_soy
#' @param LCRTotals_raw0
#' @param LCRTotals_raw
#' @param CROP string of crop name
#' 
#' @return "LCRDF7" a table containing all the relevant data
#'
#' @export
LCRTablesServer <- function(input, output, session, fsrString, 
                             grower_report, dtLCRDF, LCRTotals_raw0, LCRTotals_raw, CROP) {
  crop_upper <- toupper(CROP)
  crop_lower <- tolower(CROP)
  

  LCRTotals <- reactive({ 
    LCRTotals_raw_args <- list(fsrString(), grower_report, crop_lower)
    wrapMemoise(LCRTotals_raw0, LCRTotals_raw, LCRTotals_raw_args)
  })
  
  
  output$LCR_table <- renderDT({ 
    validate(need(!is.null(LCRTotals()), "No historical sales records exist"))
    dtLCRDF_args <- list(fsrString(), LCRTotals(), crop_upper)
    wrapMemoise(dtLCRDF_raw, dtLCRDF, dtLCRDF_args)
  })
  
}

