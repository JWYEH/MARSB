#TODO: Further consolidate server side, if not ui side of things

#' Produce KPI Metrics - Daily Visits Tab
#'
#' @title dailyvisitsKPIUI
#' @author Julie Wisch (elzej)
#'
#' @description Generate the Daily Visits subtab under the KPI Metrics heading
#'
#' @param id app namespace
#'
#' @return A tab with both a barplot and a tabular display of daily visits to the site
#'
#' @export
dailyvisitsKPIUI <- function(id) {
  ns <- NS(id)
  tagList(  
                                              br(),
                                              br(),
                                              column(9,
                                                     fluidRow(
                                                       plotlyOutput(outputId=ns("DailyVisitsPlot"))
                                                     )
                                              ),
                                              column(3, tableOutput(ns('DailyVisitsSubsetTable2')))
                                     )}

#' Produce Server Side KPI Metrics - Daily Visits Tab
#'
#' @title dailyvisitsKPIServer
#' @author Julie Wisch (elzej)
#'
#' @description Generate the Daily Visits table and plot, as well as the User Log table
#'
#' @param DailyVisits a dataframe containing the daily visit records for the app
#' @param UserLogData a dataframe containing all user visits and associated timestamps
#' @param DateRange User input daterange supplied via the conditional panel dateRange button
#'
#' @return DailyVisitsSubset2 A table showing daily visits within the supplied dateRange
#' @return UserLogDT A table showing user visit records within the supplied dateRange
#' @return DailyVisitsPlot a barplot showing the total number of visitors per day within the supplied dateRange
#'
#' @export
dailyvisitsKPIServer <- function(input, output, session, 
                                 DailyVisits, UserLogData, DateRange) {
  #Generating tables for computation
  DailyVisitsSubset <- reactive({
    filter(DailyVisits, between(Date , DateRange$MinDate(), DateRange$MaxDate()))
  })
  
  UserLogDTSubset <- reactive({
    filter(UserLogData, between(Date1 , DateRange$MinDate(), DateRange$MaxDate()))
  })
  
  #Generating tables for display
  DailyVisitsSubsetTable <- reactive({
    rowTemp <- data.frame("Category" = "Unique Visitors", "Value" =  length(unique(UserLogDTSubset()$CWID)))
    UserLogDataTemp <- rowTemp
    rowTemp <- data.frame("Category" = "Avg./Day", "Value" =  round(mean(DailyVisitsSubset()$Unique_Users), 1) )
    UserLogDataTemp <- rbind(UserLogDataTemp, rowTemp)
    rowTemp <- data.frame("Category" = "Number of Days", "Value" =  nrow(DailyVisitsSubset()))
    UserLogDataTemp <- rbind(UserLogDataTemp, rowTemp)
    rowTemp <- data.frame("Category" = "First Day", "Value" =  as.character(DateRange$MinDate()) )
    UserLogDataTemp <- rbind(UserLogDataTemp, rowTemp)
    rowTemp <- data.frame("Category" = "Last Day", "Value" =  as.character(DateRange$MaxDate()) )
    UserLogDataTemp <- rbind(UserLogDataTemp, rowTemp)
  })
  
  
  output$DailyVisitsSubsetTable2 <- renderTable({DailyVisitsSubsetTable()})
  
  #create table to display seedsman info   
  output$UserLogDT <- renderDT({
    datatable(UserLogDTSubset(), extensions = "Buttons", colnames=c("CWID", "Start Time", "URL Pathname", "URL Search", "Date", "Time", "Time Zone"),
              filter = "top", options = list(lengthMenu = list( c(25, 50, -1), c(25, 50, "All")), scrollX=TRUE, pageLength = 25,
                                             dom = "Blfrtip",
                                             buttons = list( list( extend = 'collection',
                                                                   buttons = list(
                                                                     list(extend='excel', title = NULL,
                                                                          filename = paste("UserLog", "--", Sys.Date(), sep ='' ) ),
                                                                     list(extend='csv', title = NULL,
                                                                          filename = paste("Userlog", "--", Sys.Date(), sep ='' ) )
                                                                   ),
                                                                   text = c('Download Selected Data')
                                             ))                                             
              ))
    
  })
  
  
  #Generating plots for display
  output$DailyVisitsPlot <- renderPlotly({
    
    fig <- plot_ly(DailyVisitsSubset(), x = ~Date, y = ~Unique_Users, type = 'bar', name = 'Unique Users',
                   hoverinfo = "text",  text = ~paste('Date ', Date, "<br>Total Unique Users", round(Unique_Users)), marker = list(color = 'rgb(49,130,189)'))
    fig <- fig %>% layout(title = "NUMBER OF DAILY UNIQUE USERS",
                          xaxis = list(title = "DATE", tickangle = -45),
                          yaxis = list(title = "COUNT"),
                          margin = list(b = 100),
                          barmode = 'group')
    fig <- fig %>% config(displaylogo = FALSE, modeBarButtonsToRemove = c('zoom2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'autoScale2d', 'resetScale2d', 'hoverClosestCartesian', 'hoverCompareCartesian', 'toggleSpikelines'))
    
    fig
  })
  
}

#' Produce KPI Metrics - Daily Total Users Tab
#'
#' @title dailytotalusersKPIUI
#' @author Julie Wisch (elzej)
#'
#' @description Generate the Daily Users subtab under the KPI Metrics heading
#'
#' @param id app namespace
#'
#' @return A tab with both a barplot and a tabular display of daily total users on the site
#'
#' @export
dailytotalusersKPIUI <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    br(),
    column(9,
           fluidRow(
             plotlyOutput(outputId=ns("DailyTotalVisitsPlot"))
           )
    ),
    column(3, tableOutput(ns('TotalVisitsSubsetTable2')))
    
  )
}

#' Server Side - Produce KPI Metrics Daily Total Users Tab
#'
#' @title dailytotalusersKPIServer
#' @author Julie Wisch (elzej)
#'
#' @description Generate the Daily Total Users table and plot
#'
#' @param TotalVisits a dataframe containing the aggregated daily visit records for the app
#' @param DateRange User input daterange supplied via the conditional panel dateRange button
#'
#' @return TotalVisitsSubset2 A table showing total visits within the supplied dateRange
#' @return TotalVisitsPlot a barplot showing the total number of visitors per day within the supplied dateRange
#'
#' @export
dailytotalusersKPIServer <- function(input, output, session, 
                                     TotalVisits, DateRange) {
  TotalVisitsSubset <- reactive({
    UserLogDataTemp <- filter(TotalVisits, between(Date, DateRange$MinDate(), DateRange$MaxDate()))
  })
  
  TotalVisitsSubsetTable <- reactive({
    rowTemp <- data.frame("Category" = "Total Visitors", "Value" =  sum(TotalVisitsSubset()$Total_Users) )
    UserLogDataTemp <- rowTemp
    rowTemp <- data.frame("Category" = "Avg./Day", "Value" =  round(mean(TotalVisitsSubset()$Total_Users), 1) )
    UserLogDataTemp <- rbind(UserLogDataTemp, rowTemp)
    rowTemp <- data.frame("Category" = "Number of Days", "Value" =  nrow(TotalVisitsSubset()) )
    UserLogDataTemp <- rbind(UserLogDataTemp, rowTemp)
    rowTemp <- data.frame("Category" = "First Day", "Value" =  as.character(DateRange$MinDate()) )
    UserLogDataTemp <- rbind(UserLogDataTemp, rowTemp)
    rowTemp <- data.frame("Category" = "Last Day", "Value" =  as.character(DateRange$MaxDate()) )
    UserLogDataTemp <- rbind(UserLogDataTemp, rowTemp)
    
  })
  
  output$TotalVisitsSubsetTable2 <- renderTable(TotalVisitsSubsetTable())
  
  #Plot of Daily Users
  output$DailyTotalVisitsPlot <- renderPlotly({
    
    fig <- plot_ly(TotalVisitsSubset(), x = ~Date, y = ~Total_Users, type = 'bar', name = 'Total Users', 
                   hoverinfo = "text",  text = ~paste('Date ', Date, "<br>Total Users", round(Total_Users)), marker = list(color = 'rgb(49,130,189)'))
    fig <- fig %>% layout(title = "NUMBER OF TOTAL DAILY VISITORS",
                          xaxis = list(title = "DATE", tickangle = -45),
                          yaxis = list(title = "COUNT"),
                          margin = list(b = 100),
                          barmode = 'group')
    fig <- fig %>% config(displaylogo = FALSE, modeBarButtonsToRemove = c('zoom2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'autoScale2d', 'resetScale2d', 'hoverClosestCartesian', 'hoverCompareCartesian', 'toggleSpikelines'))
    
    fig
  })
}


#' Produce KPI Metrics - Daily Total Minutes Tab
#'
#' @title dailytotalminsKPIUI
#' @author Julie Wisch (elzej)
#'
#' @description Generate the Daily Total Minutes subtab under the KPI Metrics heading
#'
#' @param id app namespace
#'
#' @return A tab with both a barplot and a tabular display of daily total minutes spent on the site
#'
#' @export
dailytotalminsKPIUI <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    br(),
    column(9,
           fluidRow(
             plotlyOutput(outputId=ns("DailyMinutesUsedPlot"))
           )
    ),
    column(3, tableOutput(ns('DailyTotalTimeSubsetTable2')))
    
  )
}

#' Server Side - Produce KPI Metrics Daily Total Minutes Tab
#'
#' @title dailytotalminsKPIServer
#' @author Julie Wisch (elzej)
#'
#' @description Generate the Daily Total Minutes table and plot
#'
#' @param DailyTotalTime a dataframe containing the aggregated daily time records for the app
#' @param DateRange User input daterange supplied via the conditional panel dateRange button
#'
#' @return DailyTotalTimeSubset2 A table showing total time of visits within the supplied dateRange
#' @return DailyMinutesUsedPlot a barplot showing the total number of minutes per day within the supplied dateRange
#'
#' @export
dailytotalminsKPIServer <- function(input, output, session, 
                                    DailyTotalTime, DateRange){
  DailyTotalTimeSubset <- reactive({
    filter(DailyTotalTime, between(Date , DateRange$MinDate(), DateRange$MaxDate()))
  })
  
  DailyTotalTimeSubsetTable <- reactive({
    rowTemp <- data.frame("Category" = "Total Min.", "Value" =  round(sum(DailyTotalTimeSubset()$Total_Time), 0) )
    UserLogDataTemp <- rowTemp
    rowTemp <- data.frame("Category" = "Min./Day", "Value" =  round(mean(DailyTotalTimeSubset()$Total_Time), 1) )
    UserLogDataTemp <- rbind(UserLogDataTemp, rowTemp)
    rowTemp <- data.frame("Category" = "Number of Days", "Value" =  nrow(DailyTotalTimeSubset()) )
    UserLogDataTemp <- rbind(UserLogDataTemp, rowTemp)
    rowTemp <- data.frame("Category" = "First Day", "Value" =  as.character(DateRange$MinDate()) )
    UserLogDataTemp <- rbind(UserLogDataTemp, rowTemp)
    rowTemp <- data.frame("Category" = "Last Day", "Value" =  as.character(DateRange$MaxDate()) )
    UserLogDataTemp <- rbind(UserLogDataTemp, rowTemp)
    
  })
  
  output$DailyTotalTimeSubsetTable2 <- renderTable(DailyTotalTimeSubsetTable())
  
  #Plot of Daily Minutes Used
  output$DailyMinutesUsedPlot <- renderPlotly({
    
    fig <- plot_ly(DailyTotalTimeSubset(), x = ~Date, y = ~Total_Time, type = 'bar', name = 'Total Daily User Time',
                   hoverinfo = "text",  text = ~paste('Date ', Date, "<br>Total Minutes", round(Total_Time)), marker = list(color = 'rgb(49,130,189)' ))
    fig <- fig %>% layout(title = "TOTAL DAILY MINUTES USED",
                          xaxis = list(title = "DATE", tickangle = -45),
                          yaxis = list(title = "MINUTES"),
                          margin = list(b = 100),
                          barmode = 'group'
    )
    fig <- fig %>% config(displaylogo = FALSE, modeBarButtonsToRemove = c('zoom2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'autoScale2d', 'resetScale2d', 'hoverClosestCartesian', 'hoverCompareCartesian', 'toggleSpikelines'))
    
    fig
  })
  
}


#' Produce KPI Metrics - Daily Average Minutes Tab
#'
#' @title dailyvisitsKPIUI
#' @author Julie Wisch (elzej)
#'
#' @description Generate the Daily Average Minutes subtab under the KPI Metrics heading
#'
#' @param id app namespace
#'
#' @return A tab with both a barplot and a tabular display of the average number of minutes per user spent on the site each day
#'
#' @export
dailyavgminsKPIUI <- function(id){
  ns <- NS(id)
  tagList(
    br(),
    br(),
    column(9,
           fluidRow(
             plotlyOutput(outputId=ns("DailyMeanMinutesUsedPlot"))
           )
    ),
    
    column(3, tableOutput(ns('DailyMeanTimeSubsetTable2')))
  )
}

#' Produce KPI Metrics - User Log Tab
#'
#' @title dailyvisitsKPIUI
#' @author Julie Wisch (elzej)
#'
#' @description Generate the User Log subtab under the KPI Metrics heading
#'
#' @param id app namespace
#'
#' @return A tab with tabular display of all visits to the site, displaying user name and timestamp
#'
#' @export
userlogKPIUI <- function(id){
  ns <- NS(id)
  tagList(                                              
          br(),
          br(),
           fluidRow(
                DTOutput(ns('UserLogDT'))
                    ))
}
                                     

#' Produce KPI Metrics Conditional Sidebar Tab
#'
#' @title conpanelKPIUI
#' @author Julie Wisch (elzej)
#'
#' @description Generate the sidebar panel that is displayed when you are on the KPI Metrics tab. 
#' @description This panel allows users to download data
#' 
#' @param id app namespace
#' @param DailyVisits a dataframe of all visits. This dataframe includes dates, which is what is used in this function
#' @param dateButtonLabel The label for the date range selector box. Default is "Date Range"
#'
#' @return A sidebar panel with a date range selector and several download dataset buttons
#'
#' @export
conpanelKPIUI <- function(id, DailyVisits, dateButtonLabel = "Date Range"){
  ns <- NS(id)
  tagList(
    h4("Select Graph Date Range",style="color:#C3CE44",align="center"),
    dateRangeInput(ns("dateRange"),
                   strong(dateButtonLabel),
                   start = min(DailyVisits$Date), end = max(DailyVisits$Date),
                   min = "2020-01-01"),
    hr(),
    h4("Download Datasets",style="color:#C3CE44",align="center"),
    fluidRow(
      downloadButton("download_data", "Download User Data")
    ),
    
    # center and size the download button
    tags$style(type='text/css', "#download_data { vertical-align: center; position: relative; left: 0%;
                                    margin-right: 0%; transform: translate(0%, 0%); height: 75px; color: black; width: 100%; font-size: 14px;}"),
    
    br(),
    
    fluidRow(
      downloadButton("download_grower_report_CORN",
                     label = HTML('Save Historical<br/>CORN Recommendations for <br/>All Customers')
      ),
      
      # center and size the download button
      tags$style(type='text/css', "#download_grower_report_CORN { vertical-align: center; position: relative; left: 0%;
                                    margin-right: 0%; transform: translate(0%, 0%); height: 75px; color: black; width: 100%; font-size: 14px;}"),
      
      br(),
      br(),
      downloadButton("download_grower_report_SOY",
                     label = HTML('Save Historical<br/>SOY Recommendations for <br/>All Customers')
      ),
      
      # center and size the download button
      tags$style(type='text/css', "#download_grower_report_SOY { vertical-align: center; position: relative; left: 0%;
                                    margin-right: 0%; transform: translate(0%, 0%); height: 75px; color: black; width: 100%; font-size: 14px;}")
    )
    
  )
}

#' Server Side - Produce KPI Metrics Conditional Sidebar Tab
#'
#' @title conpanelKPIServer
#' @author Julie Wisch (elzej)
#'
#' @description Reactive to store the user input date ranges
#'
#' @return MinDate the earliest date selected by the user
#' @return MaxDate the latest date selected by the user
#'
#' @export

conpanelKPIServer <- function(input, output, session){
  return(
    list(
      MinDate = reactive({input$dateRange[1]}), #Early date from daterange dropdown
      MaxDate = reactive({input$dateRange[2]}) #Late date from daterange dropdown
    )
  )
}


#' Server Side - Produce KPI Metrics Daily Average Minutes Tab
#'
#' @title dailyavgminsKPIServer
#' @author Julie Wisch (elzej)
#'
#' @description Generate the Daily Avg Minutes table and plot
#'
#' @param DailyMeanTime a dataframe containing the averaged daily time records for the app
#' @param DateRange User input daterange supplied via the conditional panel dateRange button
#'
#' @return DailyMeanTimeSubset2 A table showing average time of visits within the supplied dateRange
#' @return DailyMeanMinutesUsedPlot a barplot showing the avg number of minutes per day within the supplied dateRange
#'
#' @export

dailyavgminsKPIServer <- function(input, output, session, 
                                    DailyMeanTime, DateRange){
  DailyMeanTimeSubset <- reactive({
    filter(DailyMeanTime, between(Date , DateRange$MinDate(), DateRange$MaxDate()))
  })
  
  DailyMeanTimeSubsetTable <- reactive({
    
    rowTemp <- data.frame("Category" = "Avg./Day", "Value" =  round(mean(DailyMeanTimeSubset()$Mean_Time), 1) )
    UserLogDataTemp <- rowTemp
    rowTemp <- data.frame("Category" = "St. Dev.", "Value" =  round(sd(DailyMeanTimeSubset()$Mean_Time), 1) )
    UserLogDataTemp <- rbind(UserLogDataTemp, rowTemp)
    rowTemp <- data.frame("Category" = "Number of Days", "Value" =  nrow(DailyMeanTimeSubset()) )
    UserLogDataTemp <- rbind(UserLogDataTemp, rowTemp)
    rowTemp <- data.frame("Category" = "First Day", "Value" =  as.character(DateRange$MinDate()) )
    UserLogDataTemp <- rbind(UserLogDataTemp, rowTemp)
    rowTemp <- data.frame("Category" = "Last Day", "Value" =  as.character(DateRange$MaxDate()) )
    UserLogDataTemp <- rbind(UserLogDataTemp, rowTemp)
    #DailyMeanTimeSubset()
  })
  
  output$DailyMeanTimeSubsetTable2 <- renderTable(DailyMeanTimeSubsetTable())
  
  
  output$DailyMeanMinutesUsedPlot <- renderPlotly({
    
    fig <- plot_ly(DailyMeanTimeSubset(), x = ~Date, y = ~Mean_Time, type = 'bar', name = 'Average Daily User Time',
                   hoverinfo = "text",  text = ~paste('Date ', Date, "<br>Average Minutes", round(Mean_Time)), marker = list(color = 'rgb(49,130,189)'
                                                                                                                             
                   )#,
                   #error_y = ~list(array = SD_Time, color = '#000000')
    )
    fig <- fig %>% layout(title = "AVERAGE DAILY MINUTES USED",
                          xaxis = list(title = "DATE", tickangle = -45),
                          yaxis = list(title = "MINUTES"),
                          margin = list(b = 100),
                          barmode = 'group'
    )
    fig <- fig %>% config(displaylogo = FALSE, modeBarButtonsToRemove = c('zoom2d', 'pan2d', 'select2d', 'lasso2d', 'zoomIn2d', 'zoomOut2d', 'autoScale2d', 'resetScale2d', 'hoverClosestCartesian', 'hoverCompareCartesian', 'toggleSpikelines'))
    
    fig
  })
}




append_KPI_tab_server <- function(input, output, session, parent.session, passwordEntry){
  password <- reactive({req(passwordEntry$enter())
                            passwordEntry$pass()})
  observe({
  #  if (password_corn == "password123"| password_soy == "password123") {
      appendTab(session = parent.session,
                inputId = "tabselected", tab = 
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
   # }
  })
}



get_fsrStringServer <- function(input, output, session, dropdown_outputs){
  #grab fsr as string
  fsrString <- reactive({
    #req(input$fsr_name)
    fsrString <- as.character(dropdown_outputs$fsr())
    fsrString <- strsplit(fsrString, " ", fixed=FALSE)
    fsrString <- paste(fsrString[[1]][1], fsrString[[1]][2], sep = ' ')
  })
  
  return(fsrString)
}



get_seedsmanStringServer <- function(input, output, session, dropdown_outputs){
  seedsmanString <- reactive({
    req(input$seedsman_name)
    seedsmanString <- as.character(dropdown_outputs$seedsman())
    seedsmanString <- strsplit(seedsmanString, " ", fixed = FALSE)
    seedsmanString <- seedsmanString[[1]][lengths(seedsmanString)]
  })
  
  return(seedsmanString)
}

get_fsrStringServer_app <- function(input, output, session){
  #grab fsr as string
  fsrString <- reactive({
    #req(input$fsr_name)
    fsrString <- as.character(input$fsr_name)
    fsrString <- strsplit(fsrString, " ", fixed=FALSE)
    fsrString <- paste(fsrString[[1]][1], fsrString[[1]][2], sep = ' ')
  })
  
  return(fsrString)
}



get_seedsmanStringServer_app <- function(input, output, session){
  seedsmanString <- reactive({
    req(input$seedsman_name)
    seedsmanString <- as.character(input$seedsman_name)
    seedsmanString <- strsplit(seedsmanString, " ", fixed = FALSE)
    seedsmanString <- seedsmanString[[1]][lengths(seedsmanString)]
  })
  
  return(seedsmanString)
}

get_budgetRunRateServer <- function(input, output, session, runrate_df, fsrString){
  budgetrunrate <- reactive({
    req(input$fsr_name)
    budgetrunrate <- runrate_df[which(runrate_df[,"FSR"] == fsrString()),]
    budgetrunrate <- budgetrunrate$PrevailingLCRPercentage
    budgetrunrate <- paste0(round(100*as.numeric(budgetrunrate),digits=1),'%')
  })
  return(budgetrunrate)
}


KPIMetricsUIServer <- function(input, output, session, parent.session){        
  appendTab(session = parent.session,
            inputId = "tabselected", select = TRUE, tab = 
              tabPanel("KPI METRICS", value = 3,
                       mainPanel(width = 12,
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


