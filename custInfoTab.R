###MODULES
commonDropdownUI <- function(id, choices_fsr, selected_fsr, choices_seedsman, selected_seedsman,
                             choices_cust, selected_cust, choices_newcust_state, selected_newcust_state,
                             choices_newcust_county, selected_newcust_county){
  ns <- NS(id)
  
  tagList(
    selectInput(inputId = ns("fsr_name"),
                shiny::HTML("<p><span style='color: #C3CE44'>Select FSR Name | FSR Territory ID</span></p>"),
                choices = choices_fsr,
                selected = selected_fsr),
    # selectInput for seedsman ID -- common
    get_dropdown(ns('seedsman_name'), "<p><span style='color: #C3CE44'>Seedsman Account Name <br/>| Seedsman ID</span></p>",
                 choices_seedsman, selected_seedsman),
    
    # add choices for customer name -- common
    get_dropdown(ns("cust_name"), "<p><span style='color: #C3CE44'>Customer Account Name <br/>| Customer ID</span></p>",
                 choices_cust, selected_cust),
    
    # Drop-down button for add new customer -- common
    fluidRow(align = "center", column(12, dropdownButton(inputId = ns('add_new_customer'),
                                                         label = 'New Customer Location',
                                                         icon = icon("map-marked-alt"),
                                                         circle=FALSE,
                                                         get_dropdown(ns("state"), "<p><span style='color: #000000'>State</span></p>",
                                                                      choices_newcust_state, selected_newcust_state),
                                                         get_dropdown(ns("county"), "<p><span style='color: #000000'>County</span></p>",
                                                                      choices_newcust_county, selected_newcust_state)
    ))),
    tags$style(type='text/css', "#add_new_customer { color: black; width: 100%; font-size: 12px;}"),
    br()
  )
}

commonDropdownServer <- function(input, output, session, fsr_name, seedsman_name, cust_name,
                                 add_new_customer, state, county){
  return(
    list(fsr = reactive({input$fsr_name}),
         seedsman = reactive({input$seedsman_name}),
         cust = reactive({input$cust_name}),
         new_cust = reactive({input$add_new_customer}),
         state = reactive({input$state}),
         county = reactive({input$county})
    )
  )
}

commonPasswordInputUI <- function(id){
  ns <- NS(id)
  tagList(
      passwordInput("pass", shiny::HTML("<p><span style='color: #C3CE44'>Enter password to see KPI Metrics</span></p>"))
  )#end tag list
  
}

commonPasswordInputServer <- function(input, output, session, pass){
  return(
      list(pass = reactive({input$pass}),
           enter = reactive({input$enter_button}))
  )
}

cropSpecificDropdownUI <- function(id, choices_product, selected_product){
  ns <- NS(id)
  
  tagList(
    get_dropdown((ns("product_name")), "<p><span style='color: #C3CE44'>Product Name</span></p>",
                 choices_product, selected_product),
    
    # numeric input for quantity -- corn
    numericInput(inputId = ns("product_quantity"),
                 shiny::HTML("<p><span style='color: #C3CE44'>Quantity</span></p>"),
                 value = 0,
                 min = 0),
    
    
  )
}

cropSpecificDropdownServer <- function(input, output, session, product_name, product_quantity){
  ns <- session$ns
  return(
    list(name = reactive({input$product_name}),
         quantity = reactive({input$product_quantity})
    )
  )
}

commonPortfolioButtonsUI <- function(id){
  ns <- NS(id)
  tagList(
    # action button for adding product -- corn
    fluidRow(align = "center", column(12,  actionButton(inputId = ns("add_to_cart"),
                                                        label = "Add to Portfolio",
                                                        icon = icon("plus-square")))),
    tags$style(type='text/css', "#add_to_cart { color: black; width: 100%; font-size: 12px;}"),
    br(),
    # action button for clearing cart -- corn
    fluidRow(align = "center", column(12,  actionButton(inputId = ns("clear_cart"),
                                                        label = "Clear Portfolio",
                                                        icon = icon("minus-square")))),
    tags$style(type='text/css', "#clear_cart { color: black; width: 100%; font-size: 12px;}"),
    br(),
    # action button for pre-populating portfolio -- corn
    fluidRow(align = "center", column(12,  actionButton(inputId = ns('populate_cart'),
                                                        label = 'Pre-Populate Portfolio',
                                                        icon = icon("shopping-basket")))),
    tags$style(type='text/css', "#populate_cart { color: black; width: 100%; font-size: 12px;}"),
    br()
    
  )#end tag list
  
}

commonPortfolioButtonsServer <- function(input, output, session){
  return(
    list(add = reactive({input$add_to_cart}),
         clear = reactive({input$clear_cart}),
         populate = reactive({input$populate_cart})
    )
  )
}

clearingDropdownServer <- function(input, output, session, cust_df_displaynames, terrIDColumn, stateCountyToFips){
  #Clear seedsman and customer name if fsr_name selected
  observeEvent(input$fsr_name,{
    updateSelectInput(session, "seedsman_name",
                      #label = "Seedsman Account Name | Seedsman ID",
                      choices = cust_df_displaynames[cust_df_displaynames[[terrIDColumn]] == get_terr_id(input$fsr_name),
                                                     "Seedsman Account Name | Seedsman ID"],
                      selected = "")
    updateSelectInput(session, "cust_name",
                      choices = NULL,
                      selected = "")
  },ignoreInit = TRUE)
  
  #clear customer name if seedsman selected
  observeEvent(input$seedsman_name,{
    updateSelectInput(session, "cust_name",
                      #label = "Customer Account Name | Customer ID",
                      choices = cust_df_displaynames[cust_df_displaynames[[seedsmanIDColumn]] == get_id(input$seedsman_name),
                                                            'Customer Account Name | Customer ID'],
                      selected = "")
  }, ignoreInit = TRUE)
  
  observeEvent(input$cust_name, {
    updateSelectInput(session, "county",
                      selected = "")
  }, ignoreInit = TRUE)
  
  # update choices in location drop down for new customer based on selected state ####################
  observeEvent(input$state, {
    updateSelectInput(session, "county",
                      label = "County",
                      choices = append("", stateCountyToFips[stateCountyToFips$statename==input$state,
                                                             'County']))
    updateSelectInput(session, "cust_name", 
                      selected = "")
  },ignoreInit = TRUE)
  
}


custInfoUI <- function(id, ChoiceList){
  ns <- NS(id)
  tagList(
    tableOutput(ns("grower_table")),
    # # competitive info
    tableOutput(ns('comp_info')),
    # # purchase history
    tableOutput(ns('purchase_hist')),
    # switch to year view in purchase history table
    actionButton(inputId = ns('year_view'),
                label = 'Collapse Table to Order Level'),
    # # switch to product view in purcahse history table
    actionButton(inputId = ns('product_view'),
                label = 'Expand Table to Product Level'),
    # var to plot in customer plot
    get_dropdown(ns('plot_var'), 'Plot Historical Customer Metrics',
               ChoiceList, ""),
    # # customer purchase history plot
    plotOutput(outputId = ns('cust_plot'))
  )
  
}

growerTableServer <- function(input, output, session, dropdown_outputs, 
                              cust_df, custIDColumn, yearColumn, products_df){
  cust_id <- reactive({sub(".* | ", "", dropdown_outputs$cust())}) #extracts id number out of customer name dropdown
  
  grower_table <- reactive({req(dropdown_outputs$cust())
                            get_grower_characs( cust_id(),
                                                cust_df,
                                                custFeats,
                                                custFeatNames,
                                                custFeatKeys,
                                                sharedFarms,
                                                custIDColumn,
                                                yearColumn,
                                                custFeatOrder) })
  
  # render table in UI
  output$grower_table <- renderTable(grower_table(),
                                     caption = 'Customer Characteristics',
                                     caption.placement = getOption("xtable.caption.placement",
                                                                   "top"))
}

purchaseHistServer <- function(input, output, session, dropdown_outputs, 
                               cust_df, custIDColumn, custPurchaseNames, formattedPurchaseNames,
                               EffPrice, seedsmanIDColumn, yearColumn, yearView){
  cust_id <- reactive({ req(dropdown_outputs$cust())
                        sub(".* | ", "", dropdown_outputs$cust())}) #extracts id number out of customer name dropdown
  seedsman_id <- reactive({ req(dropdown_outputs$cust())
                          sub(".* | ", "", dropdown_outputs$seedsman())}) #extracts id number out of seedsman name dropdown
  
  purch_table <- reactive({ req(dropdown_outputs$cust())
                            get_customer_products(cust_id(),
                                                  cust_df,
                                                  products_df,
                                                  custIDColumn,
                                                  custPurchaseNames,
                                                  formattedPurchaseNames,
                                                  custPurchaseKeys,
                                                  orderPurchaseNames,
                                                  EffPrice,
                                                  seedsmanIDColumn,
                                                  seedsman_id(),
                                                  yearColumn,
                                                  yearView = FALSE) })
  
  output$purchase_hist <- renderTable(purch_table(),
                                      caption='Customer Purchase History',
                                      caption.placement = getOption("xtable.caption.placement",
                                                                    "top"))
}

purchaseHist_collapseServer <- function(input, output, session, dropdown_outputs, 
                               cust_df, custIDColumn, custPurchaseNames, formattedPurchaseNames,
                               EffPrice, seedsmanIDColumn, yearColumn, yearView, products_df){
  cust_id <- reactive({sub(".* | ", "", dropdown_outputs$cust())}) #extracts id number out of customer name dropdown
  seedsman_id <- reactive({sub(".* | ", "", dropdown_outputs$seedsman())}) #extracts id number out of seedsman name dropdown
  
  observeEvent(input$year_view, {
    if(!is.null(cust_id()) & !is.na(cust_id())){
      
        purch_table <- reactive({ get_customer_products(cust_id(),
                                                  cust_df,
                                                  products_df,
                                                  custIDColumn,
                                                  custPurchaseNames,
                                                  formattedPurchaseNames,
                                                  custPurchaseKeys,
                                                  orderPurchaseNames,
                                                  EffPrice,
                                                  seedsmanIDColumn,
                                                  seedsman_id(),
                                                  yearColumn,
                                                  yearView = TRUE) })

  
          output$purchase_hist <- renderTable(purch_table(),
                                      caption='Customer Purchase History',
                                      caption.placement = getOption("xtable.caption.placement",
                                                                    "top"))  }
    else{output$purchase_hist <- renderTable(data.frame('Year of Purchase' = " ", 'Total Quantity Purchased' = " ",
                                                        'Avg. Purchase List Price' = " ", 'Avg. LCR Unit' = " ",
                                                        'Avg. LCR %' = " ", 'Avg. Farm Gate Price' = " "),
                                             caption='Customer Purchase History',
                                             caption.placement = getOption("xtable.caption.placement",
                                                                           "top"))}
  })
}

purchaseHist_expandServer <- function(input, output, session, dropdown_outputs, 
                                        cust_df, custIDColumn, custPurchaseNames, formattedPurchaseNames,
                                        EffPrice, seedsmanIDColumn, yearColumn, yearView, products_df){
  cust_id <- reactive({sub(".* | ", "", dropdown_outputs$cust())}) #extracts id number out of customer name dropdown
  seedsman_id <- reactive({sub(".* | ", "", dropdown_outputs$seedsman())}) #extracts id number out of seedsman name dropdown
  
  observeEvent(input$product_view, {
    if(!is.null(cust_id()) & !is.na(cust_id())){
      
      purch_table <- reactive({ get_customer_products(cust_id(),
                                                      cust_df,
                                                      products_df,
                                                      custIDColumn,
                                                      custPurchaseNames,
                                                      formattedPurchaseNames,
                                                      custPurchaseKeys,
                                                      orderPurchaseNames,
                                                      EffPrice,
                                                      seedsmanIDColumn,
                                                      seedsman_id(),
                                                      yearColumn,
                                                      yearView = FALSE) })
      
      
      output$purchase_hist <- renderTable(purch_table(),
                                          caption='Customer Purchase History',
                                          caption.placement = getOption("xtable.caption.placement",
                                                                        "top"))  }
    else{output$purchase_hist <- renderTable(data.frame('Year of Purchase' = " ", 'Total Quantity Purchased' = " ",
                                                        'Avg. Purchase List Price' = " ", 'Avg. LCR Unit' = " ",
                                                        'Avg. LCR %' = " ", 'Avg. Farm Gate Price' = " "),
                                             caption='Customer Purchase History',
                                             caption.placement = getOption("xtable.caption.placement",
                                                                           "top"))}
  }, ignoreInit = TRUE)
}

compInfoServer <- function(input, output, session, dropdown_outputs, 
                           cust_df, custIDColumn, fipsCodeColumn, valueProp, 
                           stateCountyToFips, compTableNamesChannel, compTableNamesOther){
observeEvent(dropdown_outputs$cust(), {
    cust_id <- reactive({sub(".* | ", "", dropdown_outputs$cust())}) #extracts id number out of customer name dropdown
    #only run if a customer has been selected
    fips_code_comp <- reactive({req(dropdown_outputs$cust())
                                  as.numeric(as.character(unlist(cust_df[cust_df[[custIDColumn]] == cust_id(),
                                                                       fipsCodeColumn])))[1]})
    # # do same for competitive info table
    comp_table <- reactive({req(dropdown_outputs$cust())
                            get_value_prop_UI(valueProp,
                                              fips_code_comp(),
                                              stateCountyToFips,
                                              compTableNamesChannel,
                                              compTableNamesOther)})
    
    output$comp_info <- renderTable(comp_table(),
                                    caption = 'Customer County Competitive Information',
                                    caption.placement = getOption("xtable.caption.placement",
                                                                  "top"))
}, ignoreInit = TRUE)

observeEvent(dropdown_outputs$county(), {
    fips_code_comp_new_cust <- reactive({req(dropdown_outputs$county())
                                          get_fips(dropdown_outputs$state(),
                                                  dropdown_outputs$county(),
                                                  stateCountyToFips)})
    comp_table_new_cust <- reactive({req(dropdown_outputs$county())
                                      get_value_prop_UI(valueProp,
                                                        fips_code_comp_new_cust(),
                                                        stateCountyToFips,
                                                        compTableNamesChannel,
                                                        compTableNamesOther)})
    
    output$comp_info <- renderTable(comp_table_new_cust(),
                                             caption = 'Customer County Competitive Information',
                                             caption.placement = getOption("xtable.caption.placement",
                                                                           "top")) 
}, ignoreInit = TRUE)

}



compInfoServer_soy <- function(input, output, session, dropdown_outputs,
                               cust_df, custIDColumn, fipsCodeColumn, YieldAdv,  
                               stateCountyToFips, compTableNamesChannel, compTableNamesOther){
  observeEvent(dropdown_outputs$cust(), {
  cust_id <- reactive({sub(".* | ", "", dropdown_outputs$cust())}) #extracts id number out of customer name dropdown
  #only run if a customer has been selected
  fips_code_comp <- reactive({req(dropdown_outputs$cust())
                              as.numeric(as.character(unlist(cust_df[cust_df[[custIDColumn]] == cust_id(),
                                                                     fipsCodeColumn])))[1]})
  
  # do same for competitive info table
  comp_table_soy <- reactive({ req(dropdown_outputs$cust())
                                makeCompTable(YieldAdv,
                                            fips_code_comp(),
                                            stateCountyToFips,
                                            compTableNamesChannel,
                                            compTableNamesOther)})

  output$comp_info <- renderTable(comp_table_soy(),
                                  caption = 'Customer County Competitive Information',
                                  caption.placement = getOption("xtable.caption.placement",
                                                                "top"))
  }, ignoreInit = TRUE)
  
  observeEvent(dropdown_outputs$county(), {
    
  
  fips_code_comp_new_cust <- reactive({req(dropdown_outputs$county())
                              get_fips(dropdown_outputs$state(),
                                       dropdown_outputs$county(),
                                       stateCountyToFips)})
  comp_table_soy_new_cust <- reactive({req(dropdown_outputs$county())
                              makeCompTable(YieldAdv,
                                            fips_code_comp_new_cust(),
                                            stateCountyToFips,
                                            compTableNamesChannel,
                                            compTableNamesOther)})
  
  output$comp_info <- renderTable(comp_table_soy_new_cust(),
                                  caption = 'Customer County Competitive Information',
                                  caption.placement = getOption("xtable.caption.placement",
                                                                "top"))
  }, ignoreInit = TRUE)
  }




custInfoPlotServer <- function(input, output, session, dropdown_outputs, 
                           cust_df, custIDColumn, custFeaturesToPlot, featsToPlotCleanNames,
                           seedsmanIDColumn, discountColumn, priceColumn){
  cust_id <- reactive({sub(".* | ", "", dropdown_outputs$cust())}) #extracts id number out of customer name dropdown
  seedsman_id <- reactive({sub(".* | ", "", dropdown_outputs$seedsman())}) #extracts id number out of customer name dropdown
 
  observeEvent(input$plot_var, {
    # if not initial choice plot selected variable
    if (input$plot_var != ""){
      cust_plot <-reactive({ req(dropdown_outputs$cust())
                              make_customer_plot(cust_df,
                                      cust_id(),
                                      custIDColumn,
                                      input$plot_var,
                                      custFeaturesToPlot,
                                      featsToPlotCleanNames,
                                      seedsmanIDColumn,
                                      seedsman_id(),
                                      discountColumn,
                                      priceColumn)})
      output$cust_plot<- renderPlot(cust_plot())
    } else {  # when var is reset to initial choice, plot is cleared
      output$cust_plot <- NULL
    }
  }, ignoreInit=TRUE)


}




