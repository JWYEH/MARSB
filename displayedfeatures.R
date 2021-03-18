#This module displays the top 5 features used in both the lcr and eff price models
#It allows users to click to see more predictors or fewer predictors

#Developers can provide labels for buttons. Defaults are "Show Additional Predictors" and
#"Hide Additional Predictors"

#Developers provide 4 tables: 2 with lcr features and 2 with eff price features



featuresUI <- function(id, label_show = "Show Additional Predictors",
                       label_hide = "Hide Additional Predictors") {
  ns <- NS(id)
  tagList(
    tableOutput(ns('discount_model_info')),
    actionButton(ns("discount_feat_button"), label = label_show),
    actionButton(ns("hide_disct_features"), label = label_hide),
    tableOutput(ns('discount_addl_info')),
    tableOutput(ns('ep_model_info')),
    actionButton(ns('ep_feat_button'), label = label_show),
    actionButton(ns('hide_ep_features'), label = label_hide),
    tableOutput(ns('ep_addl_info'))
  )
}

featuresServer <- function(input, output, session, discount_table, rest_of_disc_table,
                           ep_table, rest_of_ep_table) {
  # LCR Model
  output$discount_model_info <- renderTable({discount_table},
                                            caption='Top 5 Predictors for Recommended Unit Discount',
                                            caption.placement = getOption("xtable.caption.placement", "top"))
  
  # if show additional features button clicked, display rest of feature table
  observeEvent(input$discount_feat_button,{
    output$discount_addl_info <- renderTable({rest_of_disc_table})
    shinyjs::show('discount_addl_info')
  }, ignoreInit=TRUE)
  
  # if hide additonal features button clicked, hide rest of feature table
  observeEvent(input$hide_disct_features, {
    output$discount_addl_info <- renderText("")
    shinyjs::hide('discount_addl_info')
  }, ignoreInit=TRUE)
  
  #Effective Price Model
  output$ep_model_info <- renderTable({ep_table},
                                      caption = "Top 5 Predictors for Recommended Unit Farmgate Price",
                                      caption.placement=getOption("xtable.caption.placement", "top"))
  
  # if show additional features button clicked, display rest of feature table
  observeEvent(input$ep_feat_button,{
    output$ep_addl_info <- renderTable({rest_of_ep_table})
    shinyjs::show('ep_addl_info')
  }, ignoreInit=TRUE)
  
  # if hide rest of additional features button clicked, hide rest of feature table
  observeEvent(input$hide_ep_features, {
    output$ep_addl_info <- renderText("")
    shinyjs::hide('ep_addl_info')
  }, ignoreInit=TRUE)
}