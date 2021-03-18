custPortfolioUI <- function(id){
  ns <- NS(id)
  tagList(
    tabPanel(
      "Customer Portfolio",
       tableOutput(outputId = ns('order_table'))#,
      # textOutput(outputId = ns('order_table')),
      # textOutput(outputId = ns('order_table2'))
      
    )
  )
}


custPortfolioServer <- function(input, output, session, dropdown_outputs,
                               dropdown_outputs_portfolio, product_name, product_quantity,
                               cust_df, custIDColumn, orderTableNames,
                               seedsmanIDColumn, discountColumn, yearColumn,
                               priceColumn, effectivePriceColumn, products_df){
  cart_df <- data.table()
  output$cart_ui <- renderText("Portfolio is empty.")
#Portfolio wrangling for existing customers
  observeEvent(dropdown_outputs_portfolio$add(), {
    req(dropdown_outputs$cust())
    req(dropdown_outputs$seedsman())
    req(product_quantity() > 0)
    cust_id <- sub(".* | ", "", dropdown_outputs$cust() )
    seedsmanID <- reactive({sub(".* | ", "", dropdown_outputs$seedsman())}) #extracts id number out of seedsman name dropdown
                   # output$order_table <- renderText(cust_id)
                   # output$order_table2 <- renderText({product_name()})
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

              new_row_tmp <- reactive({get_new_portfolio_row(product_name(), product_quantity(), products_df, cart_df)})
              output$order_table <- renderTable({new_row_tmp()},
                                                caption='Portfolio Information',
                                                caption.placement = getOption("xtable.caption.placement", "top"))
              
              
                      # quantity <- unlist(new_row_tmp[, 'Quantity'])
                      #     order_tab <- data.table()
                      #     order_tab <- reactive({update_order_info_table(order_tab,
                      #                                          cart_df,
                      #                                          cust_df,
                      #                                          cust_id(),
                      #                                          seedsmanID(),
                      #                                          custIDColumn,
                      #                                          orderTableNames,
                      #                                          seedsmanIDColumn,
                      #                                          discountColumn,
                      #                                          yearColumn,
                      #                                          priceColumn,
                      #                                          effectivePriceColumn,
                      #                                          initial=TRUE,
                      #                                          new_cust=FALSE)})
                          # output$order_table <- renderTable({order_tab()},
                          #                                   caption='Portfolio Information',
                          #                                   caption.placement = getOption("xtable.caption.placement", "top"))
                  
                   })
  
#Portfolio wrangling for new customers
  observeEvent(dropdown_outputs_portfolio$add(), {
    req(dropdown_outputs$state())
    req(dropdown_outputs$county())
    req(dropdown_outputs$seedsman())
    req(product_quantity() > 0)
    seedsmanID <- reactive({sub(".* | ", "", dropdown_outputs$seedsman())}) #extracts id number out of seedsman name dropdown
    fips_code <- reactive({req(dropdown_outputs$county())
                                                 get_fips(dropdown_outputs$state(),
                                                 dropdown_outputs$county(),
                                                 stateCountyToFips)})
    # output$order_table <- renderText(fips_code())
    # output$order_table2 <- renderText({product_name()})
  })

    
}






# custPortfolioUI <- function(id){
#   ns <- NS(id)
#   tagList(
#     tabPanel(
#       "Customer Portfolio", 
#       uiOutput(outputId = 'cart_ui'),
#       shinyjs::hidden(
#         div(id= paste0("show_hide"),
#             splitLayout(cellWidths = c(150,100),
#                         tags$b("* Offered Unit LCR: "),
#                         # prettyRadioButtons(inputId = paste0("dollar_percent_table"),
#                         #                    label = NULL,
#                         #                    choices = c("$" = "dollar_table","%" = "percent_table"),
#                         #                    selected = "percent_table",
#                         #                    inline = TRUE)),
#             br(),
#             # show total portfolio plot 
#             # actionButton(inputId = paste0('get_total_plot'),
#             #              label = HTML('Show Portfolio <br/> Total Plot')),
#             # fluidRow(column(12,h1("Portfolio Information",style="color:white;background-color:lightsteelblue;",align="center"))),
#             # # order info table
#             tableOutput(outputId = paste0('order_table')),
#             # #hr(),
#             # # main plot 
#             plotlyOutput(outputId=paste0("dist_plot"))#,
#             # fluidRow(column(12,h1("FSRs Teach MARS: 4 Steps",style="color:white;background-color:lightsteelblue;",align="center"))),
#             # fluidRow(column(12, h4('Step 1 - Enter the average unit LCR offered for the Customer Portfolio above, either as a percentage or as a dollar amount. (If you have already entered offered unit LCR amounts into the Offered Unit LCR column of the Customer Portfolio, skip ahead to Step 2.)'))),
#         #     br(),
#         #     splitLayout(
#         #       cellWidths = c(300,100),
#         #       numericInput(inputId = "lcr_offer",
#         #                    label = "Average Unit LCR Offered (Portfolio-Level)",
#         #                    value = 0,
#         #                    min = 0,
#         #                    max = if(input$dollar_percent == "percent") {
#         #                      100
#         #                    } else {
#         #                      800
#         #                    },
#         #                    step = 0.01,
#         #                    width="250px"),
#         #       prettyRadioButtons(inputId = "dollar_percent",
#         #                          label = " ",
#         #                          choices = c("$" = "dollar","%" = "percent"),
#         #                          selected = "percent",
#         #                          inline = TRUE)),
#         #     br(),
#         #     fluidRow(column(12, h4('Step 2 - Enter the average unit farmgate price that you were competing against, if known.'))),
#         #     br(),
#         #     numericInput(inputId = "comp_offer",
#         #                  label = "Competing Avg. Unit Farmgate Price",
#         #                  value = 0,
#         #                  min = 0,
#         #                  step = 0.01,
#         #                  width="250px"),
#         #     br(),
#         #     fluidRow(column(12, h4('Step 3 - Check the box below if the deal was lost.'))),
#         #     br(),
#         #     prettyCheckbox(inputId = 'deal_lost',
#         #                    label = 'Deal Lost',
#         #                    icon = icon("check"),
#         #                    plain = TRUE,
#         #                    inline = TRUE),
#         #     br(),
#         #     fluidRow(column(12,h4('Step 4 - Record your answers.'))),
#         #     br(),
#         #     # export cart -- corn
#         #     actionButton(inputId = 'export_cart',
#         #                  label = HTML('Record <br/> Info')),
#         #     # success message -- corn
#         #     textOutput(outputId = 'download_success'),
#         #     br(),
#         #     
#         #       shinyjs::hidden(
#         #         div(id= "show_hide_downloadlink",
#         #             'Click',
#         #             downloadLink('download_recorded', 'here'),
#         #             'to download recorded info.'))
#             )
#          ))
#     )
#   )#end tag list
#   
# }
# 
# 
# custPortfolioServer <- function(input, output, session, dropdown_outputs,
#                                 dropdown_outputs_crop, CROP, logTransformDiscount,
#                                 products_df, quantityColumn, productColumn, priceColumn,
#                                 features, countyFeatures, cust_df, discountModel, seedsmanIDColumn,
#                                 fipsCodeColumn, predict_discount_new_cust, 
#                                 predict_discount_new_cust_raw,
#                                 cartDropCols, predict_discount, predict_discount_raw, 
#                                 EffPrice, custIDColumn, epCustIDColumn, 
#                                 yearColumn, discountColumn, effectivePriceColumn,
#                                 epSeedsmanIDColumn, epYearColumn,
#                                 stdN){
#   # All things that should happen when "Add to Portfolio' is clicked #################################
#   # CORN ---------------------------------------------------------------------------------------------
#   observeEvent(input$add_to_cart, {
#     req(dropdown_outputs$seedsman())
#     req(dropdown_outputs$cust())
#     req(dropdown_outputs_crop$product_name())
#     req(dropdown_outputs_crop$product_quantity())
#     
#     shinyjs::hide(id = "show_hide_downloadlink")
#     
#     # if products have been previously added to cart and deleted through portfolio prepopulation functionality,
#     # prior to adding product to cart, use correct cart
#     # if (exists('cart_df_check')){ 
#     #   if(!is.null(cart_df_check)){
#     #     cart_df <<- cart_df_check
#     #     cart_df_check <<- NULL
#     #   }
#     # }
#     
#     # clear download success message
#     output$download_success <- NULL
#     
#     # make new row data table from user input
#     new_row <- data.table(Product = dropdown_outputs_crop$product_name(),
#                           Quantity = dropdown_outputs_crop$product_quantity())
#     # add list price
#     new_row_tmp <- merge(new_row, products_df, by = "Product")
#     
#     # track product for use later in predict functions
#     product <- unlist(new_row_tmp[, 'Product'])
#     
#     # combine with existing cart
#     new_port_tmp <- rbind(cart_df, new_row_tmp, use.names = TRUE, fill = TRUE)
#     
#     # clean $ from price column to merge
#     new_port_tmp[, 'Price'] <- as.numeric(gsub("[\\$,]", "", new_port_tmp$Price))
#     
#     # extract whole portfolio before subsetting by product to correctly make basket level effective price predicitons
#     new_port <- new_port_tmp[, .(Quantity = sum(Quantity),
#                                  Price = mean(Price)),
#                              by = Product]
#     # get order level stats to use as memoisation keys that feed into predict discount function, can't just use new_port
#     # otherwise memoisation will not work correctly, all keys must be UNIQUE
#     avgPrice <- mean(new_port$Price)
#     totalQuantity <- sum(new_port$Quantity)
#     
#     # collapse by product
#     tmp <- new_port_tmp[new_port_tmp$Product == unlist(product), ]
#     
#     # get correct quantity for product added to cart to use in discount prediction later
#     new_row_tmp <- tmp[, .(Quantity = sum(Quantity),
#                            Price = mean(Price)),
#                        by = Product]
#     quantity <- unlist(new_row_tmp[, 'Quantity'])
#     
#     # if customer is a new customer and state/county isn't null, update cart with predictions
#     #  and show correct graph
#     if (dropdown_outputs$cust() == 'New Customer' & 
#         !is.null(dropdown_outputs$state()) & !is.null(dropdown_outputs$county())){
#       # get fips code
#       fips_code <- reactive({req(dropdown_outputs$county())
#                                            get_fips(dropdown_outputs$state(),
#                                            dropdown_outputs$county(),
#                                            stateCountyToFips)})
#       # get seedsman ID
#       seedsmanID <- reactive({sub(".* | ", "", dropdown_outputs$seedsman())}) #extracts id number out of seedsman name dropdown
#       
#       
#       # predict discounts associated with new row
#       predict_discount_new_cust_args <- list(dropdown_outputs_crop$product(),
#                                              dropdown_outputs_crop$quantity(),
#                                              fips_code(), 
#                                              logTransformDiscount,
#                                              quantityColumn,
#                                              priceColumn,
#                                              productColumn,
#                                              features,
#                                              countyFeatures,
#                                              cust_df,
#                                              discountModel,
#                                              pllel,
#                                              avgPrice,
#                                              totalQuantity,
#                                              seedsmanIDColumn,
#                                              seedsmanID(),
#                                              fipsCodeColumn)
#       discounts_new_cust <- wrapMemoise(predict_discount_new_cust_raw, 
#                                         predict_discount_new_cust, predict_discount_new_cust_args)
#       
#       # update cart and table accordingly
#       cart_df <<- update_cart_new_cust(discounts_new_cust,
#                                        cart_df,
#                                        new_row,
#                                        fips_code(),
#                                        dropdown_outputs_crop$product(),
#                                        products_df)
#       
#       # if more than one item in customer's cart, call predict discount functions for all rows 
#       #  where product is not the most recently added to correctly update product level discounts 
#       #  now that order level stats have changed 
#       if (nrow(cart_df) > 1) {
#         tmp <- c()
#         # only do for products that are not in the most recently added row
#         tmp_df <- cart_df[cart_df$Product != product, ]
#         for (i in 1:nrow(tmp_df)) {
#           product_tmp <- unlist(tmp_df[i, 'Product'])
#           quantity_tmp <- unlist(tmp_df[i, 'Quantity'])
#           predict_discount_new_cust_args <- list(product_tmp,
#                                                  quantity_tmp,
#                                                  fips_code(), 
#                                                  logTransformDiscount,
#                                                  quantityColumn,
#                                                  priceColumn,
#                                                  productColumn,
#                                                  features,
#                                                  countyFeatures,
#                                                  cust_df,
#                                                  discountModel,
#                                                  pllel,
#                                                  avgPrice,
#                                                  totalQuantity,
#                                                  seedsmanIDColumn,
#                                                  seedsmanID(),
#                                                  fipsCodeColumn)
#           discounts_tmp <- wrapMemoise(predict_discount_new_cust_raw, 
#                                        predict_discount_new_cust, predict_discount_new_cust_args)
#           
#           # get mean predicted disount as percentage of list price for relevant cart_df row
#           discountPct <- paste0(format(round(as.numeric((mean(discounts_tmp)/as.numeric(gsub("[\\$,]", "", tmp_df[i, Price])) * 100)), 2), nsmall=2),
#                                 '%')
#           
#           # round mean of discount predicitons to 2 decimal places and place into 
#           #   relevant row in cart_df
#           cart_df[cart_df$Product == product_tmp, Discount := paste0('$', format(round(mean(discounts_tmp), 2), nsmall=2))]
#           
#           # put discount as percentage of list price into relevant cart_df row
#           cart_df[cart_df$Product == product_tmp, DiscountPCT := discountPct ]
#           
#           # put updated discounts into recommended column
#           cart_df[cart_df$Product == product_tmp, RecDiscountPCT := discountPct]
#           cart_df[cart_df$Product == product_tmp, RecDiscount := cart_df[cart_df$Product == product_tmp, Discount]]
#           
#           # combine with previous row's discounts so portfolio average can be calculated
#           tmp <-append(tmp, list(discounts_tmp))
#         }
#         
#         # append those to discounts for new row so we have entire distribution for the order
#         discounts_out <- unlist(append(tmp, list(discounts_new_cust)))
#         
#       } else if (nrow(cart_df) == 1) {  # if not more than one item in cart, make order total equal
#         #  to the discount's new row
#         discounts_out <- discounts_new_cust
#       }
#       
#       # generate plot for entire order
#       cart_plt <- make_plot_cart_order_new_cust(discounts_out,
#                                                 stdN,
#                                                 cart_df)
#       cart_plt <- ggplotly(cart_plt)
#       # render plot
#       output$dist_plot <- renderPlotly({cart_plt})
#       
#       # create observe event so plot can be easily referenced when "show portfolio total graph" button is
#       #  clicked
#       observeEvent(input$get_total_plot(), {
#         output$dist_plot <- renderPlotly({cart_plt})
#       })
#       
#       
#     } else { # if customer is not new customer
#       
#       cust_id <- reactive({sub(".* | ", "", dropdown_outputs$cust())}) #extracts id number out of customer name dropdown
#       seedsmanID <- reactive({sub(".* | ", "", dropdown_outputs$seedsman())}) #extracts id number out of customer name dropdown
#       # predict discounts for product associated with new row added to cart and 
#       #  updated portfolio (new_port)
#       predict_discount_args <- list(dropdown_outputs_crop$product(),
#                                     dropdown_outputs_crop$quantity(),
#                                     cust_id(),
#                                     cust_df,
#                                     logTransformDiscount,
#                                     quantityColumn,
#                                     priceColumn,
#                                     productColumn,
#                                     custIDColumn,
#                                     cartDropCols,
#                                     features,
#                                     discountModel,
#                                     pllel,
#                                     avgPrice,
#                                     totalQuantity,
#                                     yearColumn,
#                                     seedsmanIDColumn,
#                                     seedsmanID(),
#                                     countyFeatures,
#                                     fipsCodeColumn)
#       discounts <- wrapMemoise(predict_discount_raw, predict_discount, predict_discount_args)
#       
#       # update cart and table accordingly--discounts updated for most recent row predicted
#       cart_df <<- update_cart(discounts, 
#                               cart_df, 
#                               new_row, 
#                               dropdown_outputs$cust(), 
#                               cust_df,
#                               dropdown_outputs_crop$product_name(),
#                               EffPrice,
#                               custIDColumn,
#                               epCustIDColumn,
#                               productColumn,
#                               yearColumn,
#                               discountColumn,
#                               effectivePriceColumn,
#                               seedsmanIDColumn,
#                               epSeedsmanIDColumn,
#                               seedsmanID(),
#                               products_df,
#                               epYearColumn,
#                               quantityColumn,
#                               priceColumn)
#       # if more than one item in customer's cart, call predict discount functions for all rows 
#       #  where product is not the most recently added to correctly update product level discounts 
#       #  now that order level stats have changed
#       if (nrow(cart_df) > 1) {
#         tmp <- list()
#         # only do for products that are not in most recently added row
#         tmp_df <- cart_df[cart_df$Product != product, ]
#         for (i in 1:nrow(tmp_df)) {
#           product_tmp <- unlist(tmp_df[i, 'Product'])
#           quantity_tmp <- unlist(tmp_df[i, 'Quantity'])
#           predict_discount_args <- list(product_tmp,
#                                         quantity_tmp,
#                                         cust_id(),
#                                         cust_df,
#                                         logTransformDiscount,
#                                         quantityColumn,
#                                         priceColumn,
#                                         productColumn,
#                                         custIDColumn,
#                                         cartDropCols,
#                                         features,
#                                         discountModel,
#                                         pllel,
#                                         avgPrice,
#                                         totalQuantity,
#                                         yearColumn,
#                                         seedsmanIDColumn,
#                                         seedsmanID,
#                                         countyFeatures,
#                                         fipsCodeColumn)
#           discounts_tmp <- wrapMemoise(predict_discount_raw, predict_discount, predict_discount_args)
#           
#           # get mean predicted disount as percentage of list price for relevant cart_df row
#           discountPct <- paste0(format(round(as.numeric((mean(discounts_tmp)/as.numeric(gsub("[\\$,]", "", tmp_df[i, Price])) * 100)), 2), nsmall=2),
#                                 '%')
#           
#           # round mean of discount predicitons to 2 decimal places and place into
#           #  relevant cart_df row
#           cart_df[cart_df$Product == product_tmp, Discount := paste0('$', format(round(mean(discounts_tmp), 2), nsmall=2))]
#           
#           # put discount as percentage of list price into relevant cart_df row
#           cart_df[cart_df$Product == product_tmp, DiscountPCT := discountPct ]
#           
#           # get min of predicted/last allocated discount, place into relevant cart_df row 
#           if (cart_df[cart_df$Product == product_tmp, LastUnitDiscount] == "NA"){
#             cart_df[cart_df$Product == product_tmp, RecDiscount := cart_df[cart_df$Product == product_tmp, Discount]]
#             cart_df[cart_df$Product == product_tmp, RecDiscountPCT := cart_df[cart_df$Product == product_tmp, DiscountPCT]]
#           } else {
#             checkDiscounts <- list(as.numeric(gsub("[\\$,]", "", cart_df[cart_df$Product == product_tmp, Discount])), 
#                                    as.numeric(gsub("[\\$,]", "", cart_df[cart_df$Product == product_tmp, LastUnitDiscount])))
#             checkDiscountsPCT <- list(as.numeric(gsub("[\\%,]", "", cart_df[cart_df$Product == product_tmp, DiscountPCT])), 
#                                       as.numeric(gsub("[\\%,]", "", cart_df[cart_df$Product == product_tmp, LastUnitDiscountPCT])))
#             cart_df[cart_df$Product == product_tmp, RecDiscount := paste0('$', format(round(min(unlist(checkDiscounts)), 2), nsmall=2))]
#             cart_df[cart_df$Product == product_tmp, RecDiscountPCT := paste0(format(round(min(unlist(checkDiscountsPCT)), 2), nsmall=2), '%')]
#           }
#           
#           # combine with previous row's discounts
#           tmp <-append(tmp, list(discounts_tmp))
#         }
#         
#         discounts_out <- unlist(append(tmp, list(discounts)))
#       }
#       else if (nrow(cart_df) == 1) { # if not more than one item in cart, make portfolio equal
#         #  to the discount's new row
#         discounts_out <- discounts
#       }
#       # generate plot for entire order
#       cart_plt <-  make_plot_cart_order(discounts_out,
#                                         stdN,
#                                         cart_df,
#                                         cust_id,
#                                         cust_df,
#                                         custIDColumn,
#                                         discountColumn,
#                                         yearColumn,
#                                         seedsmanID,
#                                         seedsmanIDColumn,
#                                         priceColumn)
#       cart_plt <- ggplotly(cart_plt)
#       # render plot
#       output$dist_plot <- renderPlotly({cart_plt})
#       # create observe event so plot can be easily referenced when "show portfolio total graph" button is clicked
#       observeEvent(input$get_total_plot, {
#         output$dist_plot <- renderPlotly({cart_plt})
#       })
#     }
#     
#     # update order table for total order
#     # if the customer's cart is empty, don't show anything
#     if (nrow(cart_df) == 0) {
#       renderText("")
#     } else { # if not, calculate order info for display in UI
#       # correct function for existing customer
#       if (dropdown_outputs$cust() != 'New Customer') {
#         cust_id <- get_id(dropdown_outputs$cust())
#         order_tab <- data.table()
#         order_tab <- update_order_info_table(order_tab,
#                                              cart_df,
#                                              cust_df,
#                                              cust_id,
#                                              seedsmanID,
#                                              custIDColumn,
#                                              orderTableNames,
#                                              seedsmanIDColumn,
#                                              discountColumn,
#                                              yearColumn,
#                                              priceColumn,
#                                              effectivePriceColumn,
#                                              initial=TRUE,
#                                              new_cust=FALSE)
#         output$order_table <- renderTable({order_tab},
#                                           caption='Portfolio Information',
#                                           caption.placement = getOption("xtable.caption.placement", "top"))
#         
#         #infobox for lcr unit that goes along the top of the main panel
#         output$Rec.LCR.Unit <- renderInfoBox({
#           
#           x <- order_tab$`Rec. LCR Unit`
#           
#           infoBox(
#             tags$p(style = "font-size: 16px;", "Recommended LCR Unit"), 
#             tags$p(style = "font-size: 22px;", paste(x, sep = "")   ),
#             icon = icon("dollar-sign"),
#             color = "green", fill = TRUE
#           )
#         })
#         
#         #infobox lcr % that goes along the top of the main panel
#         output$Rec.LCR.Pct <- renderInfoBox({
#           
#           x <- order_tab$`Rec. LCR %`
#           
#           infoBox(
#             tags$p(style = "font-size: 18px;", "Recommended LCR %"), 
#             tags$p(style = "font-size: 22px;", x), 
#             icon = icon("percent"),
#             color = "green", fill = TRUE
#           )
#         })
#         
#         
#       } else{
#         # do for new customer
#         order_tab <- data.table()
#         order_tab <- update_order_info_table(order_tab,
#                                              cart_df,
#                                              cust_df,
#                                              cust_id,
#                                              seedsmanID,
#                                              custIDColumn,
#                                              orderTableNamesNewCust,
#                                              seedsmanIDColumn,
#                                              discountColumn,
#                                              yearColumn,
#                                              priceColumn,
#                                              effectivePriceColumn,
#                                              initial=TRUE,
#                                              new_cust=TRUE)
#         output$order_table <- renderTable({order_tab},
#                                           caption='Portfolio Information',
#                                           caption.placement = getOption("xtable.caption.placement", "top"))
#         
#         #infobox for lcr unit that goes along the top of the main panel
#         output$Rec.LCR.Unit <- renderInfoBox({
#           
#           x <- order_tab$`Prevailing LCR Unit`
#           
#           infoBox(
#             tags$p(style = "font-size: 16px;", "Recommended LCR Unit"), 
#             tags$p(style = "font-size: 22px;", paste(x, sep = "")   ), 
#             icon = icon("dollar-sign"),
#             color = "green", fill = TRUE
#           )
#         })
#         
#         
#         #infobox for lcr % that goes along the top of the main panel
#         output$Rec.LCR.Pct <- renderInfoBox({
#           
#           x <- order_tab$`Prevailing LCR %`
#           
#           infoBox(
#             tags$p(style = "font-size: 18px;", "Recommended LCR %"), 
#             tags$p(style = "font-size: 22px;", x), 
#             icon = icon("percent"),
#             color = "green", fill = TRUE
#           )
#         })
#       }
#     }
#     
#     
#     # Create cart table with buttons
#     cart_ui_obj <- make_cart_ui(cart_df, new_ids = TRUE)
#     output$cart_ui <- cart_ui_obj$ui
#     # Create listeners for each delete button
#     lapply(cart_ui_obj$ids, function(id) {
#       observeEvent(input[[id]], {
#         
#         # delete product from cart_df
#         cart_df <<- cart_df[button_id != id, ]
#         
#         # render download success message null
#         output$download_success <- NULL
#         
#         # extract correct whole portfolio before subsetting by product to correctly update basket level
#         # effective price predicitons
#         new_port <- cart_df[, c('Product', 'Quantity', 'Price')]
#         
#         # clean price in whole portfolio
#         new_port[, 'Price'] <-as.numeric(gsub("[\\$,]", "", new_port$Price))
#         
#         # get order level stats to use as memoisation keys that feed into predict discount function, can't just use new_port
#         # otherwise memoisation will not work correctly, all keys must be UNIQUE
#         avgPrice <- mean(new_port$Price)
#         totalQuantity <- sum(new_port$Quantity)
#         
#         # Output cart ui or message if nothing left in cart and clear plots
#         if (nrow(cart_df) == 0) {
#           output$cart_ui <- renderText("Portfolio is empty.")
#           output$dist_plot <- NULL
#           output$order_table <- renderText("")
#           # make info boxes null
#           #output$Total.Retail.Amount <- NULL
#           output$Rec.LCR.Unit <- NULL
#           output$Rec.LCR.Pct <- NULL
#         } else { # if cart not empty
#           
#           # get last product in cart
#           tmp_cart <- tail(cart_df, 1)
#           
#           # create new row from last product to use in updating/plotting functions
#           new_row <- tmp_cart[, c('Product', 'Quantity')]
#           
#           # track product and quantity for use later
#           product <- unlist(new_row[, 'Product'])
#           quantity <- unlist(new_row[, 'Quantity'])
#           
#           
#           # if customer is new customer and state/county not null
#           if (dropdown_outputs$cust() == 'New Customer' & !is.null(dropdown_outputs$state()) & 
#               !is.null(dropdown_outputs$county())) {
#             
#             # get fips code for new customer's county
#             fips_code <- get_fips(dropdown_outputs$state(),
#                                   dropdown_outputs$county(),
#                                   stateCountyToFips)
#             
#             # predict discounts associated for last item in cart and updated portfolio
#             predict_discount_new_cust_args <- list(product,
#                                                    quantity,
#                                                    fips_code, 
#                                                    logTransformDiscount,
#                                                    quantityColumn,
#                                                    priceColumn,
#                                                    productColumn,
#                                                    features,
#                                                    countyFeatures,
#                                                    cust_df,
#                                                    discountModel,
#                                                    pllel,
#                                                    avgPrice,
#                                                    totalQuantity,
#                                                    seedsmanIDColumn,
#                                                    seedsmanID(),
#                                                    fipsCodeColumn)
#             discounts_new_cust <- wrapMemoise(predict_discount_new_cust_raw, 
#                                               predict_discount_new_cust, predict_discount_new_cust_args)
#             
#             # update last row of cart with newly predicted discounts
#             cart_df[nrow(cart_df), Discount := paste0('$', format(round(mean(discounts_new_cust), 2), nsmall=2))]
#             
#             # discount pct
#             discountPct <- paste0(format(round(as.numeric((mean(discounts_new_cust)/as.numeric(gsub("[\\$,]", "", cart_df[nrow(cart_df), Price])) * 100)), 2), nsmall=2),
#                                   '%')
#             cart_df[nrow(cart_df), DiscountPCT := discountPct]
#             
#             # put updated discounts into recommended column
#             cart_df[nrow(cart_df), RecDiscountPCT := discountPct]
#             cart_df[nrow(cart_df) == product_tmp, RecDiscount := cart_df[nrow(cart_df), Discount]]
#             
#             # update order total plot and order information table accordingly
#             # for all rows that didn't get updated correctly, update discounts now that order level
#             # stats have changed if more than one item in cart
#             if (nrow(cart_df) > 1) {
#               tmp <- c()
#               for (i in 1:(nrow(cart_df)-1)) {
#                 product_tmp <- unlist(cart_df[i, 'Product'])
#                 quantity_tmp <- unlist(cart_df[i, 'Quantity'])
#                 predict_discount_new_cust_args <- list(product_tmp,
#                                                        quantity_tmp,
#                                                        fips_code, 
#                                                        logTransformDiscount,
#                                                        quantityColumn,
#                                                        priceColumn,
#                                                        productColumn,
#                                                        features,
#                                                        countyFeatures,
#                                                        cust_df,
#                                                        discountModel,
#                                                        pllel,
#                                                        avgPrice,
#                                                        totalQuantity,
#                                                        seedsmanIDColumn,
#                                                        seedsmanID,
#                                                        fipsCodeColumn)
#                 discounts_tmp <- wrapMemoise(predict_discount_new_cust_raw, predict_discount_new_cust, 
#                                              predict_discount_new_cust_args)
#                 
#                 # get mean predicted disount as percentage of list price for relevant cart_df row
#                 discountPct <- paste0(format(round(as.numeric((mean(discounts_tmp)/as.numeric(gsub("[\\$,]", "",
#                                                                                                    cart_df[i, Price])) * 100)), 2), nsmall=2),
#                                       '%')
#                 
#                 # round mean of discount predicitons to 2 decimal places and place into
#                 #  relevant cart_df row
#                 cart_df[i, Discount := paste0('$', format(round(mean(discounts_tmp), 2), nsmall=2))]
#                 
#                 # put discount as percentage of list price into relevant cart_df row
#                 cart_df[i, DiscountPCT := discountPct ]
#                 
#                 # put updated discounts into recommended column
#                 cart_df[i, RecDiscountPCT := discountPct]
#                 cart_df[i, RecDiscount := cart_df[i, Discount]]
#                 
#                 # store to average for order distribution
#                 tmp <-append(tmp, list(discounts_tmp))
#               }
#               
#               discounts_out <- unlist(append(tmp, list(discounts_new_cust)))
#               
#             } else if (nrow(cart_df) == 1) {  # if only one item in cart...
#               
#               discounts_out <- discounts_new_cust
#               
#             }
#             
#             cart_plt <-  make_plot_cart_order_new_cust(discounts_out,
#                                                        stdN,
#                                                        cart_df)
#             cart_plt <- ggplotly(cart_plt)
#             
#             order_tab <- update_order_info_table(order_tab, 
#                                                  cart_df, 
#                                                  cust_df, 
#                                                  cust_id(),
#                                                  seedsmanID(),
#                                                  custIDColumn,
#                                                  orderTableNamesNewCust,
#                                                  seedsmanIDColumn,
#                                                  discountColumn,
#                                                  yearColumn,
#                                                  priceColumn,
#                                                  effectivePriceColumn,
#                                                  initial=FALSE,
#                                                  new_cust=TRUE)
#             
#             output$order_table <- renderTable({order_tab},
#                                               caption='Portfolio Information',
#                                               caption.placement = getOption("xtable.caption.placement", "top"))
#             
#             #infobox for lcr unit that goes along the top of the main panel
#             output$Rec.LCR.Unit <- renderInfoBox({
#               
#               x <- order_tab$`Prevailing LCR Unit`
#               
#               infoBox(
#                 tags$p(style = "font-size: 16px;", "Recommended LCR Unit"), 
#                 tags$p(style = "font-size: 22px;", paste(x, sep = "")   ), 
#                 icon = icon("dollar-sign"),
#                 color = "green", fill = TRUE
#               )
#             })
#             
#             
#             #infobox for lcr % that goes along the top of the main panel
#             output$Rec.LCR.Pct <- renderInfoBox({
#               
#               x <- order_tab$`Prevailing LCR %`
#               
#               infoBox(
#                 tags$p(style = "font-size: 18px;", "Recommended LCR %"), 
#                 tags$p(style = "font-size: 22px;", x), 
#                 icon = icon("percent"),
#                 color = "green", fill = TRUE
#               )
#             })
#             
#             
#           } else {  # if customer not a new customer...
#             
#             # get customer id
#             cust_id <- get_id(dropdown_outputs$cust())
#             
#             
#             # predict discounts associated with new_row and new_port
#             predict_discount_args <- list(dropdown_outputs_crop$product(),
#                                           dropdown_outputs_crop$quantity(),
#                                           cust_id(),
#                                           cust_df,
#                                           logTransformDiscount,
#                                           quantityColumn,
#                                           priceColumn,
#                                           productColumn,
#                                           custIDColumn,
#                                           cartDropCols,
#                                           features,
#                                           discountModel,
#                                           pllel,
#                                           avgPrice,
#                                           totalQuantity,
#                                           yearColumn,
#                                           seedsmanIDColumn,
#                                           seedsmanID(),
#                                           countyFeatures,
#                                           fipsCodeColumn)
#             discounts <- wrapMemoise(predict_discount_raw, predict_discount, predict_discount_args)
#             
#             # update last row of cart with newly predicted discounts
#             cart_df[nrow(cart_df), Discount := paste0('$', format(round(mean(discounts), 2), nsmall=2))]
#             
#             # update discount pct
#             discountPct <- paste0(format(round(as.numeric((mean(discounts)/as.numeric(gsub("[\\$,]", "", cart_df[nrow(cart_df), Price])) * 100)), 2), nsmall=2),
#                                   '%')
#             cart_df[nrow(cart_df), DiscountPCT := discountPct]
#             
#             # Take minimum of predicted/actual and make "recommended" in last row of cart_df
#             # if NA...
#             if (cart_df[nrow(cart_df), LastUnitDiscount] == "NA"){
#               cart_df[nrow(cart_df), RecDiscount := cart_df[nrow(cart_df), Discount]]
#               cart_df[nrow(cart_df), RecDiscountPCT := cart_df[nrow(cart_df), DiscountPCT]]
#             } else {
#               checkDiscounts <- list(as.numeric(gsub("[\\$,]", "", cart_df[nrow(cart_df), Discount])), 
#                                      as.numeric(gsub("[\\$,]", "", cart_df[nrow(cart_df), LastUnitDiscount])))
#               checkDiscountsPCT <- list(as.numeric(gsub("[\\%,]", "", cart_df[nrow(cart_df), DiscountPCT])), 
#                                         as.numeric(gsub("[\\%,]", "", cart_df[nrow(cart_df), LastUnitDiscountPCT])))
#               cart_df[nrow(cart_df), RecDiscount := paste0('$', format(round(min(unlist(checkDiscounts)), 2), nsmall=2))]
#               cart_df[nrow(cart_df), RecDiscountPCT := paste0(format(round(min(unlist(checkDiscountsPCT)), 2), nsmall=2), '%')]
#             }
#             
#             # if more than one item in cart, correctly update discounts and order totals now that order
#             # level stats have changed for items that are not the last row in cart
#             if (nrow(cart_df) > 1) {
#               tmp <- list()
#               for (i in 1:(nrow(cart_df)-1)) {
#                 product_tmp <- unlist(cart_df[i, 'Product'])
#                 quantity_tmp <- unlist(cart_df[i, 'Quantity'])
#                 predict_discount_args <- list(product_tmp,
#                                               quantity_tmp,
#                                               cust_id(),
#                                               cust_df,
#                                               logTransformDiscount,
#                                               quantityColumn,
#                                               priceColumn,
#                                               productColumn,
#                                               custIDColumn,
#                                               cartDropCols,
#                                               features,
#                                               discountModel,
#                                               pllel,
#                                               avgPrice,
#                                               totalQuantity,
#                                               yearColumn,
#                                               seedsmanIDColumn,
#                                               seedsmanID(),
#                                               countyFeatures,
#                                               fipsCodeColumn)
#                 discounts_tmp <- wrapMemoise(predict_discount_raw, predict_discount, predict_discount_args)
#                 
#                 # get mean predicted disount as percentage of list price
#                 discountPct <- paste0(format(round(as.numeric((mean(discounts_tmp)/as.numeric(gsub("[\\$,]", "", cart_df[i, Price])) * 100)), 2), nsmall=2),
#                                       '%')
#                 # round mean of discount predicitons to 2 decimal places and place into
#                 # relevant cart_df row
#                 cart_df[i, Discount := paste0('$', format(round(mean(discounts_tmp), 2), nsmall=2))]
#                 
#                 # put discount as percentage of list price into relevant cart_df row
#                 cart_df[i, DiscountPCT := discountPct ]
#                 
#                 # Take minimum of predicted/actual and make "recommended"
#                 # if NA...
#                 if (cart_df[i, LastUnitDiscount] == "NA"){
#                   cart_df[i, RecDiscount := cart_df[i, Discount]]
#                   cart_df[i, RecDiscountPCT := cart_df[i, DiscountPCT]]
#                 } else {
#                   checkDiscounts <- list(as.numeric(gsub("[\\$,]", "", cart_df[i, Discount])), 
#                                          as.numeric(gsub("[\\$,]", "", cart_df[i, LastUnitDiscount])))
#                   checkDiscountsPCT <- list(as.numeric(gsub("[\\%,]", "", cart_df[i, DiscountPCT])), 
#                                             as.numeric(gsub("[\\%,]", "", cart_df[i, LastUnitDiscountPCT])))
#                   cart_df[i, RecDiscount := paste0('$', format(round(min(unlist(checkDiscounts)), 2), nsmall=2))]
#                   cart_df[i, RecDiscountPCT := paste0(format(round(min(unlist(checkDiscountsPCT)), 2), nsmall=2), '%')]
#                 }
#                 
#                 # combine with previous row's discounts
#                 tmp <-append(tmp, list(discounts_tmp))
#               }
#               # combine new_row discounts and all other newly updated discounts
#               discounts_out <- unlist(append(tmp, list(discounts)))
#             }
#             # if only one item in cart make discounts new_row
#             else if (nrow(cart_df) == 1) {  # if one item in cart...
#               
#               # make discount to be plotted only the discounts for new_row
#               discounts_out <- discounts
#               
#             }
#             
#             # make plot for whole order using discounts_out
#             cart_plt <-  make_plot_cart_order(discounts_out,
#                                               stdN,
#                                               cart_df, 
#                                               cust_id(),
#                                               cust_df,
#                                               custIDColumn,
#                                               discountColumn,
#                                               yearColumn,
#                                               seedsmanID(),
#                                               seedsmanIDColumn,
#                                               priceColumn)
#             cart_plt <- ggplotly(cart_plt)
#             
#             # update order information table using updated cart_df
#             order_tab <- update_order_info_table(order_tab,
#                                                  cart_df,
#                                                  cust_df,
#                                                  cust_id(),
#                                                  seedsmanID(),
#                                                  custIDColumn,
#                                                  orderTableNames,
#                                                  seedsmanIDColumn,
#                                                  discountColumn,
#                                                  yearColumn,
#                                                  priceColumn,
#                                                  effectivePriceColumn,
#                                                  initial=FALSE,
#                                                  new_cust=FALSE)
#             # render table in UI
#             output$order_table <- renderTable({order_tab},
#                                               caption='Portfolio Information',
#                                               caption.placement = getOption("xtable.caption.placement", "top"))
#             
#             #infobox for lcr unit that goes along the top of the main panel
#             output$Rec.LCR.Unit <- renderInfoBox({
#               
#               x <- order_tab$`Rec. LCR Unit`
#               
#               infoBox(
#                 tags$p(style = "font-size: 16px;", "Recommended LCR Unit"), 
#                 tags$p(style = "font-size: 22px;", paste(x, sep = "")   ), 
#                 icon = icon("dollar-sign"),
#                 color = "green", fill = TRUE
#               )
#             })
#             
#             #infobox for lcr % that goes along the top of the main panel
#             output$Rec.LCR.Pct <- renderInfoBox({
#               
#               x <- order_tab$`Rec. LCR %`
#               
#               infoBox(
#                 tags$p(style = "font-size: 18px;", "Recommended LCR %"), 
#                 tags$p(style = "font-size: 22px;", x), 
#                 icon = icon("percent"),
#                 color = "green", fill = TRUE
#               )
#             })
#             
#           }
#           
#           # update UI object to reflect updated cart
#           cart_ui_obj <- make_cart_ui(cart_df, new_ids = FALSE)
#           
#           # render plot in UI
#           output$dist_plot <- renderPlotly({cart_plt})
#           
#           # render UI
#           output$cart_ui <- cart_ui_obj$ui
#           
#           # observe event for switching back and forth between portfolio and product view
#           observeEvent(input$get_total_plot, {
#             output$dist_plot <- renderPlotly({cart_plt})
#           }, ignoreInit=TRUE)
#         }
#       }, ignoreInit=TRUE)
#     })
#     # Create listeners for each get discount button
#     lapply(cart_ui_obj$ids, function(id) {
#       id2 <- paste0(id, "_gd")
#       
#       # do the following for each button...
#       observeEvent(input[[id2]], {
#         
#         # make copy of cart_df to extract price for order level stats calculation
#         cart_df_tmp <- copy(cart_df)
#         
#         cart_df_tmp[, 'Price'] <-as.numeric(gsub("[\\$,]", "", cart_df_tmp$Price))
#         
#         # get order level stats to use as memoisation keys that feed into predict discount function, can't just use new_port
#         #  otherwise memoisation will not work correctly, all keys must be UNIQUE
#         #   Here we call the cart and not new_port because no product is being added to the cart
#         avgPrice <- mean(cart_df_tmp$Price)
#         totalQuantity <- sum(cart_df_tmp$Quantity)
#         
#         # if customer is new...
#         if (dropdown_outputs$cust() == 'New Customer' & !is.null(dropdown_outputs$state()) & 
#             !is.null(dropdown_outputs$county())) {
#           
#           # get fips code based on user inputted state and county
#           fips_code <- get_fips(dropdown_outputs$state(), dropdown_outputs$county(), stateCountyToFips)
#           
#           # predict discounts
#           predict_discount_new_cust_args <- list(unlist(cart_df[button_id == id, 'Product']),
#                                                  unlist(cart_df[button_id == id, 'Quantity']),
#                                                  fips_code, 
#                                                  logTransformDiscount,
#                                                  quantityColumn,
#                                                  priceColumn,
#                                                  productColumn,
#                                                  features,
#                                                  countyFeatures,
#                                                  cust_df,
#                                                  discountModel,
#                                                  pllel,
#                                                  avgPrice,
#                                                  totalQuantity,
#                                                  seedsmanIDColumn,
#                                                  seedsmanID(),
#                                                  fipsCodeColumn)
#           discounts_new_cust <- wrapMemoise(predict_discount_new_cust_raw, predict_discount_new_cust, predict_discount_new_cust_args)
#         } else {  # if customer exists...
#           # get customer id
#           cust_id <- reactive({sub(".* | ", "", dropdown_outputs$cust())}) #extracts id number out of customer name dropdown
#           seedsman_id <- reactive({sub(".* | ", "", dropdown_outputs$seedsman())}) #extracts id number out of customer name dropdown
#           
#           
#           # get predicted discounts for product associated with button_id
#           predict_discount_args <- list(unlist(cart_df[button_id == id, "Product"]),
#                                         unlist(cart_df[button_id == id, "Quantity"]),
#                                         cust_id(),
#                                         cust_df,
#                                         logTransformDiscount,
#                                         quantityColumn,
#                                         priceColumn,
#                                         productColumn,
#                                         custIDColumn,
#                                         cartDropCols,
#                                         features,
#                                         discountModel,
#                                         pllel,
#                                         avgPrice,
#                                         totalQuantity,
#                                         yearColumn,
#                                         seedsmanIDColumn,
#                                         seedsmanID(),
#                                         countyFeatures,
#                                         fipsCodeColumn)
#           discounts <- wrapMemoise(predict_discount_raw, predict_discount, predict_discount_args)
#         }
#         
#         cart_ui_obj <- make_cart_ui(cart_df, new_ids = FALSE)
#         
#         # Output cart ui or message
#         if (nrow(cart_df) == 0) {
#           output$cart_ui <- renderText("Portfolio is empty.")
#         } else {
#           output$cart_ui <- cart_ui_obj$ui
#           # The following code renders the plot of the distribution of predicted
#           #  per-unit discount for each discount listener button, some customers will not have an
#           #  observed last unit discount, thus need to plot accordingly
#           # If new customer...
#           if (dropdown_outputs$cust() == 'New Customer' & !is.null(dropdown_outputs$state()) & 
#               !is.null(dropdown_outputs$county())) {
#             
#             # get correct list of predictions to be plotted (discounts_new_cust)
#             plot_discounts <- discounts_new_cust
#             
#           } else {  # If not new customer...
#             # generate list of predictions to be plotted (discounts)
#             plot_discounts <- discounts
#           }
#           # If customer does have a last unit discount, make correct plot
#           if (cart_df[button_id == id, LastUnitDiscount] != as.character("NA")) {
#             button_plt <- make_plot_button(plot_discounts, cart_df, stdN, id, lastDisc=TRUE)
#             button_plt <- ggplotly(button_plt)
#             output$dist_plot <- renderPlotly({button_plt})
#             
#           } else { # Make plot that doesn't have vertical line, essentially the same code as above
#             button_plt <- make_plot_button(plot_discounts, cart_df, stdN, id, lastDisc=FALSE)
#             button_plt <- ggplotly(button_plt)
#             output$dist_plot <- renderPlotly({button_plt})
#             
#           }
#         }
#       }, ignoreInit=TRUE)
#     })
#     
#     # Reset input
#     updateNumericInput(session, "product_quantity", value = 0)
#     shinyjs::show(id = "show_hide")
#   }, ignoreInit=TRUE)
#   
#   # download handler for grower report ---------------------------------------------------------------
#   output$download_grower_report <- downloadHandler(
#     filename = function() {
#       paste('MARSgrowerReport', CROP, '-', Sys.Date(), '.csv', sep='')
#     },
#     content = function(con) {
#       write.csv(grower_report, con)
#     }
#   )
#   
#   
#   # END CORN ADD TO PORTFOLIO -----------------------------------------------------------------------
#   
# }
