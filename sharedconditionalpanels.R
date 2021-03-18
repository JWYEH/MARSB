conpanel_cropspecificUI <- function(id, product_name_id, product_choices, product_quantity_id, 
                                  add_to_cart_id, clear_cart_id,
                                  populate_cart_id, password_id, enter_button_id){
  ns <- NS(id)
  tagList(
    get_dropdown(ns(product_name_id), "<p><span style='color: #C3CE44'>Product Name</span></p>",
                 ns(product_choices), ""),
    
    # numeric input for quantity -- corn
    numericInput(inputId = ns(product_quantity_id),
                 shiny::HTML("<p><span style='color: #C3CE44'>Quantity</span></p>"),
                 value = 0,
                 min = 0),
    
    # action button for adding product -- corn
    div(style="display:inline-block", actionButton(inputId = ns(add_to_cart_id),
                                                   label = "Add to Portfolio",
                                                   icon = icon("plus-square"))),
    # action button for clearing cart -- corn
    div(style="display:inline-block", actionButton(inputId = ns(clear_cart_id),
                                                   label = "Clear Portfolio",
                                                   icon = icon("minus-square"))),
    br(),
    br(),
    # action button for pre-populating portfolio -- corn
    div(style="display:inline-block", actionButton(inputId = ns(populate_cart_id),
                                                   label = 'Pre-Populate Portfolio',
                                                   icon = icon("shopping-basket"))),
    passwordInput(ns(password_id), "Enter password to see KPI Metrics"),
    actionButton(ns(enter_button_id), "Enter")
    
  )
}
