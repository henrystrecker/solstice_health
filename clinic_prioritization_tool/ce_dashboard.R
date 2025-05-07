library(shiny)
library(shinydashboard)
library(leaflet)
library(DT)
library(here)
library(fresh)
library(tidyverse)
library(scales)

# Load the dataset
dash_data <- read.csv(here('finalized_dashboard_data.csv'))



ui <- dashboardPage(
  
  dashboardHeader(title = 'Collective Energy Dashboard'),
  dashboardSidebar(
    width = 250,
    tabsetPanel(
      
      tabPanel('Welcome',
               div(
                 style = 'margin-top: 10px; margin-left: 10px;',
                 div(HTML('<span style="color: #D4A017; font-size: 18px; font-weight: bold;">Welcome to the Collective Energy dashboard!</span>')),
                 br(),
                 div(style = 'color: white; font-size: 14px;', 
                     HTML("Use the sidebar on the left-hand side of the screen to customize your experience in this tool.")
                 ),
                 br(),
                 div(style = 'color: white; font-size: 14px;', 
                     HTML("The <b>Filters</b> tab will help to narrow down the pool of FQHCs by selecting only those that match the selected criteria. The dashboard displays the results automatically.")
                 ),
                 br(),
                 div(style = 'color: white; font-size: 14px;', 
                     HTML("The <b>Weights</b> tab allows you to adjust the factors that influence the selection criteria. The Aggregate Weights allows the user to input prefered weights for the overarching categories. The financial and non-monetary categories have different components that can be 
                          weighted to the user's preference. After entering the desired weights, press the <b>Apply Prioritization</b> button to update the results.")
                 )
               )
               ),
      
      tabPanel("About the Data", 
               div(
                 style = "padding: 10px;",
                 HTML("<h2 style='color: #D4A017;'>About the Data</h2>"),
                 HTML("<p style='color: white;'>The dataset used in this dashboard contains information on Federally Qualified Health Centers (FQHCs) across the United States. Each row represents an individual health center with data on geographic location, community characteristics, and prioritization factors.</p>"),
                 HTML("<p style='color: white;'>The data includes information such as site state, whether a site is located in a rural or energy community, whether it is in a Medically Underserved Area (MUA), and other relevant characteristics that influence prioritization for energy interventions.</p>"),
                 HTML("<p style='color: white;'>The dashboard allows users to filter FQHCs based on these characteristics and apply different weighting criteria to prioritize the sites for energy-related interventions.</p>")
                  )
              ),
      
      tabPanel("Filters",
        br(),
        
        # State selection dropdown
        selectInput(inputId = "state_selection", 
                    label = "Select State(s)", 
                    choices = sort(unique(dash_data$Site_State_Abbreviation)), 
                    selected = NULL,
                    multiple = TRUE),
        
        # Individual checkboxes with info icons
        checkboxInput(inputId = "filter_rural", 
                      label = tagList("Rural", 
                                      tags$span(icon("info-circle"),
                                                title = "Sites designated as rural based on DOE")), 
                      value = FALSE),
        
        checkboxInput(inputId = "filter_energy_community", 
                      label = tagList("Energy Community", 
                                      tags$span(icon("info-circle"), 
                                                title = "Sites in designated energy communities")),
                      value = FALSE),
        
        checkboxInput(inputId = "filter_mua", 
                      label = tagList("Medically Underserved Area",
                                      tags$span(icon("info-circle"), 
                                                title = "MUA or medically underserved populations")), 
                      value = FALSE),
        
        checkboxInput(inputId = "filter_excluded", 
                      label = tagList("Excluded Sites",
                                      tags$span(icon("info-circle"), 
                                                title = "Sites excluded from analysis")),
                      value = FALSE)
        ),
      
      tabPanel('Weights',
               h3('Aggregate Weights', 
                  style = 'text-align: center; background-color: #D4A017; color: black; border-radius: 5px; height: 40px; margin-bottom: 10px; padding-top: 10px; font-size: 16px;'),
               
               tags$label("Financial Aggregate Weight", style = "font-weight: bold;"),
               tags$div("Weight of the overall financial factors (aggregate weight of solar and BESS net present value).", style = "font-style: italic; font-size: 12px; margin-bottom: 5px;"),
               numericInput('total_financial_weight', label = NULL, value = 1, min = 0),
               
               tags$label("Nonmonetary Aggregate Weight", style = "font-weight: bold;"),
               tags$div("Weight of the overall non-financial factors (aggregate weight of the environmental and social equity variables).", style = "font-style: italic; font-size: 12px; margin-bottom: 5px;"),
               numericInput('total_nonmonetary_weight', label = NULL, value = 1, min = 0),
               
               h3('Financial Weights', 
                  style = 'text-align: center; background-color: #D4A017; color: black; border-radius: 5px; height: 40px; margin-bottom: 10px; padding-top: 10px; margin-top: 30px; font-size: 16px;'),
               
               tags$label("Solar Financial Weight", style = "font-weight: bold;"),
               tags$div("Solar energy projects Net Present Value. Displayed as Solar NPV in dollar amount.", style = "font-style: italic; font-size: 12px; margin-bottom: 5px;"),
               numericInput('solar_financial_weight', label = NULL, value = 1, min = 0),
               
               tags$label("BESS Financial Weight", style = "font-weight: bold;"),
               tags$div("Battery energy storage systems Net Present Value. Displayed as BESS NPV in dollar amount.", style = "font-style: italic; font-size: 12px; margin-bottom: 5px;"),
               numericInput('battery_financial_weight', label = NULL, value = 1, min = 0),
               
               h3('Nonmonetary Weights', 
                  style = 'text-align: center; background-color: #D4A017; color: black; border-radius: 5px; height: 40px; margin-bottom: 10px; padding-top: 10px; margin-top: 30px; font-size: 16px;'),
               
               tags$label("Natural Disaster Risk Weight", style = "font-weight: bold;"),
               tags$div("Natural disaster risk index score at the county level as designated by FEMA. Displayed as FEMA Risk.", style = "font-style: italic; font-size: 12px; margin-bottom: 5px;"),
               numericInput('fema_weight', label = NULL, value = 1, min = 0),
               
               tags$label("Power Outage Risk Weight", style = "font-weight: bold;"),
               tags$div("Risk of power outage based on total minutes of outage annually. Displayed as SAIDI.", style = "font-style: italic; font-size: 12px; margin-bottom: 5px;"),
               numericInput('saidi_weight', label = NULL, value = 1, min = 0),
               
               tags$label("Medically Underserved Weight", style = "font-weight: bold;"),
               tags$div("Weight the importance of clinics located in medically underserved areas. Displayed as MUA.", style = "font-style: italic; font-size: 12px; margin-bottom: 5px;"),
               numericInput('mua_weight', label = NULL, value = 1, min = 0),
               
               tags$label("Healthcare Disparities Weight", style = "font-weight: bold;"),
               tags$div("Health inequity metric based on environmental and social vulnerability. Displayed as MDI.", style = "font-style: italic; font-size: 12px; margin-bottom: 5px;"),
               numericInput('mdi_weight', label = NULL, value = 1, min = 0),
               
               tags$label("Rural Status Weight", style = "font-weight: bold;"),
               tags$div("Weight the importance of clinics located in rural sites. Displayed as Rural.", style = "font-style: italic; font-size: 12px; margin-bottom: 5px;"),
               numericInput('rural_weight', label = NULL, value = 1, min = 0),
               
               br(),
               actionButton('apply_prioritization', 'Apply Prioritization', icon = icon('check'), class = 'btn-warning'),
               br()
      )
    ),
    br()


  ),
  dashboardBody(
    
    tags$head(tags$style(HTML(paste(
      'body, .content-wrapper { background-color: black !important; color: white !important; }',
      '.main-sidebar { background-color: black !important; }',
      '.sidebar-menu>li>a { color: #D4A017 !important; }',
      '.nav-tabs > li {width: 50% !important; text-align: center;  /* Side panel tab formatting */}',
      '.main-header { background-color: black !important; border-bottom: 3px solid #D4A017 !important; }',
      '.box { border-top: 3px solid #D4A017 !important; background-color: #f5f5dc !important; color: black !important; }',
      sep = ' '
    )))),
    
    fluidRow(
      box(title = 'FQHC Locations', width = 12, leafletOutput('map'), height = 470)
    ),
    fluidRow(
      box(title = 'FQHC Prioritization Data', width = 12, DTOutput('table'), height = 1090)
    )
  ),
  
  tags$head(tags$style(HTML("
    .main-header .navbar {
      background-color: #000000 !important; /* Change background */
    }
    .main-header .logo {
      background-color: #EEB111 !important; /* Change sidebar header */
    }
  ")))
  
)

server <- function(input, output, session) {
  # Set up the map and data to be reactive with the filters on the left
  filtered_data <- reactive({
    filtered_data <- dash_data
    
    if (!is.null(input$state_selection) && length(input$state_selection) > 0) {
      filtered_data <- filtered_data[filtered_data$Site_State_Abbreviation %in% input$state_selection, ]
    }
    
    if (isTRUE(input$filter_rural)) {
      filtered_data <- filtered_data[filtered_data$community_rural_doe == TRUE, ]
    }
    
    if (isTRUE(input$filter_energy_community)) {
      filtered_data <- filtered_data[filtered_data$energy_community == TRUE, ]
    }
    
    if (isTRUE(input$filter_mua)) {
      filtered_data <- filtered_data[filtered_data$mua == TRUE, ]
    }
    
    if (isTRUE(input$filter_excluded)) {
      filtered_data <- filtered_data[filtered_data$exclude_site == FALSE, ]
    }
    
    return(filtered_data)
  })
  
  # Create reactive value for display table data and export data table
  display_data <- reactiveVal(dash_data)
  export_data <- reactiveVal(dash_data)
  
  # Update when filters change
  observe({
    if(is.null(input$apply_prioritization) || input$apply_prioritization == 0) {
      display_data(filtered_data())
      export_data(filtered_data())
    }
  })
  
  output$map <- renderLeaflet({
    data <- filtered_data()
    if (nrow(data) == 0) {
      leaflet() %>%
        addTiles() %>%
        setView(lng = -98.35, lat = 39.50, zoom = 4)  # Default view of US
    } else {
      leaflet(data) %>%
        addTiles() %>%
        addCircleMarkers(~site_lon, ~site_lat, label = ~Site_Name, color = '#D4A017', radius = 2) %>%
        setView(lng = -98.35, lat = 38.5, zoom = 4)
    }
  })
  
  # Calculate prioritization when the button is pressed
  observeEvent(input$apply_prioritization, {
    
    # Check Financial Weights aren't all zero
    financial_sum <- input$solar_financial_weight + input$battery_financial_weight
    if (financial_sum == 0) {
      showModal(modalDialog(title = tags$div('Input Error', style = 'color: black;'),
                            tags$div('All financial weights cannot be zero.', style = 'color: black;'),
                            easyClose = TRUE))
      return()
    }
    
    # Check Nonmonetary Weights aren't all zero
    nonmonetary_sum <- input$fema_weight + input$saidi_weight + input$mua_weight + input$mdi_weight + input$rural_weight
    if (nonmonetary_sum == 0) {
      showModal(modalDialog(title = tags$div('Input Error', style = 'color: black;'),
                            tags$div('All nonmonetary cannot be zero.', style = 'color: black;'),
                            easyClose = TRUE))
      return()
    }
    
    # Check Aggregate Weights aren't all zero
    aggregate_sum <- input$total_financial_weight + input$total_nonmonetary_weight
    if (aggregate_sum == 0) {
      showModal(modalDialog(title = tags$div('Input Error', style = 'color: black;'),
                            tags$div('All aggregate weights cannot be zero.', style = 'color: black;'),
                            easyClose = TRUE))
      return()
    }
    
    
    # Initialize data with filters
    result <- filtered_data()
    
    # Calculate the financial weighted score
    solar_weight <- input$solar_financial_weight / (input$solar_financial_weight + input$battery_financial_weight)
    battery_weight <- input$battery_financial_weight / (input$solar_financial_weight + input$battery_financial_weight)
    
    result$financial_weight_score <- (result$net_present_value_solar * solar_weight) + 
      (result$net_present_value_battery * battery_weight)
    
    # Standardize the non monetary weights
    weight_total <- input$mdi_weight + input$fema_weight + input$saidi_weight + input$rural_weight + input$mua_weight
    mdi_weight <- input$mdi_weight / weight_total
    fema_weight <- input$fema_weight / weight_total
    saidi_weight <- input$saidi_weight / weight_total
    rural_weight <- input$rural_weight / weight_total
    mua_weight <- input$mua_weight / weight_total
    
    # Calculate the nonmonetary weighted score
    result$nonmonetary_weight_score <- (result$zscore_mdi_percentile * mdi_weight) + 
      (result$zscore_fema_nri * fema_weight) + (result$zscore_saidi * saidi_weight) +
      (result$zscore_rural * rural_weight) + (result$zscore_mua * mua_weight)
    
    # Convert the resulting values into a new set of zscores
    result$zscore_financial <- as.numeric(scale(result$financial_weight_score))
    result$zscore_nonmonetary <- as.numeric(scale(result$nonmonetary_weight_score))
    
    # Standardize the final weights
    financial_weight <- input$total_financial_weight / (input$total_financial_weight + input$total_nonmonetary_weight)
    nonmonetary_weight <- input$total_nonmonetary_weight / (input$total_financial_weight + input$total_nonmonetary_weight)
    
    # Calculate final scores for all sites
    result$site_score <- (result$zscore_financial * financial_weight) + (result$zscore_nonmonetary * nonmonetary_weight)
    
    export_result <- result %>% arrange(desc(site_score))
    
    # Update the export data
    export_data(export_result)
    
    display_result <- result %>% 
      arrange(desc(site_score)) %>%
      mutate(rank = row_number(),
             site_score = round(site_score, digits = 2),
             net_present_value_solar = dollar(net_present_value_solar),
             net_present_value_battery = dollar(net_present_value_battery),
             saidi = round(saidi, digits = 2),
             FEMA_NRI_Risk_Score = round(FEMA_NRI_Risk_Score, digits = 2)) %>%
      select(rank, site_score, Site_Name, Site_Address, Site_City, Site_State_Abbreviation, ObjectId, net_present_value_solar, net_present_value_battery,
             saidi, FEMA_NRI_Risk_Score, mdi_percentile, energy_community, mua, community_rural_doe) %>%
      setNames(c('Rank', 'Site Score', 'Site Name', 'Address', 'City', 'State', 'ObjectId', 'Solar NPV ($)', 'Battery NPV ($)',
                 'SAIDI', 'FEMA Risk', 'MDI', 'Energy Community', 'MUA', 'Rural'))
    
    display_data(display_result)
    
  })
  
  # Single table output that uses display_data
  output$table <- renderDT({
    datatable(
      display_data(),
      options = list(scrollX = TRUE, scrollY = 900, paging = TRUE, pageLength = 50, 
                     fixedHeader = FALSE, dom = 'frtipB', buttons = c('csv', 'excel')),
      extensions = 'Buttons',
      class = 'cell-border display',
      rownames = FALSE)
  })
}

shinyApp(ui, server)