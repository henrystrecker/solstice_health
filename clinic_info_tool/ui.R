header <- dashboardHeader(
  
  # title ----
  title = span("Collective Energy Solar Assessment", 
               style = "font-size: 18px; font-weight: bold;"
               
               
  )) # END dashboardHeader



# dashboard sidebar (disabled) ----
sidebar <- dashboardSidebar(disable = TRUE) # END dashboardSidebar. [sidebar is disabled]



# dashboard body ----
body <- dashboardBody(
  
  # set theme ----
  fresh::use_theme("ce-theme.css"),
  
  
  
  # body set up with fluidRow ----
  fluidRow(
    
    
    # main content row ----
    fluidRow(
      
      # lefthand column: Clinic Information widget ----
      column(width = 4,
             box(width = NULL,
                 title = tagList(icon("circle-info"),
                                 strong("Clinic Information")),
                 uiOutput("selected_clinic_info"),
                 uiOutput("building_sqft_ui"),
                 uiOutput('monthly_energy_use_kwh'),
                 uiOutput('peak_power'),
                 fluidRow(
                   column(width = 9, uiOutput('utility_rate_slider')),
                   column(width = 3, uiOutput('utility_rate_numeric'))
                 ),
                 
                 uiOutput('vaccine_value'),
                 uiOutput('patient_rate_per_hour')
             ) # END widget Clinic Information
      ), # END lefthand column
      
      
      # righthand column ----
      column(width = 8,
             
             
             # upper main widget: Summary Results ----
             box(width = NULL,
                 title = tagList(icon("chart-simple"),
                                 strong("Summary Results")),
                 
                 valueBoxOutput("annual_savings"),
                 valueBoxOutput("solar_rebate"),
                 valueBoxOutput("solar_payback"),
                 valueBoxOutput("battery_payback"),
                 valueBoxOutput("social_vulnerability_summary"),
                 valueBoxOutput("disaster_risk")
                 
             ), # END widget Summary Results
             
             
             
             # lower main widget: Assessment Details (Solar/Battery/Risk/Social tabs) ----
             box(width = NULL,
                 title = tagList(icon("staff-snake"), strong("Assessment Details")),
                 
                 
                 # tabsetPanel for Assessment Details ----
                 tabsetPanel(
                   tabPanel("Solar", 
                            htmlOutput("annual_solar_savings"),
                            htmlOutput("solar_install_cost"),
                            htmlOutput("lifetime_solar_savings"),
                            htmlOutput("lifetime_solar_maintenance_costs"),
                            htmlOutput("solar_financial_summary")
                   ), # END tab Solar
                   
                   
                   tabPanel("Battery", 
                            tags$br(),
                            switchInput("battery_switch", "Battery: Critical Systems Only", 
                                        onLabel = "Yes", offLabel = "No", labelWidth = '180px'),
                            htmlOutput("battery_size"),
                            htmlOutput("battery_install_cost"),
                            htmlOutput("battery_savings_pv"),
                            htmlOutput("battery_maintenance_pv"),
                            htmlOutput("battery_financial_summary")
                   ), # END tab Battery
                   
                   
                   #Risk tab ----
                   tabPanel("Risk",
                            
                            sidebarLayout(
                              mainPanel(
                                tabsetPanel(
                                  tabPanel("Frequency/Duration of Outages",
                                           br(),
                                           textOutput("outages_over_4hrs_msg"),
                                           br(),
                                           plotOutput("histogram") |>
                                             withSpinner(color = "#EEB111", type = 1, size = 3),
                                  ), # END tabPanel - Frequency/Duration of Outages
                                  tabPanel("Total Outages",
                                           br(),
                                           textOutput("total_outages_msg"),
                                           br(),
                                           plotOutput("outage_plot") |>
                                             withSpinner(color = "#EEB111", type = 1, size = 3))
                                )# END tabsetPanel
                              ), #END Main Panel
                              
                              sidebarPanel(
                                # Natural disaster risk section
                                box(
                                  title = "Natural Disaster Risk", status = "warning", solidHeader = TRUE, width = NULL,
                                  textOutput("disaster_risk_msg")
                                ),
                                
                                box(
                                  title = "Outage Details",
                                  status = "info", solidHeader = TRUE, width = NULL,
                                  
                                  textOutput("saidi_msg"),
                                  textOutput("saifi_msg"),
                                  textOutput("caidi_msg")           # ← NEW line for CAIDI
                                )
                                
                              ) #END Sidebar panel
                            ) #END Sidebar LAYOUT
                            
                   ), # END tab Risk
                   
                   
                   # Social tab ----
                   tabPanel("Social",
                            
                            tabsetPanel(
                              tabPanel("Community Vulnerability",
                                       fluidRow(
                                         column(
                                           width = 4,
                                           box(width=NULL, background=NULL,  # or background="transparent"
                                               valueBoxOutput("social_vulnerability", width=12)
                                           )
                                         ),
                                         column(
                                           width = 8,
                                           box(width=NULL,
                                               htmlOutput("baseline_health"),
                                               htmlOutput("baseline_environment"),
                                               htmlOutput("climate_change_health"),
                                               htmlOutput("mdi_percentile")
                                           )
                                         )
                                       )
                              ), # END tabPanel - Community Vulnerability
                              
                              tabPanel("Community Composition",
                                       fluidRow(
                                         column(width = 3, tableOutput("community_demo")),
                                         column(width = 3, tableOutput("poverty_table")),
                                         column(width = 3, tableOutput("insurance_table")),
                                         column(width = 3, tableOutput("sex_table"))
                                       ) # END fluidRow
                              ) # END tabPanel
                              
                            )# END tabsetPanel
                            
                   ) # END tab Social
                   
                   
                 ) # END tabsetPanel Assessment Details
             ) # END box Assessment Details
             
      ) # END righthand column
    ), # END main content fluidRow
    
    # change
    
    # bottom widget: Guide ----
    column(width = 12,
           box(width = NULL,
               title = tagList(icon("book"),
                               strong("Dashboard Guide and FAQ")),
               
               # sidebarLayout for Guide ----
               sidebarLayout(
                 sidebarPanel(
                   selectInput("faq_category", "Select a Category:",
                               choices = names(faq_data),
                               selected = "Solar Potential"),
                   helpText("Select a category to view the corresponding FAQ details.") # Removed actionButton
                 ),
                 
                 
                 # mainPanel for Guide ----
                 mainPanel(
                   tabsetPanel(
                     tabPanel("FAQ",
                              uiOutput("faq_output")
                     ),
                     tabPanel("Additional Information",
                              uiOutput("additional_info")
                     )
                   )
                 )
               )
               
           ) # END box Guide
    ), # END bottom widget: Guide
    
    
  ) # END body set up with fluidRow
) # END dashboardBody



# combine all into dashboardPage ----
dashboardPage(header, sidebar, body)