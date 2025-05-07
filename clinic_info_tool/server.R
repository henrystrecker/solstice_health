server <- function(input, output, session) {
  # Welcome page pop-up ----
  showModal(modalDialog(
    title = "Welcome to the Solar & Battery Installation Assessment",
    tags$div(
      
      style = "text-align: center;",
      tags$img(src = "solarimage.png",
               alt = "Solar Installation Workers",
               style = "display: block; margin: auto; width: 100%; height: auto; max-width: 5000px; max-height: 700px; object-fit:contain;")
    ),
    tags$p(tags$b("This tool will help you assess solar & battery installation suitability.")),
    tags$p(HTML("Welcome to the Solar & Battery Installation Assessment tool. This dashboard is designed to help clinics evaluate their potential for adopting solar energy and battery energy storage systems (BESS) based on location, roof conditions, and energy requirements.
<br><br>
Our goal is to assist clinics in making informed decisions about transitioning to clean and cost-effective energy solutions that benefit both their operations and the environment.
           <br><br>
           After using this tool, please contact Collective Energy at dettelson@collectiveenergyco.com for a full screening assessment that will provide you with more detailed opportunities for going solar.")),
    easyClose = FALSE,
    footer = actionButton("close_welcome", "Close"),
    size = "xl", class = "modal-xl"
  )) 
  
  # Close welcome modal and show the new "Clinic Location" modal ----
  observeEvent(input$close_welcome, {
    removeModal()
    
    showModal(modalDialog(
      title = "Clinic Location",
      # fresh::use_theme("ce-theme.css"),  # Add custom theme
      
      # Add fullscreen styling
      tags$style(HTML("
      .modal-dialog { max-width: 100% !important; width: 100% !important; height: 100% !important; margin: 0; padding: 0; }
      .modal-content { height: 100vh !important; border-radius: 0; }
      .modal-body { height: calc(100vh - 60px); overflow: auto; }
    ")),
      
      size = "xl",  
      easyClose = FALSE,
      
      # Full-width layout
      fluidRow(
        column(4,
               textInput("clinic_search", "Enter Clinic Address (street address, city, state, zip code):", ""),
               actionButton("confirm_building_location", "Zoom to building Location"),
               br(), br(),
               uiOutput("selected_clinic_text"),  # Display selection info
               br(), br(),
               HTML("To provide an accurate assessment of your clinic's energy needs, we need to ensure that the pin is placed on the rooftop of your building. Then we can provide you more information about your solar potential, battery potential, financial assessment, and local outage risk. <br><br><br>"),
               div(style = "display: flex; justify-content: space-between;",
                   actionButton("address_correct", "Pin is on rooftop", 
                                style = "background-color: #829326; color: white; border: none;"),
                   actionButton("address_incorrect", "Pin is not on rooftop", 
                                style = "background-color: #C0392B; color: white; border: none;")
               )
        ), 
        column(8, 
               leafletOutput("clinic_map", height = "90vh")
        ) 
      )
    )) 
  })
  
  
  # Render initial Leaflet map ----
  output$clinic_map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("Esri.WorldImagery") %>%  
      setView(lng = -98.5795, lat = 39.8283, zoom = 4)  
    
  })
  
  # Define a custom yellow teardrop marker icon
  yellow_icon <- icons(
    iconUrl = "www/yellow_pin.svg",
    iconWidth = 25, iconHeight = 41,
    iconAnchorX = 12, iconAnchorY = 41,
    popupAnchorX = 1, popupAnchorY = -34
  )
  
  # Dynamically update markers based on map bounds ----
  observe({
    bounds <- input$clinic_map_bounds  
    req(bounds)  
    
    filtered_data <- clinic_data %>%
      filter(site_lon >= bounds$west & site_lon <= bounds$east,
             site_lat >= bounds$south & site_lat <= bounds$north)
    
    leafletProxy("clinic_map", data = filtered_data) %>%
      clearMarkers() %>%
      addMarkers(
        ~site_lon, ~site_lat,
        icon = yellow_icon,  
        clusterOptions = markerClusterOptions(
          iconCreateFunction = JS(
            "function(cluster) {
             var count = cluster.getChildCount();
             return new L.DivIcon({
               html: '<div style=\"background-color:#EEB111;width:40px;height:40px;border-radius:50%;display:flex;align-items:center;justify-content:center;color:#ffffff;font-size:14px;font-weight:bold;\">' + count + '</div>',
               className: 'marker-cluster',
               iconSize: [40, 40]
             });
           }"
          )
        ),
        popup = ~paste0("<b>", Site_Name, "</b><br>", complete_address),
        label = ~paste0(Site_Name, " - ", complete_address),
        labelOptions = labelOptions(
          style = list("font-size" = "12px", "font-weight" = "bold"),
          direction = "auto", textOnly = FALSE
        )
      )
  })
  
  
  # Reactive value to store selected clinic info (entire row) ----
  selected_clinic_info <- reactiveVal(NULL)
  
  # Update `selected_clinic_info()` when clicking a pin ----
  observeEvent(input$clinic_map_marker_click, {
    req(input$clinic_map_marker_click)  
    
    clicked_lat <- input$clinic_map_marker_click$lat
    clicked_lon <- input$clinic_map_marker_click$lng
    
    selected_clinic <- clinic_data %>%
      filter(site_lat == clicked_lat & site_lon == clicked_lon)
    
    if (nrow(selected_clinic) == 1) {
      updateTextInput(session, "clinic_search", value = selected_clinic$complete_address)
      selected_clinic_info(as.list(selected_clinic))  # Store as list for easy access
    }
  })
  
  # Display selected clinic information ----
  output$selected_clinic_text <- renderUI({
    req(selected_clinic_info())
    
    clinic_row <- selected_clinic_info()
    
    HTML(paste0(
      "<br><br><b>You have selected site number ", clinic_row[["ObjectId"]], 
      " at ", clinic_row[["complete_address"]], ".</b><br><br>",
      "Is the yellow pin on the roof of the building matching this address?" #updated language
    ))
  })
  
  
  
  
  ####################
  #### HENRY CODE ####
  ####################
  
  # Code to generate results based on selection of a clinic from above
  
  this_site <- reactive({
    req(selected_clinic_info())
    clinic_data %>%
      filter(ObjectId %in% selected_clinic_info()$ObjectId) %>% # UPDATE this with reactive siteID
      mutate(annual_energy_consumption_kwh = roof_area_sqm * 10.7639 * 12, # Adjust sqm to sqft, then use 12 kwh/sqft per year for energy consumption
             monthly_energy_consumption_kwh = annual_energy_consumption_kwh / 12,
             peak_power_use_kw = annual_energy_consumption_kwh / 2750, # This is based on the mean of about 35 screening sites
             solar_install_cost = (solarapi_nameplate_capacity * 1000 * 3) * 
               (1- (0.3 + 0.1 * energy_community))) # Assumption of $3/W for installation, with federal incentives factored in
  })
  
  # Building square footage input field with default value
  output$building_sqft_ui <- renderUI({
    req(this_site()$roof_area_sqm)
    # Calculate the default value based on the reactive 'this_site()' expression
    sqft_default <- as.integer(this_site()$roof_area_sqm * 10.7639)  # Convert sqm to sqft
    
    # Create the numericInput with the calculated default value
    numericInput(
      inputId = "building_sqft",
      label = "1. Building square footage",
      value = sqft_default,  # Set the default value dynamically
      min = 0
    )
  })
  
  #Monthly energy use input field with default value
  output$monthly_energy_use_kwh <- renderUI({
    # Calculate the default value based on the reactive 'this_site()' expression
    monthly_energy_use_default <- as.integer(input$building_sqft)  # Use sqft from previous step as proxy
    
    # Create the numericInput with the calculated default value
    numericInput(
      inputId = "monthly_energy_use_kwh",
      label = "2. Monthly energy use (kWh)",
      value = monthly_energy_use_default,  # Set the default value dynamically
      min = 0
    )
  })
  
  #Monthly energy use input field with default value
  output$utility_rate_slider <- renderUI({
    req(this_site()$utility_rate)
    # Calculate the default value based on the reactive 'this_site()' expression
    utility_rate_default <- this_site()$utility_rate  # Use sqft from previous step as proxy
    
    # Create the numericInput with the calculated default value
    sliderInput(
      inputId = 'utility_rate_slider', 
      label = '4. Utility Rate ($ per kWh):', 
      min = 0, 
      max = 0.8, 
      value = utility_rate_default, 
      step = 0.01)
    
  })
  
  #Monthly energy use input field with default value
  output$utility_rate_numeric <- renderUI({
    req(this_site()$utility_rate)
    # Calculate the default value based on the reactive 'this_site()' expression
    utility_rate_default <- this_site()$utility_rate  # Use sqft from previous step as proxy
    
    # Create the numericInput with the calculated default value
    numericInput(
      inputId = 'utility_rate_numeric',
      label = '',
      min = 0, 
      max = 0.8, 
      value = utility_rate_default,
      step = 0.01
    )
    
  })
  
  observe({
    updateNumericInput(session, "utility_rate_numeric", value = input$utility_rate_slider)
  })
  
  observe({
    updateSliderInput(session, "utility_rate_slider", value = input$utility_rate_numeric)
  })
  
  
  # Peak power input field with default value
  output$peak_power <- renderUI({
    # Calculate the default value based on the reactive 'this_site()' expression
    peak_power_default <- as.integer(input$monthly_energy_use_kwh * 12 / 2750)  # Use energy use from previous step as proxy
    
    # Create the numericInput with the calculated default value
    numericInput(
      inputId = "peak_power",
      label = "3. Peak Power Use (kW)",
      value = peak_power_default,  # Set the default value dynamically
      min = 0
    )
  })
  
  # Vaccine storage value input field with default value
  output$vaccine_value <- renderUI({
    # Calculate the default value based on the reactive 'this_site()' expression
    vaccine_value_default <- as.integer(input$building_sqft * 20)  # REPLACE ME WITH REASONABLE ESTIMATE (assumed $20 per sqft)
    
    # Create the numericInput with the calculated default value
    numericInput(
      inputId = "vaccine_value",
      label = "5. Dollar Value of Refrigerated Vaccines",
      value = vaccine_value_default,  # Set the default value dynamically
      min = 0
    )
  })
  
  # Patients seen per hour input field with default value
  output$patient_rate_per_hour <- renderUI({
    # Calculate the default value based on the reactive 'this_site()' expression
    patient_rate_default <- as.integer(input$building_sqft * 3 / 1000)  # REPLACE ME WITH REASONABLE ESTIMATE (assumed 3 patients per 1,000 sqft each hour)
    
    # Create the numericInput with the calculated default value
    numericInput(
      inputId = "patient_rate_per_hour",
      label = "6. Number of patients seen per hour", 
      value = patient_rate_default,  # Set the default value dynamically
      min = 0
    )
  })
  
  ###### Solar Tab Calculations #######
  
  annual_solar_savings <- reactive({
    req(this_site()$solarapi_annual_solar_generation_kwh)
    round(this_site()$solarapi_annual_solar_generation_kwh * input$utility_rate_slider, 0)})
  
  # UPDATE THE COST OF SOLAR INSTALL PER KW BELOW
  # Assumption of $3 per W capacity was made, we may need to factor in the fee or % that CE charges sites too
  solar_system_cost <- reactive({
    req(this_site()$solarapi_nameplate_capacity)
    this_site()$solarapi_nameplate_capacity * 1000 * 3})
  
  # Factor in the fee that CE charges to clients as well as the 30-40 percent reduction from federal incentives
  solar_installation_cost <- reactive({
    req(solar_system_cost())
    req(this_site()$energy_community)
    round((solar_system_cost() * (1 - (0.3 + 0.1 * this_site()$energy_community))) + (solar_system_cost() * 0.12), 0)})
  
  
  solar_savings_pv <- function(x) annual_solar_savings() / (1.05 ** x)
  lifetime_solar_savings <- reactive({
    req(annual_solar_savings())
    round(sum(sapply(1:25, function(x) solar_savings_pv(x))), 0)
  })
  
  # Function for adding up lifetime maintenance costs
  # UPDATE HERE WITH MAINTENANCE COST - currently assuming $50 per kW
  solar_maintenance_costs_pv <- function(x) (this_site()$solarapi_nameplate_capacity * 50) / (1.05 ** x)
  lifetime_solar_maintenance_costs <- reactive({
    req(this_site()$solarapi_nameplate_capacity)
    round(sum(sapply(1:25, function(x) solar_maintenance_costs_pv(x))), 0)
  })
  
  net_present_value_solar <- reactive({
    req(lifetime_solar_savings())
    req(solar_installation_cost())
    req(lifetime_solar_maintenance_costs())
    round(lifetime_solar_savings() - solar_installation_cost() - lifetime_solar_maintenance_costs(), 0)})
  
  output$annual_solar_savings <- renderUI({
    req(annual_solar_savings())
    HTML(paste0("<br> With solar, you will save <span style='font-size: 1.25em;'><b>$", 
                format(annual_solar_savings(), big.mark = ","), # Uses reactive slider to adjust results
                '</b></span> per year on your utility bill <br><br>'))
  })
  
  # Solar install cost calculation and display
  output$solar_install_cost <- renderUI({
    req(this_site()$solarapi_nameplate_capacity)
    req(solar_installation_cost())
    HTML(paste0("Installation of a standard-size solar array for your building <span style='font-size: 1.25em;'><b>(", this_site()$solarapi_nameplate_capacity, " kW) would cost $", 
                format(solar_installation_cost(), big.mark = ","),"</b></span>.<br><br>"))
  })
  
  output$lifetime_solar_savings <- renderUI({
    req(lifetime_solar_savings())
    HTML(paste0("The present value of lifetime solar savings is estimated to be <span style='font-size: 1.25em;'><b>$", 
                format(lifetime_solar_savings(), big.mark = ","), 
                '</b></span>.<br><br>'))
  })
  
  output$lifetime_solar_maintenance_costs <- renderUI({
    req(lifetime_solar_maintenance_costs())
    HTML(paste0("The present value of lifetime solar costs for maintenance is estimated to be <span style='font-size: 1.25em;'><b>$", 
                format(lifetime_solar_maintenance_costs(), big.mark = ","),
                '</b></span>.<br><br>'))
  })
  
  output$solar_financial_summary <- renderUI({
    req(net_present_value_solar())
    HTML(paste0("The net present value of a solar array on a 25 year time horizon is <span style='font-size: 1.25em;'><b>$", 
                format(net_present_value_solar(), big.mark = ","),
                '</b></span>.<br><br>'))
  })
  
  ###### Battery Tab Calculations #######
  
  critical_systems_factor <- reactive({if (input$battery_switch == TRUE){1/3} else{1}})
  
  battery_size_kwh <- reactive({
    req(critical_systems_factor())
    input$peak_power * 8 * critical_systems_factor()})
  
  # Calculate battery cost using the provided costs from CE
  battery_system_cost <- reactive({
    req(battery_size_kwh())
    battery_size_kwh() * 1100 + input$peak_power * 3220})
  
  # Include the added CE fee as well as the 30-40% tax incentive we've seen
  battery_install_cost <- reactive({
    req(battery_system_cost())
    req(this_site()$energy_community)
    req(battery_system_cost())
    round(battery_system_cost() * (1 - 0.3 - 0.1 * this_site()$energy_community) + battery_system_cost() * 0.12, 0)})
  
  county_outages_2023 <- reactive({
    req(this_site())
    all_outages_2023 %>% 
      filter(fips_code == this_site()$fips_code,
             total_duration_minutes >= 240)})
  
  county_customer_count <- reactive({
    req(this_site())
    outages_by_county_master %>%
      filter(fips_code == this_site()$fips_code)})
  
  # SHOULD THIS BE ROUNDED FOR ALL SITES OR USED AS A FRACTION OVER 15 YEARS
  observed_long_outages <- reactive({
    req(county_outages_2023()$total_customers_impacted)
    req(county_customer_count()$total_customers)
    sum(county_outages_2023()$total_customers_impacted) / mean(county_customer_count()$total_customers)})
  
  county_saidi <- reactive({
    req(this_site())
    outages_by_county_master %>%
      filter(fips_code == this_site()$fips_code) %>%
      slice_max(year, n = 1)})
  
  # Calculate expected savings from seeing patients, with a value of $311 per visit
  annual_patient_savings <- reactive({
    req(county_saidi()$saidi)
    req(critical_systems_factor())
    input$patient_rate_per_hour * county_saidi()$saidi / 60 * 311 * critical_systems_factor()})
  
  battery_savings_pv <- function(x) (annual_patient_savings() + observed_long_outages() * input$vaccine_value) / (1.05 ** x)
  lifetime_battery_savings <- reactive({
    req(annual_patient_savings())
    req(observed_long_outages())
    round(sum(sapply(1:15, function(x) battery_savings_pv(x))), 0)
  })
  
  # Battery maintenance cost is set at $10/kWh annually
  battery_maintenance_pv <- function(x) (battery_size_kwh() * 10) / (1.05 ** x)
  lifetime_battery_maintenance <- reactive({
    req(battery_size_kwh())
    round(sum(sapply(1:15, function(x) battery_maintenance_pv(x))), 0)
  })
  
  net_present_value_battery <- reactive({
    req(lifetime_battery_savings())
    req(battery_install_cost())
    req(lifetime_battery_maintenance())
    round(lifetime_battery_savings() - battery_install_cost() - lifetime_battery_maintenance(), 0)})
  
  output$battery_size <- renderUI({
    req(battery_size_kwh())
    HTML(paste0("Based on your building's energy consumption, a battery storage system with a size of <span style='font-size: 1.25em;'><b>", 
                format(round(battery_size_kwh(), 2), big.mark = ","), "kWh</b></span> is recommended. <br><br>"))
  })
  
  output$battery_install_cost <- renderUI({
    req(battery_install_cost())
    HTML(paste0("Installation of a battery at your facility would cost: <span style='font-size: 1.25em;'><b>$", 
                format(round(battery_install_cost(), 0), big.mark = ","), "</b></span>.<br><br>"))
  })
  
  output$battery_savings_pv <- renderUI({
    req(lifetime_battery_savings())
    HTML(paste0("The present value of lifetime battery storage savings is estimated to be <span style='font-size: 1.25em;'><b>$", 
                format(round(lifetime_battery_savings(), 2), big.mark = ","), "</b></span>.<br><br>"))
  })
  
  output$battery_maintenance_pv <- renderUI({
    req(lifetime_battery_maintenance())
    HTML(paste0("The present value of lifetime battery storage maintenance is estimated to be <span style='font-size: 1.25em;'><b>$", 
                format(round(lifetime_battery_maintenance(), 2), big.mark = ","), "</b></span>.<br><br>"))
  })
  
  output$battery_financial_summary <- renderUI({
    req(net_present_value_battery())
    HTML(paste0("The net present value of battery storage on a 15 year time horizon is <span style='font-size: 1.25em;'><b>$", 
                format(round(net_present_value_battery(), 2), big.mark = ","),
                '</b></span>.<br><br>'))
  })
  
  ########################
  #### END HENRY CODE ####
  ########################
  
  
  
  ####################
  ### REEVES CODE ###
  ####################
  #--------- Social Vulnerability numerics-----------#
  
  # Helper function for ordinal suffixes of percentiles ----
  addOrdinalSuffix <- function(x) {
    # Round the value to an integer
    n <- as.integer(x)
    # Special cases for 11, 12, and 13:
    if(n %% 100 %in% 11:13) {
      return(paste0(n, "th"))
    }
    last_digit <- n %% 10
    if(last_digit == 1) {
      return(paste0(n, "st"))
    } else if(last_digit == 2) {
      return(paste0(n, "nd"))
    } else if(last_digit == 3) {
      return(paste0(n, "rd"))
    } else {
      return(paste0(n, "th"))
    }
  }
  
  # Social Tab Output ----
  #--------- Vulnerability Scores (Percentiles) -----------#
  
  # Render aggregated community health vulnerability (average of others)
  output$social_vulnerability <- renderUI({
    req(this_site()$health_aggregate_percentile)
    # Get the stored proportion (e.g., 0.524)
    percentile <- this_site()$health_aggregate_percentile
    # Multiply by 100 and round to nearest integer (or use one decimal if preferred)
    pct <- round(percentile * 100)
    # Get the ordinal number with suffix (e.g., "52nd")
    ordinal <- addOrdinalSuffix(pct)
    HTML(paste0("This clinic is ranked in the <span style='font-size: 1.25em;'><b>",
                ordinal,
                "</b></span> percentile for community health vulnerability."))
  }) # END renderUI: social_vulnerability
  
  
  # Render baseline health percentile ----
  output$baseline_health <- renderUI({
    req(this_site()$avg_baseline_health)
    percentile <- this_site()$avg_baseline_health
    pct <- round(percentile * 100)
    ordinal <- addOrdinalSuffix(pct)
    HTML(paste0("Baseline Health: <span style='font-size: 1.25em;'><b>",
                ordinal,
                "</b></span> percentile. Includes chronic disease prevalence, access to healthcare, maternal and childen's health, mental health, life expectancy, and preventive care measures."))
  }) # END renderUI: baseline_health
  
  
  # Render baseline environment percentile ----
  output$baseline_environment <- renderUI({
    req(this_site()$avg_baseline_environment)
    percentile <- this_site()$avg_baseline_environment
    pct <- round(percentile * 100)
    ordinal <- addOrdinalSuffix(pct)
    HTML(paste0("Environmental Exposures & Risks: <span style='font-size: 1.25em;'><b>",
                ordinal,
                "</b></span> percentile. Indicates exposure to air, soil, and water pollution, as well as broader environmental stressors from transportation and industrial activity.
"))
  }) # END renderUI: baseline_environment
  
  
  # Render climate change health percentile ----
  output$climate_change_health <- renderUI({
    req(this_site()$avg_climate_change_health)
    percentile <- this_site()$avg_climate_change_health
    pct <- round(percentile * 100)
    ordinal <- addOrdinalSuffix(pct)
    HTML(paste0("Climate Change-Related Health Impacts: <span style='font-size: 1.25em;'><b>",
                ordinal,
                "</b></span> percentile. Indicates health risks attributed to climate change, such as heat-related deaths, spread of climate-sensitive infectious diseases, and disasters like hurricanes and wildfires."))
  }) # END renderUI: climate_change_health
  
  
  # Render Multidimensional Deprivation Index (MDI) percentile ----
  output$mdi_percentile <- renderUI({
    req(this_site()$mdi_percentile)
    percentile <- this_site()$mdi_percentile
    pct <- round(percentile * 100)
    ordinal <- addOrdinalSuffix(pct)
    HTML(paste0(
      "<a href='https://www.census.gov/topics/income-poverty/poverty/about/related-sites/rates.html' target='_blank'>Multidimensional Deprivation Index (MDI)</a>: ",
      "<span style='font-size: 1.25em;'><b>", ordinal, "</b></span> percentile."
    ))
  }) # END renderUI: mdi_percentile
  
  
  
  
  #--------- Social Tables -----------#
  # demographics table ----
  output$community_demo <- renderTable({
    req(this_site())                # make sure a clinic has been selected
    df <- this_site()               # that one‐row tibble
    
    data.frame(
      "Race/Ethnicity" = c(
        "Black",
        "Asian",
        "Native American",
        "Hispanic",
        "Multi‑Ethnic",
        "White"
      ),
      Patients = format(
        c(
          df$Black,
          df$Asian,
          df$AmerIn_AKNative,
          df$Hispanic,
          df$MultRace,
          df$White
        ),
        big.mark   = ",",
        scientific = FALSE
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }, rownames = FALSE) # END demographics table
  
  
  # Render Patients & Federal Poverty Line Table ----
  output$poverty_table <- renderTable({
    req(this_site())
    df <- this_site()
    data.frame(
      `Fed. Poverty Line` = c(
        "≤100%",
        "101%–150%",
        "151%–200%",
        ">200%",
        "Unknown"
      ),
      Patients = format(
        c(
          df$FPL_100_Percent_and_Below,
          df$FPL_101_150_Percent,
          df$FPL_151_200_Percent,
          df$FPL_Over_200_Percent,
          df$FPL_Unknown
        ),
        big.mark = ",",
        scientific = FALSE
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }, rownames = FALSE)
  
  
  # Render Insurance Status Table ----
  output$insurance_table <- renderTable({
    req(this_site())
    df <- this_site()
    data.frame(
      "Insurance Status" = c("Uninsured", "Medicaid", "Medicare", "Public Insurance", "Private Insurance"),
      Patients = format(
        c(df$Uninsured, df$Medicaid, df$Medicare, df$PublicIns, df$PrivateIns),
        big.mark   = ",",
        scientific = FALSE
      ),
      stringsAsFactors = FALSE,
      check.names = FALSE
    )
  }, rownames = FALSE)  # END renderTable: insurance_table
  
  
  # Render Sex Table ----
  output$sex_table <- renderTable({
    req(this_site())
    df <- this_site()
    data.frame(
      Sex = c("Male", "Female"),
      Patients = format(
        c(df$TotalMale, df$TotalFemale),
        big.mark   = ",",
        scientific = FALSE
      ),
      stringsAsFactors = FALSE
    )
  }, rownames = FALSE)  # END renderTable: sex_table
  
  
  
  
  #--------------------------------- ALL ValueBoxes --------------------------------------#
  # Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  #SUMMARY WIDGET
  # Render Aggregated Community Health Box ----
  output$social_vulnerability <- renderValueBox({
    req(this_site()$health_aggregate_percentile)
    # Get the stored proportion (e.g., 0.524)
    percentile <- this_site()$health_aggregate_percentile
    # Multiply by 100 and round to nearest integer
    pct <- round(percentile * 100)
    # Get the ordinal number with suffix (e.g., "52nd")
    ordinal <- addOrdinalSuffix(pct)
    valueBox(
      ordinal,                                  # Display the ordinal value (e.g., "52nd")
      "Percentile in overall community health vulnerability relative to other US counties",         # Description text
      icon = icon("stethoscope"),
      color = "aqua" 
    ) # END valueBox
  }) # END Aggregated Community Health Box
  
  
  # Render Solar Array annual savings ValueBox (25 years) ----
  output$annual_savings <- renderValueBox({
    req(annual_solar_savings())
    annual_solar_savings <- annual_solar_savings()
    formatted_solar <- paste0("$", format(annual_solar_savings, big.mark = ","))
    
    valueBox(
      formatted_solar,                 # Big number
      "Annual Savings",    # Description text
      icon = icon("sun"),              # Icon for solar (adjust as needed)
      color = "yellow"                  # Green color theme
    )
  }) # END renderValueBox: Solar Array annual savings
  # Valid colors are: red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, orange, fuchsia, purple, maroon, black.
  
  
  # # Render pay rebate ValueBox ----
  # output$pay_rebate <- renderValueBox({
  #   req(this_site())       #create a conditional display for this box output
  #   rebate <- this_site()$solarapi_nameplate_capacity * 2000 * (1 - 0.3 - 0.1 * this_site()$energy_community)  #create calculation here for rebate!!
  #   formatted_rebate <- paste0("$", format(rebate, big.mark = ","))
  #   
  #   valueBox(
  #     formatted_rebate,
  #     "Rebate",
  #     icon = icon("money-bill-wave"),
  #     color = "black"
  #   )
  # }) # END renderValueBox: pay_rebate
  
  
  # Solar payback ValueBox ----
  output$solar_payback <- renderValueBox({
    req(solar_payback_dcf())
    yrs <- solar_payback_dcf()
    valueBox(
      ifelse(is.na(yrs), "‑", paste0(yrs, " yrs")),
      "Solar discounted pay‑back (5 %)",
      icon  = icon("clock"),
      color = "olive"
    ) # END solar payback valueBox
  }) # END renderValueBox: solar_payback
  
  # Battery payback ValueBox ----
  output$battery_payback <- renderValueBox({
    req(battery_payback_dcf())
    yrs <- battery_payback_dcf()
    valueBox(
      ifelse(is.na(yrs), "‑", paste0(yrs, " yrs")),
      "Battery discounted pay‑back (5 %)",
      icon  = icon("clock"),
      color = "maroon"
    ) # END battery payback valueBox
  }) # END renderValueBox: battery_payback
  
  
  # Render Battery Storage NPV ValueBox (15 years) ----
  output$battery_npv <- renderValueBox({
    req(net_present_value_battery())
    npv_battery <- net_present_value_battery()
    formatted_battery <- paste0("$", format(npv_battery, big.mark = ","))
    
    valueBox(
      formatted_battery,
      "Battery NPV (15 years)",
      icon = icon("battery-full"),
      color = "green"
    )
  }) # END renderValueBox: battery_npv
  
  
  # Render Community Health Vulnerability ValueBox ----
  output$social_vulnerability <- renderValueBox({
    req(this_site()$health_aggregate_percentile)
    # Assume this_site() returns a row with a health_aggregate_percentile (e.g., 0.524)
    sv <- this_site()$health_aggregate_percentile[1]
    sv_pct <- round(sv * 100)
    
    # If you have an addOrdinalSuffix() helper, use it; otherwise just display the number
    ordinal <- addOrdinalSuffix(sv_pct)  # e.g., "52nd"
    
    valueBox(
      ordinal,
      "Percentile for Community Health Vulnerability",
      icon = icon("stethoscope"),
      color = "aqua"
    )
  }) # END renderValueBox: social_vulnerability
  
  
  # Duplicate for the Summary widget
  output$social_vulnerability_summary <- renderValueBox({
    req(this_site()$health_aggregate_percentile)
    sv <- this_site()$health_aggregate_percentile[1]
    sv_pct <- round(sv * 100)
    ordinal <- addOrdinalSuffix(sv_pct)
    
    valueBox(
      ordinal,
      "Percentile for Community Health Vulnerability",
      icon = icon("stethoscope"),
      color = "aqua"
    )
  }) # END renderValueBox: social_vulnerability_summary
  
  
  
  # Render Natural Disaster Risk ValueBox ----
  output$disaster_risk <- renderValueBox({
    req(this_site()$FEMA_NRI_Risk_Score)
    # Assume this_site() returns a row with a FEMA_NRI_Risk_Score
    risk_value <- this_site()$FEMA_NRI_Risk_Score[1]
    risk_rounded <- round(risk_value, 1)
    
    # Use the helper to add the ordinal suffix if desired
    ordinal_risk <- addOrdinalSuffix(risk_rounded)
    
    valueBox(
      ordinal_risk,
      "FEMA Natural Disaster Risk Percentile",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  }) # END renderValueBox: disaster_risk
  
  
  
  ### ----------  DISCOUNTED PAY‑BACK (5 %)  ---------- ###
  solar_payback_dcf <- reactive({
    req(solar_system_cost(), this_site()$energy_community, annual_solar_savings(), 
        this_site()$solarapi_nameplate_capacity)
    
    # ---------- Up‑front cost minus ITC ----------
    raw_cost   <- solar_system_cost()                    # you already compute this
    itc_factor <- ifelse(this_site()$energy_community, 0.40, 0.30)
    net_cost   <- raw_cost * (1 - itc_factor)
    
    # ---------- Year‑1 net benefit ----------
    gross_sav  <- annual_solar_savings()                 # $ saved on the bill
    o_and_m    <- this_site()$solarapi_nameplate_capacity * 50   # $50 / kW / yr
    net_year   <- gross_sav - o_and_m                    # cash‑in after O&M
    
    # ---------- Discounted cash‑flow roll‑up ----------
    cumultv <- 0
    for (t in 1:25) {                                    # 25‑year horizon
      disc_cash <- net_year / (1.05^t)                   # 5 % discount rate
      cumultv       <- cumultv + disc_cash
      
      if (cumultv >= net_cost) {                             # pay‑back reached
        prev_cumultv <- cumultv - disc_cash
        frac     <- (net_cost - prev_cumultv) / disc_cash    # interpolate within the year
        return( round( (t-1) + frac , 1) )               # years, 1 dec place
      }
    }
    NA                                                  # > horizon → not recovered
  })
  
  battery_payback_dcf <- reactive({
    req(battery_system_cost(), this_site()$energy_community, annual_patient_savings(),
        observed_long_outages(), battery_size_kwh())
    raw_cost   <- battery_system_cost()                  # kWh & kW‑based cost
    itc_factor <- ifelse(this_site()$energy_community, 0.40, 0.30)
    net_cost   <- raw_cost * (1 - itc_factor)
    
    gross_sav  <- annual_patient_savings() +
      observed_long_outages() * input$vaccine_value
    o_and_m    <- battery_size_kwh() * 10                # $10 / kWh / yr
    net_year   <- gross_sav - o_and_m
    
    cumultv <- 0
    for (t in 1:15) {                                    # 15‑year battery horizon
      disc_cash <- net_year / (1.05^t)
      cumultv       <- cumultv + disc_cash
      if (cumultv >= net_cost) {
        prev_cumultv <- cumultv - disc_cash
        frac     <- (net_cost - prev_cumultv) / disc_cash
        return( round( (t-1) + frac , 1) )
      }
    }
    NA
  })
  
  
  ########################
  ### END REEVES CODE ###
  ########################
  
  
  
  # Zoom to Clinic Location when confirmed ----
  observeEvent(input$confirm_building_location, {
    req(input$clinic_search)
    
    
    selected_clinic <- clinic_data %>%
      filter(grepl(input$clinic_search, complete_address, ignore.case = TRUE))
    
    if (nrow(selected_clinic) == 1) {
      leafletProxy("clinic_map") %>%
        setView(lng = selected_clinic$site_lon, lat = selected_clinic$site_lat, zoom = 20)
      
      selected_clinic_info(as.list(selected_clinic))  # Store as list
      
    } else {
      showModal(modalDialog("Clinic location not found."))
    }
  })
  
  # Update Widget 1 with Clinic Name, Address, and ObjectID ----
  output$selected_clinic_info <- renderUI({
    
    req(selected_clinic_info())
    
    clinic_row <- selected_clinic_info()
    
    tagList(
      h4(strong(clinic_row[["Site_Name"]])),
      p(strong("Address: "), clinic_row[["complete_address"]]),
      p(strong("Site ID: "), clinic_row[["ObjectId"]]),
      br()
    )
  })
  
  # Address Correct Button ----
  observeEvent(input$address_correct, {
    req(selected_clinic_info())  
    
    removeModal()  
    showModal(modalDialog("Clinic Location confirmed! Proceeding..."))
    
    updateTabsetPanel(session, "tabs", selected = "Assessment")  
  })
  
  # Address Incorrect Button ----
  observeEvent(input$address_incorrect, {
    corrective_survey <- "https://docs.google.com/forms/d/e/1FAIpQLSdnLkP8C1G92Scrq0a2pLo2s3jYz26DDu0foL4hxc5Mr5S35g/viewform?usp=header"
    
    showModal(modalDialog(
      title = "Please enter the correct location.",
      HTML(paste0(
        "<b>Please fill out the survey at this link before proceeding! If the location was incorrect and you proceed anyway, the results will not be correct.</b><br><br>",
        "<a href='", corrective_survey, "' target='_blank' style='color: blue; text-decoration: underline;'>",
        "Click here to open the survey</a>."
      )),
      easyClose = TRUE,
      footer = modalButton("OK")
    ))
  })
  
  
  ####################
  ## IVETTE'S CODE ### RISK WIDGET
  ####################
  
  # Power Outages Risk: Evolution of outages events (2019-2023) (Reactive)
  output$outage_plot <- renderPlot({
    # Ensure the reactive fips_code is available
    req(this_site()$fips_code, this_site()$Complete_County_Name)
    current_fips <- this_site()$fips_code
    
    # Filter the outages data dynamically based on the selected FIPS code and the desired years
    county_data <- outages_by_county_master %>% 
      filter(fips_code == current_fips, year >= 2019, year <= 2023)
    
    # Group data by year and calculate total outages per year
    total_outages_per_year <- county_data %>%
      group_by(year) %>%
      summarise(total_outages = sum(total_outages, na.rm = TRUE))
    
    # Create the plot
    ggplot(total_outages_per_year, aes(x = year, y = total_outages)) +
      geom_line(color = "#CE7002", size = 1) +
      geom_point(color = "#757575", size = 3) +
      geom_text(aes(label = total_outages), vjust = -0.5, hjust = 0.5, size = 4, color = "black") +
      labs(title = paste0("Total Outages per Year in ", this_site()$Complete_County_Name),
           x = "Year", y = "Total Outages") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            title = element_text(size = 16),
            axis.title = element_text(size = 14))
  })
  
  
  
  # Render the histogram of outage duration (2023)
  output$histogram <- renderPlot({
    # Wait until this_site() returns a valid fips_code
    req(this_site()$fips_code, this_site()$Complete_County_Name)
    # Retrieve the reactive FIPS code
    current_fips <- this_site()$fips_code
    
    # Filter the outage data for the selected county
    df <- all_outages_2023 %>% filter(fips_code == current_fips)
    
    # Create the histogram using static bins
    ggplot(df, aes(x = custom_bins)) +
      geom_bar(stat = "count", fill = "#CE7002", color = "#757575") +
      labs(title = paste0("Outage Duration in ", this_site()$Complete_County_Name, " (Minutes)"),
           x = "Minutes of Outage per Event",
           y = "Frequency") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            title = element_text(size = 16))
  })
  
  
  
  ####### HENRY PLEASE CHECK OUT THE FOLLOWING CODE TO MAKE SURE ITS CORRECT #######
  
  
  # Reactive Total Outages Message ----
  output$total_outages_msg <- renderText({
    # Ensure that the reactive fips code is available
    req(this_site()$fips_code)
    current_fips <- this_site()$fips_code
    
    # Filter the county outages data for the selected FIPS and for years 2019-2023
    county_data <- outages_by_county_master %>% 
      filter(fips_code == current_fips, year >= 2019, year <= 2023)
    
    # Sum the total outages (using na.rm = TRUE)
    total_outages <- sum(county_data$total_outages, na.rm = TRUE)
    
    # Build and return the output string
    paste("Total number of outages in your area (2019-2023):", total_outages)
  })
  
  # Reactive Outages Over 4 Hours Message ----
  output$outages_over_4hrs_msg <- renderText({
    req(this_site()$fips_code)
    current_fips <- this_site()$fips_code
    
    # Filter the county outages data similarly
    county_data <- outages_by_county_master %>% 
      filter(fips_code == current_fips, year >= 2019, year <= 2023)
    
    # Count the number of events where total_customer_minutes is greater than 240
    outages_over_4hrs <- sum(county_data$total_customer_minutes > 240, na.rm = TRUE)
    
    paste("Outages lasting more than 4 hours (2019-2023):", outages_over_4hrs)
  })
  ####### ####### ####### ####### ####### ####### ####### ####### ####### #######
  
  
  
  
  # Natural disaster risk percentile (reactive)
  output$disaster_risk_msg <- renderText({
    # Ensure a valid FIPS code is available
    req(this_site()$fips_code)
    current_fips <- this_site()$fips_code
    
    # Filter the clinic_data for the selected county using the reactive FIPS code
    county_risk <- clinic_data %>% filter(fips_code == current_fips)
    req(nrow(county_risk) > 0)  # Ensure at least one row is returned
    
    # Get the FEMA risk score from the first (and expectedly only) row
    risk_value <- county_risk$FEMA_NRI_Risk_Score[1]
    risk_value_rounded <- round(risk_value, 1)
    
    # Construct and return the text message
    paste("Your natural disaster risk is:", risk_value_rounded, "percentile")
  })
  
  
  # Reactive SAIDI Output ----
  output$saidi_msg <- renderText({
    # Ensure that the reactive object this_site() and its fips_code are available
    req(this_site()$fips_code, this_site()$Complete_County_Name)
    current_fips <- this_site()$fips_code
    
    # Filter the outages data for the selected county and the year 2023
    county_data <- outages_by_county_master %>% 
      filter(fips_code == current_fips, year == 2023)
    
    # Ensure that we have at least one row of data
    req(nrow(county_data) > 0)
    
    # Extract the SAIDI value from the first row (assuming only one row per county-year)
    saidi_value <- round(county_data$saidi[1], 1)
    
    # Construct and return the output string
    paste0("System Average Interruption Duration Index (SAIDI) is a measurement of power outage duration that the average consumer in a region can expect over a year. Customers in ", this_site()$Complete_County_Name, " can expect ", saidi_value, " minutes of power outage each year.")
  })
  
  
  # Reactive SAIFI Output ----
  output$saifi_msg <- renderText({
    req(this_site()$fips_code, this_site()$Complete_County_Name)
    current_fips <- this_site()$fips_code
    
    county_data <- outages_by_county_master %>% 
      filter(fips_code == current_fips, year == 2023)
    
    req(nrow(county_data) > 0)
    
    saifi_value <- round(county_data$saifi[1], 1)
    
    paste("System Average Interruption Frequency Index (SAIFI) measures power outage frequency that the average customer can expect in a given year. Customers in ", this_site()$Complete_County_Name, " can expect ", saifi_value, " outages in a year.")
  })
  
  
  
  ########################
  ## END IVETTE'S CODE ###
  ########################
  
  
  
  ####################
  ## TEMESGEN'S CODE # SPLASH PAGE + GUIDE/FAQ WIDGET
  ####################
  
  # Render FAQ Output ----
  output$faq_output <- renderUI({
    faq <- faq_data[[input$faq_category]]
    
    # List of Guide items based on the selected category ----
    tagList(
      h3(faq$question),
      p(faq$answer),
      h4("Why it matters:"),
      tags$ul(
        
        # Switch statement to display different list items based on the selected FAQ category ----
        switch(input$faq_category,
               
               
               # Solar Potential switch ----
               "Solar Potential" = list(
                 tags$li("More available space means a greater opportunity to offset energy costs with solar power."),
                 tags$li("Limited roof space may require high-efficiency panels or alternative placements.")
               ), # END Solar Potential switch
               
               
               # Battery Storage switch ----
               "Battery Storage" = list(
                 tags$li("Battery storage ensures power availability during outages."),
                 tags$li("Correct sizing helps reduce overall energy costs.")
               ), # END Battery Storage switch
               
               
               # Power Outage Risk switch ----
               "Power Outage Risk" = list(
                 tags$li("Analyzing outage risk helps clinics plan for backup power needs."),
                 tags$li("Reduces disruptions to critical medical operations.")
               ), # END Power Outage Risk switch
               
               
               # Natural Disaster Risk switch ----
               "Natural Disaster Risk" = list(
                 tags$li("Assessing disaster risk helps clinics build resilience strategies."),
                 tags$li("Ensures critical medical services remain operational.")
               ), # END Natural Disaster Risk switch
               
               
               # Financial Assessment switch ----
               "Financial Assessment" = list(
                 tags$li("A shorter payback period means faster cost recovery."),
                 tags$li("Financial modeling supports investment decisions in clean energy.")
               ) # END Financial Assessment switch
               
        ) # END switch
      ) # END tags$ul
    ) # END tagList
  }) # END renderUI: faq_output
  
  
  # Additional Information Tab
  output$additional_info <- renderUI({
    tagList(
      h4("Key Takeaways"),
      p("Solar adoption for clinics is a multi-faceted decision involving energy needs, financial feasibility, and social impact. The FAQ provides insights into each aspect."),
      h4("Resources"),
      tags$ul(
        tags$li(a("Google Solar API", href="https://developers.google.com/solar", target="_blank")),
        tags$li(a("National Renewable Energy Laboratory (NREL)", href="https://www.nrel.gov/", target="_blank")),
        tags$li(a("FEMA National Risk Index", href="https://hazards.fema.gov/nri/", target="_blank"))
      )
    )
  })
  
  
  ########################
  # END TEMESGEN'S CODE ##
  ########################
} # END server