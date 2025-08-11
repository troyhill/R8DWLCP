### Attempt to create a pane that shows audit status for each lab
### and lists audits in the current FY, next FY, and so on.
### possibly also a general update to match new csv files
### status: audit map/table done.




# ui.R
library(shiny)
library(shinydashboard)
library(leaflet)
library(bslib)
library(plyr)
library(ggplot2)



# prep --------------------------------------------------------------------
# load_script <- system.file("extdata", "script_load_data_20250807.R", package = "R8DWLCP")
# source(load_script)

# method_vec <- c("EPA Method 1604", "Colilert Test", "Colisure Test", "EPA Method 334.0", 
#                 "SM 4500-Cl G", "EPA Method 524.2", "EPA Method 551.1", "EPA Method 200.8", 
#                 "EPA Method 200.9", "EPA Method 300.0", "EPA Method 353.2", "EPA Method 200.9", 
#                 "EPA Method 524.2", "EPA Method 525.2", "EPA Method 507", "EPA Method 903.0", 
#                 "EPA Method 904.0", "EPA Method 908.0")
method_vec <- c("SM 9223 B Colilert (Enumerative) Total coliforms", "SM 9223 B Colilert (Enumerative) E. coli", 
                "SM 9223 B Colilert (Presence/Absence) Total coliforms", "SM 9223 B Colilert (Presence/Absence) E. coli", 
                "SM 9223 B Colisure (Presence/Absence) Total coliforms", "SM 9223 B Colisure (Presence/Absence) E. coli", 
                "SM 9222 D Membrane Filtration with mFC Fecal coliforms", "SM 9221B,C MTF LTB BGLBa (Presence/Absence) Total coliforms", 
                "SM 9222B,C MF M-Endo or LES Endo LTB BGLBa (Presence/Absence) Total coliforms", 
                "SM 9222G MF M-Endo or LES Endo NA-MUGa,c (Presence/Absence) E. coli", 
                "SM 9221F MTF LTB EC-MUGa,c (Presence/Absence)  E. coli", "IDEXX SimPlate Heterotrophic plate count", 
                "SM 9215B - Pour Plate Heterotrophic plate count", "SM 9215B PCA Heterotrophic plate count", 
                "EPA Method 334.0 Chlorine", "SM 4500-Cl G Chlorine", "EPA Method 300.1 Chlorite", 
                "EPA Method 552.2 Haloacetic acids", "EPA Method 552.2 HAA5", 
                "EPA Method 524.2 Total trihalomethanes", "EPA Method 557 Haloacetic acids", 
                "EPA Method 353.2 Nitrate", "EPA Method 353.2 Nitrite", "EPA Method 353.2 Nitrate+nitrite", 
                "EPA Method 300.0 Nitrate+nitrite", "EPA Method 300.0 Nitrite", 
                "EPA Method 300.0 Nitrate", "SM 4500-NO2- B Nitrate", "EPA Method 551.1 Trihalomethanes", 
                "EPA Method 200.7 Copper", "EPA Method 200.8 Copper", "EPA Method 200.8 Lead", 
                "EPA Method 200.8 Antimony", "EPA Method 200.8 Arsenic", "EPA Method 200.8 Barium", 
                "EPA Method 200.7 Barium", "EPA Method 200.8 Beryllium", "EPA Method 200.7 Beryllium", 
                "EPA Method 200.8 Cadmium", "EPA Method 200.7 Cadmium", "EPA Method 200.8 Chromium", 
                "EPA Method 200.7 Chromium", "EPA Method 200.8 Selenium", "EPA Method 200.8 Mercury", 
                "EPA Method 200.8 Thallium", "EPA Method 200.8 Uranium", "EPA Method 245.1 Mercury", 
                "EPA Method 335.4 Cyanide", "EPA Method OIA 1677 Cyanide", "EPA Method 300.0 Fluoride", 
                "SM 4500-F- C Fluoride", "EPA Method 524.4 Trihalomethanes", 
                "EPA Method 524.4 VOCs", "EPA Method 531.1 Carbofuran", "EPA Method 549.2 Diquat", 
                "EPA Method 547 Glyphosate", "EPA Method 531.1 Oxamyl", "EPA Method 900.0 Gross alpha", 
                "EPA Method 900.0 Gross beta", "EPA Method 900.0 Radium-226", 
                "Ra-05 Radium-228", "SM 7500-U C Uranium", "EPA Method 903.1 Radium-226", 
                "SM 7500-Ra D Radium-228", "EPA Method 515.3 2, 4, 5-TP (Silvex)", 
                "EPA Method 515.3 2, 4-D", "EPA Method 525.2 Alachlor", "EPA Method 525.2 Atrazine", 
                "EPA Method 525.2 Benzo[a]pyrene", "EPA Method 531.1 Carbofuran", 
                "EPA Method 525.2 Chlordane       ", "EPA Method 515.3 Dalapon", 
                "EPA Method 504.1 Dibromochloropropane", "EPA Method 525.2 Di(2-ethylhexyl)adipate", 
                "EPA Method 525.2 Di(2-ethylhexyl)phthalate", "EPA Method 515.3 Dinoseb", 
                "EPA Method 549.2 Diquat", "EPA Method 548.1 Endothall", "EPA Method 525.2 Endrin", 
                "EPA Method 504.1 Ethylene dibromide", "EPA Method 547 Glyphosate", 
                "EPA Method 525.2 Heptachlor", "EPA Method 525.2 Heptachlor Epoxide", 
                "EPA Method 525.2 Hexachlorobenzene", "EPA Method 525.2 Hexachlorocyclopentadiene", 
                "EPA Method 525.2 Lindane", "EPA Method 525.2 Methoxychlor", 
                "EPA Method 531.1 Oxamyl (Vydate)", "EPA Method 515.3 Pentachlorophenol", 
                "EPA Method 515.3 Picloram", "EPA Method 525.2 Simazine", "EPA Method 525.2 Toxaphene", 
                "EPA Method 524.2 1, 1, 1-Trichloroethane", "EPA Method 524.2 1, 1, 2-Trichloroethane", 
                "EPA Method 524.2 1, 1-Dichloroethylene", "EPA Method 524.2 1, 2, 4-Trichlorobenzene", 
                "EPA Method 524.2 1, 2-Dichlorobenzene", "EPA Method 524.2 1, 2-Dichloroethane", 
                "EPA Method 524.2 1, 2-Dichloropropane", "EPA Method 524.2 1, 4-Dichlorobenzene", 
                "EPA Method 524.2 Benzene", "EPA Method 524.2 Carbon Tetrachloride", 
                "EPA Method 524.2 Chlorobenzene", "EPA Method 524.2 Cis-1, 2-dichloroethylene", 
                "EPA Method 524.2 Dichloromethane", "EPA Method 524.2 Ethylbenzene", 
                "EPA Method 524.2 Styrene", "EPA Method 524.2 Tetrachloroethylene", 
                "EPA Method 524.2 Toluene", "EPA Method 524.2 Trans-1, 2-dichloroethylene", 
                "EPA Method 524.2 Trichloroethylene", "EPA Method 524.2 Vinyl chloride", 
                "EPA Method 524.2 Xylenes", "EPA Method 505 Alachlor", "EPA Method 505 Chlordane       ", 
                "EPA Method 505 Endrin", "EPA Method 505 Heptachlor", "EPA Method 505 Heptachlor epoxide", 
                "EPA Method 505 Hecachlorobenzene", "EPA Method 505 Hexachlorocyclopentadiene", 
                "EPA Method 505 Lindane", "EPA Method 505 Methoxychlor", "EPA Method 505 Polychlorinated biphenyls (as aroclors)", 
                "EPA Method 505 Toxaphene", "EPA Method 515.4 2, 4, 5-TP (Silvex)", 
                "EPA Method 515.4 2, 4-D", "EPA Method 515.4 Dalapon", "EPA Method 515.4 Dinoseb", 
                "EPA Method 515.4 Pentachlorophenol", "EPA Method 515.4 Picloram", 
                "EPA Method 525.3 Di(2-ethylhexyl)adipate", "EPA Method 525.3 Di(2-ethylhexyl)phthalate", 
                "EPA Method 536 Simazine")


# ui ----------------------------------------------------------------------



ui <- page_navbar(
  nav_panel("Audit timelines", "Current certification expirations",
            dashboardPage(
              dashboardHeader(title = ""), 
              dashboardSidebar(#disable = TRUE#,
                collapsed = TRUE
                ),
              dashboardBody(
                fluidRow(
                  leafletOutput("map_audits", width = "700px", height = "400px"),
                  # column(width = 10, height = 9, plotOutput("plot_audits")),
                  column(width = 8, DT::DTOutput("table_audits"))
                )
              )
            )),
  nav_panel("Summarize by Laboratory", "Proficiency test results: Lab summaries",
          dashboardPage(
            dashboardHeader(title = ""), 
            dashboardSidebar(#disable = TRUE, 
              collapsed = FALSE, downloadButton("report", "Generate report")),
            dashboardBody(
              fluidRow(
                       leafletOutput("map2", width = "700px", height = "400px"),
                       column(width = 10, height = 9, plotOutput("plot2")),
                       column(width = 8, DT::DTOutput("table2"))
              )
            )
          )),
  nav_panel("Summarize by method", "Proficiency test results: Method summaries",
            dashboardPage(
              dashboardHeader(title = ""),
              dashboardSidebar(selectInput("method", "Select Method", choices = method_vec) #choices = unique(method_data$method))
                               # checkboxInput("recentYear", "Show data from the most recent year", FALSE)
              ),
              dashboardBody(
                fluidRow(leafletOutput("map", width = "700px", height = "400px"),
                         column(width = 8, DT::DTOutput("table1"))
                ))
            )),
  
nav_panel("Manual file upload", "File upload menu (optional)",
          # Sidebar layout with input and output definitions ----
          # fluidPage(
          #   tags$head(
          #     tags$style("label { color: black; }") 
          #   ),
            dashboardPage(
            dashboardHeader(),
            dashboardSidebar(
            sidebarMenu(
            fileInput("lab_data", span("Browse to CSV file with proficiency test data", style="color:black"),
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            fileInput("location_file", span("Browse to CSV file with laboratory locations", style="color:black"),
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            fileInput("method_file", span("Browse to CSV file with method list", style="color:black"),
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv"))
            )
            ),
            dashboardBody(
              # tabItems(
                # First tab content
                # tabItem(tabName = "widgets",
                        h3(htmlOutput('text1'), #output$text1 <- renderText( "Proficiency test data preview")
                           plotOutput("plot_preview"),
                           h3(htmlOutput('text2'), #"Map preview",
                              leafletOutput("map_preview", width = "700px", height = "400px")),
                           h3(htmlOutput('text3'), #"Methods list",
                              tableOutput("table_preview"))
                        )
                # )
              # )
            )
          )
          ),
  title = "Region 8 Drinking Water Laboratory Certification Program Dashboard",
  id = "page"
)



server <- function(input, output, session) {
  global <- reactiveValues(target_location = NULL)
  
  current_date <- Sys.Date()
  one_year_ago <- current_date - 365
  
  output$text1 <- output$text2 <- output$text3 <- renderText("")
  
  if(!exists('input$lab_data')) {
    path_tst <- getwd()
    ### the DW LabCert sharepoint folder should be added to the user's OneDrive
    ### and accessible via file browser.
    ### loading an R package makes getwd return something in AppData instead of 
    ### the file directory chain observed during testing.
    path_tst <- gsub(x = path_tst, pattern = 'AppData', replacement = 'OneDrive - Environmental Protection Agency (EPA)/')
    path_components <- c(as.list(strsplit(x = path_tst, split = '/')[[1]][1:4]),
                         'R8 All LSASD - Region 8 Lab - DWLabCert',
                         'Work_Instruction',
                         'test-data')
    path_begin   <- do.call(file.path, as.list(path_components))
    output$text1 <- renderText(paste0("Note: R is looking for data files in ", path_begin))
    
    if (dir.exists(path_begin)) {
      file_lst   <- list.files(path_begin, recursive = TRUE, full.names = TRUE)
      
      locations_path   <- sort(grep(x = file_lst, pattern = 'data_lab_list', value = TRUE), decreasing = TRUE)[1] 
                              #grep(x = file_lst, pattern = 'data_lab_list', value = TRUE)[1]
      dat_loc        <- read.csv(locations_path)
      if(all(c("lat", "long", "Region", "laboratory_name", "laboratory_location") %in% names(dat_loc))) {
        dat_loc      <- dat_loc[grepl(x = dat_loc$Region, pattern = '8'), ]
        dat_loc      <- dat_loc[!is.na(x = dat_loc$lat), ]
        output$text2 <- renderText( "Laboratory locations from input CSV:")
        global$locations <- dat_loc
      } else {
        output$text2 <- renderText(c('WARNING: Lab location dataset should have the following column names:<br>', paste0(c("lat", "long", "Region", "laboratory_name", "laboratory_location"), collapse = ', ')))
        global$locations <- NULL
      }
      
      dat_methods <- read.csv(sort(grep(x = file_lst, pattern = 'data_NPDWS_methods_', value = TRUE), decreasing = TRUE)[1])
      if(all(c("method", "common_name", "category") %in% names(dat_methods))) {
        global$method_data <- dat_methods
        output$text3 <- renderText( "NPDWS methods found in CSV:")
      } else {
        output$text3 <- renderText(c('WARNING: Method file should have the following column names:<br>', paste0(c("method", "common_name", "category"), collapse = ', ')))
        global$method_data <- NULL
      }
      
      dat1 <- read.csv(sort(grep(x = file_lst, pattern = "data_proficiency_test", value = TRUE), decreasing = TRUE)[1])
      if(all(c('laboratory_name', 'laboratory_location', 'method', 'PT_result', 'PT_test_date') %in% names(dat1))) {
        date_tmp <- gsub(as.character(dat1$PT_test_date), pattern = '/| ', replacement = '-')
        dat1$PT_test_date <- as.Date(as.character(date_tmp), format = '%m-%d-%Y')
        dat1$PT_test_date[is.na(dat1$PT_test_date)] <- as.Date(date_tmp[is.na(dat1$PT_test_date)], format = '%Y-%m-%d')
        dat1$PT_test_date[is.na(dat1$PT_test_date)] <- as.Date(date_tmp[is.na(dat1$PT_test_date)], format = '%d-%m-%Y')
        global$lab_data <- dat1 # FROM HERE ON USE: global$data
        output$text1 <- renderText( "Proficiency test data preview")
      } else {
        output$text1 <- renderText(c('WARNING: Proficiency test dataset should have the following column names:<br>', paste0(c('laboratory_name', 'laboratory_location', 'method', 'PT_result', 'PT_test_date'), collapse = ', ')))
        global$lab_data <- NA
      }
    }
    
  }
  
  observeEvent(input$lab_data, {
    # dat <- read.csv2(input$file1$datapath)
    # DO LOTS OF OPERATIONS ON data
    dat      <- read.csv(input$lab_data$datapath, stringsAsFactors = FALSE)
    if(all(c('laboratory_name', 'laboratory_location', 'method', 'PT_result', 'PT_test_date') %in% names(dat))) {
      date_tmp <- gsub(as.character(dat$PT_test_date), pattern = '/| ', replacement = '-')
      # message(head(date_tmp), '\n')
      # message(head(as.Date(date_tmp[is.na(dat$PT_test_date)], format = '%m-%d-%Y')))
      dat$PT_test_date <- as.Date(as.character(date_tmp), format = '%m-%d-%Y')
      dat$PT_test_date[is.na(dat$PT_test_date)] <- as.Date(date_tmp[is.na(dat$PT_test_date)], format = '%Y-%m-%d')
      dat$PT_test_date[is.na(dat$PT_test_date)] <- as.Date(date_tmp[is.na(dat$PT_test_date)], format = '%d-%m-%Y')
      global$lab_data <- dat # FROM HERE ON USE: global$data
      output$text1 <- renderText( "Proficiency test data preview")
    } else {
      output$text1 <- renderText(c('WARNING: Proficiency test dataset should have the following column names:<br>', paste0(c('laboratory_name', 'laboratory_location', 'method', 'PT_result', 'PT_test_date'), collapse = ', ')))
      global$lab_data <- NA
      # message('Proficiency test dataset should have column names: ', paste0(c('laboratory_name', 'laboratory_location', 'method', 'PT_result', 'PT_test_date'), collapse = ', '))
    }
  })
  
  observeEvent(input$location_file, {
    # dat <- read.csv2(input$file1$datapath)
    # DO LOTS OF OPERATIONS ON data
    dat      <- read.csv(input$location_file$datapath, stringsAsFactors = FALSE)
    if(all(c("lat", "long", "Region", "laboratory_name", "laboratory_location") %in% names(dat))) {
      dat      <- dat[grepl(x = dat$Region, pattern = '8'), ]
      dat      <- dat[!is.na(x = dat$lat), ]
      output$text2 <- renderText( "Laboratory locations from input CSV:")
      global$locations <- dat
    } else {
      output$text2 <- renderText(c('WARNING: Lab location dataset should have the following column names:<br>', paste0(c("lat", "long", "Region", "laboratory_name", "laboratory_location"), collapse = ', ')))
      global$locations <- NULL
    }
  })
  
  observeEvent(input$method_file, {
    # dat <- read.csv2(input$file1$datapath)
    # DO LOTS OF OPERATIONS ON data
    dat      <- read.csv(input$method_file$datapath, stringsAsFactors = FALSE)
    if(all(c("method", "common_name", "category") %in% names(dat))) {
      global$method_data <- dat
      output$text3 <- renderText("NPDWS methods found in CSV:")
    } else {
      output$text3 <- renderText(c('WARNING: Method file should have the following column names:<br>', paste0(c("method", "common_name", "category"), collapse = ', ')))
      global$method_data <- NULL
    }
  })
  
  lab_data <- reactive({
    req(global$lab_data)
    df <- global$lab_data
    return(df)
  })
  
  method_data <- reactive({
    req(global$method_data)
    df <- global$method_data
    return(df)
  })
  
  locations <- reactive({
    req(global$locations)
    df <- global$locations
    return(df)
  })
  
  
  output$plot_preview <- renderPlot({
    req(global$lab_data)
    # plot(table(global$data$PT_result))#, global$data$PT_test_date)
    # print(message(summary(format(global$data$PT_test_date, format = '%m-%d-%Y'))))
    # tmp <- global$data
    # tmp$PT_test_date <- format(tmp$PT_test_date, format = '%m-%d-%Y')
    # ggplot(data = tmp, aes(y = PT_result, x = PT_test_date)) + ggplot2::geom_point()
    ggplot(data = global$lab_data, aes(y = PT_result, x = PT_test_date, col = laboratory_name)) + ggplot2::geom_point() + facet_wrap(. ~ method) +
      theme_bw() + theme(legend.position = 'none') +
      labs(y = 'Proficiency test outcome', x = '')
  })
  
  output$table_preview <- renderTable({
    # req(global$lab_data)
    # head(format(global$data$PT_test_date, format = '%m-%d-%Y')) # displays as date
    # head(global$data$PT_test_date) # displays as numbers
    req(global$method_data)
    paste0(global$method_data$method, collapse = ', ')
    # global$method_data$method
  })
  
  
  output$map_preview <- renderLeaflet({
    req(global$locations)
    locations <- global$locations
    marker_colors <- sapply(locations$laboratory_name, function(name) {
      # if (most_recent_row == FALSE) {
      return("green")
      #   } else if (most_recent_row == TRUE) {
      #     return("red")
      #   }
      # }
      # return("gray")  # Default color if no tests are available
    })
    
    # Debugging: Print marker colors and shapes
    # print(marker_colors)
    # print(marker_shapes)
    
    marker_popups <- sapply(locations$laboratory_name, function(name) {
      paste("Lab Name: ", name, "<br><br>")
    })
    # print(marker_popups)
    
    
    leaflet(locations) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = locations$long,
        lat = locations$lat,
        color = unname(marker_colors),
        radius = 8,
        popup = unname(marker_popups) # ~laboratory_name #
      )
  })

  filtered_lab_data <- reactive({
    # req(global$lab_data)
    df <- lab_data()[trimws(tolower(lab_data()$method)) == trimws(tolower(input$method)), ]
  df <- df[order(df$PT_test_date, decreasing = TRUE), ]
  return(df)
  })

  lab_data_2 <- reactive({
  # req(global$lab_data)
  df <- lab_data()
  df <- df[order(df$PT_test_date, decreasing = TRUE), ]
  return(df)
  })



  output$map <- renderLeaflet({
    ### map for method pane
    current_date <- Sys.Date()
    one_year_ago <- current_date - 365

    # Filter lab data based on selected method
    filtered_lab_data <- lab_data()[trimws(tolower(lab_data()$method)) == trimws(tolower(input$method)), ]
    filtered_lab_data <-  filtered_lab_data[order(filtered_lab_data$PT_test_date, decreasing = TRUE), ]
    # Debugging: Print filtered data
    # print(filtered_lab_data)

    # Determine marker colors and shapes
    marker_shapes <- sapply(locations()$laboratory_name, function(name) {
      ### large points: no data in the past year for the selected method, concerning.
      lab_rows <- filtered_lab_data[filtered_lab_data$laboratory_name == name, ]
      if (nrow(lab_rows) > 0 && any(lab_rows$PT_test_date >= one_year_ago)) {
        return(2)  # shape for recent data
      } else {
        return(10)   # other shape
      }
    })

    marker_colors <- sapply(locations()$laboratory_name, function(name) {
      lab_rows <- filtered_lab_data()[(filtered_lab_data()$laboratory_name == name) & (filtered_lab_data()$PT_test_date >= one_year_ago), ]
      # print(lab_rows)
      if (nrow(lab_rows) > 0) {
        # Find the most recent test date
        most_recent_row <- lab_rows[which.max(lab_rows$PT_test_date), ]
        if (most_recent_row$PT_result == "Pass") {
          return("green")
        } else if (most_recent_row$PT_result == "Fail") {
          return("red")
        }
      }
      return("gray")  # Default color if no tests are available
    })

    # Debugging: Print marker colors and shapes
    # print(marker_colors)
    # print(marker_shapes)

    marker_popups <- sapply(locations()$laboratory_name, function(name) {
      # lab_rows <- filtered_lab_data[(filtered_lab_data$laboratory_name == name) & (filtered_lab_data$PT_test_date >= one_year_ago), ]
      lab_rows <- filtered_lab_data()[filtered_lab_data()$laboratory_name == name & (filtered_lab_data()$PT_test_date >= one_year_ago), ]
      if (nrow(lab_rows) > 0) {
        most_recent_row <- lab_rows[which.max(lab_rows$PT_test_date), ]
        recent_sample_collected <- any(lab_rows$PT_test_date >= one_year_ago)
        recent_status <- ifelse(recent_sample_collected, "Proficiency test reported in last year: <strong>Yes</strong>", "Proficiency test reported in last year: <strong>No</strong>")
        paste("Lab Name: ", name, "<br>",
              recent_status, "<br>",
              "Most Recent Result: ", '<strong>', most_recent_row$PT_result, '</strong>', "<br>")
      } else {
        paste("Lab Name: ", name, "<br>", '<strong>',
              "No recent proficiency test data available", '</strong>')
      }
    })

    leaflet(locations()) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = locations()$long,
        lat = locations()$lat,
        color = unname(marker_colors),
        radius = unname(marker_shapes),
        popup = unname(marker_popups) # ~laboratory_name #
      )
  })


  observeEvent(input$map_marker_click, {
    ### action when a lab is selected in the method pane
    click <- input$map_marker_click
    selected_location <- locations()[locations()$lat == click$lat & locations()$long == click$lng, ]
    # selected_lab_data <- lab_data[(lab_data$laboratory_name == selected_location$laboratory_name) & (lab_data$PT_test_date >= one_year_ago), ] # lab_data[lab_data$laboratory_name == selected_location$laboratory_name, ]
    selected_lab_data <- filtered_lab_data()[grepl(x = filtered_lab_data()$laboratory_name, pattern = selected_location$laboratory_name), ]
    selected_lab_data <- selected_lab_data[trimws(tolower(selected_lab_data$method)) == trimws(tolower(input$method)), ]
    selected_lab_data <- selected_lab_data[order(selected_lab_data$PT_test_date, decreasing = TRUE), ]

    # if (input$recentYear) { ### not working; table isn't updating
    #   # Filter data to show only the most recent year
      selected_lab_data <- selected_lab_data[selected_lab_data$PT_test_date >= one_year_ago, ]
      selected_lab_data <- selected_lab_data[order(selected_lab_data$PT_test_date, decreasing = TRUE), ]
      if(!exists('selected_lab_data')) {
        selected_lab_data <- NA
      } 

    output$table1 <- DT::renderDT({
      selected_lab_data
    })
  })

  output$map2 <- renderLeaflet({
    ### map for lab-focused pane
    # lab_summary_tmp[order(lab_summary_tmp$PT_test_date, decreasing = TRUE), ]

    lab_summary_tmp <- plyr::ddply(lab_data_2()[lab_data_2()$PT_test_date >= one_year_ago, ], c('laboratory_name'),
                                   plyr::summarise,
                                   any_failures = any(grepl(x = tolower(trimws(PT_result)), pattern = 'fail'))
    )
    # print(head(lab_summary_tmp))

    ### maybe need to filter most recent PT, in case there was a failure but issues were resolved?
    lab_method_summary <- plyr::ddply(lab_data_2()[lab_data_2()$PT_test_date >= one_year_ago, ], c('laboratory_name', 'method'),
                                      plyr::summarise,
                                      any_failures = any(grepl(x = tolower(trimws(PT_result)), pattern = 'fail'))
    )
    lab_method_summary_yearago <- plyr::ddply(lab_data_2()[(lab_data_2()$PT_test_date <= one_year_ago) & (lab_data_2()$PT_test_date >= (one_year_ago - 365)), ], c('laboratory_name', 'method'),
                                              plyr::summarise,
                                              any_failures = any(grepl(x = tolower(trimws(PT_result)), pattern = 'fail'))
    )
    # print(head(lab_method_summary_yearago))

    ### compare last year's PTs (even failing PTs) to this year's to identify any missing methods
    ### which combinations of lab name and method are missing in current year
    current_year_name_methods  <- paste0(lab_method_summary$laboratory_name, '_', lab_method_summary$method)
    # print(current_year_name_methods)
    # print('\n1\n')
    year_ago_name_methods      <- paste0(lab_method_summary_yearago$laboratory_name, '_', lab_method_summary_yearago$method)
    # print(year_ago_name_methods)
    # print('\n2\n')
    # save(list = c('current_year_name_methods', 'year_ago_name_methods'), file= 'test.RData') # load('inst/dashboard_test02/test.RData')
    # setdiff(year_ago_name_methods, current_year_name_methods) # methods with PTs from previous year but no PTs this year
    if (length(base::setdiff(year_ago_name_methods, current_year_name_methods)) > 0) {
      lab_method_missing_methods <- data.frame(comb = base::setdiff(year_ago_name_methods, current_year_name_methods), lab = NA, method = NA)
      # print(lab_method_missing_methods)
      # print('\n3\n')
      lab_method_missing_methods$lab    <- sapply(X = strsplit(lab_method_missing_methods$comb, split = '_'), '[[', 1)
      lab_method_missing_methods$method <- sapply(X = strsplit(lab_method_missing_methods$comb, split = '_'), '[[', 2)
    } else {
      ### if all methods are identical between years, set all to NA.
      lab_method_missing_methods <- data.frame(comb = NA, lab = NA, method = NA)
      }
    # print(lab_method_missing_methods)
    # print('\n3\n')
    
    # Determine marker colors and shapes
    marker_shapes <- sapply(locations()$laboratory_name, function(name) {
      ### large points: a failing method in the past year concerning.
      lab_rows <- lab_summary_tmp[lab_summary_tmp$laboratory_name == name, ]
      if (nrow(lab_rows) > 0 && any(lab_rows$any_failures == FALSE)) {
        return(2)  # small points: no failing PTs
      } else {
        return(10)   # large points: at least one failing PT
      }
    })
    # print(marker_shapes)

    marker_colors <- sapply(locations()$laboratory_name, function(name) {
      lab_rows <- lab_summary_tmp[(lab_summary_tmp$laboratory_name == name), ]
      # print(lab_rows)
      if (nrow(lab_rows) > 0) {
        # Find the most recent test date
        most_recent_row <- lab_rows$any_failures
        if (most_recent_row == FALSE) {
          return("green")
        } else if (most_recent_row == TRUE) {
          return("red")
        }
      }
      return("gray")  # Default color if no tests are available
    })

    # Debugging: Print marker colors and shapes
    # print(marker_colors)
    # print(marker_shapes)

    marker_popups <- sapply(locations()$laboratory_name, function(name) {
      ### note any missing or failed PTs

      lab_rows <- lab_method_summary[lab_method_summary$laboratory_name == name, ]
      if (nrow(lab_rows) > 0) {
        # most_recent_row <- lab_rows[which.max(lab_rows$PT_test_date), ]
        failing_methods <- ifelse(length(lab_rows$method[lab_rows$any_failures == TRUE]) > 0, paste0(lab_rows$method[lab_rows$any_failures == TRUE], collapse = ', '), 'None')
        passing_methods <- ifelse(length(lab_rows$method[lab_rows$any_failures == FALSE]) > 0, paste0(lab_rows$method[lab_rows$any_failures == FALSE], collapse = ', '), 'None')
        missing_methods <- ifelse(length(lab_method_missing_methods$method[lab_method_missing_methods$lab == name]) > 0, paste0(lab_method_missing_methods$method[lab_method_missing_methods$lab == name], collapse = ', '), 'None')

        # recent_sample_collected <- any(lab_rows$PT_test_date >= one_year_ago)
        # recent_status <- ifelse(recent_sample_collected, "Proficiency test reported in last year: <strong>Yes</strong>", "Proficiency test reported in last year: <strong>No</strong>")
        paste("Lab Name: ", name, "<br><br>",
              # recent_status, "<br>",
              "Methods with failed PTs in last year: ", '<strong>', failing_methods, '</strong>', "<br><br>",
              "Methods with missing PTs: ", '<strong>', missing_methods, '</strong>', "<br><br>",
              "Methods with passing PTs in last year: ", '<strong>', passing_methods, '</strong>', "<br>")
      } else {
        paste("Lab Name: ", name, "<br>", '<strong>',
              "No data available", '</strong>')
      }
    })
    # print(marker_popups)

    leaflet(locations()) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = locations()$long,
        lat = locations()$lat,
        color = unname(marker_colors),
        radius = unname(marker_shapes),
        popup = unname(marker_popups) # ~laboratory_name #
      )
  })



  observeEvent(input$map2_marker_click, {
    ### action when a lab is selected in the lab pane
    click <- input$map2_marker_click
    selected_location <- locations()[locations()$lat == click$lat & locations()$long == click$lng, ]
    target_loc <- paste0(selected_location$laboratory_name, ' ', selected_location$laboratory_location)
    # selected_lab_data <- lab_data[(lab_data$laboratory_name == selected_location$laboratory_name) & (lab_data$PT_test_date >= one_year_ago), ] # lab_data[lab_data$laboratory_name == selected_location$laboratory_name, ]
    selected_lab_data <- lab_data_2()[grepl(x = paste0(lab_data_2()$laboratory_name, ' ', lab_data_2()$laboratory_location), pattern = target_loc), ]
    selected_lab_data <- selected_lab_data[order(selected_lab_data$PT_test_date, decreasing = TRUE), ]
    selected_lab_data$category <- method_data()$category[match(selected_lab_data$method, method_data()$common_name)]
    print(head(selected_lab_data$category))

    # if (input$recentYear) { ### not working; table isn't updating
    #   # Filter data to show only the most recent year
    # last_years_data <- selected_lab_data[selected_lab_data$PT_test_date >= one_year_ago, ]
    selected_lab_data <- selected_lab_data[order(selected_lab_data$PT_test_date, decreasing = TRUE), ]
    if(!exists('selected_lab_data')) {
      selected_lab_data <- NA
    } else {
      output$plot2 <- renderPlot({
        # ggplotly(
        ggplot(selected_lab_data, aes(x = PT_test_date, y = PT_result, col  = PT_result)) + # Customize your plot
          facet_wrap(category ~ method) + theme_bw() +
          geom_point() + scale_color_manual(values = c("Fail" = "red", "Pass" = "green")) +
          labs(y = 'Proficiency test result', x = '', col = 'Test result',
               title = paste("Proficiency test results for", target_loc))
        # )
      })
    }
    
    ### attempt to make target location available to markdown report
    # global$target_location(target_loc) #list(lat = click$lat, lon = click$lng, id = click$id)
    # print(global$target_location)
    
    output$table2 <- DT::renderDT({
      selected_lab_data
    })
  })
  
  
  output$contents <- renderTable({
    ### manual file upload.
    ### https://stackoverflow.com/questions/38064038/reading-an-rdata-file-into-shiny-application
    # input$file1 will be NULL initially. After the user selects
    # and uploads a file, head of that data file by default,
    # or all rows if selected, will be shown.
    req(input$file1)
    lab_data <- read.csv(input$file1$datapath)
    files_all <- input$file1
    req(input$file2)
    method_data <- read.csv(input$file2$datapath)
    files_all <- rbind(files_all, input$file2)
    req(input$file3)
    locations <- read.csv(input$file3$datapath)
    files_all <- rbind(files_all, input$file3)

    if(input$disp == "no preview") {
      return('Proficiency test data uploaded.')
    } else if(input$disp == "head") {
      return(head(lab_data))
    }
    else {
      return(lab_data)
    }
    output$files <- renderTable(input$files_all)
  })
  
  facility_data <- reactive({
    ### react to map clicks on the lab-level tab 
    req(input$map2_marker_click)
    click <- input$map2_marker_click
    selected_location <- locations()[locations()$lat == click$lat & locations()$long == click$lng, ]
    target_loc <- paste0(selected_location$laboratory_name, ' ', selected_location$laboratory_location)
    selected_lab_data <- lab_data_2()[grepl(x = paste0(lab_data_2()$laboratory_name, ' ', lab_data_2()$laboratory_location), pattern = target_loc), ]
    selected_lab_data <- selected_lab_data[order(selected_lab_data$PT_test_date, decreasing = TRUE), ]
    selected_lab_data$category <- method_data()$category[match(selected_lab_data$method, method_data()$common_name)]
    selected_lab_data <- selected_lab_data[order(selected_lab_data$PT_test_date, decreasing = TRUE), ]
    return(selected_lab_data)
  })

  
  output$report <- downloadHandler(
    filename = "report.docx",
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "markdown.Rmd")
      file.copy("markdown.Rmd", tempReport, overwrite = TRUE)

      # Set up parameters to pass to Rmd document
      params <- list(facility_list = facility_data()
                       )
      # Knit the document, passing in the `params` list, and eval it in a
      # child of the global environment (this isolates the code in the document
      # from the code in this app).
      rmarkdown::render(tempReport, output_file = file,
                       params = params,
                       envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  output$map_audits <- renderLeaflet({
    ### map for audit summary - color points based on the most immediate audit need.
    ### could filter by analyte category in the future if desired.
    ### lab_summary_tmp[order(lab_summary_tmp$PT_test_date, decreasing = TRUE), ]
    # req(method_data())
    # print(head(method_data()))
    ### TODO: include summary of PT samples (lab_data2())? 
    method_long <- reshape(method_data()[, c(3:ncol(method_data()))], direction = "long",
                              varying = list(names(method_data())[5:ncol(method_data())]),
                              v.names = "value",
                              idvar = c("common_name", "category"),
                              timevar = "lab",
                              times = names(method_data())[5:ncol(method_data())]#,times = 1950:1954
                              )
    method_long$exp_tmp  <- as.numeric(gsub(x = method_long$value, pattern = '[^0-9]', replacement = ''))
    method_long$exp      <- as.Date(strptime(method_long$exp_tmp, format = '%Y%m%d'))
    method_long$type     <- gsub("[^a-zA-Z]", "", method_long$value) #substr(method_long$value, 10, nchar(method_long$value))
    
    ### characterize fiscal year
    ### If the current month is October or later, the fiscal year is the current year plus one.
    ### If the current month is before October, the fiscal year is the current year.
    current_date <- Sys.Date()
    current_fy <- get_fiscal_year(current_date)
    
    method_long$exp_fys  <- sapply(method_long$exp, get_fiscal_year)
    method_long$lab_name <- locations()$full_name[match(method_long$lab, locations()$EPA_ID)]
    
    # print(head(current_date))
    
    ### maybe need to filter most recent PT, in case there was a failure but issues were resolved?
    lab_audit_summary_coarse <- plyr::ddply(method_long, c('lab_name'),
                                     plyr::summarise,
                                     min_exp = min(exp_fys, na.rm = TRUE))

    marker_colors <- sapply(locations()$full_name, function(name) {
      lab_rows <- lab_audit_summary_coarse[(lab_audit_summary_coarse$lab_name == name), ]
      # print(lab_rows)
      if (nrow(lab_rows) > 0) {
        # assign color based on most immediate audit need
        most_recent_row <- lab_rows$min_exp
        if (most_recent_row <= current_fy) { # equal to or less than current year
          return("red")
        } else if (most_recent_row == current_fy + 1) {
          return("yellow")
        } else if ((most_recent_row > current_fy + 1) & is.finite(most_recent_row)) {
          return("green")
        } else if (is.infinite(most_recent_row)) {
          return("gray")  # Default color if no tests are available
        }
      }
      return("gray")  # Default color if no tests are available
    })
    
    # Debugging: Print marker colors and shapes
    # print(marker_colors)
    # print(marker_shapes)
    
    marker_popups <- sapply(locations()$full_name, function(name) {
      ### note any missing or failed PTs
      
      lab_rows <- method_long[(method_long$lab_name == name) & !is.na(method_long$exp_fys), ]
      if (nrow(lab_rows) > 0) {
        ### identify impending audits
        current_yr_audits   <- ifelse(length(lab_rows$common_name[lab_rows$exp_fys == current_fy])   > 0, paste0(lab_rows$common_name[lab_rows$exp_fys == current_fy], collapse = '; '), 'None')
        next_yr_audits      <- ifelse(length(lab_rows$common_name[lab_rows$exp_fys == current_fy+1]) > 0, paste0(lab_rows$common_name[lab_rows$exp_fys == current_fy+1], collapse = '; '), 'None')
        next_next_yr_audits <- ifelse(length(lab_rows$common_name[lab_rows$exp_fys > current_fy+1]) > 0, paste0(lab_rows$common_name[lab_rows$exp_fys > current_fy+1], collapse = '; '), 'None')
        
        # recent_sample_collected <- any(lab_rows$PT_test_date >= one_year_ago)
        # recent_status <- ifelse(recent_sample_collected, "Proficiency test reported in last year: <strong>Yes</strong>", "Proficiency test reported in last year: <strong>No</strong>")
        paste("Lab Name: ", name, "<br><br>",
              # recent_status, "<br>",
              "Methods with certifications expiring in current fiscal year: ", '<strong>', current_yr_audits, '</strong>', "<br><br>",
              "Methods with certifications expiring next fiscal year: ", '<strong>', next_yr_audits, '</strong>', "<br><br>",
              "Methods with certifications expiring after the next fiscal year: ", '<strong>', next_next_yr_audits, '</strong>', "<br>")
      } else {
        paste("Lab Name: ", name, "<br>", '<strong>',
              "No data available", '</strong>')
      }
    })
    # print(marker_popups)
    
    leaflet(locations()) %>%
      addTiles() %>%
      addCircleMarkers(
        lng    = locations()$long,
        lat    = locations()$lat,
        color  = unname(marker_colors), #[match(names(marker_colors), locations()$full_name)],
        radius = 6,
        popup  = unname(marker_popups) # ~laboratory_name #
      )
  })
  
  observeEvent(input$map_audits_marker_click, {
    ### action when a lab is selected in the method pane
    click <- input$map_audits_marker_click
    selected_location <- locations()[locations()$lat == click$lat & locations()$long == click$lng, ]
    # print(selected_location$EPA_ID)
    which_col <- selected_location$EPA_ID
    selected_audit_data <- method_data()[, c("common_name", "category" , which_col)]
    selected_audit_data <- selected_audit_data[nchar(selected_audit_data[, which_col]) > 8,]
    selected_audit_data$exp_tmp  <- as.numeric(gsub(x = selected_audit_data[, ncol(selected_audit_data)], pattern = '[^0-9]', replacement = ''))
    selected_audit_data$expiration_date    <- as.Date(strptime(selected_audit_data$exp_tmp, format = '%Y%m%d'))
    selected_audit_data$current_status     <- gsub("[^a-zA-Z]", "", selected_audit_data[, which_col]) #substr(method_long$value, 10, nchar(method_long$value))
    selected_audit_data$lab_name           <- selected_location$full_name
    selected_audit_data$method_name        <- selected_audit_data$common_name
    selected_audit_data$method_category    <- selected_audit_data$category
    
    # print(selected_audit_data[, 'current_status'] )
    
    # if (input$recentYear) { ### not working; table isn't updating
    #   # Filter data to show only the most recent year
    selected_audit_data <- selected_audit_data[order(selected_audit_data$expiration_date, decreasing = TRUE), ]
    if(!exists('selected_audit_data')) {
      selected_audit_data <- NA
    } 
    
    output$table_audits <- DT::renderDT({
      selected_audit_data[, c("method_name", "method_category", "expiration_date", "current_status", "lab_name")]
    })
  })

}



# Run the application
shinyApp(ui = ui, server = server)
