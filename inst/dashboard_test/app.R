### works! TODO: update data to use uploaded csvs by integrating upload_test2



# ui.R
library(shiny)
library(shinydashboard)
library(leaflet)
library(bslib)
library(plyr)
library(ggplot2)



# prep --------------------------------------------------------------------
load_script <- system.file("extdata", "script_load_data_20250715.R", package = "R8DWLCP")
source(load_script)



# ui ----------------------------------------------------------------------



ui <- page_navbar(
  nav_panel("Summarize by method", "Proficiency test results: Method summaries",
            dashboardPage(
  dashboardHeader(title = ""),
  dashboardSidebar(selectInput("method", "Select Method", choices = unique(method_data$method))#,
                   # checkboxInput("recentYear", "Show data from the most recent year", FALSE)
                   ),
  dashboardBody(
    fluidRow(leafletOutput("map", width = "700px", height = "400px"),
             column(width = 8, DT::DTOutput("table1"))
    ))
)),
nav_panel("Summarize by Laboratory", "Proficiency test results: Lab summaries",
          dashboardPage(
            dashboardHeader(title = ""), dashboardSidebar(disable = TRUE, collapsed = TRUE),
            dashboardBody(
              fluidRow(leafletOutput("map2", width = "700px", height = "400px"),
                       column(width = 10, height = 9, plotOutput("plot2")),
                       column(width = 8, DT::DTOutput("table2"))
              )
            )
          )),
nav_panel("Manual file upload", "Optional file upload menu",
          # Sidebar layout with input and output definitions ----
          sidebarLayout(
            # Main panel for displaying outputs ----
            mainPanel(
              # Output: Data file ----
              tableOutput("contents")
            ),
            # Sidebar panel for inputs ----
            sidebarPanel(
              # Input: Select a lab data file ----
              fileInput("file1", "Browse to CSV file with proficiency test data",
                        multiple = TRUE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")),
              fileInput("file2", "Browse to CSV file with method list",
                        multiple = TRUE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")),
              fileInput("file3", "Browse to CSV file with laboratory locations",
                        multiple = TRUE,
                        accept = c("text/csv",
                                   "text/comma-separated-values,text/plain",
                                   ".csv")),
              tableOutput("files"),
              # Horizontal line ----
              tags$hr(),
              # Input: Select number of rows to display ----
              radioButtons("disp", "Display",
                           choices = c(None = 'no preview',
                                       Head = "head",
                                       All = "all"),
                           selected = "no preview")
            )
          )),
  title = "Region 8 Drinking Water Laboratory Certification Program Dashboard",
  id = "page"
)



server <- function(input, output, session) {
  current_date <- Sys.Date()
  one_year_ago <- current_date - 365

  filtered_lab_data <- reactive({df <- lab_data[trimws(tolower(lab_data$method)) == trimws(tolower(input$method)), ]
  df <- df[order(df$PT_test_date, decreasing = TRUE), ]
  return(df)
  })

  lab_data_2 <- reactive({df <- lab_data
  df <- df[order(df$PT_test_date, decreasing = TRUE), ]
  return(df)
  })



  output$map <- renderLeaflet({
    current_date <- Sys.Date()
    one_year_ago <- current_date - 365

    # Filter lab data based on selected method
    filtered_lab_data <- lab_data[trimws(tolower(lab_data$method)) == trimws(tolower(input$method)), ]
    filtered_lab_data <-  filtered_lab_data[order(filtered_lab_data$PT_test_date, decreasing = TRUE), ]
    # Debugging: Print filtered data
    # print(filtered_lab_data)

    # Determine marker colors and shapes
    marker_shapes <- sapply(locations$Laboratory.Name, function(name) {
      ### large points: no data in the past year for the selected method, concerning.
      lab_rows <- filtered_lab_data[filtered_lab_data$Laboratory.Name == name, ]
      if (nrow(lab_rows) > 0 && any(lab_rows$PT_test_date >= one_year_ago)) {
        return(2)  # shape for recent data
      } else {
        return(10)   # other shape
      }
    })

    marker_colors <- sapply(locations$Laboratory.Name, function(name) {
      lab_rows <- filtered_lab_data[(filtered_lab_data$Laboratory.Name == name) & (filtered_lab_data$PT_test_date >= one_year_ago), ]
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

    marker_popups <- sapply(locations$Laboratory.Name, function(name) {
      # lab_rows <- filtered_lab_data[(filtered_lab_data$Laboratory.Name == name) & (filtered_lab_data$PT_test_date >= one_year_ago), ]
      lab_rows <- filtered_lab_data()[filtered_lab_data()$Laboratory.Name == name & (filtered_lab_data()$PT_test_date >= one_year_ago), ]
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

    leaflet(locations) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = locations$long,
        lat = locations$lat,
        color = unname(marker_colors),
        radius = unname(marker_shapes),
        popup = unname(marker_popups) # ~Laboratory.Name #
      )
  })


  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    selected_location <- locations[locations$lat == click$lat & locations$long == click$lng, ]
    # selected_lab_data <- lab_data[(lab_data$Laboratory.Name == selected_location$Laboratory.Name) & (lab_data$PT_test_date >= one_year_ago), ] # lab_data[lab_data$Laboratory.Name == selected_location$Laboratory.Name, ]
    selected_lab_data <- filtered_lab_data()[grepl(x = filtered_lab_data()$Laboratory.Name, pattern = selected_location$Laboratory.Name), ]
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
    # lab_summary_tmp[order(lab_summary_tmp$PT_test_date, decreasing = TRUE), ]

    lab_summary_tmp <- plyr::ddply(lab_data_2()[lab_data_2()$PT_test_date >= one_year_ago, ], c('Laboratory.Name'),
                                   plyr::summarise,
                                   any_failures = any(grepl(x = tolower(trimws(PT_result)), pattern = 'fail'))
    )
    # print(head(lab_summary_tmp))

    ### maybe need to filter most recent PT, in case there was a failure but issues were resolved?
    lab_method_summary <- plyr::ddply(lab_data_2()[lab_data_2()$PT_test_date >= one_year_ago, ], c('Laboratory.Name', 'method'),
                                      plyr::summarise,
                                      any_failures = any(grepl(x = tolower(trimws(PT_result)), pattern = 'fail'))
    )
    lab_method_summary_yearago <- plyr::ddply(lab_data_2()[(lab_data_2()$PT_test_date <= one_year_ago) & (lab_data_2()$PT_test_date >= (one_year_ago - 365)), ], c('Laboratory.Name', 'method'),
                                              plyr::summarise,
                                              any_failures = any(grepl(x = tolower(trimws(PT_result)), pattern = 'fail'))
    )
    # print(head(lab_method_summary_yearago))

    ### compare last year's PTs (even failing PTs) to this year's to identify any missing methods
    ### which combinations of lab name and method are missing in current year
    current_year_name_methods  <- paste0(lab_method_summary$Laboratory.Name, '_', lab_method_summary$method)
    year_ago_name_methods      <- paste0(lab_method_summary_yearago$Laboratory.Name, '_', lab_method_summary_yearago$method)
    # setdiff(year_ago_name_methods, current_year_name_methods) # methods with PTs from previous year but no PTs this year
    if (length(base::setdiff(year_ago_name_methods, current_year_name_methods)) > 0) {
      lab_method_missing_methods <- data.frame(comb = base::setdiff(year_ago_name_methods, current_year_name_methods), lab = NA, method = NA)
      print(lab_method_missing_methods)
      print('\n3\n')
      lab_method_missing_methods$lab    <- sapply(X = strsplit(lab_method_missing_methods$comb, split = '_'), '[[', 1)
      lab_method_missing_methods$method <- sapply(X = strsplit(lab_method_missing_methods$comb, split = '_'), '[[', 2)
    } else {
      ### if all methods are identical between years, set all to NA.
      lab_method_missing_methods <- data.frame(comb = NA, lab = NA, method = NA)
    }

    # Determine marker colors and shapes
    marker_shapes <- sapply(locations$Laboratory.Name, function(name) {
      ### large points: a failing method in the past year concerning.
      lab_rows <- lab_summary_tmp[lab_summary_tmp$Laboratory.Name == name, ]
      if (nrow(lab_rows) > 0 && any(lab_rows$any_failures == FALSE)) {
        return(2)  # small points: no failing PTs
      } else {
        return(10)   # large points: at least one failing PT
      }
    })

    marker_colors <- sapply(locations$Laboratory.Name, function(name) {
      lab_rows <- lab_summary_tmp[(lab_summary_tmp$Laboratory.Name == name), ]
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

    marker_popups <- sapply(locations$Laboratory.Name, function(name) {
      ### note any missing or failed PTs

      lab_rows <- lab_method_summary[lab_method_summary$Laboratory.Name == name, ]
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

    leaflet(locations) %>%
      addTiles() %>%
      addCircleMarkers(
        lng = locations$long,
        lat = locations$lat,
        color = unname(marker_colors),
        radius = unname(marker_shapes),
        popup = unname(marker_popups) # ~Laboratory.Name #
      )
  })



  observeEvent(input$map2_marker_click, {
    click <- input$map2_marker_click
    selected_location <- locations[locations$lat == click$lat & locations$long == click$lng, ]
    target_loc <- paste0(selected_location$Laboratory.Name, ' ', selected_location$Laboratory.Location..City..State.)
    # selected_lab_data <- lab_data[(lab_data$Laboratory.Name == selected_location$Laboratory.Name) & (lab_data$PT_test_date >= one_year_ago), ] # lab_data[lab_data$Laboratory.Name == selected_location$Laboratory.Name, ]
    selected_lab_data <- lab_data_2()[grepl(x = paste0(lab_data_2()$Laboratory.Name, ' ', lab_data_2()$Laboratory.Location..City..State.), pattern = target_loc), ]
    selected_lab_data <- selected_lab_data[order(selected_lab_data$PT_test_date, decreasing = TRUE), ]
    selected_lab_data$category <- method_data$category[match(selected_lab_data$method, method_data$method)]

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
    # output$data <- list(lab_data    = lab_data,
    #                method_data = method_data,
    #                locations   = locations
    #                )
    # output$files <- renderTable(list(lab_data    = lab_data,
    #                                                 method_data = method_data,
    #                                                 locations   = locations
    #                                                 ))
  })

}



# Run the application
shinyApp(ui = ui, server = server)
