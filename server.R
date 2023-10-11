#### SERVER.R ####
# Define the server logic
server <- function(input, output, session) {
  current_error_type <- reactiveVal("All")
  desired_execution_id <- reactiveVal(NULL)
  
  # Function to get the list of files (to be used by reactivePoll)
  getDirList <- function() {
    bundles_summary_directory <- paste0(data_directory, "support-bundle-summaries/")
    list.files(path = bundles_summary_directory)
  }
  
  # Create a reactivePoll that checks the list of files every 1 second (1000 ms)
  file_list <- reactivePoll(1000, session,
                            checkFunc = function() {
                              # Use digest to create a hash that will change if the file list changes
                              digest::digest(getDirList())
                            },
                            valueFunc = function() {
                              # Return the current list of files
                              getDirList()
                            }
  )
  
  # Create a reactivePoll that checks the list of files every 1 second (1000 ms)
  fileList <- reactivePoll(1000, session,
                           checkFunc = function() {
                             # Use digest to create a hash that will change if the file list changes
                             digest::digest(getDirList())
                           },
                           valueFunc = function() {
                             # Return the current list of files
                             getDirList()
                           }
  )
  
  ##### RESOURCE USAGE REPORT ####
  reactiveDate <- shiny::reactive({
    invalidateLater(1000 * 60 * 60) # Check each hour for a date update (since application will only open once!)
    Sys.Date()
  })
  
  output$generate_report_btn_ui <- shiny::renderUI({
    shiny::actionButton("generate_report_btn", 
                        "Generate Report", 
                        style = "display: block; width: 100%; color: #fff; background-color: #337ab7;")
  })
  
  output$date_range_ui <- shiny::renderUI({
    if(!is.null(reactiveDate())) {
      shiny::dateRangeInput(inputId = "date_range_select",
                            label = "Date Range",
                            start = as.Date(reactiveDate() - lubridate::days(7)),
                            end = as.Date(reactiveDate()),
                            width = "100%",
                            min = as.Date('2020-01-01'),
                            max = as.Date(reactiveDate())
      )
    }
  })
  
  output$status_multiselect_ui <- shiny::renderUI({
    if(!is.null(usage_report_filters)) {
      div(class = "shiny-select-spacing",
          shiny::selectizeInput(inputId = "status_multiselect",
                                label = "Workload Status",
                                choices = c("All", setdiff(unlist(usage_report_filters$status), NA)),
                                selected = c("Error", "Failed"),
                                multiple = TRUE)
      )
    }
  })
  
  output$hardware_tier_multiselect_ui <- shiny::renderUI({
    if(!is.null(usage_report_filters)) {
      div(class = "shiny-select-spacing",
          shiny::selectizeInput(inputId = "hardware_tier_multiselect",
                                label = "Hardware Tier",
                                choices = c("All", setdiff(unlist(usage_report_filters$hardware_tier), NA)),
                                selected = "All",
                                multiple = TRUE)
      )
    }
  })
  
  ##### Initially hidden filters: these will stay hidden until the initial report is updated.
  # The idea here is that the user will generate a report and then include all users for that relevant timestep.
  # To prevent a bunch of API pulls, there will only be a complete rerendering if the date select range is changed
  output$starting_user_multiselect_ui <- shiny::renderUI({
    if(!is.null(report_df0())) {
      div(class = "shiny-select-spacing",
          shiny::selectizeInput(inputId = "starting_user_multiselect",
                                label = "User",
                                choices = c("All", setdiff(report_df0()$starting_user, NA)),
                                selected = "All",
                                multiple = TRUE)
      )
    }
  })
  
  output$project_name_multiselect_ui <- shiny::renderUI({
    if(!is.null(report_df0())) {
      div(class = "shiny-select-spacing",
          shiny::selectizeInput(inputId = "project_name_multiselect",
                                label = "Project Name",
                                choices = c("All", setdiff(report_df0()$project_name, NA)),
                                selected = "All",
                                multiple = TRUE)
      )
    }
  })
  
  output$target_col_select_ui <- shiny::renderUI({
    if(!is.null(report_df0())) {
      target_cols_choices <- names(report_df0())[sapply(report_df0(), function(col) length(unique(col)) <= 20 & !is.numeric(col))]
      names(target_cols_choices) <- lapply(target_cols_choices, function(x) toTitleCase(gsub("_", " ", x))) %>% stats::setNames(target_cols_choices)
      div(class = "shiny-select-spacing",
          shiny::selectizeInput(inputId = "target_col_select",
                                label = "Secondary Category",
                                choices = target_cols_choices,
                                selected = "status",
                                multiple = FALSE)
      )
    }
  })
  
  output$usage_report_filters <- shiny::renderUI({
    list(
      shiny::uiOutput("date_range_ui"),
      shiny::uiOutput("status_multiselect_ui"),
      shiny::uiOutput("hardware_tier_multiselect_ui"),
      shiny::uiOutput("starting_user_multiselect_ui"),
      shiny::uiOutput("project_name_multiselect_ui"),
      shiny::uiOutput("target_col_select_ui"),
      shiny::HTML("<br>"),
      shiny::fluidRow(
        column(10, shiny::uiOutput("generate_report_btn_ui")),
      )
    )
  })
  
  
  usage_filters <- shiny::reactiveValues(show_contents=FALSE)
  
  output$show_contents <- reactive({usage_filters$show_contents})
  outputOptions(output, "show_contents", suspendWhenHidden = FALSE)
  
  ##### RESOURCE USAGE REPORT FILTERS #####
  # Observes the clicking of the "generate report" button. Can be seen as a "gatekeeper" to hold off the update of 
  # filters until the user selects "generate report".
  
  shiny::observeEvent(input$generate_report_btn, {
      if(usage_filters$show_contents == FALSE) {
        usage_filters$show_contents = TRUE
        usage_filters$start_date <- input$date_range_select[1]
        usage_filters$end_date <- input$date_range_select[2]
        usage_filters$status <- input$status_multiselect
        usage_filters$hardware_tier <- input$hardware_tier_multiselect
        usage_filters$starting_user <- "All"
        usage_filters$project_name <- "All"
        usage_filters$target_col <- "status"
        #cat("DATE RANGE SELECTED!", paste(usage_filters$start_date, usage_filters$end_date), "\n")
        shinyjs::addClass(id="generate_report_btn", class="grayed-out-button", asis=TRUE)
        #cat("Updated the button\n")
        #browser()
      } else {
        usage_filters$start_date <- input$date_range_select[1]
        usage_filters$end_date <- input$date_range_select[2]
        usage_filters$status <- input$status_multiselect
        usage_filters$hardware_tier <- input$hardware_tier_multiselect
        usage_filters$starting_user <- input$starting_user_multiselect
        usage_filters$project_name <- input$project_name_multiselect
        usage_filters$target_col <- input$target_col_select
        shinyjs::addClass(id="generate_report_btn", class="grayed-out-button", asis=TRUE)
      }
      # usage_report_filters <- base::readRDS('./data/usage_report_filter_options.rds')
      output$bundle_error_count <- renderUI({
        fluidRow(
          bs4Dash::bs4Card(
            width = 12, # Maximum width to fill the container
            maximizable = TRUE,
            # Action Buttons
              fluidRow(
                align = "center", 
                column(
                  width = 6,
                  actionButton("analyze_support_bundles", 
                               "Analyze Support Bundles",
                               style = "display: block; width: 100%; color: #fff; background-color: #337ab7;",
                               width = "100%")
                ),
                column(
                  width = 6, 
                  actionButton("reset_category_filter", 
                               "Reset Secondary Category Filter",
                               style = "display: block; width: 100%; color: #fff; background-color: #337ab7;",
                               width = "100%")
                )
              ),
            fluidRow(
              align = "center",
              column(
                width = 12,
                div(class = "inline-radio",
                    shiny::radioButtons("use_cached_analysis", 
                                        "",
                                        choices = c("Use Cached Analytics"=TRUE, "Recreate Error Analysis"=FALSE))
                )
              )
            ),
            HTML("<br>"),
            # DTOutput
            fluidRow(width=12,
                     align = "center",
                     DT::DTOutput("report_render"),
                     HTML("<br>"),
                     shiny:: downloadButton("report_render_downloader", "Download")
            )
          )
        )
      })
  }, ignoreInit = TRUE)
  
  observeEvent(c(input$date_range_select, 
                 input$hardware_tier_multiselect, 
                 input$status_multiselect, 
                 input$starting_user_multiselect, 
                 input$project_name_multiselect, 
                 input$target_col_select), {
                   shinyjs::removeClass(id="generate_report_btn", class="grayed-out-button", asis=TRUE)
                 })
  
  ##### RESOURCE USAGE REPORT DATA #####
  report_df0 <- shiny::reactive({
    if (!is.null(usage_filters$start_date) & !is.null(usage_filters$end_date)) {
      # Create a list of dates
      #browser()
      date_list <- seq(usage_filters$start_date, usage_filters$end_date, by="days")
      
      # Identify all which already exist in the directory "{dataset_path}/resource-usage-by-day"
        # Import and upload all that exist
      existing_dates <- list.files(paste0(data_directory, "resource-usage-by-day/"))
      
      #existing_dates <- c("2023-09-29.csv", "2023-09-30.csv", "2023-10-02.csv")
      existing_dates <- stringi::stri_extract(existing_dates, regex="[0-9]{4}-[0-9]{2}-[0-9]{2}(?=\\.csv)")
      
      # Take a setdiff to find dates which need to be downloaded
      dates_to_download <- setdiff(as.character(date_list), existing_dates)
      
      # Create a list of start/end dates to minimize redundant data pulled for API
      date_vector <- as.Date(dates_to_download, format = "%Y-%m-%d")
      
      # Calculate differences between dates
      diff_dates <- c(0, diff(date_vector))
      
      # Split the vector based on consecutive days
      split_idx <- cumsum(diff_dates != 1)
      
      # Split date_vector based on split_idx
      date_lists <- split(date_vector, split_idx)
      date_lists <- lapply(date_lists, function(date) {
        if(length(date) == 1){
          return (rep(date, 2))
        } else {
          return(date)
        }
      })
      ##### Loop through each date range, pulling from the API #####
      report_df_api <- data.frame()
      
      # Edge case: The above returns a list of length 1 with an empty Date entry (registers as length 0) when no dates are found
      if(length(date_lists[[1]]) > 0) {
        for (idx in seq_along(date_lists)) {
          date_range <- c(date_lists[[idx]][1], date_lists[[idx]][length(date_lists[[idx]])])
          report_df_slice <- download_usage_report_from_api(date_range)
          report_df_api <- rbind(report_df_api, report_df_slice)
        }
      }
      
      ##### Feature Engineering #####
      if(nrow(report_df_api) > 0){
        report_df_api <- resource_usage_feature_engineering_pipeline(report_df_api)
      }
      
      # Don't really know what to do with faulty dates so I'm just gonna toss for now (very few occurences)
      report_df_api <- report_df_api[which(report_df_api$date %in% date_list),]
      
      ##### Read in cached usage reports #####
      report_df_csv <- lapply(existing_dates, function(date) {
        date_csv <- read.csv(paste0(data_directory, "resource-usage-by-day/", date, ".csv"))
      }) %>% do.call(rbind, .)
      
      if(!is.null(report_df_csv)) {
        if(length(setdiff(colnames(report_df_api), colnames(report_df_csv))) > 0){
          report_df_csv <- resource_usage_feature_engineering_pipeline(report_df_csv)
        }
      }
      
      # Save newly found dates into csv format by day
      date_splits <- split(report_df_api, report_df_api$date)
      lapply(seq_along(date_splits), function(idx) {
        date_df <- date_splits[[idx]]
        target_file_path <- paste0(data_directory, "resource-usage-by-day/", unique(date_df$date), ".csv")
        
        # Edge case: newest download of from API gives us new information (going to usually be on most recent day)
        # Issue: In saving this, data future pulls of the API will mistakenly be looked over as there is already a csv
        # Solution: Potentially hash each row and use as key? We're then getting into DB territory...
        # if(file.exists(target_file_path)) {
        #   existing_file <- read.csv(target_file_path)
        #   new_files_found <- setdiff(date_df$run_id, existing_file$run_id)
        #   if(length(new_files_found) > 0) {
        #     date_df <- rbind(existing_file, date_df[which(date_df$run_id %in% new_files_found),])
        #   }
        # }
        
        write.csv(date_df, paste0(data_directory, "resource-usage-by-day/", unique(date_df$date), ".csv"), row.names = FALSE)
      })
      
      # Bind the results together (if there was any cached data)
      if(!is.null(report_df_csv)) {
        report_df_api <- rbind(report_df_csv, report_df_api)
      }
      
      # Chuck out any weird data and order by date
      report_df_api <- report_df_api[order(report_df_api$date),]
      report_df_api <- report_df_api[which(report_df_api$date %in% date_list),]
      
      
      # Convert continuous variable "time_to_boot_s" and transform into categorical variable of form "Xs to Ys"
      # This is NOT in feature engineering function as this is not a mapped feature (changes depending on dataset)
      n <- 5
      breaks_range <- cut(report_df_api$time_to_boot_s, breaks=n, include.lowest=TRUE)
      no_brackets <- gsub("\\[|\\]", "", breaks_range)
      no_parenthesis <- gsub("\\(|\\)", "", no_brackets)
      no_negative_time <- gsub("-[0-9]+\\.[0-9]+", "0", no_parenthesis)
      # 2. Split string at comma
      entry <- no_negative_time[1]
      output_string <- lapply(no_negative_time, function(entry) {
        numbers <- strsplit(entry, ",")[[1]]
        modified_numbers <- paste0(numbers, "s")
        output_string <- paste(modified_numbers[1], "to", modified_numbers[2])
      }) %>% unlist()
      report_df_api$time_to_boot <- output_string
      
      
      
      return(report_df_api)
    }
  })
  
  report_df <- shiny::reactive({
    if(!is.null(report_df0())) {
      report_df <- report_df0()
      #browser()
      
      if(!is.null(usage_filters$status) & !("All" %in% usage_filters$status)) {
        report_df <- report_df[which(report_df$status %in% usage_filters$status),]
      }
      if(!is.null(usage_filters$hardware_tier) & !("All" %in% usage_filters$hardware_tier)) {
        report_df <- report_df[which(report_df$hardware_tier %in% usage_filters$hardware_tier),]
      }
      if(!is.null(usage_filters$starting_user) & !("All" %in% usage_filters$starting_user)) {
        report_df <- report_df[which(report_df$starting_user %in% usage_filters$starting_user),]
      }
      if(!is.null(usage_filters$project_name) & !("All" %in% usage_filters$project_name)) {
        report_df <- report_df[which(report_df$project_name %in% usage_filters$project_name),]
      }
      
      # Recalculate the "time to boot." We want this to be done each time the filters are set since
      # the bins change depending on report_df subset. The final target_col conditional is done after
      # so the newest calculations of "time_to_boot" is used.
      n <- 3
      #summary(report_df$time_to_boot_s)
      #breaks <- seq(min(report_df$time_to_boot_s), max(report_df$time_to_boot_s), length.out = n + 1)
      #browser()
      Q1 <- quantile(report_df$time_to_boot_s, 0.25)
      Q3 <- quantile(report_df$time_to_boot_s, 0.75)
      #IQR <- Q3 - Q1
      Lower_Bound <- min(report_df$time_to_boot_s)
      Upper_Bound <- max(report_df$time_to_boot_s)
      
      # Evenly spaced bins between Q1 and Q3
      bins <- seq(Q1, Q3, length.out = n + 1)
      
      # Include outliers bins
      breaks <- unique(unname(c(Lower_Bound, bins, Upper_Bound)))
      
      # Bin the data using the defined breaks
      breaks_range <- cut(report_df$time_to_boot_s, breaks=breaks, include.lowest=TRUE, right=FALSE)
      no_brackets <- gsub("\\[|\\]", "", breaks_range)
      no_parenthesis <- gsub("\\(|\\)", "", no_brackets)
      no_negative_time <- gsub("-[0-9]+\\.[0-9]+", "0", no_parenthesis)
      # 2. Split string at comma
      entry <- no_negative_time[1]
      output_string <- lapply(no_negative_time, function(entry) {
        numbers <- strsplit(entry, ",")[[1]]
        modified_numbers <- sapply(numbers, function(x) {
          base::ifelse(as.numeric(x) >= 60, paste0(as.character(base::round(as.numeric(x)/60, 1)), "min"), paste0(x, "s"))
        })
        #modified_numbers <- paste0(numbers, "s")
        output_string <- paste(modified_numbers[1], "to", modified_numbers[2])
      }) %>% unlist()
      report_df$time_to_boot <- output_string
      
      
      if(!is.null(usage_filters$target_col)) {
        report_df$target_col <- report_df[[usage_filters$target_col]]
      }

      return(report_df)
    }
  })
  
  output$report_render <- DT::renderDT({
      #browser()
      df <- report_df()
      df <- df[, which(!(colnames(df) %in% c("date", "target_col")))]
      first_cols <- c("run_id", "starting_user", "project_name", usage_filters$target_col)
      last_cols <- setdiff(colnames(df), first_cols)
      df <- df[,c(first_cols, last_cols)]
      colnames(df) <- gsub("_", " ", colnames(df)) %>% tools::toTitleCase()
      colnames(df)
      DT::datatable(df,
                    rownames = FALSE, 
                    # class = "compact",
                    selection = list(target = 'row', selected='indices'),
                    options = list(
                      scrollX = TRUE,
                      sScrollY = '75vh',
                      scrollCollapse = TRUE,
                      searching = TRUE,
                      paging = TRUE,
                      pageLength=50,
                      lengthMenu=c(10,20,50,100),
                      bInfo=TRUE,
                      autoWidth=TRUE
                    ),
                    extensions = list("Scroller")
      )
  })
  
  output$report_render_downloader <- downloadHandler(
    filename = function() {
      paste0("resource-usage-report-captured-",Sys.Date(),".csv")
    },
    content = function(con) {
      if(!is.null(report_df())) {
        df <- report_df()
        colnames(df) <- gsub("_", " ", colnames(df)) %>% tools::toTitleCase()
        write.csv(df, con, row.names = FALSE)
        #return(df)
      }
    }
  )
  
  ##### VISUALS/AGGREGATION TABLES #####
  # Donut Chart of Run Types (Batch, Endpoint, Notebook)
  report_subset_agg_dt <- shiny::reactive({
    if(!is.null(report_df())) {
      #browser()
      df <- data.table(report_df())
      
      known_counts <- table(df$target_col)
      known_counts["unknown"] <- nrow(df) - sum(known_counts)
      #browser()
      report_subset_df <- data.frame(
        target_col = names(known_counts),
        count = as.vector(known_counts)
      )
      
      report_subset_df <- report_subset_df[which(report_subset_df$count > 0),]
      report_subset_df$color <- viridis::viridis(nrow(report_subset_df))
      report_subset_df$perc <- base::round(report_subset_df$count / sum(report_subset_df$count)*100, 1)
      return(report_subset_df)
    }
  })
  
  output$run_type_donut_chart <- highcharter::renderHighchart({
    if(!is.null(report_subset_agg_dt()) & !is.null(usage_filters$target_col)) {
      #browser()
      highchart() %>%
        hc_tooltip(formatter= JS("function () { return '<b>' + this.point.target_col + '</b> ' +
                                     ' <br /> Proportion: ' + this.point.perc + '%' +
                                     ' <br /> Count: ' + this.point.count
                                                                  ;}")) %>%
        highcharter::hc_add_series(type = "pie", data = report_subset_agg_dt(), hcaes(x=target_col, y=perc, color=color), size = "80%", name = "Percentage:", 
                                   innerSize = "60%", id = 'versions') %>%
        hc_title(text=HTML(paste0(tools::toTitleCase(gsub("_", " ", usage_filters$target_col))))) %>%
        hc_subtitle(text = paste0(format(usage_filters$start_date, "%B %d, %Y"), "-", format(usage_filters$end_date, "%B %d, %Y"))) %>%
        highcharter::hc_plotOptions(
          pie=list(
            dataLabels=list(
              enabled=TRUE, 
              style=list(
                fontSize="1.4em"), 
              formatter = JS("function () {
                             // display only if larger than 1
                            return this.y > 1 ? '<b>' + this.point.name + ':</b> ' +
                            this.y + '%' : null; }"))),
          series = list(
            cursor = "pointer",
            point = list(
              events = list(
                click = JS("function() {
                                            Shiny.setInputValue('clicked_target_col_val_donut', this.target_col);
                                        }"
                )
              )
            )
          )
        )
      
    }
  })
  
  # shiny::observeEvent(input$clicked_target_col_val_donut, {
  #   cat("CLICKED!!! ", input$clicked_target_col_val_donut, "\n")
  # })
  
  # Time series of running processes
  usage_time_series_df <- shiny::reactive({
    if(!is.null(report_df())){
      df <- report_df()
      df$started_time[which(is.na(df$started_time) | df$started_time == "")] <- df$queued_time[which(is.na(df$started_time) | df$started_time == "")]
      df$started_time_hour <- lubridate::ymd_hms(df$started_time) %>% floor_date(., unit="hour")
      df$completed_time_hour <- lubridate::ymd_hms(df$completed_time) %>% ceiling_date(., unit="hour")
      df$time_diff <- df$completed_time_hour - df$started_time_hour
      # df$run_type <- base::ifelse(
      #                df$is_batch, "batch",
      #                   base::ifelse(df$is_notebook, "notebook",
      #                                   base::ifelse(df$is_endpoint, "endpoint", "unknown"))
      #                 )
      
      dt <- data.table(df)
      
      start_time_agg <- dt[, .(start_count = .N), by=.(started_time_hour, target_col)]
      complete_time_agg <- dt[, .(complete_count = .N), by=.(completed_time_hour, target_col)]
      date_time <- seq(min(na.omit(df$started_time_hour)), max(na.omit(df$completed_time_hour)), by="1 hour")
      
      target_col_unique <- as.vector(unname(unique(df$target_col)))
      
      #run_type <- unique(df$run_type)
      out <- data.table::CJ(date_time, target_col_unique)
      
      out <- data.table::merge.data.table(out, start_time_agg, by.x=c("target_col_unique", "date_time"), by.y=c("target_col", "started_time_hour"), all.x = TRUE)
      out <- data.table::merge.data.table(out, complete_time_agg, by.x=c("target_col_unique", "date_time"), by.y=c("target_col", "completed_time_hour"), all.x = TRUE)
      out[is.na(out)] <- 0
      
      target_col_partition <- split(out, out$target_col_unique)
      idx <- 1
      new_out <- lapply(seq_along(target_col_partition), function(idx) {
        out_subset <- target_col_partition[[idx]]
        out_subset$start_count_cum <- cumsum(out_subset$start_count)
        out_subset$end_count_cum <- cumsum(out_subset$complete_count)
        out_subset$running_during_timestamp <- out_subset$start_count_cum - out_subset$end_count_cum
        
        return(out_subset)
      }) %>% do.call(rbind, .)
      
      validate(
        need(try(length(which(new_out$running_during_timestamp < 0)) > 0), "Oops! Something went wrong with the calculations.")
      )
      
      new_out$timestamp <- as.numeric(new_out$date_time) * 1000
      new_out$date_time <- as.POSIXct(new_out$date_time, origin="1970-01-01", tz="UTC")
      
      return(new_out)
    }
  })
  
  output$rolling_count_running_processes_time_series <- highcharter::renderHighchart({
    if(!is.null(usage_time_series_df())) {
      df <- usage_time_series_df()
      
      hc <- df %>%
        hchart(
          "area",
          hcaes(x=timestamp, y=running_during_timestamp, group=target_col_unique, opacity=0.7),
          stacking="normal"
        ) %>%
        hc_colors(viridis::viridis(length(unique(df$target_col_unique)))) %>%
        hc_title(text=paste0("Historical Runtimes")) %>%
        hc_xAxis(title=list(text='Date'),
                 type ="datetime", 
                 labels = list(
                   format = '{value:%Y-%b-%e<br> %H:%M}'
                 )
        ) %>%
        hc_yAxis(title=list(text='Running Processes')) %>%
        hc_chart(
          zoomType = "xy") %>%
        hc_plotOptions(
          series = list(
            cursor = "pointer",
            point = list(
              events = list(
                click = JS("function() {
                  Shiny.setInputValue('clicked_target_col_val', this.target_col_unique);
                  Shiny.setInputValue('clicked_target_col_date', this.timestamp)
                }")
              )
            )
          )
        )
      
      return(hc)
    }
  })
  
  ##### FILTERING VIA CHART CLICKS #####
  # Every time a category is selected on the charts, the following code captures that category and filters
  # the usage report datatable render via proxy
  last_update <- shiny::reactiveVal("none")
  
  shiny::observeEvent(c(input$clicked_target_col_val, input$clicked_target_col_date), {
    if(!is.null(input$clicked_target_col_val)) {
      
      #cat("Made it into the clicked_target_col_val for the data table proxy\n")
      proxy <- dataTableProxy("report_render")
      #browser()
      #cat("Selected Date:", format(as.POSIXct(input$clicked_target_col_date/1000), format="%Y-%m-%dT%H:%M:%OS3Z"), "\n")
      target_timestamp <- input$clicked_target_col_date
      df <- report_df()
      df <- df[which(df$target_col == input$clicked_target_col_val),]
      
      df$started_timestamp_hour <- as.numeric(lubridate::ymd_hms(df$started_time) %>% floor_date(., unit="hour"))*1000
      df$completed_timestamp_hour <- as.numeric(lubridate::ymd_hms(df$completed_time) %>% ceiling_date(., unit="hour"))*1000
      df <- df[which(df$started_timestamp_hour <= target_timestamp & df$completed_timestamp_hour >= target_timestamp),]
      
      df$started_timestamp_hour <- NULL
      df$completed_timestamp_hour <- NULL
      rownames(df) <- NULL
      df <- df[, which(!(colnames(df) %in% c("date", "target_col")))]
      first_cols <- c("run_id", "starting_user", "project_name", usage_filters$target_col)
      last_cols <- setdiff(colnames(df), first_cols)
      df <- df[,c(first_cols, last_cols)]
      colnames(df) <- gsub("_", " ", colnames(df)) %>% tools::toTitleCase()
      
      replaceData(proxy, df, resetPaging = FALSE, rownames=FALSE)
      
      last_update("time_series")
    }
  })
  
  shiny::observeEvent(input$clicked_target_col_val_donut, {
    if(!is.null(input$clicked_target_col_val_donut)) {
      #browser()
      cat("Made it into the clicked_target_col_val_donut for the data table proxy\n")
      proxy <- dataTableProxy("report_render")
      df <- report_df()
      df <- df[which(df$target_col == input$clicked_target_col_val_donut),]
      #rownames(df) <- NULL
      df <- df[, which(!(colnames(df) %in% c("date", "target_col")))]
      first_cols <- c("run_id", "starting_user", "project_name", usage_filters$target_col)
      last_cols <- setdiff(colnames(df), first_cols)
      df <- df[,c(first_cols, last_cols)]
      colnames(df) <- gsub("_", " ", colnames(df)) %>% tools::toTitleCase()
      
      replaceData(proxy, df, resetPaging = FALSE, rownames=FALSE)
      
      last_update("donut")
    }
  })
  
  shiny::observeEvent(input$reset_category_filter, {
    proxy <- dataTableProxy("report_render")
    #browser()
    df <- report_df()
    df <- df[, which(!(colnames(df) %in% c("date", "target_col")))]
    first_cols <- c("run_id", "starting_user", "project_name", usage_filters$target_col)
    last_cols <- setdiff(colnames(df), first_cols)
    df <- df[,c(first_cols, last_cols)]
    colnames(df) <- gsub("_", " ", colnames(df)) %>% tools::toTitleCase()
    #colnames(df) <- gsub("_", " ", colnames(df)) %>% tools::toTitleCase()
    replaceData(proxy, df, resetPaging = FALSE, rownames=FALSE)
    
    # Created for the edge case that the same category is clicked after the reset (since rerender is reactive on a DIFFERENT value being taken on by last_update)
    last_update("none")
    
    proxy <- highcharter::highchartProxy("rolling_count_running_processes_time_series")
    hcpxy_redraw(proxy)
    runjs("Shiny.setInputValue('clicked_target_col_val', null);")
    runjs("Shiny.setInputValue('clicked_target_col_date', null);")
    runjs("Shiny.setInputValue('clicked_target_col_val_donut', null);")
  })
  
  metadata_df <- reactiveVal(NULL)
  
  selected_execution_ids <- reactiveVal(NULL)
  support_button_counter <- reactiveVal(0)
  shiny::observeEvent(input$analyze_support_bundles, {
    # Update the uiOutput
    #cat("This is what the support bundle click is: ", support_button_counter(), "\n")
    output$bundle_error_count <- renderUI({
      if (support_button_counter()  > 0 & !is.null(execution_ids_no_errors())) {  # Adjust the condition as per your needs
        fluidRow(
          bs4Dash::bs4Card(
            maximizable = TRUE,
            width = 6, # Maximum width to fill the container
            # Action Buttons
            fluidRow(
              align = "center", 
              column(
                width = 6, 
                actionButton("analyze_support_bundles",
                             "Analyze Support Bundles", 
                             style = "display: block; width: 100%; color: #fff; background-color: #337ab7;",
                             width = "100%")
              ),
              column(
                width = 6, 
                actionButton("reset_category_filter", 
                             "Reset Secondary Category Filter", 
                             style = "display: block; width: 100%; color: #fff; background-color: #337ab7;",
                             width = "100%")
              )
            ),
            fluidRow(
              align = "center",
              column(
                width = 12,
                div(class = "inline-radio",
                    shiny::radioButtons("use_cached_analysis", 
                                        "",
                                        choices = c("Use Cached Analytics"=TRUE, "Recreate Error Analysis"=FALSE))
                )
              )
            ),
            HTML("<br>"),
            # DTOutput
            fluidRow(width=10,
                     align = "center",
                     DT::DTOutput("report_render", width="95%"),
                     HTML("<br>"),
                     shiny:: downloadButton("report_render_downloader", "Download")
            )
          ),
          bs4Dash::bs4TabCard(
            maximizable = TRUE,
            width= 6,
            #style = "width: 50%; margin-left: auto; margin-right: auto;",
            shiny::tabPanel("Summary", 
                            shiny::uiOutput("executions_with_errors_progress_bar"),
                            br(),
                            shinycssloaders::withSpinner(highcharter::highchartOutput(height = "600px", "error_summary_bar_chart")),
                            br(),
                            textAreaInput("listBox", "Execution Ids without Found Errors", value = execution_ids_no_errors(), rows = length(unlist(execution_ids_no_errors()))+1)),
            shiny::tabPanel(height = "600px", "Data", 
                            DT::DTOutput("error_render"),
                            HTML("<br>"),
                            shiny:: downloadButton("error_render_downloader", "Download")
                            ),
            shiny::tabPanel("Context", htmlOutput("error_row_contents"))
          )
        )
      } else {
        fluidRow(
          bs4Dash::bs4Card(
            width = 12, # Maximum width to fill the container
            maximizable=TRUE,
            # DTOutput
            # Action Buttons
            fluidRow(
              align = "center", 
              column(
                width = 6, 
                actionButton("analyze_support_bundles", 
                             "Analyze Support Bundles",
                             style = "display: block; width: 100%; color: #fff; background-color: #337ab7;",
                             width = "100%")
              ),
              column(
                width = 6, 
                actionButton("reset_category_filter", 
                             "Reset Secondary Category Filter",
                             style = "display: block; width: 100%; color: #fff; background-color: #337ab7;",
                             width = "100%")
              )
            ),
            fluidRow(
              align = "center",
              column(
                width = 12,
                div(class = "inline-radio",
                    shiny::radioButtons("use_cached_analysis", 
                                        "",
                                        choices = c("Use Cached Analytics"=TRUE, "Recreate Error Analysis"=FALSE))
                )
              )
            ),
            fluidRow(width=10,
                     align = "center",
                     DT::DTOutput("report_render", width="95%"),
                     HTML("<br>"),
                     shiny:: downloadButton("report_render_downloader", "Download")
            )
          )
        )
      }
    })
    
    support_button_counter(support_button_counter() + 1)
    
    
    #proxy <- dataTableProxy("report_render")
    # selectRows(proxy = proxy,
    #            selected = input$table_rows_current)
    if(last_update() == "donut") {
      row_subset <- report_df()[which(report_df()$target_col == input$clicked_target_col_val_donut),]
    } else if (last_update() == "time_series") {
      row_subset <- report_df()
      row_subset <- row_subset[which(row_subset$target_col == input$clicked_target_col_val),]
      
      target_timestamp <- input$clicked_target_col_date
      row_subset$started_timestamp_hour <- as.numeric(lubridate::ymd_hms(row_subset$started_time) %>% floor_date(., unit="hour"))*1000
      row_subset$completed_timestamp_hour <- as.numeric(lubridate::ymd_hms(row_subset$completed_time) %>% ceiling_date(., unit="hour"))*1000
      row_subset <- row_subset[which(row_subset$started_timestamp_hour <= target_timestamp & row_subset$completed_timestamp_hour >= target_timestamp),]
    } else if (last_update() == "none") {
      row_subset <- report_df()
    }
    #browser()
    target_rows <- input$report_render_rows_selected
    target_rows <- target_rows[which(!is.na(sapply(target_rows, as.numeric)))] %>% as.numeric()
    if(length(target_rows) == 0) {
      selected_execution_ids(unique(row_subset$run_id))
    } else {
      selected_execution_ids(unique(row_subset$run_id[target_rows]))
    }
    download_list <- selected_execution_ids()
    
    
    # if(length(download_list) == 0) {
    #   download_list <- unique(row_subset$run_id)
    # }
    #cat("DOWNLOAD RUN ID LIST: ", paste0(download_list, collapse=", "), "\n")
    
    output_df <- data.frame()
    # Now loop through each id, pull execution file, perform summary analysis
    withProgress(message='Downloading and Extracting Bundle Summaries', detail="Please wait...", value=0, {
      execution_id <- download_list[1]
      for (execution_id in download_list) {
        # Done so that bundle summaries aren't recalculated
        summary_directory <- paste0(data_directory, "support-bundle-summaries")
        summary_file <- paste0(summary_directory, "/", execution_id, "-summary.csv")
        if(file.exists(summary_file) & input$use_cached_analysis == TRUE) {
          #browser()
          metadata_errors <- read.csv(summary_file)
          if(nrow(metadata_errors) > 0) {
            metadata_errors$execution_id <- execution_id
            output_df <- rbind(output_df, metadata_errors)
          }
          next
        }
        zip_url <- paste0("https://", domino_url, "/v4/admin/supportbundle/", execution_id)
        
        bundle_root_directory <- paste0(data_directory, "support-bundles")
        if(!dir.exists(bundle_root_directory)) {
          dir.create(bundle_root_directory)
        }
        
        bundle_path <- paste0(bundle_root_directory, "/support-bundle-", execution_id)
        if (!dir.exists(bundle_path)) {
          dir.create(bundle_path)
        } else {
          unlink(paste0(bundle_path, "/*"), recursive = TRUE, force = TRUE)
        }
        
        headers <- c("X-Domino-Api-Key" = domino_user_api_key)
        #response <- httr::GET(zip_url, add_headers(headers))
        
        # Define the path to save the ZIP file
        zip_file_path <- paste0(bundle_path, ".zip")  # Replace with the desired path
        # Make the GET request and save the ZIP file
        response <- GET(zip_url, add_headers(headers), write_disk(zip_file_path, overwrite = TRUE))
        
        # Check if the request was successful
        if (http_status(response)$category == "Success") {
          print(paste("File has been saved to: ", zip_file_path))
        } else {
          print(paste("Failed to fetch URL: ", http_status(response)$message))
          next
        }
        
        ###### 2. UNZIP THE FILE ######
        file_paths <- unzip(zip_file_path, exdir = bundle_path)
        file.remove(zip_file_path)
        
        file_paths <- file_paths[grep("\\.json|\\.log", file_paths)]
        
        #browser()
        metadata_errors <- identify_support_bundle_errors(file_paths=file_paths, regex_pattern_df = regex_reactive_df())
        if(nrow(metadata_errors) > 0) {
          metadata_errors$execution_id <- execution_id
          output_df <- rbind(output_df, metadata_errors)
        }
        ###### WRITE OUTPUT TO CSV ######
        
        if(!dir.exists(summary_directory)) {
          dir.create(summary_directory)
        }
        summary_csv_path <- paste0(summary_directory, "/", execution_id, "-summary.csv")
        
        write.csv(metadata_errors, summary_csv_path, row.names=FALSE)
        
        cat("Summary File Recorded at: ", summary_csv_path, "\n")
        
        incProgress(1/length(download_list))
      }
    })
    #################### END OF FOR LOOP ########################
    metadata_df(output_df)
    #browser()
  })

  
  metadata_agg <- shiny::reactive({
    if(!is.null(metadata_df())) {
      
      shiny::validate(
        need(nrow(metadata_df()) > 0, "No errors identified in the selected bundles.")
      )
      highest_vote <- data.frame(execution_id=unique(metadata_df()$execution_id))

      #execution_id <- unique(metadata_df()$execution_id)[2]
      highest_vote$Error_Type <- lapply(unique(metadata_df()$execution_id), function(execution_id) {
        cat(execution_id, "\n")
        target_rows <- metadata_df()[which(metadata_df()$execution_id == execution_id),]
        most_recent_log <- target_rows[which(target_rows$Date_Time == max(target_rows$Date_Time, na.rm=TRUE)),]
        
        # Just in case there are multiple errors caught at the same most recent time
        highest_vote <- table(most_recent_log$Error_Type)
        
        # Edge case: all returned hits are found without timestamps
        if(length(highest_vote) == 0) {
          highest_vote <- table(target_rows$Error_Type)
        }
        
        highest_vote <- names(highest_vote[which(highest_vote == max(highest_vote))])
        
        # This can be where we have a hierarchy of errors
        if(length(highest_vote) > 1) {
          highest_vote <- highest_vote[1]
        }
        
        return(highest_vote)
      }) %>% unlist()
      
      metadata_agg <- as.data.table(highest_vote)
      metadata_agg <- metadata_agg[, .N, by=Error_Type]
      all_errors <- data.frame("Error_Type"=error_types)
      metadata_agg <- data.table::merge.data.table(all_errors, metadata_agg, by="Error_Type", all.x=TRUE)
      metadata_agg[is.na(metadata_agg)] <- 0
      metadata_agg$Error_Type <- tools::toTitleCase(metadata_agg$Error_Type)
      
      return(metadata_agg)
    }
  })
  
  execution_ids_no_errors <- shiny::reactive({
    if(!is.null(metadata_df()) & !is.null(selected_execution_ids())){
      #browser()
      error_free_ids <- paste0(setdiff(selected_execution_ids(), metadata_df()$execution_id), collapse="\n")
      return(error_free_ids)
    }
  })
  
  output$error_summary_bar_chart <- highcharter::renderHighchart({
    if(!is.null(metadata_agg())) {
      df <- metadata_agg()
      
      
      
      df$colors <- viridis::viridis(nrow(df))
      hc <-
        df %>%
        hchart(
          'column',
          hcaes(x=Error_Type, y=N, color=colors, opacity=0.8),
          stacking="normal"
        ) %>%
        #hc_title(text=paste0("Errors")) %>%
        #hc_subtitle(text=paste0("Click a category to drill down into it.")) %>%
        hc_legend(FALSE) %>%
        # hc_tooltip(
        #   pointFormat = "<span style='color:{point.color}'>●</span> {series.name}: <b>{point.y}</b><br/>",
        #   borderWidth = 0,
        #   backgroundColor = 'none',
        #   shadow = FALSE,
        #   style = list(fontSize = "16px", fontWeight = "bold", color = "#000000"),
        #   useHTML = TRUE
        # ) %>%
        hc_xAxis(title=list(text='Error Type')) %>%
        hc_yAxis(title=list(text=''))
    }
  })
  
  output$error_render <- DT::renderDT({
    if(!is.null(metadata_df())){
      shiny::validate(
        need(nrow(metadata_df()) > 0, "No errors identified in the selected bundles.")
      )
      
      df <- metadata_df()
      df <- df[c("execution_id", "Date_Time", "Error_Type", "File_Path", "Line_Number", "Context")]
      colnames(df) <- gsub("_", " ", colnames(df)) %>% tools::toTitleCase(.)
      
      DT::datatable(df,rownames = FALSE, class = "compact",
                    options = list(server=FALSE, autoWidth=TRUE, scrollX = TRUE, sScrollY = '75vh',
                                   selection = list(target = 'row', selected='indices', mode='single'),
                                   scrollCollapse = TRUE, searching = TRUE,
                                   paging = TRUE, pageLength=50, lengthMenu=c(10,20,50,100)
                                   , bInfo=TRUE),
                    extensions = list("Scroller"))
      #browser()
    }
  })
  
  
  output$error_render_downloader <- downloadHandler(
    filename = function() {
      paste0("support-bundle-errors-captured-",Sys.Date(),".csv")
    },
    content = function(con) {
      if(!is.null(metadata_df())) {
        df <- metadata_df()
        df <- df[c("execution_id", "Date_Time", "Error_Type", "File_Path", "Line_Number", "Context")]
        colnames(df) <- gsub("_", " ", colnames(df)) %>% tools::toTitleCase(.)
        write.csv(df, con, row.names = FALSE)
      }
    }
  )
  
  
  
  
  
  current_path_contents <- shiny::reactiveVal()
  shiny::observeEvent(input$error_render_rows_selected, {
    #browser()
    file_path <- metadata_df()$File_Path[input$error_render_rows_selected[length(input$error_render_rows_selected)]]
    contents <- readLines(file_path)
    mentioned_lines <- metadata_df()$Line_Number[which(metadata_df()$File_Path == file_path)]
    contents[mentioned_lines] <- paste0('<span style="color: darkred; background-color: lightcoral;">', contents[mentioned_lines], "</span>")
    contents <- sapply(seq_along(contents), function(x) paste0(x, ": ", contents[x], "<br>"))
    contents <- c(paste0(file_path, ":<br><br>"), contents)
    #cat(contents, "\n")
    
    current_path_contents(contents)
  })
  
  output$error_row_contents <- renderText({
    # Check if the file input is not NULL
    if (!is.null(current_path_contents())) {
      return(current_path_contents())
    } else {
      return(NULL)
    }
  })
  
  output$executions_with_errors_progress_bar <- shiny::renderUI({
    if(!is.null(metadata_df())) {
      #browser()
      errors <- length(unique(metadata_df()$execution_id))
      total <- length(selected_execution_ids())
      percentage <- base::round(errors/total*100, 0)
      
      shinyWidgets::progressBar(
        id = "executions_progress_bar",
        value = errors,
        total = total,
        title = "Executions with Found Errors:",
        display_pct = TRUE,
        striped = TRUE
      )
    }
  })
  
  
  #### SUPPORT BUNDLE ANALYSIS - SUMMARY ####
  output$csv_file_dropdown_ui <- shiny::renderUI({
    if(!is.null(fileList())) {
      target_files <- fileList()[grep("\\.csv", fileList())]
      names(target_files) <- stringi::stri_extract(target_files, regex=".*(?=-summary)")
      #browser()
      support_bundle_summaries <- paste0("/domino/datasets/local/", domino_project_name, "/support-bundle-summaries/", fileList())
      file_sizes <- file.info(support_bundle_summaries)
      largest_file <- rownames(file_sizes)[which(file_sizes$size == max(file_sizes$size))]
      largest_file <- stringi::stri_extract(largest_file, regex="(?<=/support-bundle-summaries/).*")
      #browser()
      # project_name
      # system(paste0("ls -l /domino/datasets/local/", domino_project_name, "/support-bundle-summaries"), intern = TRUE)
      # # Identify the index of the largest file
      ##largest_file_index <- which.max(file_sizes)
      # # Extract the name of the largest file
      # largest_file <- filelist[largest_file_index]
      #largest_file <- system(paste0("find /domino/datasets/local/", domino_project_name, "/support-bundle-summaries -type f -exec du -a {} + | sort -n -r | head -n 1"), intern=TRUE)
      support_bundle_csv_files(largest_file)
      
      shiny::selectizeInput(inputId = "csv_file_dropdown",
                            label = h5("Support Bundle Summary"),
                            choices = target_files,
                            selected = largest_file,
                            multiple = FALSE)
    }
  })
  
  output$error_totals_barchart <- highcharter::renderHighchart({
    if(!is.null(support_bundle_df())) {
      support_bundle_dt <- data.table::data.table(support_bundle_df())
      support_bundle_dt$is_date_na <- base::ifelse(!is.na(support_bundle_dt$Date_Time), 'Timestamp', 'No Timestamp')
      sup_agg <- support_bundle_dt[, .N, .(Error_Type, is_date_na)]
      
      all_poss <- data.table::CJ(c("Timestamp", "No Timestamp"), error_types)
      colnames(all_poss) <- c("is_date_na", "Error_Type")
      sup_agg <- data.table::merge.data.table(sup_agg, all_poss, by=c("Error_Type", "is_date_na"), all.y = TRUE)
      sup_agg$N[which(is.na(sup_agg$N))] <- 0
      
      sup_agg$Error_Type <- tools::toTitleCase(sup_agg$Error_Type)
      bin_labels <- sort(unique(sup_agg$Error_Type))
      sup_agg <- as.data.frame(sup_agg)
      
      hc <-
        sup_agg %>%
        hchart(
          'bar',
          hcaes(x=Error_Type, y=N, group=is_date_na, opacity=0.8),
          stacking="normal"
        ) %>%
        hc_colors(c(tasteful_ryg[1], tasteful_ryg[3])) %>%
        hc_title(text=paste0("Errors")) %>%
        hc_subtitle(text=paste0("Click a category to drill down into it.")) %>%
        hc_legend(FALSE) %>%
        # hc_tooltip(
        #   pointFormat = "<span style='color:{point.color}'>●</span> {series.name}: <b>{point.y}</b><br/>",
        #   borderWidth = 0,
        #   backgroundColor = 'none',
        #   shadow = FALSE,
        #   style = list(fontSize = "16px", fontWeight = "bold", color = "#000000"),
        #   useHTML = TRUE
        # ) %>%
        hc_xAxis(title=list(text='Error Type'), categories=bin_labels) %>%
        hc_yAxis(title=list(text='')) %>%
        # hc_add_event_point("click", JS("function (event) {
        #         Shiny.setInputValue('clicked_bar', event.point.Error_Type);
        #      }"))
        # hc_add_event_point("click", function(e) {
        # # Use Shiny.setInputValue to send the clicked category to Shiny
        #   shinyjs::runjs(sprintf("Shiny.setInputValue('clicked_bar', '%s');", e$Error_Type))
        #  })
        hc_plotOptions(
          series = list(
            cursor = "pointer",
            point = list(
              events = list(
                click = JS("function() {
                Shiny.setInputValue('clicked_bar', this.Error_Type);
              }")
              )
            )
          )
        )
      return(hc)
    }
  })
  
  
  observeEvent(input$clicked_bar, {
    # Update the reactive value with the category of the clicked bar
    current_error_type(input$clicked_bar)
  })
  
  support_bundle_csv_files <- shiny::reactiveVal()
  
  observeEvent(input$reset_btn, {
    current_error_type("All")
    support_bundle_csv_files(input$csv_file_dropdown)
  })
  
  # observeEvent(input$csv_file_dropdown, {
  #   current_error_type("All")
  # })
  
  # observe({
  #   print(paste("Error Type Changed To: ", current_error_type()))
  # })
  
  df_time_series <- shiny::reactive({
    if(!is.null(support_bundle_subset())) {
      output_df <- support_bundle_subset()[which(!is.na(support_bundle_subset()$Date_Time)),]
      
      shiny::validate(
        need(nrow(output_df) > 0, "No errors caught have associated dates")
      )
      
      return(output_df)
    }
  })
  
  output$time_series_chart <- renderHighchart({
    if(!is.null(df_time_series())) {
      
      # Convert Date_Time to milliseconds
      df <- df_time_series()
      df$Date_Time <- as.POSIXct(df$Date_Time) %>% floor_date(., unit='sec')
      dt <- data.table::data.table(df)
      dt_agg <- dt[, .N, by=.(Error_Type, Date_Time)]
      
      # min_datetime <- dt[, .(min_date_time=min(Date_Time)), by=execution_id]
      # map_to_date_time <- min_datetime$min_date_time
      # names(map_to_date_time) <-  min_datetime$execution_id
      # dt$time_after_start <- as.numeric(dt$Date_Time) - as.numeric(map_to_date_time[dt$execution_id])
      
      all_seconds <- seq(min(dt_agg$Date_Time), max(dt_agg$Date_Time), by = "1 sec") %>% floor_date(., unit='sec')
      all_types <- unique(dt_agg$Error_Type)
      all_poss <- data.table::CJ(Date_Time=all_seconds, Error_Type=all_types)
      
      dt_agg <- data.table::merge.data.table(dt_agg, all_poss, by=c("Date_Time", "Error_Type"), all.y=TRUE)
      dt_agg$N[which(is.na(dt_agg$N))] <- 0
      dt_agg$Date_Time <- as.integer(dt_agg$Date_Time) * 1000
      
      #browser()
      dt_agg %>%
        hchart("line",
               hcaes(x=Date_Time, y=N, group=Error_Type)) %>%
        hc_xAxis(type = 'datetime') %>%
        hc_colors(viridis::viridis(length(unique(dt_agg$Error_Type)))) %>%
        hc_chart(zoomType = "x") %>%
        hc_title(text=paste("Errors Over Time"))
    }
  })
  
  support_bundle_df <- shiny::reactive({
    if(!is.null(support_bundle_csv_files())) {
      browser()
      support_bundle_df <- lapply(support_bundle_csv_files(), function(target_file) {
        #cat("READING IN THE FOLLOWING FILE: ", target_file, "\n")
        df <- read.csv(paste0(data_directory, "support-bundle-summaries/", target_file))
        return(df)
      }) %>% do.call(rbind, .)
      
      return(support_bundle_df)
    }
  })
  
  support_bundle_subset <- shiny::reactive({
    if(!is.null(current_error_type()) & !is.null(support_bundle_df())) {
      if(current_error_type() != "All") {
        support_bundle_subset <- support_bundle_df()[which(support_bundle_df()$Error_Type == tolower(current_error_type())),]
      } else {
        support_bundle_subset <- support_bundle_df()
      }
      
      #browser()
      
      # shiny::validate(
      #   need(try(nrow(support_bundle_subset)> 0 & !is.na(support_bundle_subset)), paste0("No errors caught in target support bundle"))
      # )
      
      return(support_bundle_subset)
    } 
  })
  
  output$bundle_error_dt_render <- DT::renderDT({
    if(!is.null(support_bundle_subset())){
      df <- support_bundle_subset()
      df <- df[c("Date_Time", "Error_Type", "File_Path", "Line_Number", "Context")]
      colnames(df) <- gsub("_", " ", colnames(df))
      
      DT::datatable(df,rownames = FALSE, class = "compact",
                    options = list(scrollX = TRUE, sScrollY = '75vh',
                                   scrollCollapse = TRUE, searching = TRUE,
                                   paging = TRUE, pageLength=50, lengthMenu=c(10,20,50,100)
                                   , bInfo=TRUE),
                    extensions = list("Scroller"))
    }
  })
  
  output$bundle_error_dt_render_downloader <- downloadHandler(
    filename = function() {
      paste0("support-bundle-errors-captured-",Sys.Date(),".csv")
    },
    content = function(con) {
      if(!is.null(support_bundle_subset())) {
        df <- support_bundle_subset()
        df <- df[c("Date_Time", "Error_Type", "File_Path", "Line_Number", "Context")]
        colnames(df) <- gsub("_", " ", colnames(df))
        write.csv(df, con, row.names = FALSE)
      }
    }
  )
  
  ##### UPLOAD TAB #####
  ##### ERROR REGEX MATCHING UPDATE TABLE #####
  
  regex_df <- read.csv('./data/regex_lookup.csv')
  regex_reactive_df <- reactiveVal(regex_df)
  
  output$regex_lookup_dt <- DT::renderDT({
    if(!is.null(regex_reactive_df())){
      # shiny::validate(
      #   need(nrow(regex_df()) > 0, "No errors identified in the selected bundles.")
      # )
      
      df <- regex_reactive_df()
      #browser()
      #df <- df[c("execution_id", "Date_Time", "Error_Type", "File_Path", "Line_Number", "Context")]
      colnames(df) <- gsub("_", " ", colnames(df)) %>% tools::toTitleCase(.)
      
      DT::datatable(df, 
                    editable = TRUE,
                    rownames = FALSE, class = "compact",
                    options = list(autoWidth = TRUE, 
                                   scrollX = TRUE, 
                                   sScrollY = '75vh',
                                   scrollCollapse = TRUE, 
                                   searching = TRUE,
                                   paging = TRUE, 
                                   pageLength = 50, 
                                   lengthMenu = c(10, 20, 50, 100),
                                   bInfo = TRUE,
                                   rowCallback = JS(
                                     "function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {
                                     $(nRow).attr('id', 'row_' + aData[0]);
                                   }"
                                   )
                    ), 
                    extensions = list("Scroller"), 
                    selection = "single" # Allow single row selection for deleting
      )
    }
  })
  
  proxy <- dataTableProxy("regex_lookup_dt")
  
  observeEvent(input$add_row, {
    new_row <- data.frame(error_type = "EDIT HERE", regex_pattern = "EDIT HERE") # Sample new row
    updated_df <- rbind(regex_reactive_df(), new_row)
    regex_reactive_df(updated_df)
    
    replaceData(proxy, updated_df, resetPaging = FALSE)
  })
  
  # Delete the selected row from the DataTable
  observeEvent(input$delete_row, {
    selected_row <- input$regex_lookup_dt_rows_selected
    if (!is.null(selected_row)) {
      updated_df <- regex_reactive_df()[-selected_row, ]
      regex_reactive_df(updated_df)
      replaceData(proxy, updated_df, resetPaging = FALSE)
    }
  })
  
  observeEvent(input$regex_lookup_dt_cell_edit, {
    info <- input$regex_lookup_dt_cell_edit
    modified_df <- regex_reactive_df()
    
    # Update the changed cell in the data frame
    modified_df[info$row, info$col+1] <- info$value
    regex_reactive_df(modified_df)
    
    # Update the table using the proxy
    replaceData(proxy, modified_df, resetPaging = FALSE, rownames = FALSE)
  })
  
  observeEvent(input$save_table, {
    write.csv(regex_reactive_df(), "./data/regex_lookup.csv", row.names = FALSE)
    
    # Triggers a "grayed out" back to a "normal" state so user knows that the click went through
    shinyjs::addClass(id="save_table", class="grayed-out-button", asis=TRUE)
    Sys.sleep(1)
    shinyjs::removeClass(id="save_table", class="grayed-out-button", asis=TRUE)
  })
  
  # Function to get the list of files (to be used by reactivePoll)
  get_support_bundle_dirs <- function() {
    all_available_support_bundles <- list.files(paste0(data_directory, 'support-bundles/'))
    return(all_available_support_bundles)
  }
  
  # Create a reactivePoll that checks the list of files every 1 second (1000 ms)
  support_bundle_dirs <- reactivePoll(1000, session,
                            checkFunc = function() {
                              # Use digest to create a hash that will change if the file list changes
                              digest::digest(get_support_bundle_dirs())
                            },
                            valueFunc = function() {
                              # Return the current list of files
                              get_support_bundle_dirs()
                            }
  )
  
  
  
  
  execution_ids <- shiny::reactive({
    if(!is.null(support_bundle_dirs())) {
      #browser()
      all_available_support_bundles <- support_bundle_dirs()[grep("^support-bundle-[a-f0-9]{24}$", support_bundle_dirs())]
      all_available_support_bundles <- stringi::stri_extract(all_available_support_bundles, regex="(?<=support-bundle-).*")
    }
  })
  
  output$support_bundle_ids_multiselect_ui <- shiny::renderUI({
    if (!is.null(execution_ids())) {
      div(class = "shiny-select-spacing",
          shiny::selectizeInput(inputId = "support_bundle_ids_multiselect",
                                label = "Choose Support Bundle Ids",
                                choices = execution_ids(),
                                selected = execution_ids()[1],
                                multiple = TRUE)
      )
    }
  })
  
  
  
  output$support_bundle_file_select_ui <- shiny::renderUI({
    if(!is.null(input$support_bundle_ids_multiselect)) {
      #browser()
      execution_ids()
      
      directories <- paste0(data_directory, "support-bundles/support-bundle-", input$support_bundle_ids_multiselect)
      path_choices <- list.files(directories, full.names=TRUE)

      shiny::validate(
        need(length(path_choices) > 0, as.character("No associated files found. This tends to be the result of a (500) Internal Server Error on pull of support bundle. Refer to documentation to manually extract by execution id."))
      )

      div(class = "shiny-select-spacing",
          shiny::selectizeInput(inputId = "support_bundle_file_select",
                                label = "Choose File",
                                choices = path_choices,
                                selected = path_choices[1],
                                multiple = FALSE)
      )
    }
  })
  
  output$file_content <- renderText({
    # Check if the file input is not NULL
    if (!is.null(input$support_bundle_file_select)) {
      # Read the file content
      #browser()
      file_content <- readLines(input$support_bundle_file_select, warn = FALSE)
      
      # Convert the content to a single string and return
      paste(file_content, collapse = "<br>")
    } else {
      return(NULL)
    }
  })
  
  
  #### TAKE A TOUR ####
  output$take_a_tour_resource_usage_btn_ui <- shiny::renderUI({
    if(!is.null(input$generate_report_btn)){
      if(input$generate_report_btn > 0) {
        actionButton("take_a_tour_resource_usage_btn", "Take a Tour",
                     style = "display: block; width: 100%; color: #fff; background-color: #337ab7;")
      }
    }
  })
  
  helptext <- reactive({
    data.table(
      btn  = c(
              rep("take_a_tour_resource_usage_btn", 6),
              rep("take_a_tour_bundle_summary_btn", 3),
              rep("take_a_tour_bundle_upload_btn", 7)
              ),
      step = c(seq(6), seq(3), seq(5)),
      element = c("#usage_report_filters", 
                  "#run_type_donut_chart", 
                  "#rolling_count_running_processes_time_series", 
                  "#report_render",
                  "#analyze_support_bundles",
                  "#error_summary_bar_chart",
                  # Bundle Summary
                  "#error_totals_barchart",
                  "#time_series_chart",
                  "#bundle_error_dt_render",
                  # Bundle Upload
                  #"#bundle_upload_from_execution_id",
                  #"#bundle_upload_from_zip_file",
                  "#regular_expression_table",
                  "#add_row",
                  "#delete_row",
                  "#save_table",
                  "#explore_source_files"
      ),
      intro = c("Use the toggles to update the filters which generate the resulting report. The secondary category filter determines the categorical breakdown of the graphs.",
                "Clicking any category on this chart will filter the data table below...",
                "whereas any clicks on this chart will filter by category and time.",
                "Here you can find the metadata for all workloads executed in Domino. Select a few of these (or nothing at all!) and click 'next.'",
                paste0("When you have made your selection, click here to run the bundle analysis. You will see a tabbox appear to the right.
                                  <br><br><b>Summary:</b> Counts of all error types found within selected support bundles.
                                  <br><b>Data:</b> Metadata surrounding each error identified
                                  <br><b>Context:</b> When a row in data is selected, outputs the corresponding file and highlights all caught errors.
                                  <br><br>
                                You can also use the 'Reset Secondary Filter' button to remove any filters imposed on the table by chart clicks."),
                "This is a test to see if I can actually render this out in the tutorial.",
                ###### BUNDLE SUMMARY
                "This figure displays the number of caught errors identified in the support bundles for the selected execution id. Files include logs for the domino session (domino), the underlying k8s infrastructure (cluster), and user input (user). This is further subdivided by the number errors which were accompanied by a timestamp.<br><br> Refer to the 'Customize Error Search Analysis' chart in the 'Upload' tab for regular expressions used in the bundle analysis.",
                "Here we break down the prior chart by the time in which they occured. It's important to note that these are only lines identified as errors which also contained a timestamp.",
                "Underlying data for the above charts. This table filters down to any error type selected in the above charts.",
                # Bundle Upload
                #"If you can't find an execution id in the summary tables, put it into the text box above and it will be added to the database.",
                #"You can also opt to directly upload a support bundle from your local computer. This is a great way to analyse support bundles from tenants that are different from that which the application resides.",
                "The error analysis conducted in the resource usage tab relies on regular expression patterns to identify lines which may potentially contain errors and the respective class.",
                "To add a regular expression, click the below button. You can then manually enter your selection into the table",
                "If you need to remove a pattern, click here. Be careful; the only way to undo a delete is to refresh the page. This erases all unsaved work.",
                "Save any patterns you have entered or deleted from the source table.",
                "Browse existing execution logs to identify potential errors. These can be either searched individually or multiple ids can be searched concurrently."
      )
    )
  })
  
  observeEvent(
    eventExpr = input$take_a_tour_resource_usage_btn,
    handlerExpr = {
      introjs(session, 
              options = list(
                "showBullets"="false", "showProgress"="true", 
                "showStepNumbers"="false","nextLabel"="Next","prevLabel"="Prev","skipLabel"="Skip",
                steps=helptext()[btn == "take_a_tour_resource_usage_btn"]
              )
      )
    }
  )
  
  output$take_a_tour_bundle_summary_btn_ui <- shiny::renderUI({
        actionButton("take_a_tour_bundle_summary_btn", "Take a Tour",
                     style = "display: block; width: 100%; color: #fff; background-color: #337ab7;")
  })
  
  observeEvent(
    eventExpr = input$take_a_tour_bundle_summary_btn,
    handlerExpr = {
      introjs(session, 
              options = list(
                "showBullets"="false", "showProgress"="true", 
                "showStepNumbers"="false","nextLabel"="Next","prevLabel"="Prev","skipLabel"="Skip",
                steps=helptext()[btn == "take_a_tour_bundle_summary_btn"]
              )
      )
    }
  )
  
  output$take_a_tour_bundle_upload_btn_ui <- shiny::renderUI({
    actionButton("take_a_tour_bundle_upload_btn", "Take a Tour",
                 style = "display: block; width: 100%; color: #fff; background-color: #337ab7;")
  })
  
  observeEvent(
    eventExpr = input$take_a_tour_bundle_upload_btn,
    handlerExpr = {
      introjs(session, 
              options = list(
                "showBullets"="false", "showProgress"="true", 
                "showStepNumbers"="false","nextLabel"="Next","prevLabel"="Prev","skipLabel"="Skip",
                steps=helptext()[btn == "take_a_tour_bundle_upload_btn"]
              )
      )
    }
  )
}