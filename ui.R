#### UI.R ####
# Define the UI
ui <- shiny::fluidPage(
    rintrojs::introjsUI(),
    shinyjs::useShinyjs(),
    bs4Dash::dashboardPage(
      bs4Dash::dashboardHeader(title = 
                               HTML('<img src = "Allstate-Symbol.png", width="60", height="60">')#,
                               #status = "secondary"
                      # tags$div(
                      #   tags$img(src = "Allstate-Symbol.png", height = "45px", align='left') #,style = "vertical-align: middle; padding-right: 10px;"),
                      #   #tags$span("All State", style = "vertical-align: middle;")#,
                      #   #style = "padding-left: 10px;" # Added padding to move content a bit right from the left edge
                      # )
      ),
      bs4Dash::dashboardSidebar(
        disable=FALSE,
        skin = "light",
        collapsed=FALSE,
        bs4Dash::sidebarMenu(
          id = "tabSelected",
          bs4Dash::menuItem("Resource Usage", tabName = "resource_usage", icon=icon("file")),
          bs4Dash::menuItem("Support Bundle Analysis",
            bs4Dash::menuSubItem("Individual Bundles", tabName = "bundle_summary", icon=icon("dashboard")),
            bs4Dash::menuSubItem("Regex Error Filters", tabName = "bundle_upload", icon=icon("upload"))
          )
        ),
        conditionalPanel(
          condition = "input.tabSelected == 'bundle_summary'", # condition for showing elements
          fluidRow(
            column(12, shiny::uiOutput("csv_file_dropdown_ui"))
          ),
          fluidRow(
            column(10, actionButton("reset_btn", "Reset Filters",
                                    style = "display: block; width: 100%; color: #fff; background-color: #337ab7;")
                   )
          ),
          HTML("<br>"),
          shiny::fluidRow(
            column(2),
            column(6, 
                   shiny::uiOutput("take_a_tour_bundle_summary_btn_ui")
            ),
            column(2)
          )
        ),
        conditionalPanel(
          condition = "input.tabSelected == 'resource_usage'", # condition for showing elements,
            shiny::uiOutput("usage_report_filters"),
            HTML("<br>"),
            shiny::fluidRow(
              column(2),
              column(6, 
                     shiny::uiOutput("take_a_tour_resource_usage_btn_ui")
                     ),
              column(2)
            )
        ),
        conditionalPanel(
          condition = "input.tabSelected == 'bundle_upload'",
          HTML("<br>"),
          shiny::fluidRow(
            column(2),
            column(6, 
                   shiny::uiOutput("take_a_tour_bundle_upload_btn_ui")
            ),
            column(2)
          )
        )
      ),
      bs4Dash::dashboardBody(
        tags$head(
          #tags$link(rel = "stylesheet", type = "text/css", href = "styles.css"),
          tags$style(
            HTML("
                .center-div {
                  display: flex;
                  align-items: center;
                  justify-content: center;
                  height: 150px; /* Should be the same as the well height */
                  font-size: 20px;
                }
                .skin-blue .main-header .logo { padding-left: 10px; }

                .shiny-select-spacing {
                   margin-top: -20px;
                   margin-bottom: -20px;  # You can adjust this value to increase or decrease space
                }

                .grayed-out-button {
                    background-color: #d3d3d3 !important;  /* Gray background */
                    border: 1px solid #a9a9a9 !important;  /* Gray border */
                    color: #7a7a7a !important;             /* Gray text */
                    cursor: not-allowed !important;        /* Show a 'not-allowed' cursor */
                    pointer-events: none;                  /* Make button not clickable */
                }
                
                .inline-radio .radio { display: inline-block; margin-right: 10px;}
               ")
          )
        ),
        bs4Dash::tabItems(
          bs4Dash::tabItem(tabName = "bundle_summary",
                  fluidRow(
                    bs4Dash::bs4Card(width=5, 
                                     maximizable=TRUE, 
                                     shinycssloaders::withSpinner(highcharter::highchartOutput("error_totals_barchart", 
                                                                  width="100%"))),
                    bs4Dash::bs4Card(width=7, 
                                     maximizable=TRUE, 
                                     shinycssloaders::withSpinner(highcharter::highchartOutput("time_series_chart", 
                                                                  width="100%")))
                  ),
                  fluidRow(
                    bs4Dash::bs4Card(width=12, maximizable=TRUE, 
                                     DT::DTOutput("dt_render"),
                                     HTML("<br>"),
                                     shiny:: downloadButton("dt_render_downloader", "Download")
                                     )
                  )
          ),
          bs4Dash::tabItem(tabName = "bundle_upload",
                  # fluidRow(
                  #   column(5,
                  #          bs4Dash::bs4Card(id = "bundle_upload_from_execution_id", width=12,
                  #            #style = "background-color: #f7f7f7;", # You can adjust the background color as needed
                  #            fluidRow(textInput("text", "Enter Execution ID", value = "Type here...", width="100%")),
                  #            fluidRow(actionButton("download_support_bundle_button", "Download Support Bundle", 
                  #                         style = "display: block; width: 100%; color: #fff; background-color: #337ab7;")) # Adjust styles as needed
                  #          )
                  #   ),
                  #   column(2,
                  #          div(class = "center-div", HTML("<b style='font-size:20px;'>OR</b>"))
                  #   ),
                  #   column(5,
                  #          bs4Dash::bs4Card(id = "bundle_upload_from_zip_file", width=12,
                  #            #style = "background-color: #f7f7f7;", # You can adjust the background color as needed
                  #            fileInput("support_bundle_zip_file", "Upload a support bundle ZIP file",
                  #                      accept = c('application/zip',
                  #                                 'application/x-zip-compressed',
                  #                                 'multipart/x-zip',
                  #                                 'application/x-compress',
                  #                                 'application/x-compressed',
                  #                                 'application/x-gzip')
                  #            )
                  #          )
                  #   )
                  # ),
                  fluidRow(
                    bs4Dash::bs4Card(id = "regular_expression_table",
                      title="Customize Error Search Analysis",
                      width = 6,
                      fluidRow(
                        DT::DTOutput("regex_lookup_dt")
                      ),
                      HTML("<br>"),
                      fluidRow(
                        column(4, actionButton("add_row", "Add Row",
                                               style = "display: block; width: 100%; color: #fff; background-color: #337ab7; width='100%'")),
                        column(4, actionButton("delete_row", "Delete Selected Row",
                                               style = "display: block; width: 100%; color: #fff; background-color: #337ab7;")),
                        column(4, actionButton("save_table", "Save Table",
                                               style = "display: block; width: 100%; color: #fff; background-color: #337ab7;"))
                      )
                    ),
                    bs4Dash::bs4Card(id = "explore_source_files",
                      title="View Source Files",
                      maximizable = TRUE,
                      fluidRow(width=12,
                        uiOutput("support_bundle_ids_multiselect_ui")
                      ),
                      fluidRow(width=12,
                        uiOutput("support_bundle_file_select_ui")
                      ),
                      #fileInput("file", "Choose .log or .json File", accept = c(".log", ".json")),
                      htmlOutput("file_content")
                      )
                  )
          ),
          bs4Dash::tabItem(tabName = "resource_usage",
            conditionalPanel(
              condition = '!output.show_contents',
              # fluidRow(    tags$div(
              #   "Please generate a report using the selectors to the left.",
              #   style = "background-color: #337ab7; color: #FFFFFF; padding: 10px; 
              #      border: 1px solid #FFFFFF; border-radius: 5px; text-align: center;"
              # )),
              fluidRow(
                div(class = "container-fluid",
                  div(class = "alert", 
                      role = "alert",
                      style = "background-color: #337ab7; color: #FFFFFF; ",
                      strong(HTML("<center>Welcome!</center> ")), HTML("<center>Please generate a report using the selectors to the left.</center>")
                  )
                )
              )
            ),
            conditionalPanel(
              condition = 'output.show_contents',
              fluidRow(
                column(8,
                  bs4Dash::bs4Card(width=12, maximizable=TRUE, 
                    #style = "background-color: $f7f7f7;",
                    shinycssloaders::withSpinner(highcharter::highchartOutput("rolling_count_running_processes_time_series"))
                  )
                ),
                column(4,
                  bs4Dash::bs4Card(width=12, maximizable=TRUE, 
                    #style = "background-color: #f7f7f7;",
                    shinycssloaders::withSpinner(highcharter::highchartOutput("run_type_donut_chart"))
                  )
                )
              ),
              uiOutput("bundle_error_count")
            )
          )
        )
      )
  )
)
