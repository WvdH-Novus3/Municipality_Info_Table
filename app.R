# municipality_app.R - Standalone Municipality Management Application
library(shiny)
library(DT)
library(dplyr)
library(DBI)
library(RPostgres)
library(config)
library(here)
library(shinydashboard)

# UI
ui <- dashboardPage(

  # Header
  dashboardHeader(title = "Municipality Information Management System"),

  # Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("Municipality Data", tabName = "data", icon = icon("table")),
      menuItem("Statistics", tabName = "stats", icon = icon("chart-bar")),
      menuItem("Export", tabName = "export", icon = icon("download"))
    )
  ),

  # Body
  dashboardBody(
    tabItems(

      # Municipality Data Tab
      tabItem(tabName = "data",
              fluidRow(
                box(
                  title = "Filters and Controls", status = "primary", solidHeader = TRUE, width = 12,
                  fluidRow(
                    column(3,
                           selectInput("province_filter",
                                       "Filter by Province:",
                                       choices = c("All" = "", "Western Cape" = "WC", "Eastern Cape" = "EC",
                                                   "Gauteng" = "GP", "KwaZulu-Natal" = "KZN", "Limpopo" = "LP",
                                                   "Mpumalanga" = "MP", "Northern Cape" = "NC", "North West" = "NW",
                                                   "Free State" = "FS"),
                                       selected = "")
                    ),
                    column(3,
                           selectInput("type_filter",
                                       "Filter by Type:",
                                       choices = c("All" = "", "Local Municipality" = "LM",
                                                   "Metropolitan" = "MET", "District" = "DM"),
                                       selected = "")
                    ),
                    column(3,
                           textInput("search_text",
                                     "Search Municipality:",
                                     placeholder = "Enter municipality name...")
                    ),
                    column(3,
                           br(),
                           actionButton("reset_filters", "Reset Filters", class = "btn-warning", icon = icon("refresh")),
                           br(), br(),
                           actionButton("save_changes", "Save Changes", class = "btn-success", icon = icon("save"))
                    )
                  )
                )
              ),

              # Status messages
              fluidRow(
                column(12,
                       conditionalPanel(
                         condition = "output.show_save_message",
                         div(class = "alert alert-success alert-dismissible",
                             icon("check-circle"), " ",
                             textOutput("save_message", inline = TRUE),
                             tags$button(type = "button", class = "close", `data-dismiss` = "alert",
                                         tags$span(HTML("&times;"))))
                       ),
                       conditionalPanel(
                         condition = "output.show_error_message",
                         div(class = "alert alert-danger alert-dismissible",
                             icon("exclamation-triangle"), " ",
                             textOutput("error_message", inline = TRUE),
                             tags$button(type = "button", class = "close", `data-dismiss` = "alert",
                                         tags$span(HTML("&times;"))))
                       )
                )
              ),

              # Main table
              fluidRow(
                box(
                  title = "Municipality Information Table", status = "info", solidHeader = TRUE, width = 12,
                  p(icon("info-circle"), " Double-click any cell to edit. Changes are highlighted until saved."),
                  DTOutput("municipality_table")
                )
              )
      ),

      # Statistics Tab
      tabItem(tabName = "stats",
              fluidRow(
                valueBoxOutput("total_municipalities"),
                valueBoxOutput("total_provinces"),
                valueBoxOutput("unsaved_changes")
              ),

              fluidRow(
                box(
                  title = "Municipalities by Province", status = "primary", solidHeader = TRUE, width = 6,
                  plotOutput("province_chart")
                ),
                box(
                  title = "Municipality Types", status = "success", solidHeader = TRUE, width = 6,
                  plotOutput("type_chart")
                )
              ),

              fluidRow(
                box(
                  title = "Table Summary", status = "info", solidHeader = TRUE, width = 12,
                  verbatimTextOutput("detailed_summary")
                )
              )
      ),

      # Export Tab
      tabItem(tabName = "export",
              fluidRow(
                box(
                  title = "Export Options", status = "warning", solidHeader = TRUE, width = 12,
                  h4("Download Current Data"),
                  p("Export the current municipality data (including any unsaved changes) to various formats."),
                  br(),
                  downloadButton("download_csv", "Download as CSV", class = "btn-primary", icon = icon("file-csv")),
                  br(), br(),
                  downloadButton("download_excel", "Download as Excel", class = "btn-success", icon = icon("file-excel")),
                  br(), br(),
                  h4("Database Operations"),
                  p("Advanced operations for database management."),
                  actionButton("refresh_data", "Refresh from Database", class = "btn-info", icon = icon("sync")),
                  br(), br(),
                  actionButton("backup_table", "Create Backup", class = "btn-warning", icon = icon("database"))
                )
              )
      )
    )
  )
)

# Server
server <- function(input, output, session) {

  # Load municipality data from database
  load_municipality_data <- function() {
    tryCatch({
      # Load configuration
      app_config <- config::get(
        file = here::here("config.yml"),
        config = Sys.getenv("R_CONFIG_ACTIVE", "QA")
      )

      # Database connection
      MUNICIPALITY_INFO <- dbConnect(
        RPostgres::Postgres(),
        dbname = app_config$MUNbdName,
        host = app_config$MUNdbHost,
        port = app_config$MUNdbPort,
        user = app_config$dbUser,
        password = app_config$dbPassword
      )

      # Load municipality data
      mun_data <- dbGetQuery(MUNICIPALITY_INFO, "SELECT * FROM mun_info ORDER BY \"No.\"")

      # Close connection immediately
      dbDisconnect(MUNICIPALITY_INFO)

      return(mun_data)

    }, error = function(e) {
      if(exists("MUNICIPALITY_INFO") && dbIsValid(MUNICIPALITY_INFO)) {
        dbDisconnect(MUNICIPALITY_INFO)
      }
      stop(paste("Error loading data:", e$message))
    })
  }

  # Reactive values to store data and track changes
  values <- reactiveValues(
    original_data = NULL,
    current_data = NULL,
    changed_rows = c(),
    save_message = "",
    error_message = "",
    show_save_message = FALSE,
    show_error_message = FALSE
  )

  # Initialize data
  observe({
    if(is.null(values$original_data)) {
      data <- load_municipality_data()
      values$original_data <- data
      values$current_data <- data
    }
  })

  # Filtered data based on inputs
  filtered_data <- reactive({
    req(values$current_data)

    data <- values$current_data

    # Apply province filter
    if(input$province_filter != "") {
      data <- data %>% filter(Province == input$province_filter)
    }

    # Apply type filter
    if(input$type_filter != "") {
      data <- data %>% filter(Type == input$type_filter)
    }

    # Apply search filter
    if(input$search_text != "" && !is.null(input$search_text)) {
      data <- data %>%
        filter(grepl(input$search_text, NameOfMunicipality, ignore.case = TRUE))
    }

    return(data)
  })

  # Render the editable table
  output$municipality_table <- renderDT({
    req(filtered_data())

    # Create row styling for changed rows
    row_styling <- rep("", nrow(filtered_data()))
    if(length(values$changed_rows) > 0) {
      filtered_indices <- which(values$current_data$No. %in% filtered_data()$No.)
      changed_in_filtered <- intersect(filtered_indices, values$changed_rows)
      if(length(changed_in_filtered) > 0) {
        for(i in changed_in_filtered) {
          filtered_row <- which(filtered_data()$No. == values$current_data$No.[i])
          if(length(filtered_row) > 0) {
            row_styling[filtered_row] <- "background-color: #fff3cd !important;"
          }
        }
      }
    }

    datatable(
      filtered_data(),
      editable = list(target = "cell", disable = list(columns = c(0))), # Disable editing No. column
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "500px",
        dom = 'Bfrtip',
        buttons = list('copy', 'print'),
        columnDefs = list(
          list(width = '60px', targets = 0),
          list(width = '80px', targets = 1),
          list(width = '250px', targets = 7),
          list(className = 'dt-center', targets = c(0, 6))
        ),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#3c8dbc', 'color': '#fff'});",
          "}"
        )
      ),
      rownames = FALSE,
      class = "display nowrap stripe hover"
    ) %>%
      formatStyle(
        columns = 1:ncol(filtered_data()),
        valueColumns = rep(1, ncol(filtered_data())),
        backgroundColor = styleEqual(1:nrow(filtered_data()), row_styling)
      )
  }, server = FALSE)

  # Handle cell edits
  observeEvent(input$municipality_table_cell_edit, {
    info <- input$municipality_table_cell_edit

    # Get the row number in the full dataset
    edited_row_no <- filtered_data()$No.[info$row]
    full_data_row <- which(values$current_data$No. == edited_row_no)

    if(length(full_data_row) > 0) {
      # Update the data
      col_name <- names(values$current_data)[info$col + 1]
      values$current_data[full_data_row, col_name] <- info$value

      # Track changed rows
      values$changed_rows <- unique(c(values$changed_rows, full_data_row))

      # Clear messages
      values$show_save_message <- FALSE
      values$show_error_message <- FALSE
    }
  })

  # Reset filters
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "province_filter", selected = "")
    updateSelectInput(session, "type_filter", selected = "")
    updateTextInput(session, "search_text", value = "")
  })

  # Save changes to database
  observeEvent(input$save_changes, {
    if(length(values$changed_rows) == 0) {
      values$error_message <- "No changes to save."
      values$show_error_message <- TRUE
      values$show_save_message <- FALSE
      return()
    }

    tryCatch({
      # Load configuration
      app_config <- config::get(
        file = here::here("config.yml"),
        config = Sys.getenv("R_CONFIG_ACTIVE", "QA")
      )

      # Database connection
      MUNICIPALITY_INFO <- dbConnect(
        RPostgres::Postgres(),
        dbname = app_config$MUNbdName,
        host = app_config$MUNdbHost,
        port = app_config$MUNdbPort,
        user = app_config$dbUser,
        password = app_config$dbPassword
      )

      # Update changed rows
      changes_saved <- length(values$changed_rows)
      for(row_idx in values$changed_rows) {
        row_data <- values$current_data[row_idx, ]

        # Create update query
        update_cols <- names(row_data)[-1] # Exclude No. column
        set_clause <- paste(paste0('"', update_cols, '"', " = $", 1:length(update_cols)), collapse = ", ")

        query <- paste0(
          'UPDATE mun_info SET ', set_clause,
          ' WHERE "No." = $', length(update_cols) + 1
        )

        # Execute update - params must be an unnamed list/vector
        param_values <- c(unlist(row_data[update_cols], use.names = FALSE), row_data$No.)
        dbExecute(MUNICIPALITY_INFO, query, params = param_values)
      }

      # Close connection
      dbDisconnect(MUNICIPALITY_INFO)

      # Update original data and clear changed rows
      values$original_data <- values$current_data
      values$changed_rows <- c()

      # Show success message
      values$save_message <- paste("Successfully saved", changes_saved, "changes to the database.")
      values$show_save_message <- TRUE
      values$show_error_message <- FALSE

    }, error = function(e) {
      values$error_message <- paste("Error saving changes:", e$message)
      values$show_error_message <- TRUE
      values$show_save_message <- FALSE

      if(exists("MUNICIPALITY_INFO") && dbIsValid(MUNICIPALITY_INFO)) {
        dbDisconnect(MUNICIPALITY_INFO)
      }
    })
  })

  # Refresh data from database
  observeEvent(input$refresh_data, {
    tryCatch({
      data <- load_municipality_data()
      values$original_data <- data
      values$current_data <- data
      values$changed_rows <- c()
      values$save_message <- "Data refreshed from database."
      values$show_save_message <- TRUE
      values$show_error_message <- FALSE
    }, error = function(e) {
      values$error_message <- paste("Error refreshing data:", e$message)
      values$show_error_message <- TRUE
      values$show_save_message <- FALSE
    })
  })

  # Statistics outputs
  output$total_municipalities <- renderValueBox({
    valueBox(
      value = nrow(values$current_data %||% data.frame()),
      subtitle = "Total Municipalities",
      icon = icon("building"),
      color = "blue"
    )
  })

  output$total_provinces <- renderValueBox({
    provinces <- length(unique(values$current_data$Province %||% character()))
    valueBox(
      value = provinces,
      subtitle = "Provinces",
      icon = icon("map"),
      color = "green"
    )
  })

  output$unsaved_changes <- renderValueBox({
    valueBox(
      value = length(values$changed_rows),
      subtitle = "Unsaved Changes",
      icon = icon("edit"),
      color = if(length(values$changed_rows) > 0) "yellow" else "green"
    )
  })

  # Charts
  output$province_chart <- renderPlot({
    req(values$current_data)
    province_counts <- values$current_data %>%
      count(Province, sort = TRUE)

    barplot(province_counts$n, names.arg = province_counts$Province,
            main = "Municipalities by Province",
            col = rainbow(nrow(province_counts)),
            las = 2, cex.names = 0.8)
  })

  output$type_chart <- renderPlot({
    req(values$current_data)
    type_counts <- values$current_data %>%
      count(Type, sort = TRUE)

    pie(type_counts$n, labels = paste(type_counts$Type, "\n(", type_counts$n, ")"),
        main = "Municipality Types",
        col = c("#3c8dbc", "#00a65a", "#f39c12"))
  })

  # Detailed summary
  output$detailed_summary <- renderText({
    req(filtered_data())

    total_records <- nrow(values$current_data %||% data.frame())
    filtered_records <- nrow(filtered_data())
    changed_records <- length(values$changed_rows)

    provinces <- filtered_data() %>% count(Province, sort = TRUE)
    types <- filtered_data() %>% count(Type, sort = TRUE)

    paste(
      "=== MUNICIPALITY DATA SUMMARY ===\n",
      "Total Records in Database:", total_records, "\n",
      "Currently Displayed:", filtered_records, "\n",
      "Unsaved Changes:", changed_records, "\n\n",
      "=== CURRENT VIEW BREAKDOWN ===\n",
      "Provinces:", paste(paste0(provinces$Province, " (", provinces$n, ")"), collapse = ", "), "\n\n",
      "Types:", paste(paste0(types$Type, " (", types$n, ")"), collapse = ", "), "\n\n",
      "=== DATABASE STATUS ===\n",
      if(changed_records > 0) "⚠️  You have unsaved changes!" else "✅ All changes saved"
    )
  })

  # Download handlers
  output$download_csv <- downloadHandler(
    filename = function() {
      paste("municipality_data_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(values$current_data, file, row.names = FALSE)
    }
  )

  output$download_excel <- downloadHandler(
    filename = function() {
      paste("municipality_data_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      openxlsx::write.xlsx(values$current_data, file)
    }
  )

  # Control message display
  output$show_save_message <- reactive({ values$show_save_message })
  output$show_error_message <- reactive({ values$show_error_message })
  outputOptions(output, "show_save_message", suspendWhenHidden = FALSE)
  outputOptions(output, "show_error_message", suspendWhenHidden = FALSE)

  output$save_message <- renderText({ values$save_message })
  output$error_message <- renderText({ values$error_message })
}

# Run the application
shinyApp(ui = ui, server = server)
