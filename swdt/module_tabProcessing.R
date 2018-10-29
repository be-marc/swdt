# User interface
tabProcessingUI <- function(id) {
  # Create a namespace
  ns <- NS(id)

  fluidRow(
    column(
      4,
      bs_accordion(id = glue("help_text_", id)) %>%
        bs_set_opts(use_heading_link = TRUE, panel_type = "default") %>%
        bs_append(
          title = "Help",
          content = shiny::includeMarkdown("help/help_tabProcessing.md")
        ),
      tags$script(HTML(
        glue("document.getElementById(\"help_text_", id, "-0-collapse\").classList.remove('in');")
      )),
      panel(
        heading = "Filter",
        uiOutput(ns("date_range")),
        div(
          style = "display: inline-block; vertical-align:bottom; ",
          actionButton(ns("current_month"), "Current Month")
        ),
        div(
          style = "display: inline-block; vertical-align:bottom; ",
          actionButton(ns("last_month"), "Last Month")
        ),
        DTOutput(ns("table")),
        actionButton(ns("calculate"), "Calculate")
      )
    ),
    column(
      8,
      tags$style(
        type = "text/css",
        "#tabProcessing-map {height: calc(100vh - 80px) !important;}"
      ),
      leafletOutput(ns("map"),
        height = 700,
        width = "100%"
      )
    )
  )
}

# Server
tabProcessing <- function(input, output, session, tabAOIInput, app_session) {
  files <- reactive({
    #' Creates data table with available Sentinel-1 scenes
    #'
    files <- list.files(tabAOIInput()$image_path(), "^S1.*\\.tif$")
    paths <- list.files(tabAOIInput()$image_path(),
      "^S1.*\\.tif$",
      full.names = TRUE
    )
    thumbs <- list.files(tabAOIInput()$thumb_path(), "^S1.*\\.png$")
    thumbs <-
      str_sub(tabAOIInput()$thumb_path(), 7) %>%
      paste0("/", thumbs)

    as_tibble(files) %>%
      separate(value,
        c("Mission", "Mode", "E", "Date", "Polarisation"),
        "_+",
        extra = "drop",
        fill = "right"
      ) %>%
      dplyr::select(-one_of("E")) %>%
      mutate(Date = str_sub(Date, 1, 8)) %>%
      mutate(Date = as.Date(Date, "%Y%m%d")) %>%
      cbind(files) %>%
      cbind(paths) %>%
      cbind(thumbs) %>%
      mutate_if(is.factor, as.character) %>%
      arrange(Date)
  })

  start_date <- reactiveVal()
  end_date <- reactiveVal()

  output$date_range <- renderUI({
    #' Render date range input
    #'
    # Set date range to last available month
    max_date <-
      files() %>%
      dplyr::select(Date) %>%
      filter(Date == max(Date)) %>%
      pull()

    month(max_date) <- month(max_date) + 1
    day(max_date) <- 1
    max_date <- max_date - 1

    end_date(max_date)

    min_date <- max_date
    day(min_date) <- 1

    start_date(min_date)

    dateRangeInput(session$ns("date_range"),
      label = "Date Range",
      start = isolate(start_date()),
      end = isolate(end_date()),
      language = "de"
    )
  })

  observeEvent(input$current_month, {
    #' Set date range to current month
    #'
    date <- Sys.Date()
    day(date) <- 1
    start_date(date)

    month(date) <- month(date) + 1
    end_date(date - 1)
  })

  observeEvent(input$last_month, {
    #' Set date range to last month
    #'
    date <- Sys.Date()
    day(date) <- 1
    month(date) <- month(date) - 1
    start_date(date)

    month(date) <- month(date) + 1
    end_date(date - 1)
  })

  observe({
    #' Update date range
    #'
    updateDateRangeInput(session,
      "date_range",
      start = start_date(),
      end = end_date()
    )
  })

  observeEvent(input$date_range, {
    #' Validate date range input
    #'
    if (input$date_range[1] > input$date_range[2]) {
      showModal(
        modalDialog("You cannot enter a start date later than the end date.")
      )
      # Cannot updated by observe function because reactiveVal does not change
      updateDateRangeInput(session,
        "date_range",
        start = isolate(start_date()),
        end = isolate(end_date())
      )
    } else {
      start_date(input$date_range[1])
      end_date(input$date_range[2])
    }
  })

  output$table <- renderDT({
    #' Render table with available Sentinel-1 scenes
    #'
    req(start_date())
    req(end_date())

    files() %>%
      dplyr::select("Mission", "Mode", "Date") %>%
      filter(Date > start_date()) %>%
      filter(Date < end_date())
  },
  style = "bootstrap",
  server = TRUE, selection = "single",
  options = list(
    iDisplayLength = 10,
    aLengthMenu = c(5, 10),
    bLengthChange = 0,
    bFilter = 0,
    bInfo = 0,
    bAutoWidth = 1
  )
  )

  thumb_extent <- reactive({
    #' Get raster extent for thumbs
    #'
    req(files())

    extent_raster <- files() %>%
      slice(1) %>%
      dplyr::select(paths) %>%
      pull() %>%
      raster() %>%
      projectRaster(crs = crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")) %>%
      extent()

    glue(
      "[[", extent_raster@ymin,
      ", ", extent_raster@xmin,
      "], [", extent_raster@ymax, ", ",
      extent_raster@xmax, "]]"
    )
  })

  output$map <- renderLeaflet({
    #' Render leaflet ouput
    #'
    map <-
      tabAOIInput()$shape_aoi() %>%
      leaflet() %>%
      addTiles() %>%
      addPolygons(fill = FALSE, color = "#008cba")

    # Add Senintel-1 raster file
    if (is.null(input$table_row_last_clicked)) {
      map
    } else {
      r <-
        files() %>%
        slice(input$table_row_last_clicked) %>%
        dplyr::select(thumbs) %>%
        pull()

      map %>%
        htmlwidgets::onRender(paste0("function(el, x) {
                                 var map = this;
                                 var imageUrl = \'", r, "\';
                                 var imageBounds = ", thumb_extent(), ";
                                 L.imageOverlay(imageUrl, imageBounds).addTo(map);
    }"))
    }
  })

  temporal_statistics <- reactiveValues(minimum = NULL, maximum = NULL)

  observeEvent(input$calculate, {
    #' Calculate minium and maximum backscatter raster files from time series
    #' Searches for cached data in sqlite database
    #'
    shinyjs::disable("calculate")
    withProgress(
      message = "Calculation",
      detail = "Searching",
      value = 0, {

        # Create database folder
        if (!dir.exists("./database")) {
          dir.create("./database")
        }

        # Conntect to data base
        con <- dbConnect(RSQLite::SQLite(),
          dbname = "./database/swdt.sqlite"
        )

        # Create table if missing
        if (length(dbListTables(con)) == 0) {
          dbGetQuery(con, "CREATE TABLE temporal_statistic(
                             id INTEGER PRIMARY KEY NOT NULL,
                             aoi TEXT,
                             creation_date TEXT,
                             start_time TEXT,
                             end_time TEXT,
                             path_min TEXT,
                             path_max TEXT)")
        }

        # Search for cached data
        res <- dbGetQuery(con, glue(
          "SELECT * FROM temporal_statistic WHERE start_time = \'",
          strftime(start_date(), "%Y-%m-%dT%H:%M:%S%z"),
          "\' AND end_time = \'",
          strftime(end_date(), "%Y-%m-%dT%H:%M:%S%z"),
          "\' AND aoi = \'",
          tabAOIInput()$aoi,
          "\'"
        ))

        # Calculate, no cached data
        if (nrow(res) == 0) {
          s <-
            files() %>%
            filter(Date > start_date()) %>%
            filter(Date < end_date()) %>%
            dplyr::select("paths") %>%
            pull() %>%
            raster::stack()

          path_min <- glue(
            tabAOIInput()$image_path(),
            "/minimum/minimum_",
            tabAOIInput()$aoi,
            "_",
            strftime(Sys.time(), "%Y-%m-%dT%H-%M-%S"),
            "_",
            strftime(start_date(), "%Y-%m-%d"),
            "_",
            strftime(end_date(), "%Y-%m-%d"),
            ".tif"
          )
          
          if(!dir.exists(dirname(path_min))) {
            dir.create(dirname(path_min))
          }

          path_max <- glue(
            tabAOIInput()$image_path(),
            "/maximum/maximum-",
            tabAOIInput()$aoi,
            "_",
            strftime(Sys.time(), "%Y-%m-%dT%H-%M-%S"),
            "_",
            strftime(start_date(), "%Y-%m-%d"),
            "_",
            strftime(end_date(), "%Y-%m-%d"),
            ".tif"
          )
          
          if(!dir.exists(dirname(path_max))) {
            dir.create(dirname(path_max))
          }

          if (tabAOIInput()$parallel() == "True") {
            # Parallel calculation with tsar package
            incProgress(0.2, detail = "Minimum")
            
            tsar(s,
              workers = list(minimum = function(x) return(min(x, na.rm = T))),
              cores = 4,
              out.name = path_min,
              out.bandnames = NULL,
              out.dtype = "FLT4S",
              separate = FALSE,
              na.in = NA,
              na.out = -999,
              overwrite = TRUE,
              verbose = FALSE,
              nodelist = NULL,
              bandorder = "BSQ",
              maxmemory = 1000,
              compress_tif = F
            )

            temporal_statistics[["minimum"]] <- raster(path_min)

            incProgress(0.6, detail = "Maximum")

            tsar(s,
              workers = list(maximum = function(x) return(max(x, na.rm = T))),
              cores = 4,
              out.name = path_max,
              out.bandnames = NULL,
              out.dtype = "FLT4S",
              separate = FALSE,
              na.in = NA,
              na.out = -999,
              overwrite = TRUE,
              verbose = FALSE,
              nodelist = NULL,
              bandorder = "BSQ",
              maxmemory = 1000,
              compress_tif = F
            )

            temporal_statistics[["maximum"]] <- raster(path_max)
          } else {
            # Calculation with raster package
            incProgress(0.2, detail = "Minimum")

            r_minimum <- calc(s, min)
            temporal_statistics[["minimum"]] <- r_minimum
            writeRaster(r_minimum, path_min, overwrite = TRUE)

            incProgress(0.6, detail = "Maximum")

            r_maximum <- calc(s, max)
            temporal_statistics[["maximum"]] <- r_maximum
            writeRaster(r_maximum, path_max, overwrite = TRUE)
          }

          # Write to database
          dbGetQuery(con, glue(
            "INSERT INTO temporal_statistic (
                             aoi,
                             creation_date,
                             start_time, 
                             end_time, 
                             path_min, 
                             path_max) VALUES (\'",
            tabAOIInput()$aoi,
            "\', \'",
            strftime(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
            "\', \'",
            strftime(start_date(), "%Y-%m-%dT%H:%M:%S%z"),
            "\', \'",
            strftime(end_date(), "%Y-%m-%dT%H:%M:%S%z"),
            "\', \'",
            path_min,
            "\', \'",
            path_max,
            "\')"
          ))
        } else { # Cached data available
          incProgress(0.5, detail = "Data in cache")

          r_minimum <-
            res %>%
            dplyr::select(path_min) %>%
            slice(1) %>%
            pull() %>%
            raster()

          temporal_statistics[["minimum"]] <- r_minimum

          r_maximum <-
            res %>%
            dplyr::select(path_max) %>%
            slice(1) %>%
            pull() %>%
            raster()

          temporal_statistics[["maximum"]] <- r_maximum
        }
      }
    )
    shinyjs::enable("calculate")
    showModal(
      modalDialog("Minimum and maximum backscatter raster files successfully calculated.",
        footer = tagList(
          modalButton("Dismiss"),
          actionButton(session$ns("next_tab"), "Next")
        )
      )
    )
  })

  observeEvent(input$next_tab, {
    #' Change to water extent tab after calculation
    #'
    removeModal()
    updateTabsetPanel(app_session, inputId = "navbar", selected = "water_extent_minimum")
  })

  tabProcessingOutput <- reactive({
    #' Module ouput
    #'
    list(
      temporal_statistics = temporal_statistics,
      start_date = start_date,
      end_date = end_date
    )
  })

  return(tabProcessingOutput)
}
