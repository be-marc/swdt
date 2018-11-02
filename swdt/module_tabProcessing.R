# User interface
tabProcessingUI <- function(id) {
  # Create a namespace
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      width = 4,
      bsplus::bs_accordion(id = glue::glue("help_text_", id)) %>%
        bsplus::bs_set_opts(use_heading_link = TRUE, panel_type = "default") %>%
        bsplus::bs_append(
          title = "Help",
          show = FALSE,
          content = shiny::includeHTML(
            suppressWarnings(
              rmarkdown::render(
                "help/help_tabProcessing.md",
                rmarkdown::html_document(
                  template = paste0(getwd(), "/template/pandoc_template.html")),
                quiet = TRUE
              )
            )
          )
        ),
      shinyWidgets::panel(
        heading = "Filter",
        shiny::uiOutput(ns("date_range")),
        shiny::div(
          style = "display: inline-block; vertical-align:bottom; ",
          shiny::actionButton(ns("current_month"), "Current Month")
        ),
        shiny::div(
          style = "display: inline-block; vertical-align:bottom; ",
          shiny::actionButton(ns("last_month"), "Last Month")
        ),
        DT::DTOutput(ns("table")),
        shiny::actionButton(ns("calculate"), "Calculate")
      )
    ),
    shiny::column(
      width = 8,
      shiny::tags$style(
        type = "text/css",
        "#tabProcessing-map {height: calc(100vh - 80px) !important;}"
      ),
      leaflet::leafletOutput(ns("map"),
        height = 700,
        width = "100%"
      )
    )
  )
}

# Server
tabProcessing <- function(input, output, session, tabAOIInput, app_session) {
  files <- shiny::reactive({
    #' Creates data table with available Sentinel-1 scenes
    #'
    files <- list.files(tabAOIInput()$image_path(), "^S1.*\\.tif$")
    paths <- list.files(tabAOIInput()$image_path(),
      "^S1.*\\.tif$",
      full.names = TRUE
    )

    thumbs <- list.files(tabAOIInput()$thumb_path(), "^S1.*\\.png$")
    thumbs <-
      stringr::str_sub(tabAOIInput()$thumb_path(), 7) %>%
      paste0("/", thumbs)

    tibble::as_tibble(files) %>%
      tidyr::separate(value,
        c("Mission", "Mode", "E", "Date", "Polarisation"),
        "_+",
        extra = "drop",
        fill = "right"
      ) %>%
      dplyr::select(-one_of("E")) %>%
      dplyr::mutate(Date = str_sub(Date, 1, 8)) %>%
      dplyr::mutate(Date = as.Date(Date, "%Y%m%d")) %>%
      cbind(files) %>%
      cbind(paths) %>%
      cbind(thumbs) %>%
      dplyr::mutate_if(is.factor, as.character) %>%
      dplyr::arrange(Date)
  })

  start_date <- shiny::reactiveVal()
  end_date <- shiny::reactiveVal()

  output$date_range <- shiny::renderUI({
    #' Render date range input
    #'
    # Set date range to last available month
    max_date <-
      files() %>%
      dplyr::select(Date) %>%
      dplyr::filter(Date == max(Date)) %>%
      dplyr::pull()

    lubridate::month(max_date) <- lubridate::month(max_date) + 1
    lubridate::day(max_date) <- 1
    max_date <- max_date - 1

    end_date(max_date)

    min_date <- max_date
    lubridate::day(min_date) <- 1

    start_date(min_date)

    shiny::dateRangeInput(session$ns("date_range"),
      label = "Date Range",
      start = isolate(start_date()),
      end = isolate(end_date()),
      language = "de"
    )
  })

  shiny::observeEvent(input$current_month, {
    #' Set date range to current month
    #'
    date <- Sys.Date()
    lubridate::day(date) <- 1
    start_date(date)

    lubridate::month(date) <- lubridate::month(date) + 1
    end_date(date - 1)
  })

  shiny::observeEvent(input$last_month, {
    #' Set date range to last month
    #'
    date <- Sys.Date()
    lubridate::day(date) <- 1
    lubridate::month(date) <- lubridate::month(date) - 1
    start_date(date)

    lubridate::month(date) <- lubridate::month(date) + 1
    end_date(date - 1)
  })

  shiny::observe({
    #' Update date range
    #'
    shiny::updateDateRangeInput(session,
      "date_range",
      start = start_date(),
      end = end_date()
    )
  })

  shiny::observeEvent(input$date_range, {
    #' Validate date range input
    #'
    if (input$date_range[1] > input$date_range[2]) {
      shiny::showModal(
        shiny::modalDialog("You cannot enter a start date later than the end date.")
      )
      # Cannot updated by observe function because reactiveVal does not change
      shiny::updateDateRangeInput(session,
        "date_range",
        start = isolate(start_date()),
        end = isolate(end_date())
      )
    } else {
      start_date(input$date_range[1])
      end_date(input$date_range[2])
    }
  })

  output$table <- DT::renderDT({
    #' Render table with available Sentinel-1 scenes
    #'
    req(start_date())
    req(end_date())

    files() %>%
      dplyr::select("Mission", "Mode", "Date") %>%
      dplyr::filter(Date > start_date()) %>%
      dplyr::filter(Date < end_date())
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

  thumb_extent <- shiny::reactive({
    #' Get raster extent for thumbs
    #'
    req(files())

    extent_raster <- files() %>%
      slice(1) %>%
      dplyr::select(paths) %>%
      dplyr::pull() %>%
      raster::raster() %>%
      raster::projectRaster(crs = crs("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs ")) %>%
      raster::extent()

    c(extent_raster@ymin, extent_raster@xmin, extent_raster@ymax, extent_raster@xmax)
  })

  output$map <- leaflet::renderLeaflet({
    #' Render leaflet ouput
    #'
    map <-
      tabAOIInput()$shape_aoi() %>%
      leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addPolygons(fill = FALSE, color = "#008cba")

    # Add Senintel-1 raster file
    if (is.null(input$table_row_last_clicked)) {
      map
    } else {
      png <-
        files() %>%
        dplyr::slice(input$table_row_last_clicked) %>%
        dplyr::select(thumbs) %>%
        dplyr::pull()
      map %>%
        htmlwidgets::onRender("function(el, x, data) {
          var map = this;
          var imageUrl = data.png;
          var imageBounds = [[data.thumb_extent[0], data.thumb_extent[1]], [data.thumb_extent[2], data.thumb_extent[3]]];
          L.imageOverlay(imageUrl, imageBounds).addTo(map);
        }", data = list(png = png, thumb_extent = thumb_extent()))
    }
  })

  temporal_statistics <- shiny::reactiveValues(minimum = NULL, maximum = NULL)

  shiny::observeEvent(input$calculate, {
    #' Calculate minium and maximum backscatter raster files from time series
    #' Searches for cached data in sqlite database
    #'
    shinyjs::disable("calculate")
    shiny::withProgress(
      message = "Calculation",
      detail = "Searching",
      value = 0, {

        # Create database folder
        if (!dir.exists("./database")) {
          dir.create("./database")
        }

        # Conntect to data base
        con <- DBI::dbConnect(RSQLite::SQLite(),
          dbname = "./database/swdt.sqlite"
        )

        # Create table if missing
        if (length(DBI::dbListTables(con)) == 0) {
          DBI::dbGetQuery(con, "CREATE TABLE temporal_statistic(
                             id INTEGER PRIMARY KEY NOT NULL,
                             aoi TEXT,
                             creation_date TEXT,
                             start_time TEXT,
                             end_time TEXT,
                             path_min TEXT,
                             path_max TEXT)")
        }

        # Search for cached data
        res <- DBI::dbGetQuery(con, glue::glue(
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
            dplyr::filter(Date > start_date()) %>%
            dplyr::filter(Date < end_date()) %>%
            dplyr::select("paths") %>%
            dplyr::pull() %>%
            raster::stack()

          path_min <- glue::glue(
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

          if (!dir.exists(dirname(path_min))) {
            dir.create(dirname(path_min))
          }

          path_max <- glue::glue(
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

          if (!dir.exists(dirname(path_max))) {
            dir.create(dirname(path_max))
          }

          if (tabAOIInput()$parallel() == "True") {
            # Parallel calculation with tsar package
            shiny::incProgress(0.2, detail = "Minimum")

            tsar::tsar(s,
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

            temporal_statistics[["minimum"]] <- raster::raster(path_min)

            shiny::incProgress(0.6, detail = "Maximum")

            tsar::tsar(s,
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

            temporal_statistics[["maximum"]] <- raster::raster(path_max)
          } else {
            # Calculation with raster package
            shiny::incProgress(0.2, detail = "Minimum")

            r_minimum <- calc(s, min)
            temporal_statistics[["minimum"]] <- r_minimum
            raster::writeRaster(r_minimum, path_min, overwrite = TRUE)

            shiny::incProgress(0.6, detail = "Maximum")

            r_maximum <- calc(s, max)
            temporal_statistics[["maximum"]] <- r_maximum
            raster::writeRaster(r_maximum, path_max, overwrite = TRUE)
          }

          # Write to database
          DBI::dbGetQuery(con, glue::glue(
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
          shiny::incProgress(0.5, detail = "Data in cache")

          r_minimum <-
            res %>%
            dplyr::select(path_min) %>%
            dplyr::slice(1) %>%
            dplyr::pull() %>%
            raster::raster()

          temporal_statistics[["minimum"]] <- r_minimum

          r_maximum <-
            res %>%
            dplyr::select(path_max) %>%
            dplyr::slice(1) %>%
            dplyr::pull() %>%
            raster::raster()

          temporal_statistics[["maximum"]] <- r_maximum
        }
      }
    )
    shinyjs::enable("calculate")
    shiny::showModal(
      shiny::modalDialog("Minimum and maximum backscatter raster files successfully calculated.",
        footer = shiny::tagList(
          shiny::modalButton("Dismiss"),
          shiny::actionButton(session$ns("next_tab"), "Next")
        )
      )
    )
  })

  shiny::observeEvent(input$next_tab, {
    #' Change to water extent tab after calculation
    #'
    shiny::removeModal()
    shiny::updateTabsetPanel(app_session, inputId = "navbar", selected = "water_extent_minimum")
  })

  tabProcessingOutput <- shiny::reactive({
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
