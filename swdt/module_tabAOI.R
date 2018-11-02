# User interface
tabAOIUI <- function(id) {
  # Create a namespace
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      3,
      bsplus::bs_accordion(id = glue("help_text_", id)) %>%
        bsplus::bs_set_opts(use_heading_link = TRUE, panel_type = "default") %>%
        bsplus::bs_append(
          title = "Help",
          content = shiny::includeHTML(
            suppressWarnings(
              rmarkdown::render("help/help_tabAOI.md",
                                rmarkdown::html_document(
                                  template = paste0(getwd(), "/template/pandoc_template.html")),
                quiet = TRUE
              )
            )
          )
        ),
      shinyWidgets::panel(
        heading = "AOI",
        uiOutput(ns("aoi")),
        actionButton(ns("start_session"), "Start Session"),
        actionButton(ns("restart_session"), "Restart Session")
      )
    ),
    shiny::column(
      9,
      leaflet::leafletOutput(ns("map"), height = 700, width = "100%")
    )
  )
}

# Server
tabAOI <- function(input, output, session, config, app_session) {
  shiny::observe({
    shinyjs::disable("restart_session")
  })

  aoi_data <- reactiveVal(NULL)

  shiny::observe({
    #' Check configuration file
    #'
    true_config <-
      config %>%
      dplyr::filter(dir.exists(Image))

    false_config <-
      suppressWarnings(dplyr::setdiff(config, true_config))

    # Validation
    if (nrow(true_config) == 0) {
      shiny::showModal(
        shiny::modalDialog("No valid path in configuration file")
      )
    } else {
      aoi_data(true_config)

      if (nrow(false_config) > 0) {
        false_names <-
          false_config %>%
          dplyr::select(Name) %>%
          dplyr::pull()

        shiny::showModal(
          shiny::modalDialog(glue(
            "No valid path in configuration file for aoi ",
            glue::glue_collapse(false_names, ",", last = " and ")
          ))
        )
      }
    }
  })

  output$aoi <- shiny::renderUI({
    #' Render aoi selection
    #'
    if (is.null(aoi_data())) {
      shinyjs::disable("start_session")
    }
    shiny::req(aoi_data())
    shiny::selectInput(session$ns("aoi"),
      choices = aoi_data()$Name,
      label = NULL,
      selected = aoi_data()$Name[1]
    )
  })

  uuid <- shiny::reactiveVal(NULL)
  image_path <- shiny::reactiveVal(NULL)
  shape_path <- shiny::reactiveVal(NULL)
  thumb_path <- shiny::reactiveVal(NULL)
  parallel <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$start_session, {
    #' Starts Session
    #'
    shiny::req(input$aoi)
    shinyjs::disable("aoi")
    shinyjs::disable("start_session")

    input$aoi %>%
      glue("-", uuid::UUIDgenerate()) %>%
      uuid()

    aoi_data() %>%
      dplyr::filter(Name == input$aoi) %>%
      dplyr::select(Image) %>%
      dplyr::pull() %>%
      image_path()

    aoi_data() %>%
      dplyr::filter(Name == input$aoi) %>%
      dplyr::select(Shape) %>%
      dplyr::pull() %>%
      shape_path()

    aoi_data() %>%
      dplyr::filter(Name == input$aoi) %>%
      dplyr::select(Thumb) %>%
      dplyr::pull() %>%
      thumb_path()

    aoi_data() %>%
      dplyr::filter(Name == input$aoi) %>%
      dplyr::select(Parallel) %>%
      dplyr::pull() %>%
      parallel()

    # Change to processing tab
    shiny::updateTabsetPanel(app_session, inputId = "navbar", selected = "processing")

    shinyjs::enable("restart_session")
  })

  shape_aoi <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$aoi, {
    #' Read shapefile
    #'
    shiny::req(input$aoi)

    path <- aoi_data() %>%
      dplyr::filter(Name == input$aoi) %>%
      dplyr::select(Shape) %>%
      dplyr::pull()

    dsn <- dirname(path)
    layer <-
      basename(path) %>%
      tools::file_path_sans_ext()

    rgdal::readOGR(dsn, layer, verbose = FALSE) %>%
      shape_aoi()
  })

  output$map <- renderLeaflet({
    #' Render leaflet map
    #'
    shiny::req(input$aoi)
    shiny::req(shape_aoi())


    if (input$aoi == "NA") {
      leaflet::leaflet() %>%
        leaflet::setView(lng = 25.19, lat = 54.54, zoom = 4) %>%
        leaflet::addTiles()
    } else {
      shape_aoi() %>%
        leaflet::leaflet() %>%
        leaflet::addTiles() %>%
        leaflet::addPolygons(fill = FALSE, color = "#008cba")
    }
  })

  shiny::observeEvent(input$restart_session, {
    #' Restart session
    #'
    session$reload()
  })

  tabAOIOutput <- shiny::reactive({
    #' Module output
    #'
    list(
      aoi = input$aoi,
      uuid = uuid,
      shape_aoi = shape_aoi,
      image_path = image_path,
      shape_path = shape_path,
      thumb_path = thumb_path,
      parallel = parallel
    )
  })

  return(tabAOIOutput)
}
