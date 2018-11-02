# User interface
tabWaterExtentUI <- function(id) {
  # Create a namespace
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      4,
      bsplus::bs_accordion(id = glue("help_text_", id)) %>%
        bsplus::bs_set_opts(use_heading_link = TRUE, panel_type = "default") %>%
        bsplus::bs_append(
          title = "Help",
          show = FALSE,
          content = shiny::includeHTML(
            suppressWarnings(
              rmarkdown::render(
                "help/help_tabWaterExtent.md",
                rmarkdown::html_document(
                  template = paste0(getwd(), "/template/pandoc_template.html")
                ),
                quiet = TRUE
              )
            )
          )
        ),
      shinyWidgets::panel(
        heading = "Classification",
        shiny::div(
          style = "position:relative;",
          shiny::div(
            style = "vertical-align:bottom;",
            numericInput(ns("threshold"),
              "Threshold",
              value = 0,
              width = "200px",
              step = 0.5
            )
          ),
          shiny::div(
            style = "position:absolute; bottom:0px; right:0px;",
            shinyWidgets::dropdownButton(
              shiny::numericInput(ns("outlier"), "Outlier", value = 30),
              circle = FALSE,
              status = "default",
              icon = NULL,
              width = "300px",
              size = "sm",
              right = FALSE
            )
          )
        ),
        shinycssloaders::withSpinner(
          shiny::plotOutput(ns("histogram"),
            click = ns("plot_click"),
            height = 250
          ),
          type = 8,
          color = "#008cba"
        ),
        shiny::div(
          style = "display: inline-block; vertical-align:top; ",
          shiny::numericInput(ns("filter_size"),
            label = "Filter",
            value = 3,
            width = "200px",
            step = 2
          )
        ),
        shiny::div(
          style = "display: inline-block; vertical-align:bottom; ",
          switchInput(ns("filter"),
            label = "Filter",
            value = FALSE
          )
        )
      ),
      shinyWidgets::panel(
        heading = "Statistics",
        shinycssloaders::withSpinner(plotlyOutput(ns("pie")),
          type = 8,
          color = "#008cba"
        )
      )
    ),
    shiny::column(
      8,
      shinycssloaders::withSpinner(leaflet::leafletOutput(ns("map"),
        height = 700,
        width = "100%"
      ),
      type = 8,
      color = "#008cba"
      )
    )
  )
}

tabWaterExtent <- function(input,
                           output,
                           session,
                           tabAOIInput,
                           tabProcessingInput,
                           mode) {
  layer <- shiny::reactiveVal(NULL)

  shiny::observe({
    #' Switch between minimum and maximum
    #'
    shiny::req(tabProcessingInput()$temporal_statistics$minimum)
    shiny::req(tabProcessingInput()$temporal_statistics$maximum)

    if (mode == "minimum") {
      layer(tabProcessingInput()$temporal_statistics$minimum)
    } else if (mode == "maximum") {
      layer(tabProcessingInput()$temporal_statistics$maximum)
    }
  })

  observe({
    #' Deactivated input widgets at start
    #'
    shinyjs::disable("filter_size")
    shinyjs::disable("base_raster")
  })

  observeEvent(input$filter, {
    #' Enable and disable filter size input
    #'
    if (input$filter) {
      shinyjs::enable("filter_size")
    } else {
      shinyjs::disable("filter_size")
    }
  })

  pass_filter_size <- shiny::reactiveVal(3)
  pass_threshold <- shiny::reactiveVal(NULL)

  shiny::observe({
    #' Calculates threshold
    #'
    shiny::req(layer())

    threshold <-
      layer() %>%
      raster::as.matrix() %>%
      thres.gray()

    # Round
    threshold <- 0.5 * round(threshold / 0.5)

    pass_threshold(threshold)
    shiny::updateNumericInput(session, "threshold",
      value = isolate(pass_threshold())
    )
  })

  shiny::observeEvent(input$plot_click$x, {
    #' Workaround by passing plot_click$x through in reavtive value
    #' plot_click$x resets to NULL after a second
    #'
    if (!is.null(input$plot_click$x)) {
      # Round
      threshold <- 0.5 * round(input$plot_click$x / 0.5)
      pass_threshold(threshold)
      # Update numericInput of threshold with histogram selection
      shiny::updateNumericInput(session,
        "threshold",
        value = pass_threshold()
      )
    }
  })

  shiny::observeEvent(input$threshold, {
    #' Set pass_threshold if numeric input widgets changes
    #'
    shiny::req(pass_threshold())
    if (input$threshold != pass_threshold()) {
      # Prevents double calculation after plot click
      pass_threshold(input$threshold)
    }
  })

  shiny::observeEvent(input$filter_size, {
    #' Validates filter size
    #'
    if (shiny::isolate(input$filter_size) %% 2 == 0) {
      shiny::showModal(
        shiny::modalDialog("Filter size musst be an uneven number.")
      )
      shiny::updateNumericInput(session, "filter_size", value = pass_filter_size())
    } else {
      pass_filter_size(input$filter_size)
    }
  })

  shiny::observeEvent(input$map_click, {
    #' Add raster value popups to map
    #' Help by AF7
    #'
    click <- input$map_click
    if (!is.null(click)) {
      value <-
        sf::st_point(c(click$lng, click$lat)) %>%
        sf::st_sfc() %>%
        sf::st_set_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
        sf::st_transform("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +nadgrids=@null +wktext +no_defs") %>%
        sf::as_Spatial() %>%
        raster::extract(x = layer()) %>%
        round(2) %>%
        as.character()

      leaflet::leafletProxy("map") %>%
        leaflet::addPopups(click$lng, click$lat,
          popup = glue::glue("<b>dB:</b> ", value),
          options = popupOptionspopupOptions(closeButton = TRUE)
        )
    }
  })

  water_extent <- shiny::reactiveVal(NULL)

  classify <- function(r, threshold, filter, filter_size, updateProgress) {
    #' Classify raster files based on threshold
    #'
    updateProgress(value = 0.1, detail = "Classify")

    r <- raster::reclassify(r, c(
      -Inf,
      threshold,
      0,
      threshold,
      Inf,
      1
    ))

    if (filter) {
      updateProgress(value = 0.3, detail = "Filter")

      if (tabAOIInput()$parallel() == "True") {
        # Apply median filter in parallel with spatial.tools package
        sfQuickInit(cpus = 4)

        # Smoother funktion
        median_smoother <- function(inraster) {
          smoothed <- apply(inraster, 3, median)
          return(smoothed)
        }
        
        # Run filter and convert to raster layer
        r <- rasterEngine(inraster = r, fun = median_smoother, window_dims = c(3, 3))
        r <- brickstack_to_raster_list(r)[[1]]
        sfQuickStop()
      } else {
        # Apply median filter
        r <- raster::focal(
          x = r,
          w = matrix(
            1,
            filter_size,
            filter_size
          ),
          fun = median,
        )
      }
    }

    updateProgress(value = 0.8, detail = "Write")
    return(r)
  }

  compute_water_extent <- shiny::reactive({
    #' Compute water extent
    #'
    shiny::req(layer())
    shiny::req(pass_threshold())

    # Initialize progressbar
    progress <- Progress$new()
    progress$set(message = "Classification", detail = "Get", value = 0)

    updateProgress <- function(value = NULL, detail = NULL) {
      progress$set(value = value, detail = detail)
    }

    # Classify raster
    r <- classify(
      layer(),
      pass_threshold(),
      input$filter,
      pass_filter_size(),
      updateProgress
    )
    progress$close()
    water_extent(r)
    r
  })

  stretch_radar <- shiny::reactive({
    #' Stretch radar image
    #'
    layer() %>%
      raster::stretch(minq = 0.05, maxq = 0.95)
  })


  output$map <- leaflet::renderLeaflet({
    #' Render leaflet ouput
    #'
    # Map coulering
    pal <- leaflet::colorFactor(c("#008cba", "#f4f1e0"),
      c(0, 1),
      na.color = "transparent"
    )
    pal_radar <- leaflet::colorNumeric(c("#000000", "#FFFFFF"),
      raster::values(stretch_radar()),
      na.color = "transparent"
    )

    # Create map
    tabAOIInput()$shape_aoi() %>%
      leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addPolygons(fill = FALSE, color = "#008cba") %>%
      leaflet::addRasterImage(compute_water_extent(),
        colors = pal,
        project = FALSE,
        group = "Classified",
        layerId = "Classified",
        opacity = 1
      ) %>%
      leaflet::addRasterImage(stretch_radar(),
        colors = pal_radar,
        project = FALSE,
        group = "Radar",
        opacity = 1
      ) %>%
      htmlwidgets::onRender("function(el,x,data){
          var map = this;
          var opacitySlider = new L.Control.opacitySlider();
          var opacityLayer = map.layerManager.getLayer('image', data.layerId)

          opacitySlider.setOpacityLayer(opacityLayer);
          map.addControl(opacitySlider);
        }", data = list(layerId = "Classified")) %>%
      leaflet::addLegend(
        position = "topright",
        pal = pal, values = c(0, 1),
        title = "Class",
        opacity = 1,
        labFormat = leaflet::labelFormat(transform = function(x) {
          return(ifelse(x == 0, "Water", "Land"))
        })
      ) %>%
      leaflet::addLayersControl(
        overlayGroups = c("Radar", "Classified"),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )
  })

  output$pie <- plotly::renderPlotly({
    #' Render pie chart with statistical measures
    #'
    val <- raster::getValues(compute_water_extent())
    water_pixel <- tibble::tibble(Val = val) %>%
      dplyr::filter(Val == 0) %>%
      dplyr::summarise(Val = n()) %>%
      dplyr::pull()

    land_pixel <- tibble::tibble(Val = val) %>%
      dplyr::filter(Val == 1) %>%
      dplyr::summarise(Val = n()) %>%
      dplyr::pull()

    pixel_size <-
      compute_water_extent() %>%
      res()

    tibble(Class = character(), Area = numeric()) %>%
      tibble::add_row(
        Class = "Water",
        Area = round(water_pixel * pixel_size[1] * pixel_size[2] * 0.0001)
      ) %>%
      tibble::add_row(
        Class = "Land",
        Area = round(land_pixel * pixel_size[1] * pixel_size[2] * 0.0001)
      ) -> data

    colors <- c("#008cba", "rgb(128,133,133)")

    plotly::plot_ly(data,
      type = "pie", labels = ~Class, values = ~Area,
      textposition = "inside",
      textinfo = "label+percent",
      insidetextfont = list(color = "#FFFFFF"),
      hoverinfo = "text",
      text = ~paste(Area, " ha"),
      marker = list(
        colors = colors,
        line = list(color = "#FFFFFF", width = 1)
      ),
      showlegend = FALSE
    ) %>%
      plotly::layout(
        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)
      )
  })

  sample_raster <- shiny::reactive({
    #' Get sample from raster file
    #'
    sample <- layer() %>%
      raster::sampleRandom(size = 10000)

    tibble::tibble(dB = sample)
  })

  compute_hist_data <- shiny::reactive({
    #' Compute histogram data by removing outlier
    #'
    plot_objekt <-
      ggplot2::ggplot(sample_raster(), ggplot2::aes(x = dB)) +
      ggplot2::geom_histogram(binwidth = 0.5)

    plot_data <- ggplot2::ggplot_build(plot_objekt)$data[[1]] %>%
      dplyr::filter(count > input$outlier)

    x_min <-
      plot_data %>%
      dplyr::slice(1) %>%
      dplyr::select(xmax) %>%
      dplyr::pull()

    x_max <-
      plot_data %>%
      dplyr::slice(nrow(plot_data)) %>%
      dplyr::select(xmin) %>%
      dplyr::pull()


    sample_raster() %>%
      dplyr::filter(dB > x_min) %>%
      dplyr::filter(dB < x_max)
  })

  output$histogram <- shiny::renderPlot({
    #' Render histogram output
    #'
    shiny::req(layer())
    shiny::req(pass_threshold())

    # Add Open Sans font from Bootstrap layout to ggplot
    sysfonts::font_add_google("Open Sans", "opensans")
    showtext::showtext_auto()

    # Plot histogram
    ggplot2::ggplot(compute_hist_data(), ggplot2::aes(x = dB)) +
      ggplot2::geom_histogram(binwidth = 0.5, fill = "#008cba", alpha = 0.8) +
      ggplot2::xlab("VH Polarization Backscatter [dB]") +
      ggplot2::ylab("Frequency") +
      ggplot2::theme(
        text = ggplot2::element_text(size = 14, family = "opensans"),
        panel.background = ggplot2::element_rect(fill = "#ffffff"),
        axis.line = ggplot2::element_line(size = 1, colour = "#e7e7e7"),
        axis.ticks = ggplot2::element_blank()
      ) +
      ggplot2::geom_vline(xintercept = pass_threshold(), size = 1)
  })

  tabWaterExtentOutput <- shiny::reactive({
    #' Module ouput
    #'
    list(
      water_extent = water_extent(),
      threshold = pass_threshold,
      filter = input$filter,
      filter_size = pass_filter_size
    )
  })

  return(tabWaterExtentOutput)
}
