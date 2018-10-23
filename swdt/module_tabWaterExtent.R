# User interface
tabWaterExtentUI <- function(id) {
  # Create a namespace
  ns <- NS(id)

  fluidRow(
    column(
      4,
      bs_accordion(id = glue("help_text_", id)) %>%
        bs_set_opts(use_heading_link = TRUE, panel_type = "default") %>%
        bs_append(
          title = "Help",
          content = shiny::includeMarkdown("help/help_tabWaterExtent.md")
        ),
      tags$script(HTML(
        glue("document.getElementById(\"help_text_", id, "-0-collapse\").classList.remove('in');")
      )),
      panel(
        heading = "Classification",
        div(
          style = "position:relative;",
          div(
            style = "vertical-align:bottom;",
            numericInput(ns("threshold"),
              "Threshold",
              value = 0,
              width = "200px",
              step = 0.5
            )
          ),
          div(
            style = "position:absolute; bottom:0px; right:0px;",
            dropdownButton(
              numericInput(ns("outlier"), "Outlier", value = 30),
              circle = FALSE,
              status = "default",
              icon = NULL,
              width = "300px",
              size = "sm",
              right = FALSE
            )
          )
        ),
        withSpinner(
          plotOutput(ns("histogram"),
            click = ns("plot_click"),
            height = 250
          ),
          type = 8,
          color = "#008cba"
        ),
        div(
          style = "display: inline-block; vertical-align:top; ",
          numericInput(ns("filter_size"),
            label = "Filter",
            value = 3,
            width = "200px",
            step = 2
          )
        ),
        div(
          style = "display: inline-block; vertical-align:bottom; ",
          switchInput(ns("filter"),
            label = "Filter",
            value = FALSE
          )
        )
      ),
      panel(
        heading = "Statistics",
        withSpinner(plotlyOutput(ns("pie")),
                    type = 8,
                    color = "#008cba"
        )
      )
    ),
    column(
      8,
      tags$style(
        type = "text/css",
        "#tabWaterExtentMinimum-map {height: calc(100vh - 80px) !important;}
        #tabWaterExtentMaximum-map {height: calc(100vh - 80px) !important;}"
      ),
      withSpinner(leafletOutput(ns("map"), height = 700, width = "100%"),
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
  layer <- reactiveVal(NULL)

  observe({
    #' Switch between minimum and maximum
    #'
    req(tabProcessingInput()$temporal_statistics$minimum)
    req(tabProcessingInput()$temporal_statistics$maximum)

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

  pass_filter_size <- reactiveVal(3)
  pass_threshold <- reactiveVal(NULL)

  observe({
    #' Calculates threshold
    #'
    req(layer())

    threshold <-
      layer() %>%
      raster::as.matrix() %>%
      thres.gray()

    # Round
    threshold <- 0.5 * round(threshold / 0.5)

    pass_threshold(threshold)
    updateNumericInput(session, "threshold", value = isolate(pass_threshold()))
  })

  observeEvent(input$plot_click$x, {
    #' Workaround by passing plot_click$x through in reavtive value
    #' plot_click$x resets to NULL after a second
    #'
    if (!is.null(input$plot_click$x)) {
      # Round
      threshold <- 0.5 * round(input$plot_click$x / 0.5)
      pass_threshold(threshold)
      # Update numericInput of threshold with histogram selection
      updateNumericInput(session,
        "threshold",
        value = pass_threshold()
      )
    }
  })

  observeEvent(input$threshold, {
    #' Set pass_threshold if numeric input widgets changes
    #'
    req(pass_threshold())
    if (input$threshold != pass_threshold()) {
      # Prevents double calculation after plot click
      pass_threshold(input$threshold)
    }
  })

  observeEvent(input$filter_size, {
    #' Validates filter size
    #'
    if (isolate(input$filter_size) %% 2 == 0) {
      showModal(
        modalDialog("Filter size musst be an uneven number.")
      )
      updateNumericInput(session, "filter_size", value = pass_filter_size())
    } else {
      pass_filter_size(input$filter_size)
    }
  })

  observeEvent(input$map_click, {
    #' Add raster value popups to map
    #' Help by AF7
    #'
    click <- input$map_click
    if (!is.null(click)) {
      value <-
        st_point(c(click$lng, click$lat)) %>%
        st_sfc() %>%
        st_set_crs("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
        st_transform("+proj=merc +lon_0=0 +k=1 +x_0=0 +y_0=0 +a=6378137 +b=6378137 +towgs84=0,0,0,0,0,0,0 +units=m +nadgrids=@null +wktext +no_defs") %>%
        as_Spatial() %>%
        raster::extract(x = layer()) %>%
        round(2) %>%
        as.character()

      leafletProxy("map") %>%
        addPopups(click$lng, click$lat,
          popup = glue("<b>dB:</b> ", value),
          options = popupOptions(closeButton = TRUE)
        )
    }
  })

  water_extent <- reactiveVal(NULL)

  classify <- function(r, threshold, filter, filter_size, updateProgress) {
    #' Classify raster files based on threshold
    #'
    updateProgress(value = 0.1, detail = "Classify")

    r <- reclassify(r, c(
      -Inf,
      threshold,
      0,
      threshold,
      Inf,
      1
    ))

    if (filter) {
      updateProgress(value = 0.3, detail = "Filter")

      r <- focal(
        x = r,
        w = matrix(
          1,
          filter_size,
          filter_size
        ),
        fun = median,
      )
    }

    updateProgress(value = 0.8, detail = "Write")
    return(r)
  }

  compute_water_extent <- reactive({
    #' Compute water extent
    #'
    req(layer())
    req(pass_threshold())

    # Initialize progressbar
    progress <- shiny::Progress$new()
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

  stretch_radar <- reactive({
    #' Stretch radar image
    #'
    layer() %>%
      stretch(minq = 0.05, maxq = 0.95)
  })

  output$map <- renderLeaflet({
    #' Render leaflet ouput
    #'
    # Map coulering
    pal <- colorFactor(c("#008cba", "#f4f1e0"),
      c(0, 1),
      na.color = "transparent"
    )
    pal_radar <- colorNumeric(c("#000000", "#FFFFFF"),
      values(stretch_radar()),
      na.color = "transparent"
    )

    # Create map
    tabAOIInput()$shape_aoi() %>%
      leaflet() %>%
      addTiles() %>%
      addPolygons(fill = FALSE, color = "#008cba") %>%
      addRasterImage(compute_water_extent(),
        colors = pal,
        project = FALSE,
        group = "Classified",
        opacity = 1
      ) %>%
      addRasterImage(stretch_radar(),
        colors = pal_radar,
        project = FALSE,
        group = "Radar",
        opacity = 1
      ) %>%
      onRender("function(el,x,data){
               var map = this;
               var labels = map.layerManager._byGroup.Classified;
               var opacitySlider = new L.Control.opacitySlider();
               
               for (const prop in labels) {
               opacitySlider.setOpacityLayer(labels[prop]);
               }
               
               map.addControl(opacitySlider);}") %>%
      addLegend(
        position = "topright",
        pal = pal, values = c(0, 1),
        title = "Class",
        opacity = 1,
        labFormat = labelFormat(transform = function(x) {
          return(ifelse(x == 0, "Water", "Land"))
        })
      ) %>%
      addLayersControl(
        overlayGroups = c("Radar", "Classified"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  output$pie <- renderPlotly({
    #' Render pie chart with statistical measures
    #' 
    val <- raster::getValues(compute_water_extent())
    water_pixel <- tibble(Val = val) %>%
      filter(Val == 0) %>%
      summarise(Val = n()) %>%
      pull()
    
    land_pixel <- tibble(Val = val) %>%
      filter(Val == 1) %>%
      summarise(Val = n()) %>%
      pull()
    
    pixel_size <-
      compute_water_extent() %>%
      res()
    
    tibble(Class = character(), Area = numeric()) %>%
      add_row(
        Class = "Water",
        Area = round(water_pixel * pixel_size[1] * pixel_size[2] * 0.0001)
      ) %>%
      add_row(
        Class = "Land",
        Area = round(land_pixel * pixel_size[1] * pixel_size[2] * 0.0001)
      ) -> data
    
    colors <- c('#008cba', 'rgb(128,133,133)')
    
    plot_ly(data, type = 'pie', labels = ~Class, values = ~Area,
            textposition = 'inside',
            textinfo = 'label+percent',
            insidetextfont = list(color = '#FFFFFF'),
            hoverinfo = 'text',
            text = ~paste(Area, ' ha'),
            marker = list(colors = colors,
                          line = list(color = '#FFFFFF', width = 1)),
            showlegend = FALSE) %>%
      layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
             yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  })

  sample_raster <- reactive({
    #' Get sample from raster file
    #'
    sample <- layer() %>%
      sampleRandom(size = 10000)

    tibble(dB = sample)
  })

  compute_hist_data <- reactive({
    #' Compute histogram data by removing outlier
    #'
    plot_objekt <-
      ggplot(sample_raster(), aes(x = dB)) +
      geom_histogram(binwidth = 0.5)

    plot_data <- ggplot_build(plot_objekt)$data[[1]] %>%
      filter(count > input$outlier)

    x_min <-
      plot_data %>%
      slice(1) %>%
      dplyr::select(xmax) %>%
      pull()

    x_max <-
      plot_data %>%
      slice(nrow(plot_data)) %>%
      dplyr::select(xmin) %>%
      pull()


    sample_raster() %>%
      filter(dB > x_min) %>%
      filter(dB < x_max)
  })

  output$histogram <- renderPlot({
    #' Render histogram output
    #'
    req(layer())
    req(pass_threshold())

    # Add Open Sans font from Bootstrap layout to ggplot
    font_add_google("Open Sans", "opensans")
    showtext_auto()

    # Plot histogram
    ggplot(compute_hist_data(), aes(x = dB)) +
      geom_histogram(binwidth = 0.5, fill = "#008cba", alpha = 0.8) +
      xlab("VH Polarization Backscatter [dB]") + ylab("Frequency") +
      theme(
        text = element_text(size = 14, family = "opensans"),
        panel.background = element_rect(fill = "#ffffff"),
        axis.line = element_line(size = 1, colour = "#e7e7e7"),
        axis.ticks = element_blank()
      ) +
      geom_vline(xintercept = pass_threshold(), size = 1)
  })
  
  tabWaterExtentOutput <- reactive({
    #' Module ouput
    #'
    list(
      water_extent = water_extent()
    )
  })
  
  return(tabWaterExtentOutput)
}
