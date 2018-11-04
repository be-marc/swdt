# User interface
tabWaterDynamicUI <- function(id) {
  # Create a namespace
  ns <- shiny::NS(id)

  shiny::fluidRow(
    shiny::column(
      3,
      bsplus::bs_accordion(id = glue("help_text_", id)) %>%
        bsplus::bs_set_opts(use_heading_link = TRUE, panel_type = "default") %>%
        bsplus::bs_append(
          title = "Help",
          show = FALSE,
          content = shiny::includeHTML(
            suppressWarnings(
              rmarkdown::render(
                'help/help_tabWaterDynamic.md', 
                rmarkdown::html_document(
                  template = paste0(getwd(), 
                                    '/template/pandoc_template.html')), 
                   quiet = TRUE)
              )
            )
        ),
      shinyWidgets::panel(
        heading = "Visualization",
        colourpicker::colourInput(ns("color_class_0"), "Never flooded", "#f4f1e0"),
        colourpicker::colourInput(ns("color_class_1"), "Temporarily flooded", "#9ecae1"),
        colourpicker::colourInput(ns("color_class_2"), "Permanently flooded", "#008CBA")
      ),
      shinyWidgets::panel(
        shiny::downloadButton(ns("download"), "Download")
      )
    ),
    shiny::column(
      9,
      shinycssloaders::withSpinner(
        leaflet::leafletOutput(ns("map"),
          height = 700,
          width = "100%"
        ),
        type = 8,
        color = "#008cba"
      )
    )
  )
}

tabWaterDynamic <- function(input,
                            output,
                            session,
                            tabAOIInput,
                            tabProcessingInput,
                            tabWaterExtentMinimumInput,
                            tabWaterExtentMaximumInput) {
  compute_map <- shiny::reactive({
    #' Compute map
    #'
    shiny::withProgress(message = "Classification", value = 0, {
      raster::stack(
        tabWaterExtentMinimumInput()$water_extent(),
        tabWaterExtentMaximumInput()$water_extent()
      ) %>%
        raster::calc(sum) %>%
        raster::reclassify(c(-Inf, 0, 0, 0, 1, 1, 2, Inf, 2))
    })
  })

  water_dynamic_map <- shiny::reactiveVal()

  output$map <- leaflet::renderLeaflet({
    #' Render leaflet ouput
    #'
    shinyjs::disable("download")
    r <- compute_map()
    water_dynamic_map(r)
    shinyjs::enable("download")

    # Create color palete
    pal <- leaflet::colorFactor(c(
      input$color_class_2,
      input$color_class_1,
      input$color_class_0
    ),
    c(0, 1, 2),
    na.color = "transparent"
    )

    # Render map
    tabAOIInput()$shape_aoi() %>%
      leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addPolygons(fill = FALSE, color = "#008cba") %>%
      leaflet::addRasterImage(r, colors = pal, project = FALSE) %>%
      leaflet::addLegend(
        position = "topright",
        pal = pal, values = c(0, 1, 2),
        title = "Class",
        opacity = 1,
        labFormat = leaflet::labelFormat(transform = function(x) {
          label <- tibble::tibble(Value = x) %>%
            dplyr::mutate(Label = dplyr::case_when(
              Value == 0 ~ "Permanently flooded",
              Value == 1 ~ "Temporarily flooded",
              Value == 2 ~ "Never flooded"
            )) %>%
            dplyr::select(Label) %>%
            dplyr::pull()
          return(label)
        })
      )
  })
  
  output$download <- shiny::downloadHandler(
    #' Download tiff file
    #'
    glue::glue(tabAOIInput()$aoi, "_", 
         strftime(tabProcessingInput()$start_date(), "%Y-%m-%d"), 
         "_", 
         strftime(tabProcessingInput()$end_date(), "%Y-%m-%d"),
         "_", 
         tabWaterExtentMinimumInput()$threshold(),
         "_",
         tabWaterExtentMinimumInput()$filter,
         "_", 
         tabWaterExtentMinimumInput()$filter_size(),
         "_", 
         tabWaterExtentMaximumInput()$threshold(),
         "_",
         tabWaterExtentMaximumInput()$filter,
         "_", 
         tabWaterExtentMaximumInput()$filter_size(),
         ".tif"),
    content = function(file) {
      # Create color map
      color_table <- tibble::tibble(class = c(0, 1, 2),
                            color = c(input$color_class_2, 
                                      input$color_class_1,
                                      input$color_class_0))
      
      # Write raster
      rgdal::writeGDAL(as(water_dynamic_map(), 'SpatialGridDataFrame'), 
                file, 
                colorTables = list(color_table$color), 
                mvFlag = 255,
                type="Byte")
    },
    contentType = "image/tiff"
  )
}