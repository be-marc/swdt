# Sentinel-1 Surface Water Dynamics Toolkit
# Marc Becker
# 2018

# Libraries
library(shiny)
library(shinyjs)
library(shinyWidgets)
library(shinycssloaders)
library(colourpicker)
library(DT)
library(leaflet)
library(sf)
library(glue)
library(tibble)
library(dplyr)
library(tidyr)
library(stringr)
library(raster)
library(uuid)
library(DBI)
library(showtext)
library(ggplot2)
library(tsar)
library(xml2)
library(htmlwidgets)
library(modeest)
library(lubridate)
library(bsplus)
library(tools)
library(plotly)
library(RSQLite)
library(rgdal)
library(rmarkdown)

# Source modules
source("module_tabAOI.R")
source("module_tabProcessing.R")
source("module_tabWaterExtent.R")
source("module_tabWaterDynamic.R")
source("thresholding.R")

# User Interface
ui <- tagList(
  # Add styles
  shiny::includeCSS("www/styles/custom.css"),
  shiny::includeCSS("www/styles/Control.Opacity.css"),
  shiny::includeCSS("www/styles/jquery-ui-1.10.3.custom.min.css"),
  # Add scripts
  shiny::includeScript("www/scripts/Control.Opacity.js"),
  shiny::includeScript("www/scripts/jquery-ui-1.10.3.custom.min.js"),
  shiny::includeScript("www/scripts/navigation_right.js"),
  shiny::includeScript("www/scripts/navigation_modal.js"),
  shinyjs::useShinyjs(),
  shiny::navbarPage(
    id = "navbar",
    theme = "styles/bootstrap.css",
    "Sentinel-1 Water Dynamics Toolkit",
    shiny::tabPanel(
      title = "AOI",
      id = "aoi",
      value = "aoi",
      tabAOIUI("tabAOI")
    ),
    shiny::tabPanel(
      title = "Processing",
      id = "processing",
      value = "processing",
      tabProcessingUI("tabProcessing")
    ),
    shiny::tabPanel(
      title = "Water Extent Minimum",
      id = "water_extent_minimum",
      value = "water_extent_minimum",
      tabWaterExtentUI("tabWaterExtentMinimum")
    ),
    shiny::tabPanel(
      title = "Water Extent Maximum",
      id = "water_extent_maximum",
      value = "water_extent_maximum",
      tabWaterExtentUI("tabWaterExtentMaximum")
    )
    ,
    shiny::tabPanel(
      "Water Dynamics",
      id = "water_dynamic",
      value = "water_dynamic",
      tabWaterDynamicUI("tabWaterDynamic")
    )
  )
)

# Server
server <- function(input, output, session) {
  read_config <- function() {
    xml <- xml2::read_xml("./config.xml")
    name <-
      xml %>%
      xml2::xml_find_all("//aoi/name") %>%
      xml2::xml_text()

    image <-
      xml %>%
      xml2::xml_find_all("//aoi/images") %>%
      xml2::xml_text()

    shape <-
      xml %>%
      xml2::xml_find_all("//aoi/shape") %>%
      xml2::xml_text()

    thumb <-
      xml %>%
      xml2::xml_find_all("//aoi/thumbs") %>%
      xml2::xml_text()

    parallel <-
      xml %>%
      xml2::xml_find_all("//aoi/parallel") %>%
      xml2::xml_text()

    tibble::tibble(
      Name = name,
      Image = image,
      Shape = shape,
      Thumb = thumb,
      Parallel = parallel
    )
  }

  # Modules
  tabAOIOutput <- shiny::callModule(tabAOI,
    "tabAOI",
    config = read_config(),
    app_session = session
  )

  tabProcessingOutput <- shiny::callModule(
    tabProcessing,
    "tabProcessing",
    tabAOIOutput,
    tabWaterExtentMinimumOutput,
    tabWaterExtentMaximumOutput,
    app_session = session
  )

  tabWaterExtentMinimumOutput <- shiny::callModule(
    tabWaterExtent,
    "tabWaterExtentMinimum",
    tabAOIOutput,
    tabProcessingOutput,
    mode = "maximum"
  )

  tabWaterExtentMaximumOutput <- shiny::callModule(
    tabWaterExtent,
    "tabWaterExtentMaximum",
    tabAOIOutput,
    tabProcessingOutput,
    mode = "minimum"
  )

  tabWaterDynamicOutput <- shiny::callModule(
    tabWaterDynamic,
    "tabWaterDynamic",
    tabAOIOutput,
    tabProcessingOutput,
    tabWaterExtentMinimumOutput,
    tabWaterExtentMaximumOutput
  )

  shiny::observe({
    #' Restrict access to tabs
    #'
    if (is.null(tabAOIOutput()$uuid())) {
      shinyjs::disable(selector = "#navbar li a[data-value=processing]")
    } else {
      shinyjs::enable(selector = "#navbar li a[data-value=processing]")
    }

    if (is.null(tabProcessingOutput()$temporal_statistics$minimum)) {
      shinyjs::disable(selector = "#navbar li a[data-value=\"water_extent_minimum\"]")
    } else {
      shinyjs::enable(selector = "#navbar li a[data-value=\"water_extent_minimum\"]")
    }

    if (is.null(tabWaterExtentMinimumOutput()$water_extent())) {
      shinyjs::disable(selector = "#navbar li a[data-value=\"water_extent_maximum\"]")
    } else {
      shinyjs::enable(selector = "#navbar li a[data-value=\"water_extent_maximum\"]")
    }

    if (is.null(tabWaterExtentMaximumOutput()$water_extent())) {
      shinyjs::disable(selector = "#navbar li a[data-value=\"water_dynamic\"]")
    } else {
      shinyjs::enable(selector = "#navbar li a[data-value=\"water_dynamic\"]")
    }
  })

  shiny::observeEvent(input$nav_processing, {
    #' Show modal if processing tab is unavailable
    #' Javascript: /www/scripts/navigation_modal.js
    #'
    if (is.null(tabAOIOutput()$uuid())) {
      shiny::showModal(
        shiny::modalDialog("No aoi selected.")
      )
    }
  })

  observeEvent(input$nav_water_extent_minimum, {
    #' Show modal if water extent minimum tab is unavailable
    #' Javascript: /www/scripts/navigation_modal.js
    #'
    if (is.null(tabProcessingOutput()$temporal_statistics$minimum)) {
      shiny::showModal(
        shiny::modalDialog("No processing executed.")
      )
    }
  })

  observeEvent(input$nav_water_extent_maximum, {
    #' Show modal if water extent maximum tab is unavailable
    #' Javascript: /www/scripts/navigation_modal.js
    #'
    if (is.null(tabWaterExtentMinimumOutput()$water_extent())) {
      shiny::showModal(
        shiny::modalDialog("No minimum water extent calculated.")
      )
    }
  })

  observeEvent(input$nav_water_dynamic, {
    #' Show modal if water dynamic tab is unavailable
    #' Javascript: /www/scripts/navigation_modal.js
    #'
    if (is.null(tabWaterExtentMaximumOutput()$water_extent())) {
      shiny::showModal(
        shiny::modalDialog("No maximum water extent calculated.")
      )
    }
  })

  shiny::observeEvent(input$nav_about, {
    #' Show about modal
    #' Javascript: /www/scripts/navigation_right.js
    #'
    shiny::showModal(
      shiny::modalDialog(shiny::includeHTML(
        suppressWarnings(
          rmarkdown::render("modal/tab_about.md",
            rmarkdown::html_document(template = paste0(
              getwd(),
              "/template/pandoc_template.html"
            )),
            quiet = TRUE
          )
        )
      ),
      size = "l"
      )
    )
  })

  shiny::observeEvent(input$nav_imprint, {
    #' Show imprint modal
    #' Javascript: /www/scripts/navigation_right.js
    #'
    shiny::showModal(
      shiny::modalDialog(shiny::includeHTML(
        suppressWarnings(
          rmarkdown::render("modal/tab_imprint.md",
            rmarkdown::html_document(template = paste0(
              getwd(),
              "/template/pandoc_template.html"
            )),
            quiet = TRUE
          )
        )
      ),
      size = "l"
      )
    )
  })

  shiny::observeEvent(input$nav_data_protection, {
    #' Show data protrection modal
    #' Javascript: /www/scripts/navigation_right.js
    #'
    shiny::showModal(
      shiny::modalDialog(shiny::includeHTML(
        suppressWarnings(
          rmarkdown::render("modal/tab_data_protection.md",
            rmarkdown::html_document(template = paste0(
              getwd(),
              "/template/pandoc_template.html"
            )),
            quiet = TRUE
          )
        )
      ),
      size = "l"
      )
    )
  })

  shiny::observeEvent(input$restart_session, {
    #' Restart session
    #'
    session$reload()
  })
  
  shiny::observeEvent(tabProcessingOutput()$clear_data, {
    #' Clear data if processing is done again
    #' 
    if(tabProcessingOutput()$clear_data > 0) {
      tabWaterExtentMinimumOutput()$water_extent(NULL)
      tabWaterExtentMaximumOutput()$water_extent(NULL)
    }
  })
}

# Run the application
shiny::shinyApp(ui = ui, server = server)
