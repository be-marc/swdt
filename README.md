
### General

The Sentinel-1 Water Dynamics Toolkit (SWDT) is a `Shiny` application
for processing Senintel-1 image time series to water dynamic maps.
Currently the application includes prepared Sentinel-1 GRD images of the
Fuente de Piedra Lagoon in Spain from January to December 2017.

The development of SWDT is supported by the Remote Sensing Section of
the Department of Geography at Friedrich-Schiller University Jena.

### Installation

You can download the whole application with demo data from Github and
restore the missing packages with `packrat`. Note that SWDT depends on
[`tsar`](https://github.com/johntruckenbrodt/tsar) and a
[`bsplus`](https://github.com/be-marc/bsplus) fork, which have to be
installed from GitHub.

### Data

Adding your own data involves several steps at the moment.

  - Pre-processing of Sentinel-1 GRD images with the
    [`pyroSAR`](https://github.com/johntruckenbrodt/pyroSAR) python
    library
  - Projection to EPSG 3857 and same extent
  - Supplying an area of interest as a shapefile in EPSG 4326
  - Converting of the pre-processed Sentinel-1 images to png files
    (utilities/tif\_to\_png.R)

After the data is pre-processed you have to add the following lines to
the `config.xml` file into the `config` tags.

    <aoi>
      <name>Fuente</name>
      <images>./data/fuente</images>
      <shape>./data/fuente</shape>
      <thumbs>/thumb/fuente</thumbs>
      <parallel>False</parallel>
    </aoi>

  - `<name>` Name of area of interest
  - `<images>` Path to directory with Sentinel-1 raster files
  - `<shape>` Path to aoi shapefile
  - `<thumbs>` Path to directory with Sentinel-1 png files
  - `<parallel>`Activate parallel processing with `tsar` package

`<images>` and `<shape>` can be an absolute paths or a path relative to
the shiny app root. `<thumbs>` must be a folder in the `./www` folder of
the shiny app.

### Features

  - Instant results in full-screen leaflet maps
  - Parallel processing of minimum and maximum backscatter of Senintel-1
    time series with `tsar` package
  - Set classification parameters interactively with histograms
  - Already calculated time series are cached and stored in a `sqlite`
    database

### Technical features

SWDT can give you some inspiration for your own shiny application.

#### Reactivity and modules

  - Pass information between multiple shiny modules with reactive lists
  - Add features to a plot by clicking inside
  - Complex reactivity between multiple input widgets on the same
    reactive value

#### Shiny

  - Use modals with disabled navigation bar elements
  - Filter DT tables with input widgets
  - Use modals and progression bars
  - Help text generated from markdown files
  - Left and right floating navigation bar

#### Styling

  - Customize `ggplot2` with `showtext` fonts to create a consistent
    layout
  - CSS modifications to customize the appearance of `DTOutput`

#### Leaflet for R

  - Add Leaflet.OpacityControls plugin to `Leaflet for R`
  - Add referenced png images to map
  - Change the label of categorical legend entries
