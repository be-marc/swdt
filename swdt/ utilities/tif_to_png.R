# Script for converting pyroSAR tif files to png files
# 
library(tools)
library(raster)
library(rgdal)
library(glue)

# Config
in_path <- "./swdt/data/fuente"
out_path <- "./swdt/www/thumb/fuente/"

paths <- list.files(in_path, "^S1", full.names = TRUE)
names <- list.files(in_path, "^S1")

rgdal::setCPLConfigOption("GDAL_PAM_ENABLED", "FALSE")

for(i in 1:length(paths)) {
  raster(paths[i]) %>%
    stretch(minq=0.05, maxq=0.95) -> r 
    r[r == 255] <- 254
    r %>%
    as('SpatialPixelsDataFrame') %>%
    rgdal::writeGDAL(glue(out_path, file_path_sans_ext(names[i]), ".png"),
                     drivername = "PNG", type = "Byte", mvFlag = 255) 
}