---
title: "tabProcessing"
output: html_document
---

This interface allows the processing of Sentinel-1 time series to minimum and maximum backscatter raster files.

The imagery was downloaded from the [Copernicus Open Access Hub](https://scihub.copernicus.eu/){target="_blank"} 
and processed using [SNAP](http://step.esa.int/main/toolboxes/snap/){target="_blank"}. 
During processing the imagery was  
- subsetted to the site extent,  
- masked from border noise artifacts,  
- updated with more precise orbit information,  
- geocoded,  
- topographically normalized and  
- scaled logarithmically to Decibel (dB).

See [here](S1A__IW___A_20141115T181801_bnr_Orb_Cal_TF_TC_dB_proc.xml){target="_blank"} for an exemplary SNAP workflow.

After processing the files were further resampled to the exact site boundaries and converted to single-layer GeoTiffs using [GDAL](https://www.gdal.org/){target="_blank"}.  
