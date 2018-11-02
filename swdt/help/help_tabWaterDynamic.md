---
title: "tabWaterDynamic"
output: html_document
bibliography: swdt_bibliography.bib
nocite: | 
  @Brisco15, @DiBaldassarre11, @Martinis16, @White15
---
This interface allows the creation of a water dynamics map based on the water extent maps.

By combining the maximum and minimum water extent from the multi-temporal minimum and maximum backscatter respectively, a simple water dynamics map can be created.

This map is only an approximation since several effects might influence the quality of the classification.  
For example, the maximum backscatter statistic might be heavily influenced by the sea state as described in the earlier section and contrast might not be sufficient to threshold the image.  
Furthermore, dry sandy soils are known for appearing very dark in the image because the radiation is "swallowed" by the sand and thus does not return to the sensor.  
Another source of error is the presence of vegetation. Flooding underneath vegetation is not possible with this approach using Sentinel-1 data. While the used wavelength of approx. 5 cm (C-Band) is not well suited for looking through vegetation, other wavelengths like the 20 cm L-Band used by e.g. ALOS PALSAR might be used to achieve this. See the references for further reading on the subject.

#### References