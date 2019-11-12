library(dataRetrieval) # package that reads and downloads water data from USGS Water Quality Portal website
library(tidyverse)


# read in desired data: Inorganic cations/anions in stream water, on Big Island, Hawaii
water_data <- readWQPdata(statecode ="Hawaii", 
                          countycode = "Hawaii County", 
                          SiteType = c("Stream", "Spring", "Well"),
                          SampleMedia = "Water",
                          CharacteristicType = c("Inorganics, Major, Metals", 
                                                 "Inorganics, Major, Non-metals"))

# data fram with site info: name, lat/long, drainage area, etc
siteInfo <- attr(water_data, "siteInfo")

Summary <- water_data %>%
  select("MonitoringLocationIdentifier",
           "ActivityMediaSubdivisionName", 
           "ActivityStartDate", 
           "ActivityStartTime.Time",
           "CharacteristicName",
           "ResultSampleFractionText",
           "ResultMeasureValue",
           "ResultMeasure.MeasureUnitCode",
           "USGSPCode") %>%
  filter(ResultMeasure.MeasureUnitCode %in% c("mg/l", 'mg/l CaCO3', 'mg/l as Na')) %>%
  distinct() %>%
  group_by(MonitoringLocationIdentifier) %>%
  left_join(siteInfo, by = "MonitoringLocationIdentifier")


