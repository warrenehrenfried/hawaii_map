## This script uses the dataRetrieval package created by the USGS for retrieva
## of their water quality data. The data frame has been summarized to only
## contain necessary information, and has removed any data with "total" rather
## than "dissolved" concentrations

library(dataRetrieval) # package that reads and downloads water data from USGS Water Quality Portal website
library(tidyverse) # perform dplyr piping operations


# read in desired data: Major inorganic ions in stream and well water, 
# on the Big Island, Hawaii
siteData <- readWQPdata(statecode ="Hawaii", 
                        countycode = "Hawaii County", 
                        SiteType = c("Stream", "Spring", "Well"),
                        SampleMedia = "Water",
                        CharacteristicType = c("Inorganics, Major, Metals", 
                                               "Inorganics, Major, Non-metals"))

# data frame with site info: name, lat/long, drainage area, etc
siteInfo <- attr(siteData, "siteInfo")

# combine both tables for one comprehensive table of data + site ID
Hawaii_water_data <- siteData %>%
  left_join(siteInfo, by = "MonitoringLocationIdentifier") %>%
  select("MonitoringLocationIdentifier",
         "MonitoringLocationName",
         "MonitoringLocationTypeName",
         "ActivityStartDate", 
         "ActivityStartTime.Time",
         "CharacteristicName",
         "ResultSampleFractionText",
         "ResultMeasureValue",
         "ResultMeasure.MeasureUnitCode",
         "USGSPCode",
         "WellDepthMeasure.MeasureValue",
         "WellDepthMeasure.MeasureUnitCode",
         "LatitudeMeasure",
         "LongitudeMeasure") %>%
  filter(ResultMeasure.MeasureUnitCode %in% c("mg/l", 'mg/l CaCO3', 'mg/l as Na'),
         ResultSampleFractionText != "Total") %>% #,
         #CharacteristicName == "Sulfate") %>%
  distinct() %>%
  group_by(MonitoringLocationIdentifier)

cname <- Hawaii_water_data %>%
  group_by(CharacteristicName, MonitoringLocationIdentifier,
           LatitudeMeasure, LongitudeMeasure) %>%
  summarise(avg = mean(ResultMeasureValue))


