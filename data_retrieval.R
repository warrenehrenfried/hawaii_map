library(dataRetrieval) # package that reads and downloads water data from USGS Water Quality Portal website
library(dplyr)


# read in desired data: Inorganic cations/anions in stream water, on Big Island, Hawaii
water_data <- readWQPdata(statecode ="Hawaii", 
                          countycode = "Hawaii County", 
                          SiteType = "Stream",
                          SampleMedia = "Water",
                          CharacteristicType ="Inorganics, Major, Metals")

unique(water_data$ResultMeasure.MeasureUnitCode)

# data fram with site info: name, lat/long, drainage area, etc
siteInfo <- attr(water_data, "siteInfo")

Summary <- water_data %>%
  filter(ResultMeasure.MeasureUnitCode %in% "mg/l") %>%
  group_by(MonitoringLocationIdentifier) %>%
  summarise(count=n(),
            start=min(ActivityStartDateTime),
            end=max(ActivityStartDateTime),
            max = max(ResultMeasureValue, na.rm = TRUE)) %>%
  filter(count > 300) %>%
  arrange(-count) %>%
  left_join(siteInfo, by = "MonitoringLocationIdentifier")



