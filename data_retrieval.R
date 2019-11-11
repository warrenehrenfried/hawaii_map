library(dataRetrieval)
library(dplyr)

water_data <- readWQPdata(statecode="Hawaii", 
                        countycode = 001, 
                        siteType = "Stream",
                        SampleMedia = "Water",
                        characteristictype="Inorganics, Major, Metals")
unique(water_data$ResultMeasure.MeasureUnitCode)

siteInfo <- attr(phosData, "siteInfo")

wiSummary <- phosData %>%
  filter(ResultMeasure.MeasureUnitCode %in% c("mg/l","mg/l as P")) %>%
  group_by(MonitoringLocationIdentifier) %>%
  summarise(count=n(),
            start=min(ActivityStartDateTime),
            end=max(ActivityStartDateTime),
            max = max(ResultMeasureValue, na.rm = TRUE)) %>%
  filter(count > 300) %>%
  arrange(-count) %>%
  left_join(siteInfo, by = "MonitoringLocationIdentifier")



