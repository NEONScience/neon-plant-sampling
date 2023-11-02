
library(tidyverse)
library(restR2)

# load AOP file
aop <- read.csv(paste(getwd(), 'ltr_vegArea/allLitterPlots_CalculatedCHMpct_updatedAllSites.csv', sep='/'), 
                stringsAsFactors = F)

names(aop)

ing_fields <- c("plotID", "subplotID", "startDate", "endDate", "flightYear",
                "totalSampledArea",  "qualifyingVegetationArea", 
                "qualifyingVegetationPercent")

intersect(names(aop), ing_fields)

test <- aop[1:100,]%>%
  select(siteID, plotID=namedLocation, subplotID, totalSampledArea=plotArea, flightYear = Year,
         qualifyingVegetationArea=CHM_area, qualifyingVegetationPercent=CHM_percent)

test$startDate <- "2023-10-12"
test$endDate <- "2023-10-12"

setdiff(ing_fields, names(test))
intersect(ing_fields, names(test))

write.csv(test, paste(getwd(), 'ltr_vegArea/test_ingest_20231012.csv',sep='/'),
                      row.names=F)
