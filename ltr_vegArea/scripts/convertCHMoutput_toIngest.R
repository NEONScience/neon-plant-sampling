
library(tidyverse)
library(restR2)

# load AOP file
aop <- read.csv(paste(getwd(), 'ltr_vegArea/allLitterPlots_CalculatedCHMpct_20231026.csv', sep='/'), 
                stringsAsFactors = F)

names(aop)

ing_fields <- c("plotID", "subplotID", "startDate", "endDate", "flightYear",
                "totalSampledArea",  "qualifyingVegetationArea", 
                "qualifyingVegetationPercent")

intersect(names(aop), ing_fields)

out <- aop%>%
  select(startDate, endDate, siteID, plotID=namedLocation, subplotID, totalSampledArea=plotArea, flightYear = Year,
         qualifyingVegetationArea=CHM_area, qualifyingVegetationPercent=CHM_percent)

out$subplotID <- ifelse(nchar(out$subplotID==2), paste(out$subplotID, "400", sep='_'), out$subplotID)

setdiff(ing_fields, names(out))
intersect(ing_fields, names(out))


### subset of (deprecated?) plots errored out on ingest

# 
# BLAN_041.basePlot.ltr activity date '2019-06-01T13:00Z' does not have a valid active period. Row=3039.
# BLAN_041.basePlot.ltr activity date '2021-08-13T13:00Z' does not have a valid active period. Row=4024.
# BLAN_041.basePlot.ltr activity date '2022-05-21T12:00Z' does not have a valid active period. Row=4843.

# DELA_049.basePlot.ltr activity date '2019-04-29T13:00Z' does not have a valid active period. Row=3257.
# DELA_049.basePlot.ltr activity date '2019-04-29T13:00Z' does not have a valid active period. Row=3258.
# DELA_049.basePlot.ltr activity date '2021-05-07T13:00Z' does not have a valid active period. Row=4304.
# DELA_049.basePlot.ltr activity date '2021-05-07T13:00Z' does not have a valid active period. Row=4305.
# DELA_049.basePlot.ltr activity date '2023-05-27T13:00Z' does not have a valid active period. Row=5644.
# DELA_049.basePlot.ltr activity date '2023-05-27T13:00Z' does not have a valid active period. Row=5645.
out$flightYear[out$plotID=='BLAN_041.basePlot.ltr']
out$flightYear[out$plotID=='DELA_049.basePlot.ltr']

out <- out%>%
  filter(!(plotID=="BLAN_041.basePlot.ltr" & flightYear>2018))%>%
  filter(!(plotID=="DELA_049.basePlot.ltr" & flightYear>2018))

#verify
out$flightYear[out$plotID=='BLAN_041.basePlot.ltr']
out$flightYear[out$plotID=='DELA_049.basePlot.ltr']

write.csv(out, paste(getwd(), 'ltr_vegArea/ingest_20231102.csv',sep='/'),
                      row.names=F)


