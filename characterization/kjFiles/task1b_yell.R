## load libraries ##
library(tidyverse)
library(neonUtilities)
library(restR)
library(magrittr)
library(readxl)
library(neonutils)
library(neondata)
library(neontask1b)

## Pull portal data for YELL vst and div ##
# get dpids
#div = "DP1.10058.001"
#vst = "DP1.10098.001"
#nst = "DP1.10045.001"

##
div_portal <- loadByProduct(dpID= "DP1.10058.001",
                     site = "YELL",
                     package = "basic",
                     check.size = FALSE, 
                     token = Sys.getenv('NEON_KEY'))

vst_portal <- loadByProduct(dpID= "DP1.10098.001",
                     site = "YELL",
                     package = "basic",
                     check.size = FALSE, 
                     token = Sys.getenv('NEON_KEY'))

# nst <- loadByProduct(dpID= "DP1.10045.001",
#                      site = "YELL",
#                      package = "basic",
#                      check.size = FALSE, 
#                      token = Sys.getenv('NEON_KEY'))

# unlist all data frames
list2env(div_portal,.GlobalEnv)
#list2env(nst,.GlobalEnv) #nst table not generated for YELL
list2env(vst_portal,.GlobalEnv)

#rm(nst) 
rm(div_portal)
rm(vst_portal)
#rm(categoricalCodes_10045)
rm(categoricalCodes_10058)
rm(categoricalCodes_10098)
rm(validation_10058)
rm(validation_10098)
rm(variables_10058)
rm(variables_10098)

## look at csp div and vst formatting
#load(file = "neon-tos-sampling-design/code/neondata/data/vst.rda")
#load(file = "neon-tos-sampling-design/code/neondata/data/div.rda")

divExample <- read.csv('C:/Users/kjones/Documents/GitHub/CSP/neon-tos-sampling-design/code/neondata/data-raw/vst_div/20180822/div_PUUM_merged.csv', stringsAsFactors = FALSE)

mapExample <- read.csv('C:/Users/kjones/Documents/GitHub/CSP_yellOneOff/neon-tos-sampling-design/code/neondata/data-raw/vst_div/20180822/vst_mapping_PUUM.csv', stringsAsFactors = FALSE)

indExample <- read.csv('C:/Users/kjones/Documents/GitHub/CSP_yellOneOff/neon-tos-sampling-design/code/neondata/data-raw/vst_div/20180822/vst_apparentindividual_PUUM.csv', stringsAsFactors = FALSE)

plotExample <- read.csv('C:/Users/kjones/Documents/GitHub/CSP_yellOneOff/neon-tos-sampling-design/code/neondata/data-raw/vst_div/20180822/vst_perplot_PUUM.csv', stringsAsFactors = FALSE)

spatialExample <- read.csv('C:/Users/kjones/Documents/GitHub/CSP_yellOneOff/neon-tos-sampling-design/code/neondata/data-raw/vst_div/20180822/PUUM_spatialData.csv', stringsAsFactors = FALSE)


## format and save so it looks the same as data delivered to csp ##
#div
csp_divNames <- names(divExample)
#vst
csp_mapNames <- names(mapExample)
csp_indNames <- names(indExample)
csp_plotNames <- names(plotExample)
csp_spatialNames <- names(spatialExample)

#my new plots
pList <- c("YELL_011", "YELL_016", "YELL_006", "YELL_013", "YELL_003")


## prep div df
div_1_sub <- select(div_1m2Data, intersect(names(div_1m2Data), csp_divNames), subplotID, endDate)
div_100_sub <- select(div_10m2Data100m2Data, intersect(names(div_10m2Data100m2Data), csp_divNames), subplotID, endDate) 

div <- bind_rows(div_1_sub, div_100_sub)
div$year <- substr(div$endDate, 1, 4)

div_yell <- div%>%
  filter(plotID%in%pList & year=="2019" & !is.na(taxonID))


write.csv(div_yell, 'neon-tos-sampling-design/code/neondata/data-raw/vst_div/20180817/div_YELL_merged.csv', 
          row.names = FALSE)
rm(div)

##prep vst map df
mapNames <- intersect(names(vst_mappingandtagging), csp_mapNames)
map_sub <- select(vst_mappingandtagging, 
                  intersect(names(vst_mappingandtagging), csp_mapNames), 
                            supportingStemTagID=supportingStemIndividualID,
                            identicficationQualifier=identificationQualifier)

write.csv(map_sub, 'neon-tos-sampling-design/code/neondata/data-raw/vst_div/20210210/vst_mapping_YELL.csv', 
          row.names = FALSE)

##prep vst ind df
intersect(csp_indNames, names(vst_apparentindividual))
setdiff(csp_indNames, names(vst_apparentindividual))

vst_apparentindividual <- vst_apparentindividual%>%
  rename(maxCanopyDiameter = maxCrownDiameter,
         ninetyCanopyDiameter = ninetyCrownDiameter,
         stemStatus = plantStatus, 
         shrubShape = shape, 
         maxBaseCanopyDiameter = maxBaseCrownDiameter,
         ninetyBaseCanopyDiameter = ninetyBaseCrownDiameter)

vst_apparentindividual$stemDiameter <- ifelse(vst_apparentindividual$growthForm=="sap", 
                                ifelse(is.na(vst_apparentindividual$stemDiameter),
                                       vst_apparentindividual$basalStemDiameter,
                                       vst_apparentindividual$stemDiameter),
                                vst_apparentindividual$stemDiameter)
 

ind_sub <- vst_apparentindividual%>%
  filter(plotID%in%pList & eventID=="vst_YELL_2019" & stemStatus!="No longer qualifies")%>%
  select(intersect(csp_indNames, names(vst_apparentindividual)))

unique(ind_sub$growthForm)

#growth form coded in csp, full name on portal, recode

ind_sub$growthForm <- ifelse(ind_sub$growthForm=="small shrub", "sms",
                              ifelse(ind_sub$growthForm=="single bole tree", "sbt", 
                                     ifelse(ind_sub$growthForm=="sapling", "sap", ind_sub$growthForm)))

ind$stemStatus <- ifelse(ind$stemStatus=="Live", 1,
                         ifelse(ind$stemStatus=="Standing dead", 2,
                                ifelse(ind$stemStatus=="Dead, broken bole", 10, ind$stemStatus)))

write.csv(ind, 'neon-tos-sampling-design/code/neondata/data-raw/vst_div/20180817/vst_apparentindividual_YELL.csv', 
          row.names = FALSE)




write.csv(vst_yell, 'kjFiles/YELL_vst_2019.csv', 
          row.names = FALSE)


### prep spatial data
# load(file = "neon-tos-sampling-design/code/neondata/data/perPlot.rda")
# load(file = "neon-tos-sampling-design/code/neondata/data/plotArea.rda")
# load(file = "neon-tos-sampling-design/code/neondata/data/plotCoordinates.rda")

#perPlot
intersect(csp_plotNames, names(vst_perplotperyear))
setdiff(csp_plotNames, names(vst_perplotperyear))


plot_sub <- vst_perplotperyear%>%
  filter(plotID%in%pList & eventID=="vst_YELL_2019")%>%
  select(intersect(csp_plotNames, names(vst_perplotperyear)), 
         nestedSubplotAreaShrubSapling)

plot_sub$nestedSubplotAreaShrubSapling <- as.integer(plot_sub$nestedSubplotAreaShrubSapling)

plot_sub$nestedSubplotAreaLiana <- ifelse(plot_sub$nestedSubplotAreaLiana=="noneSelected", 100, 
plot_sub$nestedSubplotAreaLiana)

#save(perPlot, file = "neon-tos-sampling-design/code/neondata/data/perPlot.rda")


plot_sub$nestedSubplotAreaOther <- 100

write.csv(plot_sub, 'neon-tos-sampling-design/code/neondata/data-raw/vst_div/20180817/vst_perplot_YELL.csv', 
          row.names = FALSE)

#plot area
daveFile <- read.csv('kjFiles/allSpatialInfo2018December26.csv')
intersect(csp_spatialNames, names(daveFile))
setdiff(csp_spatialNames, names(daveFile))

spatial_sub <-  daveFile%>%
  filter(plotID%in%pList & subtype=="basePlot")%>%
  select(intersect(csp_spatialNames, names(daveFile)))%>%
  distinct()
 
spatial_sub$plotSize <- 400

write.csv(spatial_sub, 'neon-tos-sampling-design/code/neondata/data-raw/vst_div/YELL_spatial_data.csv', 
          row.names = FALSE) 

## plotCoordinates
# all neon dat
# csp object
load(file = "neon-tos-sampling-design/code/neondata/data/plotCoordinates.rda")
names(plotCoordinates)

#anything missing?
setdiff(names(plotCoordinates), names(daveFile))
# referencePointPosition - not sure if this is used anywhere?

coord_out <- daveFile%>%
  filter(plotID%in%pList)%>%
  select(one_of(names(plotCoordinates)))%>%
  distinct(plotID, .keep_all = TRUE)

write.csv(coord_out, 'neon-tos-sampling-design/code/neondata/data-raw/vst_div/pointSpatialData_YELL.csv', 
          row.names = FALSE) 

#########################################################
rm(list = ls())

# Get vst and div site level summaries
vst_yell <- read.csv('kjFiles/YELL_vst_2019.csv', 
                     stringsAsFactors=FALSE)


div_yell <- read.csv('kjFiles/YELL_div_2019.csv', 
                     stringsAsFactors=FALSE)


## run abundance code ##
## add missing fields that are called in nested functions 
## these are both shrubgroup fields, YELL does not have shrubgroups
vst_yell$canopyArea <- NA
vst_yell$liveCanopyArea <- NA
##vstAbundance doesn't work, YELL plots are not in csp plot lists ##
vstAbundance <- vstAbundanceBySite(vst_yell)
divAbundance <- divAbundanceBySite(div_yell)

