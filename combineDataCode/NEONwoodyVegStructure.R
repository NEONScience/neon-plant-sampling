

rm(list=ls())

knitr::opts_chunk$set(echo = TRUE)

options("stringsAsFactors" = FALSE)

library(neonUtilities)
library(tidyverse)


###filter sites and plots

#we should set this up so it filters to plots and years that we have available from the plant presence and percent cover or herbaceous cover data.  
#Until we figure that out we can filter using this file that specifies which site-plot combinations are sampled for for that herbaceous cover data as designated by the 'div'

applicableModulesMaster<-read.csv(file = "C:/Users/dbarnett/Documents/GitHub/neonPlantSampling/combineDataCode/applicableModules.csv",head=TRUE,sep=",", stringsAsFactors = F)

divPlots <- applicableModulesMaster[grepl("div", applicableModulesMaster$applicableModules), ]
#if want only distributed plots where more likely to have canopy cover measures
divPlots <- filter(divPlots, plotType == "distributed")
divPlots <- select(divPlots, plotID)
divPlotsFilter <- as.character(divPlots[,1])

####Pull in the woody data

dpid=as.character('DP1.10098.001')

sites <- c("SRER", "OSBS")

df <- loadByProduct(dpID=dpid,
                    #site = 'all',
                    site = sites,
                    package = "basic",
                    check.size = FALSE)

##unlist to create separate dataframes
perplot <- df$vst_perplotperyear
appind <- df$vst_apparentindividual
map <- df$vst_mappingandtagging
shrub <- df$vst_shrubgroup

####filter by plots where plant diversity or plant pres and abundance is sampled - do this later, find the latest year and then filter the diversity/herbaceous data by this vegetation structure data
plot_df <- select(perplot, domainID, siteID, plotType, plotID, nlcdClass, eventID, treesPresent,
           treesAbsentList, shrubsPresent, shrubsAbsentList, lianasPresent,
           lianasAbsentList, nestedSubplotAreaShrubSapling, nestedSubplotAreaLiana,
          totalSampledAreaTrees, totalSampledAreaShrubSapling, totalSampledAreaLiana,
           remarks)

#this was an initial filter, just in case want to go back to it...
#plot_df <- filter(perplot, plotID %in% divPlotsFilter) %>%
#  select(domainID, siteID, plotType, plotID, nlcdClass, eventID, treesPresent,
#         treesAbsentList, shrubsPresent, shrubsAbsentList, lianasPresent,
#         lianasAbsentList, nestedSubplotAreaShrubSapling, nestedSubplotAreaLiana,
#         totalSampledAreaTrees, totalSampledAreaShrubSapling, totalSampledAreaLiana,
#         remarks)

print(c("this dataset contains the following growth forms:", unique(appind$growthForm)))

#individual measures of stem and crown
ai_df <- appind %>%
  filter(eventID %in% plot_df$eventID & plotID%in%plot_df$plotID)%>%
  select(-uid, -namedLocation, -date, -domainID)

## add taxonID, the map data, just pulling out the names because taxID not in apparent individual name, rank etc
tax <- map%>%
  select(plotID, individualID, taxonID, scientificName, taxonRank)

tax$scientificName <- as.character(tax$scientificName)

#add scientific name to the apparent individual table
df_out <- left_join(ai_df, tax)

##filter shrub group table by plots where plant diversity or plant pres and abundance is sampled
sh_df <-  select(shrub, siteID, eventID, plotID, individualID=groupID, taxonID, scientificName, taxonRank, 
         canopyArea, volumePercent, livePercent)


#sh_df <- filter(shrub, plotID %in% divPlotsFilter) %>%
#  select(siteID, eventID, plotID, individualID=groupID, taxonID, scientificName, taxonRank, 
#         canopyArea, volumePercent, livePercent)


####Pull in the non woody data

dpid=as.character('DP1.10045.001')


nonWood <- loadByProduct(dpID=dpid,
                         site = sites,
                         #site = 'all',
                         package = "basic",
                         check.size = FALSE)

##unlist to create separate dataframes
nonWoodyPerPlot <- nonWood$vst_perplotperyear
nonWoodyPerInd <- nonWood$nst_perindividual

##filter by plots where plant diversity or plant pres and abundance is sampled
nonWoodyPlot_df <- select(nonWoodyPerPlot, domainID, siteID, plotType, plotID, nlcdClass, eventID, cactiPresent,
         cactiAbsentList, fernsPresent, fernsAbsentList, yuccasPresent,
         yuccasAbsentList, palmsPresent, palmsAbsentList, ocotillosPresent, ocotillosAbsentList,
         xerophyllumPresent, xerophyllumAbsentList, nestedSubplotAreaOther, totalSampledAreaOther, 
         remarks)


#nonWoodyPlot_df <- filter(nonWoodyPerPlot, plotID %in% divPlotsFilter) %>%
#  select(domainID, siteID, plotType, plotID, nlcdClass, eventID, cactiPresent,
#         cactiAbsentList, fernsPresent, fernsAbsentList, yuccasPresent,
#         yuccasAbsentList, palmsPresent, palmsAbsentList, ocotillosPresent, ocotillosAbsentList,
#         xerophyllumPresent, xerophyllumAbsentList, nestedSubplotAreaOther, totalSampledAreaOther, 
#         remarks)

print(c("this dataset contains the following growth forms:", unique(nonWoodyPerInd$growthForm)))

#filter individual table by perplot 
nonwind_df <- nonWoodyPerInd %>%
  filter(eventID %in% nonWoodyPlot_df$eventID & plotID%in%nonWoodyPlot_df$plotID)%>%
  select(-uid, -namedLocation, -date, -domainID)

####calculate areas
#get crown areas; calculated as area of an elipse
df_out$coverArea <- (df_out$maxCrownDiameter/2) * (df_out$ninetyCrownDiameter/2) * pi

#get basal area; in an ideal world with have crown cover for all individuals but older data is unlikely to have these and tower plot data will not; create basal area should that be of use
df_out$basalArea <- ifelse(!is.na(df_out$stemDiameter), pi*df_out$stemDiameter, pi*df_out$basalStemDiameter) 

## remove records that do not contain the necessary data to calculate either of these values, small trees, dead things, things that no longer qualify
#df_out$ck <- ifelse(is.na(df_out$coverArea)&is.na(df_out$basalArea), 0, 1) 
#df_out <- filter(df_out, ck==1)

#what proportion of living material is sp of interest (what is alive etc), only shrub group table
sh_df$coverArea <- sh_df$canopyArea*sh_df$volumePercent/100*sh_df$livePercent/100
sh_df$growthForm <- 'shrubgroup'

#calc area for nonwoody
nonwind_df$coverArea <- (nonwind_df$maxCrownDiameter/2) * (nonwind_df$ninetyCrownDiameter/2) * pi

#assume that if don't have crown diameter than capturing in the diversity data - e.g., pad counts of opuntia
#nonwind_df <- nonwind_df %>% filter(!is.na(coverArea))

#combine woody and shrub group data; aligns matching fields 
df_out <- bind_rows(df_out, sh_df)

#combine woody/shrubgroup and non woody
df_outBoth <- bind_rows(df_out, nonwind_df)

#remove dead etc individuals
target <- c("Live", "Live, insect damaged", "Live, disease damaged", "Live, physically damaged", "Live, other damage", "Live, broken bole")
df_outBoth <- filter(df_outBoth, plantStatus %in% target)

####Calculate area and cover
#remove null taxonid; just cleaning odd instances usually in early data
df_outBoth <- filter(df_outBoth, !is.na(scientificName))

#sum area values by scientific name and growth form, add plotID, add eventID to selects above
countTax <- df_outBoth%>%
  group_by(siteID, eventID, plotID, taxonID, scientificName,  taxonRank, growthForm)%>% 
  summarize(indCount=n(), totalCrownArea=sum(coverArea), totalBasalArea=sum(basalArea))

# calculate sampled area, the relevant sampling area depends on the growth form being considered
countTax$plotAreaSampled <- NA
countTax$plotAreaSampled <- as.numeric(countTax$plotAreaSampled)

for(i in 1:nrow(countTax)){
  countTax$plotAreaSampled[i] <- ifelse(countTax$growthForm[i] %in%c("multi-bole tree", "single bole tree"),
                                        plot_df$totalSampledAreaTrees[countTax$plotID[i]==plot_df$plotID],
                                        ifelse(countTax$growthForm[i] %in%c("sapling", "small tree", "single shrub", "small shrub", "shrubgroup"),
                                               plot_df$totalSampledAreaShrubSapling[countTax$plotID[i]==plot_df$plotID],
                                               ifelse(countTax$growthForm[i] %in%c("palm", "fern", "cactus", "yucca", "xerophyllum", "ocotillo", "tree fern"),
                                                      nonWoodyPlot_df$totalSampledAreaOther[countTax$plotID[i]==nonWoodyPlot_df$plotID],
                                                      (plot_df$totalSampledAreaLiana)
                                               )
                                        )
  )
} 


#calc percent cover 
countTax$percentCoverCanopy <- countTax$totalCrownArea/countTax$plotAreaSampled
countTax$percentCoverBasal <- countTax$totalBasalArea/countTax$plotAreaSampled
countTax$stemDensity <- countTax$indCount/countTax$plotAreaSampled

#create year col
countTax$year <- substr(countTax$eventID, start = 10, stop = 13)

#get most recent year of data
countTax_sub <- countTax%>%
  group_by(siteID)%>%
  filter(eventID==max(eventID))

####Pull in the 1m2 (only! to make the data more manageable and because we only need that for cover for now) plant presence and percent cover/herbaceous data







####Now we need to:
#1. figure out what data we want to use - the need is to grab the years with the data most likely to have canopy cover measures: distributed plots with the most recent data
#2. determine the number of instances where we only have stem diameter - this SHOULD, with the above filtering mostly be for small stature individuals, but at some sites they have not started measuring canopy diameters
  #we might want to drop those sites? 
#3. my goal is to then figure out how we capture the small stature saplings etc at the plot scale with the plant presence and percent cover/herbaceous data 





Summary <- countTax_sub %>%
  group_by(siteID, year, growthForm) %>%
  summarize(records=n(), recordsStemDensity=sum(!is.na(stemDensity)), recordsBasalCover=sum(!is.na(percentCoverBasal)), recordsCanopyCover=sum(!is.na(percentCoverCanopy))) %>%
  ungroup()

Summary$perStemDensity <- Summary$recordsStemDensity/Summary$records
Summary$perBasalCover <- Summary$recordsBasalCover/Summary$records
Summary$perCanopyCover <- Summary$recordsCanopyCover/Summary$records 


Summary <- countTax_sub %>%
  group_by(growthForm) %>%
  summarize(records=n(), recordsStemDensity=sum(!is.na(stemDensity)), recordsBasalCover=sum(!is.na(percentCoverBasal)), recordsCanopyCover=sum(!is.na(percentCoverCanopy))) %>%
  ungroup()

Summary$perStemDensity <- Summary$recordsStemDensity/Summary$records
Summary$perBasalCover <- Summary$recordsBasalCover/Summary$records
Summary$perCanopyCover <- Summary$recordsCanopyCover/Summary$records 

countSites <- countTax_sub %>%
  group_by(siteID) %>%
  summarize(records=n()) %>%
  ungroup()

countSites1 <- select(countTax_sub, siteID, plotID) %>% unique()

countSites <- countSites1 %>%
  group_by(siteID) %>%
  summarize(records=n()) %>%
  ungroup()

countSites2 <- select(countTax_sub2, siteID, plotID) %>% unique()

countSitesNew <- countSites2 %>%
  group_by(siteID) %>%
  summarize(records=n()) %>%
  ungroup()


taxonID <- countTax_sub %>%
  ungroup()

taxonID <- countTax %>%
  ungroup() %>%
  select(taxonID) %>%
  unique()



