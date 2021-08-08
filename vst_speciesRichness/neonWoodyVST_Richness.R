

####Getting and setting up the data####
#this clears the global environment and stored objects, lists etc
rm(list=ls())

# install neonUtilities from GitHub, should be installed at this point
#install_github("NEONScience/NEON-utilities/neonUtilities", dependencies=TRUE)

# load neonUtilities and other required packages 
library (neonUtilities)
#library(devtools)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)



####################################################################################################################################################################################
####Woody vegetation structure data

vstDataProductID=as.character('DP1.10098.001')

sitesSpecified <- 'UKFS'

df <- loadByProduct(dpID=vstDataProductID,
                    site = if(exists('sitesSpecified')){
                      sitesSpecified} else {
                      'all'},
                    package = "basic",
                    check.size = FALSE)

####unlist to create separate dataframes
perplot <- df$vst_perplotperyear #characteristics of sampling: namedLocation, eventID, plot characteristics (nlcd, location, elevation), sampling areaby growth form
appind <- df$vst_apparentindividual #individual observations: namedLocation, eventID, individual info (growth form and condition), height, diameter etc
map <- df$vst_mappingandtagging #individual identification and within plot location: namedLocation, eventID, taxonID, relative location within plot
shrub <- df$vst_shrubgroup

####Evaluate and process the woody data

#select subset of variables in perplot
plot_df <- dplyr::select(perplot, domainID, siteID, plotType, plotID, nlcdClass, eventID, treesPresent,
           treesAbsentList, shrubsPresent, shrubsAbsentList, lianasPresent,
           lianasAbsentList, nestedSubplotAreaShrubSapling, nestedSubplotAreaLiana,
          totalSampledAreaTrees, totalSampledAreaShrubSapling, totalSampledAreaLiana,
           remarks)

print(c("this dataset contains the following growth forms:", unique(appind$growthForm)))

#filter appind by perplot by eventID and plotID 
ai_df <- appind %>%
  dplyr::filter(eventID %in% plot_df$eventID & plotID%in%plot_df$plotID)%>%
  dplyr::select(-uid, -namedLocation, -date, -domainID)

#get individual taxonID from mapping and tagging 
taxVST <- map%>%
  dplyr::select(plotID, individualID, taxonID, scientificName, taxonRank)
taxVST$scientificName <- as.character(taxVST$scientificName)
#add scientific name to the apparent individual table
df_out <- left_join(ai_df, taxVST)


####evaluate and process shrub data
#subset the data
sh_df <-  select(shrub, siteID, eventID, plotID, individualID=groupID, taxonID, scientificName, taxonRank, 
         canopyArea, volumePercent, livePercent)


####Pull in the non woody data which is a separate data product but collected at the same time with similar methods and different from plant diversity data product
vst_nonWoodDataProductID=as.character('DP1.10045.001')

#get the data
nonWood <- loadByProduct(dpID=vst_nonWoodDataProductID,
                         site = if(exists('sitesSpecified')){
                           sitesSpecified} else {
                           'all'},
                         package = "basic",
                         check.size = FALSE)

#unlist to create separate dataframes
nonWoodyPerPlot <- nonWood$vst_perplotperyear
nonWoodyPerInd <- nonWood$nst_perindividual

#select subset of data 
nonWoodyPlot_df <- select(nonWoodyPerPlot, domainID, siteID, plotType, plotID, nlcdClass, eventID, cactiPresent,
         cactiAbsentList, fernsPresent, fernsAbsentList, yuccasPresent,
         yuccasAbsentList, palmsPresent, palmsAbsentList, ocotillosPresent, ocotillosAbsentList,
         xerophyllumPresent, xerophyllumAbsentList, nestedSubplotAreaOther, totalSampledAreaOther, 
         remarks)

print(c("this dataset contains the following growth forms:", unique(nonWoodyPerInd$growthForm)))

#filter individual table by perplot 
nonwind_df <- nonWoodyPerInd %>%
  dplyr::filter(eventID %in% nonWoodyPlot_df$eventID & plotID%in%nonWoodyPlot_df$plotID)%>%
  select(-uid, -namedLocation, -date, -domainID)

#combine woody and shrub group data; aligns matching fields 
df_out <- bind_rows(df_out, sh_df)

#combine woody/shrubgroup and non woody; if else accomodates those sites or subsets of data which don't have non woody data
if(exists('nonwind_df') && is.data.frame(get('nonwind_df'))){
  df_outBoth <- bind_rows(df_out, nonwind_df)} else {
    df_outBoth <- df_out}

#remove dead etc individuals
target <- c("Live", "Live, insect damaged", "Live, disease damaged", "Live, physically damaged", "Live, other damage", "Live, broken bole")
df_outBoth <- filter(df_outBoth, plantStatus %in% target)

#remove null taxonid; just cleaning odd instances usually in early data
df_outBoth <- filter(df_outBoth, !is.na(scientificName))


#adjust code for species richness
countTax <- dplyr::select(df_outBoth, siteID, eventID, plotID, taxonID, scientificName,  taxonRank, growthForm) %>% unique()

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

#are different sp counted across different areas within eventID and plotID? yes.  
  #do these areas overlap such that richness could be inflated?  not sure, I guess would need subplotIDs for that
#get rid of growthForm
countTaxDups <- select(countTax, -siteID, -taxonRank, -scientificName, -growthForm)

countTaxDupsCount <- countTaxDups %>%
  unique() %>%
  group_by(eventID, plotID, taxonID) %>%
  count()

#an example of a single species measured in a plot at two different scales of area
exampleDupsPerPlot <- filter(countTax, eventID == "vst_UKFS_2015" & plotID == "UKFS_043" & taxonID == "CAOV2")

#decisions need to be made about how to deal with individuals not identified to species
#make count of all unique determinations by area
richness <- countTax %>%
  select(-taxonRank, -scientificName, -growthForm) %>%
  unique() %>%
  group_by(siteID, eventID, plotID, plotAreaSampled) %>%
  summarize(speciesRichness=n())%>%
  ungroup()
  
  
