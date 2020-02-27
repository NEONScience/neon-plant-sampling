

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

####################################################################################################################################################################
####specify site or sites or comment out if want to run all sites

sitesSpecified <- c("SRER", "OSBS")

####################################################################################################################################################################
####Taxonomy table import and processing

###Read morphs table for updating unknown plant species
morphs <- read.csv('D:/Arquivos_pessoais/Área de Trabalho/NCEAS/morphospecies/neonUnknowns.csv', 
                   header = T, stringsAsFactors = F)
# Laís: this is convinient if just one or few sites are being generate
#morphs <- filter(morphs, siteid == "SRER")

###Read Ian's taxonomy table
tax <- read.csv('D:/Arquivos_pessoais/Área de Trabalho/NCEAS/IanTaxonomy/taxonomy_temp10.csv', 
                header = T, stringsAsFactors = F)

#Laís: I do not know why the first column name was weird when imported. You may not need this line of code.
#Dave: same for me
names(tax)[names(tax) == "ï..Accepted.Symbol"] <- "Accepted.Symbol"

#taxSelect <- select(tax, 'Accepted.Symbol', 'Synonym.Symbol', 'Symbol', 'neon_code', 'Scientific.Name', 'bestname', 'Duration','GrowthForm', 'Native.Status')

# Laís variation
# all sites besides AK and HI
taxSelect_ALL <- select(tax, 'Accepted.Symbol', 'Synonym.Symbol', 'Symbol', 'neon_code', 'Scientific.Name', 'bestname', 
                        'Duration','GrowthForm', 'inv_L48') # inv_L48 reports the native status for all sites besides the AK and HI ones.
# AK sites
taxSelect_AK <- select(tax, 'Accepted.Symbol', 'Synonym.Symbol', 'Symbol', 'neon_code', 'Scientific.Name', 'bestname', 
                       'Duration','GrowthForm', 'inv_AK') # inv_AK reports the native status for Alaska sites
# HI sites
taxSelect_HI <- select(tax, 'Accepted.Symbol', 'Synonym.Symbol', 'Symbol', 'neon_code', 'Scientific.Name', 'bestname', 
                       'Duration','GrowthForm', 'inv_HI') # inv_HI reports the native status for Hawaii site

#Laís: removes the rows with NAs and select unique NEON codes
# all sites besides AK and HI
taxSelect_ALL <- taxSelect_ALL[!is.na(taxSelect_ALL$neon_code),]
taxSelect_ALL <- distinct(taxSelect_ALL, neon_code, .keep_all = TRUE)
# AK sites
taxSelect_AK <- taxSelect_AK[!is.na(taxSelect_AK$neon_code),]
taxSelect_AK <- distinct(taxSelect_AK, neon_code, .keep_all = TRUE)
# HI sites
taxSelect_HI <- taxSelect_HI[!is.na(taxSelect_HI$neon_code),]
taxSelect_HI <- distinct(taxSelect_HI, neon_code, .keep_all = TRUE)

##################################################################################################################################################################################################
####herbaceous/plant presence and percent cover data
#Retrieve data using loadByProduct() function to pipe to R session
#Download data as list for all sites or specify sites, can also feed a list of sites

#specify data product identification
presDataProductID <- as.character('DP1.10058.001')

allDiv <- loadByProduct(dpID = presDataProductID, 
                    site = if(exists('sitesSpecified')){
                          sitesSpecified} else {
                          'all'},
                    package = 'basic', 
                    check.size = FALSE)

###extract 1m2 data from list of lists and some processing
data_1m2 <- allDiv[["div_1m2Data"]]

#clean some errors that are not yet removed
data_1m2 <- data_1m2 %>% filter(!(siteID == "WOOD" & boutNumber == 2))
data_1m2 <- data_1m2 %>% filter(!(siteID == "TALL" & boutNumber == 3))
data_1m2 <- data_1m2 %>% filter(!(siteID == "DSNY" & boutNumber == 3))

#Clean up taxonomy - update available NEON unknowns
#all taxonomy will carry because already coded and easier to leave, but only work with taxonID moving forward until link with NCEAS Invasive Sp Working Group taxonomy table
#realize for loops are slow but apply was giving me a hard time here
for(w in 1:nrow(morphs)){
  data_1m2$taxonID[which(data_1m2$morphospeciesID==morphs$title[w])]<-morphs$taxonid[w]
}

#keep only the plant data (get rid of the 'other variables such as cover of wood, rock and soil):
data_1m2 <- dplyr::filter(data_1m2, divDataType == "plantSpecies")

#remove targetTaxaPrest No, these are now instances when there were no plant species present in the 1m2 subplots
data_1m2 <- filter(data_1m2, targetTaxaPresent == 'Y')

#remove uniqueID field that provides a unique identifier for each record in the NEON data
data_1m2 <- dplyr::select(data_1m2, -c(uid))

###create year column
data_1m2$year <- substr(data_1m2$endDate, start = 1, stop = 4)

#remove irrelevant fields from 1m2 data
data_1m2 <- dplyr::select(data_1m2, -c(otherVariablesPresent, identificationReferences, remarks, measuredBy, recordedBy, samplingProtocolVersion))

#change data frame name to make match code that previously included the 10 and 100m2 data 
data <- data_1m2

#this crashes my memory, I suggest we identify columns we want and run this one more time
#Laís: in my computer it worked
data <- unique(data)

####handling the taxonomy for the 1m2 herbaceous or plant presence and percent cover data
#Laís: selecting the sites by state, so the Ian's taxonomy can be incorporated separately for AK and HI sites
# all sites besides AK and HI
data_L48 <- filter(data, siteID != "BONA" & siteID != "DEJU" & siteID != "HEAL" & siteID != "TOOL" 
                   & siteID != "BARR" & siteID != "PUUM")
# AK sites
data_AK <- filter(data, siteID == "BONA" | siteID == "DEJU" | siteID == "HEAL" | siteID == "TOOL" | siteID == "BARR")
# HI sites
data_HI <- filter(data, siteID == "PUUM")

#Laís: checking if the slice is right
nrow(data_L48) + nrow(data_HI) + nrow(data_AK) == nrow(data)

#Laís: incorporates Ian's taxonomy
# all sites besides AK and HI # deu ruim
data_L48 <- dplyr::left_join(data_L48, taxSelect_ALL, by = c("taxonID" = "neon_code"))
# AK sites
data_AK <- dplyr::left_join(data_AK, taxSelect_AK, by = c("taxonID" = "neon_code"))
# HI sites
data_HI <- dplyr::left_join(data_HI, taxSelect_HI, by = c("taxonID" = "neon_code"))

#Laís: checking if the join is correct
nrow(data_L48) + nrow(data_HI) + nrow(data_AK) == nrow(data)

#Laís: renames the invasive status column
names(data_L48)[names(data_L48) == "inv_L48"] <- "Native.Status"
names(data_AK)[names(data_AK) == "inv_AK"] <- "Native.Status"
names(data_HI)[names(data_HI) == "inv_HI"] <- "Native.Status"

#Laís: combining all the 3 separate tables
herb_data_ALL <- rbind(data_L48, data_AK, data_HI)

#Laís: checking if the binding is correct
nrow(herb_data_ALL) == nrow(data)


####################################################################################################################################################################################
####Woody vegetation structure data

vstDataProductID=as.character('DP1.10098.001')

df <- loadByProduct(dpID=vstDataProductID,
                    site = if(exists('sitesSpecified')){
                      sitesSpecified} else {
                      'all'},
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

vst_nonWoodDataProductID=as.character('DP1.10045.001')


nonWood <- loadByProduct(dpID=vst_nonWoodDataProductID,
                         site = if(exists('sitesSpecified')){
                           sitesSpecified} else {
                           'all'},
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

#remove null taxonid; just cleaning odd instances usually in early data
df_outBoth <- filter(df_outBoth, !is.na(scientificName))

####Calculate area and cover
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

#Laís: selecting the sites by state, so the Ian's taxonomy can be incorporated separately for AK and HI sites
# all sites besides AK and HI
vst_data_L48 <- filter(countTax, siteID != "BONA" & siteID != "DEJU" & siteID != "HEAL" & siteID != "TOOL" 
                   & siteID != "BARR" & siteID != "PUUM")
# AK sites
vst_data_AK <- filter(countTax, siteID == "BONA" | siteID == "DEJU" | siteID == "HEAL" | siteID == "TOOL" | siteID == "BARR")
# HI sites
vst_data_HI <- filter(countTax, siteID == "PUUM")

#Laís: checking if the slice is right
nrow(vst_data_L48) + nrow(vst_data_HI) + nrow(vst_data_AK) == nrow(data)

#Laís: incorporates Ian's taxonomy
# all sites besides AK and HI # deu ruim
vst_data_L48 <- dplyr::left_join(vst_data_L48, taxSelect_ALL, by = c("taxonID" = "neon_code"))
# AK sites
vst_data_AK <- dplyr::left_join(vst_data_AK, taxSelect_AK, by = c("taxonID" = "neon_code"))
# HI sites
vst_data_HI <- dplyr::left_join(vst_data_HI, taxSelect_HI, by = c("taxonID" = "neon_code"))

#Laís: checking if the join is correct
nrow(vst_data_L48) + nrow(vst_data_HI) + nrow(vst_data_AK) == nrow(data)

#Laís: renames the invasive status column
names(vst_data_L48)[names(vst_data_L48) == "inv_L48"] <- "Native.Status"
names(vst_data_AK)[names(vst_data_AK) == "inv_AK"] <- "Native.Status"
names(vst_data_HI)[names(vst_data_HI) == "inv_HI"] <- "Native.Status"

#Laís: combining all the 3 separate tables
vst_data_ALL <- rbind(vst_data_L48, vst_data_AK, vst_data_HI)

#Laís: checking if the binding is correct
nrow(vst_data_ALL) == nrow(countTax)

#get most recent year of data
vst_data_SUBSET <- vst_data_ALL%>%
  group_by(siteID)%>%
  filter(eventID==max(eventID))

#Lais' checks of number of sites
table(vst_data_SUBSET$siteID)

##############################################################################################################################################################
####filter the herbaceous/pres and percent cover data by the vegetation structure data which is not sampled every year at the 'best' (for these purposes) plots
#filter div data by woody data

herb_data_ALL$siteYear <- paste(herb_data_ALL$siteID, herb_data_ALL$year, sep = "_")

vst_data_SUBSET$siteYear <- paste(vst_data_SUBSET$siteID, vst_data_SUBSET$year, sep = "_")

vst_filter <- paste(vst_data_SUBSET$siteID, vst_data_SUBSET$year, sep = "_")

herb_data_SUBSET <- filter(herb_data_ALL, siteYear %in% vst_filter)

##############################################################################################################################################################


#Exporting data

#Laís: creating a file to be exported
#Dave: These will need to be updated; I'm not sure if we want to combine the data, filter the woody information from the 1m2 herbaceous data or otherwise.  
write.csv(data_ALL, file = "D:/Arquivos_pessoais/Área de Trabalho/NCEAS/data/NEONdata_ALL.csv")
write.csv(data_ALL_lastyr, file = "D:/Arquivos_pessoais/Área de Trabalho/NCEAS/data/NEONdata_ALL_lastyr.csv")
write.csv(data_SRER, file = "D:/Arquivos_pessoais/Área de Trabalho/NCEAS/data/NEONdata_SRER.csv")
write.csv(data2_SRER, file = "D:/Arquivos_pessoais/Área de Trabalho/NCEAS/data/NEONdata_SRER_lastyr.csv")










