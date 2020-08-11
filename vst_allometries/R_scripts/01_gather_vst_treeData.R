library(restR)
library(here)
library(readr)
library(neonUtilities)

# pmd_df <- restR::get.os.l0(stack = 'prod', tab = 'DP0.10098.001:vst_perplotperyear_in') 
#                              startDate = '2012-01-01', endDate = '2020-07-26')
# 

##
vstDat <- loadByProduct(dpID="DP1.10098.001",
                        site = "SJER",
                        #startdate = NA,
                        #enddate = NA,
                        package = "basic",
                        check.size = FALSE, 
                        token = Sys.getenv('NEON_KEY'))

# unlist all data frames
#list2env(vstDat ,.GlobalEnv)

# unlist target data frames
#perplot <- vstDat$vst_perplotperyear
appind <- vstDat$vst_apparentindividual
map <- vstDat$vst_mappingandtagging
#shrub <- vstDat$vst_shrubgroup

appind$plotEvent <- paste(appind$plotID, appind$eventID, sep="_")

lowMorton <- NULL
maxEvent <- NULL

plots <- sort(unique(appind$plotID))

for (i in 1:length(plots)){
  sub <- filter(appind, plotID==plots[i])
  maxEvent <- c(maxEvent, max(sort(sub$plotEvent)))
  rm(sub)
}

df_sub <- appind%>%
  filter(plantStatus%in%c("1", "4", "5", "6", "7", "9", 
                          "Live", "Live, disease damaged", "Live, insect damaged", "Live, other damage", "Live, physically damaged") & 
           !growthForm%in%c("single shrub", "small shrub"), plotEvent%in%maxEvent)%>% 
  arrange(plotID, desc(eventID))

df_sub <- df_sub[!duplicated(df_sub$individualID), ]

map_sub <- map%>%
  arrange(individualID, desc(eventID))%>%
  distinct(individualID, .keep_all = TRUE)%>%
  select(-eventID, -subplotID)

df_out <- df_sub%>%
  left_join(map_sub)%>%
  select(domainID, siteID, plotID, subplotID, pointID, stemAzimuth, 
         stemDistance, taxonID, individualID, growthForm, stemDiameter)%>%
  arrange(domainID, plotID, subplotID, desc(stemDiameter))

write.csv(df_out, "vst_allometries/SourceData/vst_data_cleaned.csv", row.names=FALSE)

