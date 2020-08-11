library(restR)
library(here)
library(readr)


pmd_df <- restR::get.os.l0(stack = 'prod', tab = 'DP0.10098.001:vst_perplotperyear_in') 
                             startDate = '2012-01-01', endDate = '2020-07-26')


if (!require("neonUtilities")) install.packages("neonUtilites")
library(neonUtilities)

#ltr = dpID="DP1.10033.001"
#phe = dpID= "DP1.10055.001"

##
df <- loadByProduct(dpID="DP1.10098.001",
                        #site = "SJER",
                        #startdate = NA,
                        #enddate = NA,
                        package = "basic",
                        check.size = FALSE, 
                        token = Sys.getenv('NEON_KEY'))

# unlist all data frames
#list2env(vstDat ,.GlobalEnv)

perplot <- df$vst_perplotperyear
appind <- df$vst_apparentindividual
map <- df$vst_mappingandtagging
shrub <- df$vst_shrubgroup

appind$plotEvent <- paste(appind$plotID, appind$eventID, sep="_")

lowMorton <- NULL
maxEvent <- NULL

plots <- sort(unique(appind$plotID))

for (i in 1:length(plots)){
  sub <- filter(appind, plotid==plots[i])
  maxEvent <- c(maxEvent, max(sort(sub$plotEvent)))
  rm(sub)
}

df_sub <- appind%>%
  filter(plantStatus%in%c("1", "4", "5", "6", "7", "9", 
                          "Live", "Live, disease damaged", "Live, insect damaged", "Live, other damage", "Live, physically damaged") & !growthform%in%c("single shrub", "small shrub"), plotEvent%in%maxEvent)%>% 
  arrange(plotID, desc(eventID))

### suspect stem diameter at BONA, reset
df_sub$stemDiameter[df_sub$siteID=='BONA'& df_sub$stemDiameter==120] <- 12.0
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

write.csv(df_out, "SourceData/vst_data_cleaned.csv", row.names=FALSE)








