#   At high-growth increment sites, identify subset of Tower Plots for VST subsampling based on 5 lowest Morton Order
##  Load libraries
library(plyr)
library(dplyr)
library(openxlsx)

##  Read in `applicableModules.csv`, filter to VST Tower Plots, filter to high growth rate sites, arrange by site and MO
if(file.exists("/Users/cmeier")){
  inputPath <- "~/Documents/gitRepositories/devTOS/spatialData/supportingDocs"
}

am <- read.csv(paste(inputPath, "applicableModules.csv", sep = "/"), header = TRUE, stringsAsFactors = FALSE)

hgr <- c("BART", "HARV", "BLAN", "SCBI", "SERC", "DSNY", "JERC", "OSBS", "GUAN", "STEI", "TREE", "UNDE", "KONZ",
         "UKFS", "GRSM", "MLBS", "ORNL", "DELA", "LENO", "TALL", "RMNP", "CLBJ", "ABBY", "PUUM")

am %>% 
  filter(siteID %in% hgr, plotType=="tower", grepl("vst", applicableModules)) %>%
  arrange(siteID, mortonOrder) %>%
  select(mortonOrder, plotID, siteID) -> vstTower

vstTower <- vstTower[c("mortonOrder", "siteID", "plotID")]



##  Grab lowest 5 MO plots from each site
### Create data frame to hold output
df <- data.frame()

### For loop to get lowest MO plots
for(i in 1:length(hgr)){
  temp <- dplyr::filter(vstTower, siteID==hgr[i])
  temp <- temp[1:5,]
  df <- dplyr::bind_rows(df, temp)
}



##  Write out to .xlsx file
outPath <- "~/Documents/gitRepositories/neonPlantSampling/vst_towerPlotSubsample"
write.xlsx(df, file = paste(outPath, "vst_towerPlotSubsample.xlsx", sep = "/"), sheetName = "Tower Subsample",
           colNames = TRUE, rowNames = FALSE, firstRow = TRUE, colWidths = "auto")
