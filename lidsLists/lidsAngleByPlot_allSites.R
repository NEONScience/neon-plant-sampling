### Creating a single dataframe of LIDS angles by plot for all sites for Fulcrum app maintenance
#   Load required libraries
library(TeachingDemos)
library(dplyr)

#   Obtain list of plotIDs from applicableModules.csv
#	Read in plot-level data from applicableModules.csv, and return to parent working directory
if(file.exists("~/Documents/gitRepositories")){
  acceptedPlot <- read.csv("~/Documents/gitRepositories/devTOS/spatialData/supportingDocs/applicableModules.csv",
                           header = TRUE, stringsAsFactors = FALSE)
}

if(file.exists("C:/GitHub")){
  acceptedPlot <- read.csv("C:/GitHub/devTOS/spatialData/supportingDocs/applicableModules.csv",
                           header = TRUE, stringsAsFactors = FALSE)
}

#   Obtain and sort list of plots that support CDW
acceptedPlot %>% filter(grepl('cdw', applicableModules)) %>% arrange(siteID, plotType, plotID) -> cdwPlots

#   Create a dataframe for plotIDs and LIDS angles
lidsDF <- tibble(cdwPlots$plotID, cdwPlots$plotType)
lidsDF %>% rename(plotID = "cdwPlots$plotID", plotType = 'cdwPlots$plotType') -> lidsDF

#   Create a vector of angles from which to randomly sample; angles are in 10 deg increments
theAngles = seq(from=0, to=350, by=10)


##  Generate random angle based on plotID setseed
#   Use a "for" loop to generate random LIDS azimuths for each value of plotID
lidsDF$lidsAngle1 <- ""
for (i in 1:nrow(lidsDF)){
  # Use the plotID as a set.seed so that randomly selected azimuths are reproducible
  randomSeed = lidsDF$plotID[i]
  char2seed(randomSeed, set=TRUE); angle1 = sample(theAngles, 1)
  
  # Store "angle" in "angle1" field in lids.df
  lidsDF$lidsAngle1[i] = angle1
  
  # End "for" loop bracket
}

#   Set for loop output to correct data type
lidsDF$lidsAngle1 <- as.numeric(lidsDF$lidsAngle1)

#   Add two additional angles at 120˚ intervals based on initial angle
lidsDF %>%
  mutate(lidsAngle2 = lidsAngle1 + 120) %>%
  mutate(lidsAngle2 = ifelse(lidsAngle2 >= 360, lidsAngle2-360, lidsAngle2)) %>%
  mutate(lidsAngle3 = lidsAngle1 + 240) %>%
  mutate(lidsAngle3 = ifelse(lidsAngle3 >= 360, lidsAngle3-360, lidsAngle3)) -> lidsDF


##  Write-out output to csv
# if(file.exists("C:/GitHub")){ write.csv(lidsDF, "C:/GitHub/neon-plant-sampling/lidsLists/all_lidsLists_2020-03-05.csv", row.names = FALSE, fileEncoding = "UTF-8") }

fileName <- "[Insert path and name...]"
write.csv(lidsDF, file = fileName, row.names = FALSE, fileEncoding = "UTF-8")
