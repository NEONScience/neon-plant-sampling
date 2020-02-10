### This function assigns random orientations to LIDS transects on a per plot basis for CDW tally sampling. The first azimuth is randomly chosen (to the nearest 10 deg), and subsequent azimuths are spaced 120˚ from the first. The plotID is used as a set-seed so that the output can be updated while maintaining consistency.


### Assumptions made by the function:
##  Initial working directory is the default directory associated with the Rproj file.

##  Lists of accepted plots with associated spatial data fields exist in a file called 'applicableModules.csv' which exists in path:
#   /spatialData/supportingDocs
# There are "plotID" "plotType" "subtype" and "plotSize" columns in this file.

##	The following variables are required as inputs:
#	siteCode = 4-letter unique NEON site code; must enter with "", e.g. "HARV"
# basePlotType = "distributed" or "tower"


### lidsList = function(siteCode, basePlotType){ --> function retired; use 'lidsAngleByPlot_allSites.R' script
### LIDS lists now being served via Fulcrum
  
  # Load 'TeachingDemos' package so that plotIDs may be turned into unique numeric seeds for angle randomization
  require(TeachingDemos)
  require(dplyr)
  
  #	Read in plot-level data from applicableModules.csv, and return to parent working directory
  if(file.exists("~/Documents/gitRepositories")){
    acceptedPlot <- read.csv("~/Documents/gitRepositories/devTOS/spatialData/supportingDocs/applicableModules.csv",
                             header = TRUE, stringsAsFactors = FALSE)
  }
  
  if(file.exists("~/Documents/workDocuments/gitRepositories")){
    acceptedPlot <- read.csv("~/Documents/workDocuments/gitRepositories/devTOS/spatialData/supportingDocs/applicableModules.csv",
                             header = TRUE, stringsAsFactors = FALSE)
  }
  
  # Use dplyr 'filter' and get plotIDs that support 'cdw' sampling; use 'applicableModules' field to find 'cdw' plots
  cdwPlots <- filter(acceptedPlot,
                     siteID == siteCode,
                     plotType == basePlotType,
                     grepl('cdw', applicableModules))
  
  plotID = sort(as.character(cdwPlots$plotID))
  
  # Create a dataframe to hold function output, and insert list of desired plotIDs
  lids.df = data.frame(matrix(data=NA, nrow=length(plotID), ncol=4))
  cNames = c("plotID","lidsAngle1","lidsAngle2","lidsAngle3")
  colnames(lids.df) = cNames
  lids.df$plotID = as.character(plotID)
  
  # Create a vector of angles from which to randomly sample; angles are in 10 deg increments
  theAngles = seq(from=0, to=350, by=10)
  
  ##  Use a "for" loop to generate random LIDS azimuths for each value of plotID
  for (i in 1:nrow(lids.df)){
    # Use the plotID as a set.seed so that randomly selected azimuths are reproducible
    randomSeed = lids.df$plotID[i]
    char2seed(randomSeed, set=TRUE); angle1 = sample(theAngles, 1)
    
    # Store "angle" in "angle1" field in lids.df
    lids.df$lidsAngle1[i] = angle1
    
    # End "for" loop bracket
  }
  
  # Calculate lidsAngle2 and lidsAngle3 from lidsAngle1
  angle2 = lids.df$lidsAngle1 + 120
  angle2[angle2 >= 360] = angle2[angle2 >= 360] - 360
  angle3 = lids.df$lidsAngle1 + 240
  angle3[angle3 >= 360] = angle3[angle3 >= 360] - 360
  
  # Add calculated angles to the dataframe
  lids.df$lidsAngle2 = angle2
  lids.df$lidsAngle3 = angle3
  
  # Add an empty column for remarks (e.g. "no CDW present")
  lids.df$remarks = ""
  
  # Within the 'lidsLists' directory, create a subdirectory, then write output to a .csv file and return to the original working directory
  setwd("lidsLists")
  outputName = paste(siteCode, basePlotType, "lidsList.csv", sep="_")
  write.csv(lids.df, file=outputName, row.names=FALSE)
  setwd("../")
  print("Output complete")

# End function bracket
}