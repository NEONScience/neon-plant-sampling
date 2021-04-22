### This function randomly chooses two subplots for Plant Biomass and Productivity sampling from the four possible subplots present in a 40x40m Tower plot. The plotID is used as a set.seed so that the function output is reproducible. Subplots are not randomly chosen for 20x20m Tower plots.

### Assumptions made by the function:
##  Initial working directory is the default directory associated with the Rproj file.

##  Lists of accepted plots with associated spatial data fields exist in a file called 'applicableModules.csv' which exists in path:
#   /spatialData/supportingDocs
# There are "plotID" "plotType" "subtype" and "plotSize" columns in this file.

##	The following variables are required as inputs:
#	siteCode = 4-letter unique NEON site code; must enter with "", e.g. "HARV"


randomSubplots = function(siteCode){
  
  #  Load 'TeachingDemos' package so plotIDs may be turned into unique numeric seeds
  require(TeachingDemos)
  require(dplyr)
  require(openxlsx)
  
  #	Read in plot-level data from applicableModules.csv
  amPath <- "~/Documents/gitRepositories/devTOS/spatialData/supportingDocs"
  acceptedPlot <- read.csv(paste(amPath, "applicableModules.csv", sep = "/"), header=T, stringsAsFactors=F)
  
  
  
  ### Determine the sizes of Tower plots present for given value of 'siteCode', and use if/else statement to stop code below if there are tower base plots with plotSize = 400 in 'acceptedPlot', and warn user that function was not run.
  # Filter 'acceptedPlot' to obtain only Tower baseplots for the desired site
  towerPlots <- filter(acceptedPlot,
                       siteID == siteCode,
                       plotType == 'tower',
                       subtype == 'basePlot')
  
  if (400 %in% towerPlots$plotSize){
    print("Random subplots are not required when Tower base plotSize = 400")
  } else {  
  
  # Obtain plotIDs for those Tower plots that have subtype = 'basePlot'
    plotID <- sort(as.character(towerPlots$plotID))
    
  # Create a dataframe to hold function output, and insert list of desired plotIDs
  subplots.df <- data.frame(matrix(data=NA, nrow=length(plotID), ncol=3))
  cNames <- c("plotID","aSubplotID","bSubplotID")
  colnames(subplots.df) <- cNames
  subplots.df$plotID <- plotID
  
  # Create a vector of subplotIDs from which to randomly sample; currenty, subplotIDs are specific to a 40m x 40m plot
  theSubplots <- c(21,23,39,41)
  
  # Use method for sampling random numbers in R versions prior to 3.6
  RNGkind(sample.kind = "Rounding")
  
  ##  Use a "for" loop to generate two randomly selected subplots for each value of plotID
  for (i in 1:nrow(subplots.df)){
    # Use the plotID as a set.seed so that randomly selected subplots are reproducible
    randomSeed <- subplots.df$plotID[i]
    char2seed(randomSeed, set=TRUE); theChosen <- sort(sample(theSubplots, 2))
    
    # Store randomly chosen subplotIDs in subplots.df
    subplots.df$aSubplotID[i] <- theChosen[1]
    subplots.df$bSubplotID[i] <- theChosen[2]
    
  }
  
  # Write output to a .csv file, and return working directory to default repository directory (i.e., devTOS)
  outPath <- "~/Documents/gitRepositories/devTOS/towerSubplotLists"
  outputName <- paste(siteCode, "randomTowerSubplots.xlsx", sep="_")
  
  # Write out to .xlsx file
  write.xlsx(subplots.df, file = paste(outPath, outputName, sep = "/"), 
             colNames = TRUE, rowNames = FALSE, keepNA = FALSE, firstRow = TRUE)
  
  print("Output complete")

  # End if/else
  }
  
# End function
}