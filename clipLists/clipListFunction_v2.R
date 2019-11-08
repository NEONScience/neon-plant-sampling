#	This function defines randomized clip-harvest locations for 20m x 20m plots/subplots, based on 3m x 0.5m grid cells that contain clip-strips. Grid cells are placed with a 1m buffer from any plot boundary, and those grid cells that overlap 10m2 nested subplots used for Veg Structure measurements are omitted; for 40 x 40m and larger Tower plots, cells that overlap additional 1m2 nested subplots (surrounded by a 1m buffer, 9m2 total) associated with a 20 x 20m "Distributed plot" superimposed on the plot centroid are also omitted.

# Version 2 of this function incorporates a `chem` input variable that, when flagged 'TRUE', will write out Clip Lists to a `SITE_distHerbChem_clipList` folder for all plots that may be sampled for Herbaceous Biogeochemistry and that are NOT sampled for standard Herbaceous Biomass.

#	The function assumes that the starting working directory is the default associated with the Git repository.

#	The function assumes there is a file called 'applicableModules.csv,' in a 'spatialData/supportingDocs/' directory, and that this .csv contains the following fields:
  # siteID
  # plotID
  # plotType, values in lower case (e.g., distributed, tower)
  # subtype, to filter on values = 'basePlot' 
  # nlcdClass, to filter on values != deciduousForest, evergreenForest, mixedForest when plotType = distributed
  # applicableModules, to filter distributed plots and select only those that support 'hbp' sampling

# The function assumes that a table named 'nestedCoords.csv,' specifying the location of exclusion zones that do not support herbaceous clip sampling, is located in a child directory named 'clipLists'

# The following variables are required as inputs:
  #	siteCode = 4-letter unique NEON site code; must enter with "", e.g. "HARV"


clipList <- function(siteCode, chem = NULL){

#	Package 'dplyr' used to filter list of accepted plots according to specified column values
# Package 'TeachingDemos' used to turn clipIDs into unique numeric seeds for grid cell randomization.
require(dplyr)
require(TeachingDemos)

#	Input one-sided subplot size, buffer width, grid cell dimensions, and clip-strip dimensions, all in meters.
subplotSize <- 20
bufferWidth <- 1
yDistance <- subplotSize - 2*bufferWidth
xDistance <- subplotSize - 2*bufferWidth
cellHeight <- 3
cellWidth <- 0.5
clipWidth <- 0.1
clipHeight <- 2

# Calculate the total number of grid cells per plot / subplot
gridCol <- xDistance/cellWidth
gridRow <- yDistance/cellHeight
totalCell <- gridCol*gridRow

#	Read in plot-level data from applicableModules.csv, and return to parent working directory
setwd("spatialData/supportingDocs")
acceptedPlot <- read.csv("applicableModules.csv", header=T, stringsAsFactors = F)
setwd("../../")

#	Read in table that specifies 'x' ranges and 'y' ranges for locations of exclusion areas within the plot that do not support herbaceous clip sampling - i.e., areas potentially used for plant diversity sampling.
setwd("clipLists")
nestedCoords <- read.csv("nestedCoords.csv", header=T, stringsAsFactors=F)



### Create clipLists for `cfc` Distributed Plots when `chem` = TRUE
if (chem==TRUE){
  # Filter `acceptedPlot` to obtain Distributed basePlots with applicableModules==`cfc`
  acceptedPlot %>% filter(siteID==siteCode,
                          plotType == "distributed",
                          subtype == "basePlot",
                          grepl('cfc', applicableModules)) -> cfcDistPlots
  
  # Filter `cfcDistPlots` to obtain forested nlcdClass plots - i.e., those that have not had Clip Lists made for them already
  cfcDistPlots %>% filter(nlcdClass %in% c("deciduousForest", "evergreenForest", "mixedForest")) -> cfcForestDist
  
  # Create list of Dist Plots in which `cfc` AND standard `hbp` sampling occur - needed to identify which plots need `Chem` label added to existing lists
  cfcDistPlots %>% filter(!nlcdClass %in% c("deciduousForest", "evergreenForest", "mixedForest")) -> cfcHbpDistPlots
  
  # Identify Tower Plots used for `cfc` sampling, append to `cfcHbpDistPlots`; creates list of plotIDs used below when chem=FALSE/NULL
  acceptedPlot %>% filter(siteID==siteCode,
                          plotType == "tower",
                          subtype == "basePlot",
                          grepl('cfc', applicableModules)) -> cfcTowerPlots
  
  chemFalsePlots <- rbind(cfcHbpDistPlots, cfcTowerPlots)
  
  # Create a subdirectory in the 'clipList' directory to hold `cfc` distributed plot .csv output, and set as working directory
  dirName <- paste(siteCode, "distHerbChem_clipList", sep="_")
  dir.create(dirName)
  setwd(dirName)
  
  # Write .csv for chemFalsePlots
  chemFalseName <- paste(siteCode, "cfc_Tower_nonForestDist.csv", sep = "_")
  write.csv(chemFalsePlots, file = chemFalseName, row.names = FALSE, fileEncoding = "UTF-8")
  
  # Extract plotIDs from cfcForestDist, and sort
  cfcDistID <- sort(as.character(cfcForestDist$plotID))
  
  # `for` loop to generate random clip-harvest locations for each plot listed in `cfcDistID`
  for (i in 1:length(cfcDistID)){
    
    # Create an 8-column dataframe to hold clipID, subplotID, and the easting and northing offsets for the SW corner of the clip-strip; additional 'cell' fields are employed to filter out cells that overlap exclusion zones within the plot, and these fields are deleted with code below before the .csv is written.
    grid.df <- data.frame(matrix(data=NA, nrow=totalCell, ncol=8), stringsAsFactors=FALSE)
    cNames <- c("clipID","subplotID","offsetEasting","offsetNorthing","x1Cell","x2Cell","y1Cell","y2Cell")
    colnames(grid.df) <- cNames
    grid.df$clipID <- as.character(grid.df$clipID)
    grid.df$subplotID <- as.integer(grid.df$subplotID)
    grid.df$offsetEasting <- as.numeric(grid.df$offsetEasting)
    grid.df$offsetNorthing <- as.numeric(grid.df$offsetNorthing)
    grid.df$x1Cell <- as.numeric(grid.df$x1Cell)
    grid.df$x2Cell <- as.numeric(grid.df$x2Cell)
    grid.df$y1Cell <- as.numeric(grid.df$y1Cell)
    grid.df$y2Cell <- as.numeric(grid.df$y2Cell)
    
    #  Create gridIDs for a 20m x 20m distributed plot, then create clipIDs from the gridIDs, and add clipIDs and subplotIDs to the grid.df dataframe.
    gridID <- seq(from=1, to=totalCell, by=1)
    grid.df[,"clipID"] <- paste(cfcDistID[i], formatC(gridID, width=3, format="d", flag="0"), sep="_")
    grid.df[,"subplotID"] <- 31
    
    #  Create x-axis and y-axis coordinates for the SW corner of each clip-harvest strip within each grid cell and add to the dataframe. The clip strip itself is 0.1m W x 2m H oriented North/South, and is centered within the 0.5m x 3m grid cell such that there is a 0.2m buffer on the West/East sides of the strip, and a 0.5m buffer on the North/South sides of the strip.
    #	Define X and Y coords of SW corner of grid cells in each row of cells and each column of cells within the plot
    xTemp <- seq(from=bufferWidth, to=xDistance+cellWidth, by=cellWidth)
    yTemp <- seq(from=bufferWidth, to=(yDistance-1), by=cellHeight)
    
    #	Create offset for column and row X and Y coords so that position corresponds to clip-strip within grid cell
    xOffset <- xTemp + (cellWidth-clipWidth)/2
    yOffset <- yTemp + (cellHeight-clipHeight)/2
    
    #  Calculate SW corner of each clip-strip within each grid cell, as well as grid cell corner positions, and add to the data frame created above. Grid cell corner positions are needed to determine whether individual cells overlap %cover nested subplots.
    grid.df[,"offsetEasting"] <- rep(xOffset, times=gridRow)
    grid.df[,"offsetNorthing"] <- rep(yOffset, each=gridCol)
    grid.df[,"x1Cell"] <- rep(xTemp, times=gridRow)
    grid.df[,"x2Cell"] <- grid.df$x1Cell + cellWidth
    grid.df[,"y1Cell"] <- rep(yTemp, each=gridCol)
    grid.df[,"y2Cell"] <- grid.df$y1Cell + cellHeight
    
    ##  Remove rows of "grid.df" that overlap 10m2 nested subplots used for Veg Structure measurements, and 9m2 area that includes 1m2 nested subs used for %cover estimates in the plant diversity protocol; ignore the 16cm of the 10m2 subplot that overlaps the 50 cm grid cell buffer, as technicians will position themselves on the West/East side of clip-strips for harvesting.
    # Obtain coordinates for nested subplots associated with 20m x 20m distributed plots
    nestedTemp <- filter(nestedCoords, subplotID==31)
    
    #	Create an empty vector to hold list of gridIDs that overlap nested subplots
    gridOmit <- c()
    
    # Using location of nested subplots specified in "nestedTemp" table, identify overlapping grid cells with a 'for' loop, store associated gridIDs in "gridOmit"; then remove elements of "gridOmit" from "grid.df" and store in a new dataframe named "clip.df"
    for(j in 1:nrow(nestedTemp)){
      
      # Filter grid.df to identify cells that overlap nested subplots with dimensions specified in nestedTemp
      tempGrid <- filter(grid.df, x1Cell < nestedTemp$x2[j] &
                           x2Cell > nestedTemp$x1[j] &
                           y1Cell < nestedTemp$y2[j] &
                           y2Cell > nestedTemp$y1[j])
      
      # Add values of clipID that overlap nested subplots to gridOmit vector created above
      gridOmit <- append(gridOmit, tempGrid$clipID)
      
      # End gridOmit 'for' loop          
    }
    
    # Filter grid.df to remove clipIDs identified in gridOmit that overlap nested subplots
    clip.df <- filter(grid.df, !clipID %in% gridOmit)
    
    #  Randomize rows of clip.df and use set.seed to ensure results are repeatable on a per plot basis
    char2seed(cfcDistID[i], set=TRUE); clip.df <- clip.df[sample(nrow(clip.df)),]
    
    #  Remove 'cell' columns from data frame so that coordinates are only provided for the clip-strip itself; add columns for 'status' and 'date'
    clip.df <- clip.df[,1:4]
    clip.df$status <- ""
    clip.df$date <- ""
    
    #	Define file name for .csv, and write the output to the `distHerbChem` plot subdirectory created above
    outputName = paste(cfcDistID[i], "herbChem_clipList.csv", sep="_")
    write.csv(clip.df, file=outputName, row.names=FALSE, fileEncoding = "UTF-8")      
    
    # End distributed plot forested `cfc` 'for' loop  
  }
  
  print("Herbaceous Chemistry forested plot Clip List output complete.")
  
  # Return to clipLists working directory
  setwd("../")
  
  
# End `chem` `if` statement  
} else {

### Create clipLists for 'distributed' plots.
# Filter 'acceptedPlot' to obtain only those distributed basePlots that do not have forested NLCD types, and that do have 'hbp' in applicableModules.
distPlots <- filter(acceptedPlot, siteID == siteCode,
                    plotType == "distributed",
                    subtype == "basePlot",
                    !nlcdClass %in% c("deciduousForest","evergreenForest","mixedForest"),
                    grepl('hbp', applicableModules))

# Extract distributed plotIDs, and sort
distID <- sort(as.character(distPlots$plotID))

# If/else to deal with possibility that no plotIDs exist within potentially sample-able nlcdClasses at the site.
  if (length(distID)==0){
    print("All 'hbp' distributed plots are within forested NLCD classes, no Clip Lists generated for distributed plots.")
  } else {
    # Create a subdirectory in the 'clipList' directory to hold distributed plot .csv output, and set as working directory
    dirName <- paste(siteCode, "distributed_clipList", sep="_")
    dir.create(dirName)
    setwd(dirName)    
    
    # 'for' loop to generate random clip-harvest locations for each plot listed in 'distID'
    for (i in 1:length(distID)){
      
      # Create an 8-column dataframe to hold clipID, subplotID, and the easting and northing offsets for the SW corner of the clip-strip; additional 'cell' fields are employed to filter out cells that overlap exclusion zones within the plot, and these fields are deleted with code below before the .csv is written.
      grid.df <- data.frame(matrix(data=NA, nrow=totalCell, ncol=8), stringsAsFactors=FALSE)
      cNames <- c("clipID","subplotID","offsetEasting","offsetNorthing","x1Cell","x2Cell","y1Cell","y2Cell")
      colnames(grid.df) <- cNames
      grid.df$clipID <- as.character(grid.df$clipID)
      grid.df$subplotID <- as.integer(grid.df$subplotID)
      grid.df$offsetEasting <- as.numeric(grid.df$offsetEasting)
      grid.df$offsetNorthing <- as.numeric(grid.df$offsetNorthing)
      grid.df$x1Cell <- as.numeric(grid.df$x1Cell)
      grid.df$x2Cell <- as.numeric(grid.df$x2Cell)
      grid.df$y1Cell <- as.numeric(grid.df$y1Cell)
      grid.df$y2Cell <- as.numeric(grid.df$y2Cell)
      
      #  Create gridIDs for a 20m x 20m distributed plot, then create clipIDs from the gridIDs, and add clipIDs and subplotIDs to the grid.df dataframe.
      gridID <- seq(from=1, to=totalCell, by=1)
      grid.df[,"clipID"] <- paste(distID[i], formatC(gridID, width=3, format="d", flag="0"), sep="_")
      grid.df[,"subplotID"] <- 31
      
      #  Create x-axis and y-axis coordinates for the SW corner of each clip-harvest strip within each grid cell and add to the dataframe. The clip strip itself is 0.1m W x 2m H oriented North/South, and is centered within the 0.5m x 3m grid cell such that there is a 0.2m buffer on the West/East sides of the strip, and a 0.5m buffer on the North/South sides of the strip.
      #	Define X and Y coords of SW corner of grid cells in each row of cells and each column of cells within the plot
      xTemp <- seq(from=bufferWidth, to=xDistance+cellWidth, by=cellWidth)
      yTemp <- seq(from=bufferWidth, to=(yDistance-1), by=cellHeight)
      
      #	Create offset for column and row X and Y coords so that position corresponds to clip-strip within grid cell
      xOffset <- xTemp + (cellWidth-clipWidth)/2
      yOffset <- yTemp + (cellHeight-clipHeight)/2
      
      #  Calculate SW corner of each clip-strip within each grid cell, as well as grid cell corner positions, and add to the data frame created above. Grid cell corner positions are needed to determine whether individual cells overlap %cover nested subplots.
      grid.df[,"offsetEasting"] <- rep(xOffset, times=gridRow)
      grid.df[,"offsetNorthing"] <- rep(yOffset, each=gridCol)
      grid.df[,"x1Cell"] <- rep(xTemp, times=gridRow)
      grid.df[,"x2Cell"] <- grid.df$x1Cell + cellWidth
      grid.df[,"y1Cell"] <- rep(yTemp, each=gridCol)
      grid.df[,"y2Cell"] <- grid.df$y1Cell + cellHeight
      
      ##  Remove rows of "grid.df" that overlap 10m2 nested subplots used for Veg Structure measurements, and 9m2 area that includes 1m2 nested subs used for %cover estimates in the plant diversity protocol; ignore the 16cm of the 10m2 subplot that overlaps the 50 cm grid cell buffer, as technicians will position themselves on the West/East side of clip-strips for harvesting.
      # Obtain coordinates for nested subplots associated with 20m x 20m distributed plots
      nestedTemp <- filter(nestedCoords, subplotID==31)
      
      #	Create an empty vector to hold list of gridIDs that overlap nested subplots
      gridOmit <- c()
      
      # Using location of nested subplots specified in "nestedTemp" table, identify overlapping grid cells with a 'for' loop, store associated gridIDs in "gridOmit"; then remove elements of "gridOmit" from "grid.df" and store in a new dataframe named "clip.df"
      for(j in 1:nrow(nestedTemp)){
        
        # Filter grid.df to identify cells that overlap nested subplots with dimensions specified in nestedTemp
        tempGrid <- filter(grid.df, x1Cell < nestedTemp$x2[j] &
                             x2Cell > nestedTemp$x1[j] &
                             y1Cell < nestedTemp$y2[j] &
                             y2Cell > nestedTemp$y1[j])
        
        # Add values of clipID that overlap nested subplots to gridOmit vector created above
        gridOmit <- append(gridOmit, tempGrid$clipID)
        
      # End gridOmit 'for' loop          
      }
      
      # Filter grid.df to remove clipIDs identified in gridOmit that overlap nested subplots
      clip.df <- filter(grid.df, !clipID %in% gridOmit)
      
      #  Randomize rows of clip.df and use set.seed to ensure results are repeatable on a per plot basis
      char2seed(distID[i], set=TRUE); clip.df <- clip.df[sample(nrow(clip.df)),]
      
      #  Remove 'cell' columns from data frame so that coordinates are only provided for the clip-strip itself; add columns for 'status' and 'date'
      clip.df <- clip.df[,1:4]
      clip.df$status <- ""
      clip.df$date <- ""
      
      #	Define file name for .csv, and write the output to the distributed plot subdirectory created above
      outputName = paste(distID[i], "clipList.csv", sep="_")
      write.csv(clip.df, file=outputName, row.names=FALSE)      
      
    # End distributed plot 'for' loop  
    }
    
  print("Distributed plot output complete")
  
  # Return to clipLists working directory to begin working with tower plots
  setwd("../")
    
  # End distributed plot if/else  
  } 




### Create clipLists for 'tower' plots.
# Filter 'acceptedPlot' to obtain only tower basePlots with subtype = baseplot
towerPlots <- filter(acceptedPlot, siteID == siteCode,
                     plotType == "tower",
                     subtype == "basePlot")

# Extract tower plotIDs, and sort
towerID <- sort(as.character(towerPlots$plotID))

# Create a subdirectory in the 'clipList' directory to hold tower plot .csv output, and set as working directory
dirName <- paste(siteCode, "tower_clipList", sep="_")
dir.create(dirName)
setwd(dirName)    
  
# 'if/else' to assign subplotID = 31 for 20m x 20m tower plots, otherwise obtain unique subplotIDs from 'nestedTowerCoords' file.
if (400 %in% towerPlots$plotSize){
  subplotID <- 31
} else {
  subplotID <- filter(nestedCoords, subplotID != 31)
  subplotID <- unique(sort(subplotID$subplotID))
}

# Define the starting number for gridIDs within each subplot; gridIDs are assigned so that gridIDs per subplot do not overlap within a given plotID. Then create a dataframe to associate values of gridStart with subplotIDs. This dataframe is used below to pull the correct value of gridStart for randomly sampled values of subplotID.
gridStart <- c()
gridInit <- 0

for (m in 1:length(subplotID)){
  gridStart <- append(gridStart, gridInit)
  gridInit <- gridInit + 250
}

gridStart.df <- data.frame(subplotID, gridStart)

# 'for' loop to generate random clip-harvest locations for each plot listed in 'towerID'
for (i in 1:length(towerID)){
  
  # If/else statement defines subplotIDs within a plot for which clipIDs will be generated, and stores these subplotIDs in 'theChosen'
  if (length(subplotID)==1){
    theChosen <- subplotID
  } else {
    randomSeed <- towerID[i]
    char2seed(randomSeed, set=TRUE); theChosen <- sort(sample(subplotID, 2))
  }
  
  #	Nested 'for' loop to generate random clip-harvest locations for each subplot within a given plotID.
  for (j in 1:length(theChosen)){
    
    # Create an 8-column dataframe to hold clipID, subplotID, and the easting and northing offsets for the SW corner of the clip-strip; additional 'cell' fields are employed to filter out cells that overlap exclusion zones within the plot, and these fields are deleted with code below before the .csv is written.
    grid.df <- data.frame(matrix(data=NA, nrow=totalCell, ncol=8), stringsAsFactors=FALSE)
    cNames <- c("clipID","subplotID","offsetEasting","offsetNorthing","x1Cell","x2Cell","y1Cell","y2Cell")
    colnames(grid.df) <- cNames
    grid.df$clipID <- as.character(grid.df$clipID)
    grid.df$subplotID <- as.integer(grid.df$subplotID)
    grid.df$offsetEasting <- as.numeric(grid.df$offsetEasting)
    grid.df$offsetNorthing <- as.numeric(grid.df$offsetNorthing)
    grid.df$x1Cell <- as.numeric(grid.df$x1Cell)
    grid.df$x2Cell <- as.numeric(grid.df$x2Cell)
    grid.df$y1Cell <- as.numeric(grid.df$y1Cell)
    grid.df$y2Cell <- as.numeric(grid.df$y2Cell)
    
    #	Create gridIDs specific to the subplotID, matching values of subplotID against those in gridStart.df to select a starting point for the gridID sequence that ensures gridIDs do not overlap for any subplotID within a given plotID. Then create clipIDs from the gridIDs, and add clipIDs and subplotIDs to the grid.df dataframe.
    gridID <- seq(from=1, to=totalCell, by=1) + gridStart.df$gridStart[gridStart.df$subplotID==theChosen[j]]
    grid.df[,"clipID"] <- paste(towerID[i],formatC(gridID, width=3, format="d", flag="0"), sep="_")
    grid.df[,"subplotID"] <- theChosen[j]
    
    #  Create x-axis and y-axis coordinates for the SW corner of each clip-harvest strip within each grid cell and add to the dataframe. The clip strip itself is 0.1m W x 2m H oriented North/South, and is centered within the 0.5m x 3m grid cell such that there is a 0.2m buffer on the West/East sides of the strip, and a 0.5m buffer on the North/South sides of the strip.
    #	Define X and Y coords of SW corner of grid cells in each row of cells and each column of cells within the plot
    xTemp <- seq(from=bufferWidth, to=xDistance+cellWidth, by=cellWidth)
    yTemp <- seq(from=bufferWidth, to=(yDistance-1), by=cellHeight)
    
    #	Create offset for column and row X and Y coords so that position corresponds to clip-strip within grid cell
    xOffset <- xTemp + (cellWidth-clipWidth)/2
    yOffset <- yTemp + (cellHeight-clipHeight)/2
    
    #  Calculate SW corner of each clip-strip within each grid cell, as well as grid cell corner positions, and add to the data frame created above. Grid cell corner positions are needed to determine whether individual cells overlap %cover nested subplots.
    grid.df[,"offsetEasting"] <- rep(xOffset, times=gridRow)
    grid.df[,"offsetNorthing"] <- rep(yOffset, each=gridCol)
    grid.df[,"x1Cell"] <- rep(xTemp, times=gridRow)
    grid.df[,"x2Cell"] <- grid.df$x1Cell + cellWidth
    grid.df[,"y1Cell"] <- rep(yTemp, each=gridCol)
    grid.df[,"y2Cell"] <- grid.df$y1Cell + cellHeight
    
    #  Remove rows of "grid.df" that overlap 10m2 nested subplots used for Veg Structure measurements, and 9m2 area that includes 1m2 nested subs used for %cover estimates in the plant diversity protocol; ignore the 16cm of the 10m2 subplot that overlaps the 50 cm grid cell buffer, as technicians will position themselves on the West/East side of clip-strips for harvesting.
    #	Create an empty vector to hold list of gridIDs that overlap nested subplots
    gridOmit <- c()
    
    # Using location of nested subplots specified in "nestedCoords" table for the selected subplotID within the larger tower baseplot, identify overlapping grid cells with a 'for' loop, store associated gridIDs in "gridOmit"; then remove elements of "gridOmit" from "grid.df" and store in a new dataframe named "clip.df"
    # Identify coordinates from 'nestedCoords' from the selected subplot
    nestedTemp <- filter(nestedCoords, subplotID==theChosen[j])
    
    # Nested 'for' loop to filter out overlapping grid cells
    for(k in 1:nrow(nestedTemp)){
      
      # Filter grid.df to identify cells that overlap nested subplots with dimensions specified in nestedDistCoords
      tempGrid <- filter(grid.df, x1Cell < nestedTemp$x2[k] &
                           x2Cell > nestedTemp$x1[k] &
                           y1Cell < nestedTemp$y2[k] &
                           y2Cell > nestedTemp$y1[k])
      
      # Add values of clipID that overlap nested subplots to gridOmit vector created above
      gridOmit <- append(gridOmit, tempGrid$clipID)
      
      # End gridOmit 'for' loop          
    }
    
    # Filter grid.df to remove clipIDs identified in gridOmit that overlap nested subplots
    clip.df <- filter(grid.df, !clipID %in% gridOmit)
    
    # Define new randomSeed using plotID and subplotID
    randomSeed <- paste(towerID[i], theChosen[j], sep="_")
    
    #  Randomize rows of clip.df and use set.seed to ensure results are repeatable on a per subplot basis
    char2seed(randomSeed, set=TRUE); clip.df <- clip.df[sample(nrow(clip.df)),]
    
    #  Remove 'cell' columns from data frame so that coordinates are only provided for the clip-strip itself; add columns for 'status' and 'date'
    clip.df <- clip.df[,1:4]
    clip.df$status <- ""
    clip.df$date <- ""
    
    #	Define file name for .csv, and write the output to the tower plot subdirectory created above
    outputName <- paste(towerID[i], formatC(theChosen[j], width=2, format="d", flag="0"), "clipList.csv", sep="_")
    write.csv(clip.df, file=outputName, row.names=FALSE)
    
  # End nested 'for' for subplots within plots
  }
  
# End tower 'for' loop
}

print("Tower plot output complete")
setwd("../../")

# End of `cfc` if/else
}

# End of function
}
