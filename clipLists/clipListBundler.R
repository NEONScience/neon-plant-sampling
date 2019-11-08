### Bundle per plot Clip List files downloaded from Sharepoint into one 'Distributed Plot' and one 'Tower Plot' data frame per site.
### Create new 'plotID' column, and create one new Excel file per site, with two tabs, one each for 'Distributed' and 'Tower' plots.
### Using .csv files from Sharepoint, as some files have been updated by technicians with sampling information.

# Load libraries
library(stringr)
library(plyr)
library(dplyr)
library(xlsx)

# Set working directory
if(file.exists("~/Documents/neonScienceDocs")){
  setwd("~/Documents/neonScienceDocs/gitRepositories/devTOS/clipLists/_excelVersionInput")
}

##########  Function to collate existing .csv Clip Lists into one Excel file
# User supplied 'dirInput' must be a quoted string that includes site and plotType - e.g., "BART_tower"
clipListBundle <- function(dirInput){
  # Set wd and define variables based on user input
  setwd(dirInput)
  splitInput <- str_split(dirInput, "_")
  siteID <- splitInput[[1]][1]
  plotType <- splitInput[[1]][2]
  
  # Read in files in user-specified directory, and create data frame from all files
  theLists <- list.files(path = ".")
  listsDF <- data.frame()
  for (i in 1:length(theLists)){
    tempList <- read.csv(theLists[i], header = T, stringsAsFactors = F)
    listsDF <- plyr::rbind.fill(listsDF, tempList)
  }
  listsDF <- tbl_df(listsDF)
  
  ##  Add and re-order columns to 'listsDF'
  # Add plotID column to allow sorting by plotID
  listsDF$plotID <- str_sub(listsDF$clipID, start=1, end=8)
  
  # Order original column names, and add any columns added by Field Ops to the right of the original fields
  cNames <- colnames(listsDF)
  orderedNames <- c("plotID", "subplotID", "clipID", "offsetEasting", "offsetNorthing", "status")
  newNames <- setdiff(cNames, orderedNames)
  listsDF <- listsDF[c(orderedNames, newNames)]
  
  # Write to .xlsx file
  setwd("../../_excelClipLists")
  fileName <- paste0(siteID, "_clipList.xlsx")
  write.xlsx2(listsDF, file = fileName, sheetName = plotType, col.names = TRUE, row.names = TRUE, append = TRUE, showNA = FALSE)
  setwd("../_excelVersionInput")
  print("Output complete.")
  
# End function bracket  
}


### Run 'clipListBundle' function for all folders in '_excelVersionInput'
# List input directories
inputDirs <- list.files(path = ".", include.dirs = TRUE)

# Use 'for' loop to execute 'clipListBundle' function for each directory
for (i in 1:length(inputDirs)){
  clipListBundle(inputDirs[i])
}






