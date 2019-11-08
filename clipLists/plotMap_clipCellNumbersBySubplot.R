### Create a table that lists the clipCellNumber, easting offset, and northing offset from the SW corner of the plot / subplot
#  Create a table with the following fields: cellNumSubplot31, cellNumSubplot21, cellNumSubplot23, cellNumSubplot39, cellNumSubplot41, eastingOffset, northingOffset

# Input one-sided subplot size, buffer width, and grid cell dimensions, all in meters.
subplotSize = 20
bufferWidth = 1
yDistance = subplotSize - 2*bufferWidth
xDistance = subplotSize - 2*bufferWidth
cellHeight = 3
cellWidth = 0.5
clipWidth = 0.1
clipHeight = 2

# Calculate the total number of grid cells per plot / subplot
gridCol = xDistance/cellWidth
gridRow = yDistance/cellHeight
totalCell = gridCol*gridRow

# Define sequences of clipCellNumber for each subplotID (i.e. - 31, 21, 23, 39, 41)
cellNum31 = 1:totalCell
cellNum21 = 1:totalCell
cellNum23 = seq(from=251, to=251+(totalCell-1), by=1)
cellNum39 = seq(from=501, to=501+(totalCell-1), by=1)
cellNum41 = seq(from=751, to=751+(totalCell-1), by=1)

# Create a data frame to hold the clipCellNumber values for each subplotID, and eastingOffset and northingOffset values in meters for each clipCellNumber
cellNum.df = data.frame(cellNum31, cellNum21, cellNum23, cellNum39, cellNum41, stringsAsFactors=F)
cellNum.df$eastingOffset = NA
cellNum.df$northingOffset = NA
cnames = c("cellNumSubplot31", "cellNumSubplot21", "cellNumSubplot23", "cellNumSubplot39", "cellNumSubplot41", "eastingOffset", "northingOffset")
colnames(cellNum.df) = cnames

# Create sequence of easting and northing values corresponding to SW corner of Clip Strips; these will be combined below to produce coordinates for Clip Strips for each element of clipCellNumber
swCellEasting = seq(from=bufferWidth, to=xDistance+cellWidth, by=cellWidth)
swClipEasting = swCellEasting + (cellWidth-clipWidth)/2
swCellNorthing = seq(from=bufferWidth, to=(yDistance-1), by=cellHeight)
swClipNorthing = swCellNorthing + (cellHeight-clipHeight)/2

# Combine easting and northing values from sequences above to create (easting, northing) pairs for each clipCellNumber, and add to cellNum.df
cellNum.df$eastingOffset = rep(swClipEasting, times=gridRow)
cellNum.df$northingOffset = rep(swClipNorthing, each=gridCol)

# Write cellNum.df to a .csv file for incorporation into an appendix in the Herbaceous Clip protocol.
write.csv(cellNum.df, file="Table_clipCellNumber_offsets.csv", row.names=FALSE)
