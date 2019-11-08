###	Sampling simulation using a spatially-explicit stem map as input data. The assumption is that the stem map covers a rectangular area, and is oriented to align with the cardinal directions. If these assumptions are not met, the data can be rotated so that the assumption is met. Currently, only rectangular (or square) stem map areas can be accommodated by the following code - circular or irregularly shaped stem map areas will not work.

###	Super-impose a grid of sub-plots of the specified size over a spatially-explicit stem map. Randomly sample from this grid of subplots to calculate aboveground biomass with a given subplot size and number.

###	The sampling simulation function requires the following inputs:
#	(1) plotSize.m = length of one side of the subsampling plot in meters;
#	(2) plotNum = number of subplots desired;
#	(3) iterNum = number of sampling iterations used to develop distribution of AGB means #		associated with a given plot size and plot number; and

### Additionally, the following files must be in the same working directory as `samplingSimulation_stemMap.R`:
#	(1) input stem map data used for the sampling simulation
#	(2)	"Jenkins_parameters.csv"

#=========================================================================================



###	Define function for sample simulation

sampleSim = function(plotSize.m, plotNum, iterNum){

#	Calculate plot area in hectares
plotArea.ha = (plotSize.m^2)/10000

#	Calculate total sampled area in hectares
sampledArea.ha = plotArea.ha*plotNum



###	Read in user-supplied stem map data; it is necessary to prepare the stem map with code in "Data_preparation.R" prior to sample simulation.
stemmap.df = read.csv(file.choose(), header=T)



###	Load table of Jenkins parameters to use for biomass estimation of individual stems. Parameters come from Jenkins etal. 2003 Forest Science.
jpar.df = read.csv("Jenkins_parameters.csv", header=T)



###	Calculate biomass (kg) of each stem in stemmap.df with a "for" loop using the appropriate Jenkins parameters
stemmap.df$agb.kg = NA
for (i in 1:nrow(stemmap.df)){

#	Retrieve correct Jenkins parameters for the stem based on "Jenkins_type" code
temp.b0 = jpar.df$b0[jpar.df$groupID==as.character(stemmap.df$Jenkins_type[i])]
temp.b1 = jpar.df$b1[jpar.df$groupID==as.character(stemmap.df$Jenkins_type[i])]

#	Using Jenkins biomass equation, calculate the biomass in kg for stem "i" using stem "i" DBH value in "stemmap.df"
stemmap.df$agb.kg[i] = round(exp(temp.b0 + temp.b1*log(stemmap.df$dbh.cm[i])), digits=1)

#	Bracket for end of stem-mass "for" loop
}



###	Define the size of the spatially explicit stem map dataset in meters. Note the sizes calculated are not necessarily the area of the plot, but the area defined by the outer-most stems mapped within the plot. Calculated distances are rounded up to the nearest meter.

#	Length of E/W stem map boundary (m); E/W direction is defined as the "X" direction.
xDist.m = ceiling(max(stemmap.df$xdist) - min(stemmap.df$xdist))

#	Length of N/S stem map boundary (m); N/S direction is defined as the "Y" direction.
yDist.m = ceiling(max(stemmap.df$ydist) - min(stemmap.df$ydist))



###	Determine the number of columns and rows for the grid of subplots in the xDist.m and yDist.m directions based on the number of whole subplots that will fit within the stem map area, then calculate the total number of grid cells to create.

#	The 'trunc' function creates an integer by truncating the value of the argument toward zero.
nCol = trunc(xDist.m/plotSize.m)
nRow = trunc(yDist.m/plotSize.m)
nTotal = nCol*nRow


###	Create a three-column matrix to hold grid cell ID, and xDist.m and yDist.m associated with the SW corner of each grid cell:
#	Column 1 = 	grid cell ID
#	Column 2 = 	x-axis coordinate; corresponds to x-location of grid cell SW corner; 
#				currently, code assumes cell locations are relative to SW corner of 
#				stem map (SW corner = 0,0 position)
#	Column 3 =	y-axis coordinate; corresponds to y-location of grid cell SW corner

#	Create the matrix
grid.mat = matrix(data=NA, nrow=nTotal, ncol=3)
colnames(grid.mat) = c("cellID","Xcoord","Ycoord")

#	Create grid cell IDs and add to the matrix
gridID = seq(from=1, to=nTotal, by=1)
grid.mat[,1] = gridID

#	Create x-axis and y-axis coordinates for each grid cell and add to the matrix
tempX = seq(from=0, to=((nCol-1)*plotSize.m), by=plotSize.m)
grid.mat[,2] = rep(tempX, times=nRow)
tempY = seq(from=0, to=((nRow-1)*plotSize.m), by=plotSize.m)
grid.mat[,3] = rep(tempY, each=nCol)



###	Calculate the "true" total biomass for the area in which the sampling simulation will occur (Mg ha-1), and the ± 10% biomass values. Filter stemmap.df so that "true" biomass is calculated based on the size of the sampling grid. Want the area being used for the sampling simulation to match the area being used to calculate "truth".

#	Calculate the area of the sampling grid (ha)
gridArea.ha = nTotal*plotArea.ha


##	Filter stemmap.df to select only those stems in the sampling grid, and calculate the total AGB of those stems

#	Filter stemmap.df first in x-distance, then in y-distance
gridStem = stemmap.df[stemmap.df$xdist <= nCol*plotSize.m,]
gridStem = gridStem[gridStem$ydist <= nRow*plotSize.m,]

#	Sum biomass values (kg) for all stems in gridStem, convert to Mg ha-1, and calculate ± 10% values
trueAGB.mgha = round((sum(gridStem$agb.kg)/1000)/gridArea.ha, digits=1)
trueAGB.mgha = append(trueAGB.mgha, c(0.9*trueAGB.mgha, 1.1*trueAGB.mgha))
names(trueAGB.mgha) = c("trueAGB","-10%","+10%")



###	Use an if/else statement to determine whether plotNum > nTotal; if plotNum < nTotal, employ sample-iteration "for" loop to sample from "gridID" n=iterNum times

if (plotNum >= nTotal) {
print(paste("Total number of grid cells at the specified plotSize.m is",nTotal,"; please enter a value for plotNum <",nTotal), quote=FALSE)

} else {

cat(paste("The total number of grid cells at the specified plotSize.m =",nTotal,"\nThe number of grid cells subsampled at each iteration =",plotNum,"\nThe total sampled area across all plots at the specified plot size and plot number =",sampledArea.ha, "ha\n"))


##	Plot "stemmap.df" and plot grid points over the top of the stem map.
plot(stemmap.df$xdist, stemmap.df$ydist, type="n", xlab="Relative easting (m)", ylab="Relative northing (m)", main="Stem map with grid cells (grey lines), and subsample boundary (blue lines);\nsymbol size ~ DBH", cex.main=0.9)

#	Add points with symbols sized according to DBH
symbols(stemmap.df$xdist, stemmap.df$ydist, circles=stemmap.df$dbh.cm, inches=0.1, add=TRUE)

#	Superimpose sampling grid over stem map; blue lines indicate boundary of superimposed sampling grid; symbol size indicates relative DBH of stems.
abline(v=grid.mat[,2], col=8)
abline(h=grid.mat[,3], col=8)
v1X = c(max(grid.mat[,2])+plotSize.m, max(grid.mat[,2])+plotSize.m)
v1Y = c(0, max(grid.mat[,3])+plotSize.m)
lines(v1X, v1Y, col=4)
v2X = c(0,0)
v2Y = c(0, max(grid.mat[,3])+plotSize.m)
lines(v2X, v2Y, col=4)
h1X = c(0, max(grid.mat[,2])+plotSize.m)
h1Y = c(max(grid.mat[,3])+plotSize.m, max(grid.mat[,3])+plotSize.m)
lines(h1X, h1Y, col=4)
h2X = c(0, max(grid.mat[,2])+plotSize.m)
h2Y = c(0,0)
lines(h2X, h2Y, col=4)


##	Create "agbIter" matrix. First column will hold mean AGB value in Mg ha-1 for each iteration of the sampling loop with a user defined plotSize.m, plotNumber, and iterNum; second column holds 0/1 flag value indicating whether mean AGB for a given iteration is within ± 10% of the true AGB.
agbIter = matrix(data=NA, nrow=iterNum, ncol=2)
colnames(agbIter) = c("AGB","Flag")



###	Sample-iteration "for" loop used to obtain distribution of biomass means according to user-specified plotSize.m and plotNum; loop iterates according to user-specified iterNum value.

for (i in 1:iterNum){

#	Create a random sample of n=plotNum subplots from the list of available grid cells, and sort according to increasing gridID number
plotRandom = sort(sample(gridID, size=plotNum, replace=F))


##	Use coordinates in grid.mat associated with randomly sampled grid cells (subplots) to filter stemmap.df dataset and select only those stems that fall within each grid cell. Calculate AGB (Mg ha-1) for stems that fall within the grid cell.

#	Create temporary vector used to hold the total AGB for each cell in plotRandom
plotRandom.agb = NA


###	Use a "for" loop to step through each element of the plotRandom vector, and calculate AGB for each grid cell in plotRandom.
for (k in 1:length(plotRandom)){

#	Isolate X and Y coordinates associated with random grid cell "k"
tempCell = grid.mat[plotRandom[k],]

#	Define "xdist" and "ydist" range within stemmap.df for grid cell "k" based on plotSize.m; filter first by "xdist" then by "ydist" to obtain a temporary matrix 	containing only those stems that fall within the coordinates associated with grid cell "k".
tempStem = stemmap.df[which(stemmap.df$xdist >= tempCell[2] & stemmap.df$xdist < tempCell[2]+plotSize.m),]
tempStem = tempStem[which(tempStem$ydist >= tempCell[3] & tempStem$ydist < tempCell[3]+plotSize.m),]


##	Calculate biomass of all stems in tempStem, and store the total AGB for the grid cell in plotRandom.agb; use "if/else" for the case of no stems occurring within random grid cell "k"

if (nrow(tempStem) == 0){
#	AGB value for random cell "k" is zero if there are no stems in "tempStem"
plotRandom.agb[k] = 0

} else {

#	Calculate the biomass of all stems in tempStem (Mg ha-1)
plotRandom.agb[k] = (sum(tempStem$agb.kg)/1000)/plotArea.ha

#	Bracket for end of if/else statement
}

#	Bracket for end of plotRandom AGB "for" loop
}



###	Calculate mean AGB in Mg ha-1 for sampling iteration "i", and store in agbIter matrix 
agbIter[i,1] = round(mean(plotRandom.agb), digits=1)

#	If/else statement to assign Flag value based on whether mean(plotRandom.agb) is within ± 10% of trueAGB.
if (mean(plotRandom.agb) >= trueAGB.mgha[2] && mean(plotRandom.agb) <= trueAGB.mgha[3]){

#	Assign "Flag" column to 1
agbIter[i,2] = 1

} else {

#	Assign "Flag" column to 0
agbIter[i,2] = 0

#	Bracket for end of "Flag" if/else statement
}

#	Bracket for end of sample-iteration "for" loop
}

#	Bracket for end of sample-iteration "else" statement
}



###	Summary output

##	Plot distribution of subsampled means, plot trueAGB.mgha ± 10%, calculate % of iterations with mean within ± 10% of trueAGB.mgha

#	Plot distribution of subsampled means and trueAGB.mgha ± 10%
quartz()
hist(agbIter[,1], main="Distribution of AGB subsample means", xlab="AGB (Mg ha-1)")
abline(v=trueAGB.mgha[1], col=2, lwd=2)
abline(v=trueAGB.mgha[2], col=4, lty=2, lwd=2)
abline(v=trueAGB.mgha[3], col=4, lty=2, lwd=2)

#	Calculate and report %iterations with mean within ± 10% of trueAGB.mgha
inRange = round((sum(agbIter[,2])/iterNum)*100, digits=1)
cat(paste("The % of iterations with mean subsampled AGB within ± 10% of the true AGB is",inRange,"%"))


##	Return list of function-generated results of interest to user
results = list(sampledAGB = agbIter, trueAGB = trueAGB.mgha, confidence = inRange, sampledArea = sampledArea.ha)
return(results)


#	Bracket for end of function
}

