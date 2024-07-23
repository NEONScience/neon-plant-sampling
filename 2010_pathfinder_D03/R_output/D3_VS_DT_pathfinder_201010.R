Plotting Donaldson Tract vegetation structure data

All trees were Pinus elliottii in a wet flatland ecosystem with a vibrant Saw Palmetto understory (Serenoa repens). Position data were collected as distance (m) along two rows of plantation trees. Rows were planted in a straight line, with orientation that were slightly off true North. GPS data were collected at the start and end of each of the two rows that were used for collection of vegetation structure data.

In order to obtain coordinates of each tree in UTM, the following steps were carried out:

	1.	Plotted known, corrected UTM coordinates of the start and end of row1 and row2 trees that were used for vegetation structure measurements.
	
	E1 = c(387749.836, 387742.830)
	N1 = c(3292588.897, 3292409.839)
	
	E2 = c(387743.780, 387738.428)
	N2 = c(3292580.986, 3292412.430)
	
	2.	Create one triangle for each row of trees. As rows are slightly from off True North (NE to SW), calculate angle "theta" of line from origin (0,0) relative to -Y axis. This assumes the start of each row is at the N end of the row. To calculate "theta", also calculate the length "a" of the adjacent side of the triangle (runs parallel to N/S axis), and the length "o" of the opposite side (runs parallel to E/W axis).

		Calculation of "o" absolute values:
		
		o1 = E1[1]-E1[2], o1 = 7.006
		o2 = E2[1]-E2[2], o2 = 5.352
		
		Calculation of "a" absolute values:
		
		a1 = N1[1]-N1[2], a1 = 179.058
		a2 = N2[1]-N2[2], a2 = 168.556
		
		Calculation of "theta":
		
		Use atan function and deg function (below) to obtain "theta" in degrees.
		
		deg = function(radians) {
				degrees = (radians*180)/pi
				return(degrees)
				}
		
		theta1 = deg(atan(o1/a1)), theta1 = 2.240668
		theta2 = deg(atan(o2/a2)), theta2 = 1.818648
		
	3.	For each plant i, can now create a triangle with hypotenuse hi (the distance along the row where plant i is located) and theta (above). Can then use cos(theta) = ai/hi and sin(theta) = oi/hi to calculate ai and oi. Easting and Northing coordinates for plant i (Ei, Ni) can then be obtained as follows:
	
		Ei = Es - oi, 	where Es is the known, corrected Easting UTM coordinate of the row 
						start position.
		
		Ni = Ns - ai,	where Ns is the known, corrected Northing UTM coordinate of the 		
						row start position.
						

Next, plot UTM (Ei, Ni) for each tree, and scale the symbol size to the maximum canopy width of the tree. Use lines and points to overlay the transect onto the plot.

Getting the data into R:

dtvs = read.csv("D3_VS_DT_pathfinder.csv", header=T)

str(dtvs)
'data.frame':	100 obs. of  10 variables:
 $ transectID   : int  1 1 1 1 1 1 1 1 1 1 ...
 $ easting      : num  387750 387750 387750 387750 387749 ...
 $ northing     : num  3292589 3292587 3292584 3292581 3292577 ...
 $ dbh.cm       : num  19 19.2 26.2 10 18 17.5 21.5 18.4 17.7 17.5 ...
 $ stemrad.m    : num  0.095 0.096 0.131 0.05 0.09 0.088 0.108 0.092 0.089 0.088 ...
 $ maxcanopy.m  : num  2.4 2.4 2.5 1.7 3.6 3.3 2.4 2.4 2.3 2.3 ...
 $ canopyrad.m  : num  1.2 1.2 1.3 0.9 1.8 1.7 1.2 1.2 1.2 1.2 ...
 $ firstbranch.m: num  12.1 10.2 9.6 10.7 10.4 7.9 11.1 11.3 10.7 12.2 ...
 $ height.m     : num  16.2 16.6 15.1 14.1 16.9 13 16.1 17.4 16.2 16.7 ...
 $ status       : Factor w/ 3 levels "Alive","Dead",..: 1 1 3 1 1 1 1 1 1 1 ...




Making a plot with the radius of each point corresponding directly to the maximum canopy diameter (m):

	1.	First plot the starting and ending coordinates of each row of trees, and draw a line along each row. Values for "xlim" and "ylim" arguments obtained from GPS Pathfinder software displaying DT_orthoimage.tif
	
	plot(E1,N1,pch=13, xlim=c(387725,387765), ylim=c(3292395,3292600), xlab="Easting (UTM)", ylab="Northing (UTM)", main="D3 Donaldson Tract Slash Pine structure measurements: Maximum canopy diameter")
	points(E2,N2, pch=13)
	lines(E1,N1, lty=2)
	lines(E2,N2, lty=2)
	
	
	2.	Add symbols for individual trees, with symbol size representing the maximum canopy diameter (m) of the trees.
	
	symbols(easting, northing, circles=canopyrad.m, inches=FALSE, add=TRUE, fg="#005826")
	
	## The 'add=TRUE' argument puts the symbols on the existing plot, rather than drawing a new one.
	
	

Making a plot with the radius of each point corresponding directly to the stem radius:

	1.	Make a plot as above first.
	
	attach(dtvs)
	plot(E1,N1,pch=13, xlim=c(387725,387765), ylim=c(3292395,3292600), xlab="Easting (UTM)", ylab="Northing (UTM)", main="D3 Donaldson Tract Slash Pine structure measurements: Diameter at breast height")
	points(E2,N2, pch=13)
	lines(E1,N1, lty=2)
	lines(E2,N2, lty=2)

	2.	Add symbols with radius corresponding to stem radius.
	
	symbols(easting, northing, circles=stemrad.m, inches=FALSE, add=TRUE, fg="#005826")
	detach(dtvs)