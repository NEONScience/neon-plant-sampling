Plot LAI values at each spatially explicit position (in UTM coordinates) along the transects at both Donaldson Tract and Ordway-Swisher.

Use quantile function to create bins of LAI values that will be colored according to an intensity scale like gray, but likely use green for symbol fill color (specified with bg argument within symbols function).

Symbol radius will correlate directly with the amount of vegetation that the LAI sensor "sees" at each measurement point. Calculate this based on average height of vegetation that is at least 2 m tall within the OS01 transect containing the VS plot.

## Getting the OS VS data into R to calculate height:

osvs = read.csv("D3_VS_OS_pathfinder.csv", header=T)
hgt2 = subset(osvs, height.m > 2)
str(hgt2)
'data.frame':	139 obs. of  12 variables:
 $ easting      : num  403875 403912 403904 403868 403875 ...
 $ northing     : num  3285083 3285108 3285106 3285064 3285091 ...
 $ dbh.cm       : num  NA NA NA NA 5.4 NA 5 4.6 4.8 5.2 ...
 $ stemrad.m    : num  NA NA NA NA 0.027 NA 0.025 0.023 0.024 0.026 ...
 $ maxcanopy.m  : num  1 1.5 2.1 0.5 0.8 0.8 1 1.2 1.2 1.4 ...
 $ mincanopy.m  : num  NA NA NA 0.5 0.6 NA 0.9 0.8 1.1 1 ...
 $ canopyrad.m  : num  0.5 0.75 1.05 0.25 0.35 0.4 0.48 0.5 0.58 0.6 ...
 $ firstbranch.m: num  NA NA NA 3 1.8 NA 3 2.5 3.2 1.7 ...
 $ height.m     : num  2.3 2.5 2.8 3.8 2.4 2.7 4 5.3 5 5.2 ...
 $ species      : Factor w/ 5 levels "Asin","Divi",..: 2 2 5 3 3 3 3 3 3 3 ...
 $ color        : Factor w/ 3 levels "\"#000000\"",..: 1 1 1 3 3 3 3 3 3 3 ...
 $ status       : Factor w/ 5 levels "Alive","Dead",..: 1 1 5 1 3 1 1 1 1 1 ...



## LAI-2200 lens "sees" a cone with base radius ~3X the height of the vegetation.

# At OS, plot circles with radius:

	==>	mean(hgt2$height.m)*3 = 28.36835 m
	==>	plot circles with radius of 28 m





# At DT, plot circles with radius dependent on the fact that 3 rings used for data collection:
# Outer-most view of sensor is 38˚ + 7.5˚ = 45.5˚

tan(pi/4) = 1
	==>	Radius of sensor view is approximately the height of the vegetation if only 3 rings are used for data collection

dtvs = read.csv("D3_VS_DT_pathfinder.csv", header=T)
	
	==>	mean(dtvs$height.m) = 15.5 m
	==>	plot circles with radius of 15 m


## Using data from both DT and OS, calculate quintiles for LAI data and determine LAI min. and max. values

lai = read.csv("D3_LAI_UTM_coordinates.csv", header=T)

quantile(lai$LAI, probs=seq(0,1,0.2), type=8, na.rm=TRUE)
  
		  0%  20%  40%  60%  80% 100% 
		0.01 0.44 0.66 0.89 1.16 4.45

max(lai$LAI, na.rm=TRUE)	# Without na.rm statement, function returns "NA"
max = 4.45

min(lai$LAI, na.rm=TRUE)	# Without na.rm statement, function returns "NA"
min = 0.01

## Use Illustrator to determine hex values for colors to assign to each quantile

81-100%	==>	"#00DE00"
61-80%	==>	"#72B333"
41-60%	==>	"#BCD68D"
21-40%	==>	"#FEFBB3"
0%-20%	==>	"#FFFF87"




## Get LAI data into R and form sub-dataframes for DT and OS, and then sub-sub dataframes for each group of transects to be plotted together.

lai = read.csv("D3_LAI_UTM_coordinates.csv", header=T)
up = subset(lai, group=="upper")
lo = subset(lai, group=="lower")



## Create plots for Donaldson Tract LAI measurements

# Use Pathfinder Office to determine xlim and ylim values for "upper" transect group
# Create an empty plot to contain the points:
plot(up$easting, up$northing, xlim=c(386900,387400), ylim=c(3292950,3293500), xlab="Easting (UTM)", ylab="Northing (UTM)", main="D3 Donaldson Tract LAI data for transects DT05 (right) and DT06 (left)", type="n")

# Create sub-groups of points based on LAI quintiles, to be colored using hex codes above:
up.20 = subset(up, lai.quant==0.2)
up.40 = subset(up, lai.quant==0.4)
up.60 = subset(up, lai.quant==0.6)
up.80 = subset(up, lai.quant==0.8)
up.100 = subset(up, lai.quant==1.0)


# Create "z" vector to be used for circle sizes. Plot is too messy if radius of 15 m is used, so use 5 m radius:

zup.20 = mat.or.vec(length(up.20$easting),1)
zup.20 = zup.20+5

zup.40 = mat.or.vec(length(up.40$easting),1)
zup.40 = zup.40+5

zup.60 = mat.or.vec(length(up.60$easting),1)
zup.60 = zup.60+5

zup.80 = mat.or.vec(length(up.80$easting),1)
zup.80 = zup.80+5

zup.100 = mat.or.vec(length(up.100$easting),1)
zup.100 = zup.100+5

# Add symbols to plot with appropriate colors
symbols(up.20$easting, up.20$northing, circles=zup.20, inches=FALSE, add=TRUE, fg="#000000", bg="#FFFF87")

symbols(up.40$easting, up.40$northing, circles=zup.40, inches=FALSE, add=TRUE, fg="#000000", bg="#FEFBB3")

symbols(up.60$easting, up.60$northing, circles=zup.60, inches=FALSE, add=TRUE, fg="#000000", bg="#BCD68D")

symbols(up.80$easting, up.80$northing, circles=zup.80, inches=FALSE, add=TRUE, fg="#000000", bg="#72B333")

symbols(up.100$easting, up.100$northing, circles=zup.100, inches=FALSE, add=TRUE, fg="#000000", bg="#00DE00")

# Add transect lines:
dt05 = subset(lai, transectID=="DT05")
dt06 = subset(lai, transectID=="DT06")
lines(dt05$easting, dt05$northing, lty=2)
lines(dt06$easting, dt06$northing, lty=2)



# Use Pathfinder Office to determine xlim and ylim values for "lower" DT03 and DT04 transects
# Create an empty plot to contain the points. Note that "\n" can be used to create new line in "main" title if necessary:
plot(lo$easting, lo$northing, xlim=c(387025,387650), ylim=c(3292030,3292500), xlab="Easting (UTM)", ylab="Northing (UTM)", main="D3 Donaldson Tract LAI data for transects DT03 (bottom) and DT04 (top)", type="n")

# Create sub-groups of points based on LAI quintiles, to be colored using hex codes above:
lo.20 = subset(lo, lai.quant==0.2)
lo.40 = subset(lo, lai.quant==0.4)
lo.60 = subset(lo, lai.quant==0.6)
lo.80 = subset(lo, lai.quant==0.8)
lo.100 = subset(lo, lai.quant==1.0)

# Create "z" vector to be used for circle sizes. Plot is too messy if radius of 15 m is used, so use 5 m radius:

zlo.20 = mat.or.vec(length(lo.20$easting),1)
zlo.20 = zlo.20+5

zlo.40 = mat.or.vec(length(lo.40$easting),1)
zlo.40 = zlo.40+5

zlo.60 = mat.or.vec(length(lo.60$easting),1)
zlo.60 = zlo.60+5

zlo.80 = mat.or.vec(length(lo.80$easting),1)
zlo.80 = zlo.80+5

zlo.100 = mat.or.vec(length(lo.100$easting),1)
zlo.100 = zlo.100+5

# Add symbols to plot with appropriate colors
symbols(lo.20$easting, lo.20$northing, circles=zlo.20, inches=FALSE, add=TRUE, fg="#000000", bg="#FFFF87")

symbols(lo.40$easting, lo.40$northing, circles=zlo.40, inches=FALSE, add=TRUE, fg="#000000", bg="#FEFBB3")

symbols(lo.60$easting, lo.60$northing, circles=zlo.60, inches=FALSE, add=TRUE, fg="#000000", bg="#BCD68D")

symbols(lo.80$easting, lo.80$northing, circles=zlo.80, inches=FALSE, add=TRUE, fg="#000000", bg="#72B333")

symbols(lo.100$easting, lo.100$northing, circles=zlo.100, inches=FALSE, add=TRUE, fg="#000000", bg="#00DE00")

# Add transect lines:
dt03 = subset(lai, transectID=="DT03")
dt04 = subset(lai, transectID=="DT04")
lines(dt03$easting, dt03$northing, lty=2)
lines(dt04$easting, dt04$northing, lty=2)



## Create plots for Ordway-Swisher LAI measurements

# Use Pathfinder Office to determine xlim and ylim values for "tower" transect group
# Create an empty plot to contain the points:
plot(tower$easting, tower$northing, xlim=c(403800,404500), ylim=c(3284700,3285300), xlab="Easting (UTM)", ylab="Northing (UTM)", main="D3 Ordway-Swisher LAI for tower airshed (transects OS01 through OS04)", type="n")

# Create groups for plotting of adjacent transects:
west = subset(lai, group=="west")
tower = subset(lai, group=="tower")
east = subset(lai, group=="east")

# Create sub-groups of points based on LAI quintiles, to be colored using hex codes above:
tower.20 = subset(tower, lai.quant==0.2)
tower.40 = subset(tower, lai.quant==0.4)
tower.60 = subset(tower, lai.quant==0.6)
tower.80 = subset(tower, lai.quant==0.8)
tower.100 = subset(tower, lai.quant==1.0)

# Create "z" vector to be used for circle sizes. Plot is too messy if radius of 15 m is used, so use 5 m radius:

ztower.20 = mat.or.vec(length(tower.20$easting),1)
ztower.20 = ztower.20+5

ztower.40 = mat.or.vec(length(tower.40$easting),1)
ztower.40 = ztower.40+5

ztower.60 = mat.or.vec(length(tower.60$easting),1)
ztower.60 = ztower.60+5

ztower.80 = mat.or.vec(length(tower.80$easting),1)
ztower.80 = ztower.80+5

ztower.100 = mat.or.vec(length(tower.100$easting),1)
ztower.100 = ztower.100+5

# Add symbols to plot with appropriate colors:
symbols(tower.20$easting, tower.20$northing, circles=ztower.20, inches=FALSE, add=TRUE, fg="#000000", bg="#FFFF87")

symbols(tower.40$easting, tower.40$northing, circles=ztower.40, inches=FALSE, add=TRUE, fg="#000000", bg="#FEFBB3")

symbols(tower.60$easting, tower.60$northing, circles=ztower.60, inches=FALSE, add=TRUE, fg="#000000", bg="#BCD68D")

symbols(tower.80$easting, tower.80$northing, circles=ztower.80, inches=FALSE, add=TRUE, fg="#000000", bg="#72B333")

symbols(tower.100$easting, tower.100$northing, circles=ztower.100, inches=FALSE, add=TRUE, fg="#000000", bg="#00DE00")

# Add transect lines:
os01 = subset(lai, transectID=="OS01")
os02 = subset(lai, transectID=="OS02")
os03 = subset(lai, transectID=="OS03")
os04 = subset(lai, transectID=="OS04")
lines(os01$easting, os01$northing, lty=2)
lines(os02$easting, os02$northing, lty=2)
lines(os03$easting, os03$northing, lty=2)
lines(os04$easting, os04$northing, lty=2)



# Use Pathfinder Office to determine xlim and ylim values for "west" transect group
# Create an empty plot to contain the points:
plot(west$easting, west$northing, xlim=c(403350,403950), ylim=c(3284600,3285050), xlab="Easting (UTM)", ylab="Northing (UTM)", main="D3 Ordway-Swisher LAI for OS11 and OS12 transects", type="n")

# Create sub-groups of points based on LAI quintiles, to be colored using hex codes above:
west.20 = subset(west, lai.quant==0.2)
west.40 = subset(west, lai.quant==0.4)
west.60 = subset(west, lai.quant==0.6)
west.80 = subset(west, lai.quant==0.8)
west.100 = subset(west, lai.quant==1.0)

# Create "z" vector to be used for circle sizes; use 5 m radius as above:

zwest.20 = mat.or.vec(length(west.20$easting),1)
zwest.20 = zwest.20+5

zwest.40 = mat.or.vec(length(west.40$easting),1)
zwest.40 = zwest.40+5

zwest.60 = mat.or.vec(length(west.60$easting),1)
zwest.60 = zwest.60+5

zwest.80 = mat.or.vec(length(west.80$easting),1)
zwest.80 = zwest.80+5

zwest.100 = mat.or.vec(length(west.100$easting),1)
zwest.100 = zwest.100+5

# Add symbols to plot with appropriate colors:
symbols(west.20$easting, west.20$northing, circles=zwest.20, inches=FALSE, add=TRUE, fg="#000000", bg="#FFFF87")

symbols(west.40$easting, west.40$northing, circles=zwest.40, inches=FALSE, add=TRUE, fg="#000000", bg="#FEFBB3")

symbols(west.60$easting, west.60$northing, circles=zwest.60, inches=FALSE, add=TRUE, fg="#000000", bg="#BCD68D")

symbols(west.80$easting, west.80$northing, circles=zwest.80, inches=FALSE, add=TRUE, fg="#000000", bg="#72B333")

symbols(west.100$easting, west.100$northing, circles=zwest.100, inches=FALSE, add=TRUE, fg="#000000", bg="#00DE00")

# Add transect lines:
os11 = subset(lai, transectID=="OS11")
os12 = subset(lai, transectID=="OS12")
lines(os11$easting, os11$northing, lty=2)
lines(os12$easting, os12$northing, lty=2)




# Use Pathfinder Office to determine xlim and ylim values for "east" transect group
# Create an empty plot to contain the points:
plot(east$easting, east$northing, xlim=c(404600,405000), ylim=c(3284750,3285300), xlab="Easting (UTM)", ylab="Northing (UTM)", main="D3 Ordway-Swisher LAI for OS08 and OS09 transects", type="n")

# Create sub-groups of points based on LAI quintiles, to be colored using hex codes above:
east.20 = subset(east, lai.quant==0.2)
east.40 = subset(east, lai.quant==0.4)
east.60 = subset(east, lai.quant==0.6)
east.80 = subset(east, lai.quant==0.8)
east.100 = subset(east, lai.quant==1.0)

# Create sub-groups for plotting transect lines:
os08 = subset(lai, transectID=="OS08")
os09 = subset(lai, transectID=="OS09")

# Create "z" vector to be used for circle sizes; use 5 m radius as above:

zeast.20 = mat.or.vec(length(east.20$easting),1)
zeast.20 = zeast.20+5

zeast.40 = mat.or.vec(length(east.40$easting),1)
zeast.40 = zeast.40+5

zeast.60 = mat.or.vec(length(east.60$easting),1)
zeast.60 = zeast.60+5

zeast.80 = mat.or.vec(length(east.80$easting),1)
zeast.80 = zeast.80+5

zeast.100 = mat.or.vec(length(east.100$easting),1)
zeast.100 = zeast.100+5

# Add symbols to plot with appropriate colors:
symbols(east.20$easting, east.20$northing, circles=zeast.20, inches=FALSE, add=TRUE, fg="#000000", bg="#FFFF87")

symbols(east.40$easting, east.40$northing, circles=zeast.40, inches=FALSE, add=TRUE, fg="#000000", bg="#FEFBB3")

symbols(east.60$easting, east.60$northing, circles=zeast.60, inches=FALSE, add=TRUE, fg="#000000", bg="#BCD68D")

symbols(east.80$easting, east.80$northing, circles=zeast.80, inches=FALSE, add=TRUE, fg="#000000", bg="#72B333")

symbols(east.100$easting, east.100$northing, circles=zeast.100, inches=FALSE, add=TRUE, fg="#000000", bg="#00DE00")

# Add transect lines:
lines(os08$easting, os08$northing, lty=2)
lines(os09$easting, os09$northing, lty=2)