Plotting Ordway-Swisher vegetation structure data

Position data were originally collected with Y coordinates being the distance (m) along the plot center-line, -X being distance to the left of the center-line, and +X being distance to the right of the center-line. Y spanned a range from 0 to 120 m, and X was 10 m on either side of the center-line, resulting in a plot with 120 m x 20 m dimensions.

In order to obtain coordinates of each tree/shrub in UTM, the following steps were carried out:

	1.	Plotted known, corrected UTM coordinates of first four positions (0, 50, 100, 150 m) along OS01 LAI transect, which contains the entire VS plot.
	
	E = c(403835.04, 403876.8, 403919.41, 403963.86)
	N = c(3285052.24, 3285080.38, 3285104.02, 3285129.41)
	
	2.	Created 3 triangles, one for each 50 m segment of the center-line of the VS plot - i.e. a hypotenuse for 0-50 m, 50-100 m, and 100-150 m. For each triangle, calculated "phi" (angle E of true N), the length of "a" the adjacent side (runs along N/S axis), and the length of "o" the opposite side (runs along E/W axis). The first triangle (phi1, a1, o1) originates at the plot origin. Subsequent 2 triangles originate at the 50 and 100 m marks for which GPS data exist.
	
		Calculation of "o" values:
		
		o1 = E[2]-E[1], o1 = 41.76
		o2 = E[3]-E[2], o2 = 42.61
		o3 = E[4]-E[3], o3 = 44.45
		
		Calculation of "a" values:
		
		a1 = N[2]-N[1], a1 = 28.14
		a2 = N[3]-N[2], a2 = 23.64
		a3 = N[4]-N[3], a3 = 25.39
		
		Calculation of angles "phi":
		
		Use atan function and deg function (below) to obtain phi in degrees:
		
		phi1 = deg(atan(o1/a1)), phi1 = 56.02592
		phi2 = deg(atan(o2/a2)), phi2 = 60.97852
		phi3 = deg(atan(o3/a3)), phi3 = 60.26484
		
		rad = function(degrees) {
			radians = (degrees*pi)/180
			return(radians) 	
			}

		deg = function(radians) {
			degrees = (radians*180)/pi
			return(degrees)
			}
		
	3. Calculate rotation angle theta (th1, th2, th3), which is clockwise and relative to vertical Y axis - i.e axis around which veg data are centered.
	
		th1 = 360 - phi1, th1 = 303.9741
		th2 = 360 - phi2, th2 = 299.0215
		th3 = 360 - phi3, th3 = 299.7352
	
	4.	Rotate Xi and Yi (new X and Y positions) using the following formulas:
	
		Xi = Xcos(theta) - Ysin(theta)
		Yi = Xsin(theta) + Ycos(theta)
		
		Calculations are currently carried out in Excel, but could easily write an R function to accomplish rotation.
		
	5.	Translate Xi and Yi to get new Easting and Northing coordinates within a 50 m segment of the plot. For example, simply add Xi to E0 to get new Ei coordinate. Similar for adding Yi to N0 to get new Ni coordinate.
	


==========================================================================================
Plot UTM coordinates of each tree and shrub, and scale the symbol size to the average diameter of the plant. Color code the symbol according to species (e.g. green = Pipa, red = Qula, black = other). Use lines and points to overlay the plot center-line onto the plot.

Getting the data into R:

vs = read.csv("D3_VS_OS_pathfinder.csv", header=T)

str(vs)
'data.frame':	286 obs. of  12 variables:
 $ easting      : num  403922 403917 403871 403882 403928 ...
 $ northing     : num  3285101 3285111 3285066 3285095 3285102 ...
 $ dbh.cm       : num  NA NA NA NA NA NA NA NA NA NA ...
 $ stemrad.m    : num  NA NA NA NA NA NA NA NA NA NA ...
 $ maxcanopy.m  : num  0.6 0.7 0.7 0.7 0.8 0.9 1 1 1.1 1.1 ...
 $ mincanopy.m  : num  NA NA NA NA NA NA NA NA NA NA ...
 $ canopyrad.m  : num  0.3 0.35 0.35 0.35 0.4 0.45 0.5 0.5 0.55 0.55 ...
 $ firstbranch.m: num  NA NA NA NA NA NA NA NA NA NA ...
 $ height.m     : num  1 1.2 1.5 1.4 1.1 1.8 1.2 2.3 1.2 1.1 ...
 $ species      : Factor w/ 5 levels "Asin","Divi",..: 1 1 2 5 1 2 1 2 1 5 ...
 $ color        : Factor w/ 3 levels "\"#000000\"",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ status       : Factor w/ 5 levels "Alive","Dead",..: 1 1 1 2 1 1 1 1 1 2 ...


###	Making the plot

##	Create subsets that can be used to plot each of the following groups: 1) live Pipa; 2) dead Pipa; 3) live Qula; 4) dead Qula; and 5) other species.

#	Select live Pipa individuals, including one datum with status="Horizontal"
pipa.l = vs[which(vs$species=='Pipa' & !vs$status=='Dead'),]
#	Select dead Pipa individuals
pipa.d = vs[which(vs$species=='Pipa' & vs$status=='Dead'),]
#	Select live Qula individuals, including one datum with status="Horizontal"
qula.l = vs[which(vs$species=='Qula' & !vs$status=='Dead'),]
#	Select dead Qula individuals, mostly those with a flush of live leaves at ground level
qula.d = vs[which(vs$species=='Qula' & vs$status=='Dead'),]
#	Select other species (mostly small stature shrubs)
other = vs[!vs$species %in% c("Pipa","Qula"),]


#	Plot GPS coordinates from plot center-line. E and N values come from high-res GPS (Pathfinder Office)

E = c(403835.04, 403876.8, 403919.41, 403963.86)
N = c(3285052.24, 3285080.38, 3285104.02, 3285129.41)
plot(E, N, pch=13, xlim=c(403830,403945), ylim=c(3285045,3285125), xlab="Easting (UTM)", ylab="Northing (UTM)", main="D3 Ordway-Swisher vegetation structure measurements: Average canopy diameter along OS01 transect")
lines(E, N, lty=2)


#	Add symbols for individual trees, with symbol color specific to species or groups of species, and symbol size corresponding directly to the average canopy diameter of the trees. The add="TRUE" argument puts the symbols on the existing plot, rather than drawing a new one.
#	For more sophisticated application of alpha (transparency) values to "fg" or "bg" colors, see https://www.rforge.net/doc/packages/snippets/col.html

symbols(other$easting, other$northing, circles=other$canopyrad.m, inches=FALSE, add=TRUE, fg="#000000", bg="gray60")
symbols(qula.d$easting, qula.d$northing, circles=qula.d$canopyrad.m, inches=FALSE, add=TRUE, fg="#f26522", bg="#945232")
symbols(qula.l$easting, qula.l$northing, circles=qula.l$canopyrad.m, inches=FALSE, add=TRUE, fg="#f26522")
symbols(pipa.d$easting, pipa.d$northing, circles=pipa.d$canopyrad.m, inches=FALSE, add=TRUE, fg="#005826", bg="#41614F")
symbols(pipa.l$easting, pipa.l$northing, circles=pipa.l$canopyrad.m, inches=FALSE, add=TRUE, fg="#005826")






==========================================================================================
Create a 3D scatterplot using Easting and Northing for x and y variables, and Height for the z variable. Points colored according to "Pipa", "Qula", and "Other"

vs = read.csv("D3_VS_OS_pathfinder.csv", header=T)

###	Subset the dataframe by species so that different species can be plotted with different colors. 

#	Subset the dataframe to get only "Pipa" individuals
pipa = vs[vs$species=="Pipa",]

#	Subset the dataframe to get only "Qula" individuals
qula = vs[vs$species=="Qula",]

#	Subset the dataframe to get only non "Pipa" and non "Qula" individuals
other = vs[!vs$species %in% c("Pipa", "Qula"),]

#	Create and plot the 3D scatterplot object, adding "Pipa" points first
library(scatterplot3d)

s3d = scatterplot3d(pipa$easting, pipa$northing, pipa$height.m, type="h", main="Tree and shrub height within the OS01 vegetation structure plot", xlab="UTM Easting (m)", ylab="UTM Northing (m)", zlab="Tree height (m)", pch=19, color="#005826", box=F, angle=70, scale.y=1.2, xlim=c(403830,403950), ylim=c(3285040,3285120))
 
#	Add "Qula" points to the 3D scatterplot
s3d$points3d(qula$easting, qula$northing, qula$height.m, type="h", pch=19, col="#f26522")

#	Add "Other" points to the 3D scatterplot
s3d$points3d(other$easting, other$northing, other$height.m, type="h", pch=21)



==========================================================================================
Create a 3D scatterplot using Easting and Northing for x and y variables, and Height for the z variable. Points colored according to Pipa live, Pipa dead, Qula live, Qula dead, and other.

vs = read.csv("D3_VS_OS_pathfinder.csv", header=T)

##	Create subsets that can be used to plot each of the above groups.

#	Select live Pipa individuals, including one datum with status="Horizontal"
pipa.l = vs[which(vs$species=='Pipa' & !vs$status=='Dead'),]
#	Select dead Pipa individuals
pipa.d = vs[which(vs$species=='Pipa' & vs$status=='Dead'),]
#	Select live Qula individuals, including one datum with status="Horizontal"
qula.l = vs[which(vs$species=='Qula' & !vs$status=='Dead'),]
#	Select dead Qula individuals, mostly those with a flush of live leaves at ground level
qula.d = vs[which(vs$species=='Qula' & vs$status=='Dead'),]
#	Select other species (mostly small stature shrubs)
other = vs[!vs$species %in% c("Pipa","Qula"),]



#	Create and plot the 3D scatterplot object, adding live Pipa points first, as these trees have the greatest height values.

s3d = scatterplot3d(pipa.l$easting, pipa.l$northing, pipa.l$height.m, type="h", main="Tree and shrub height within the OS01 vegetation structure plot", xlab="Easting (m)", ylab="Northing (m)", zlab="Tree height (m)", pch=21, color="#005826", box=F, angle=70, scale.y=1.2, xlim=c(403830,403950), ylim=c(3285040,3285120))

#	Add dead Pipa points to the scatterplot
s3d$points3d(pipa.d$easting, pipa.d$northing, pipa.d$height.m, type="h", pch=19, col="#41614F")

#	Add dead Qula points to the scatterplot
s3d$points3d(qula.d$easting, qula.d$northing, qula.d$height.m, type="h", pch=19, col="#945232")

#	Add live Qula points to the scatterplot
s3d$points3d(qula.l$easting, qula.l$northing, qula.l$height.m, type="h", pch=21, col="#f26522")

#	Add "Other" points to the scatterplot
s3d$points3d(other$easting, other$northing, other$height.m, type="h", pch=19)





==========================================================================================
Create a separate 3D scatterplot of DBH (x-axis) vs. canopy diameter (y-axis) vs. height (z-axis) for live "Pipa" and "Qula" individuals > 5 cm DBH.

#	Subset the "vs" dataframe to get live Pipa > 5 cm DBH
pipa.5cm = vs[which(vs$species=='Pipa' & vs$dbh.cm >= 5 & vs$status=='Alive'),]

#	Subset the "vs" dataframe to get live Qula > 5 cm DBH
qula.5cm = vs[which(vs$species=='Qula' & vs$dbh.cm >= 5 & vs$status=='Alive'),]

#	Remove NA value in Qula dataset
qula.5cm = qula.5cm[-c(13),]

#	Calculate the geometric mean canopy diameter for plotting
pipa.5cm$avecanopy.m = sqrt(pipa.5cm$maxcanopy.m*pipa.5cm$mincanopy.m)
qula.5cm$avecanopy.m = sqrt(qula.5cm$maxcanopy.m*qula.5cm$mincanopy.m)

###	Create the 3D scatterplot for Pipa > 5 cm DBH
s3d = scatterplot3d(pipa.5cm$dbh.cm, pipa.5cm$avecanopy.m, pipa.5cm$height.m, type="h", main="Pipa allometric relationships", xlab="Diameter breast height (cm)", ylab="Canopy diameter (m)", zlab="Tree height (m)", pch=21, color="#005826", scale.y=0.7, box=F, angle=60, xlim=c(0,40), ylim=c(0,14), zlim=c(0,25))

#	Add regression plane to 3D scatterplot
pipa.lm = lm(height.m~avecanopy.m+dbh.cm, data=pipa.5cm)
s3d$plane3d(pipa.lm, lty.box="solid")

###	Create 3D scatterplot for Qula > 5 cm DBH
s3d = scatterplot3d(qula.5cm$dbh.cm, qula.5cm$avecanopy.m, qula.5cm$height.m, type="h", pch=21, color="#f26522", scale.y=0.7, main="Qula allometric relationships", xlab="Diameter breast height (cm)", ylab="Canopy diameter (m)", zlab="Tree height (m)", box=F, xlim=c(0,40), ylim=c(0,14), zlim=c(0,25), angle=60)

==== or ==== 

#	Add Qula points to existing plot
s3d$points3d(qula.5cm$dbh.cm, qula.5cm$avecanopy.m, qula.5cm$height.m, type="h", pch=21, col="#f26522")





==========================================================================================
vs = read.csv("D3_VS_OS_pathfinder.csv", header=T)

Create log-log 2D plots with height.m~dbh.cm, and avecanopy.m~dbh.cm, and height.m~avecanopy.m

par(mfrow=c(1,3))

#	Plot height vs. DBH
plot(log(height.m)~log(dbh.cm), pipa.5cm, pch=21, col="#005826", xlab="log [DBH (cm)]", ylab="log [Tree height (m)]", cex=1.5)
points(log(height.m)~log(dbh.cm), qula.5cm, pch=21, col="#f26522", cex=1.5)

#	Add trendlines for Pipa and Qula to above plot
p1.lm = lm(log(height.m)~log(dbh.cm), pipa.5cm)
abline(p1.lm, col="#005826")
q1.lm = lm(log(height.m)~log(dbh.cm), qula.5cm)
abline(q1.lm, col="#f26522")

summary(p1.lm)

Call:
lm(formula = log(height.m) ~ log(dbh.cm), data = pipa.5cm)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.47116 -0.08070  0.01219  0.09308  0.22115 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.90128    0.07467   12.07   <2e-16 ***
log(dbh.cm)  0.59410    0.02733   21.74   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.1338 on 58 degrees of freedom
Multiple R-squared: 0.8907,	Adjusted R-squared: 0.8888 
F-statistic: 472.6 on 1 and 58 DF,  p-value: < 2.2e-16

summary(q1.lm)

Call:
lm(formula = log(height.m) ~ log(dbh.cm), data = qula.5cm)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.96818 -0.09420  0.02556  0.13631  0.48571 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  0.18680    0.22346   0.836    0.407    
log(dbh.cm)  0.78776    0.09253   8.514 4.41e-11 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.2699 on 47 degrees of freedom
Multiple R-squared: 0.6066,	Adjusted R-squared: 0.5983 
F-statistic: 72.48 on 1 and 47 DF,  p-value: 4.409e-11





#	Plot average canopy diameter vs. DBH
plot(log(avecanopy.m)~log(dbh.cm), pipa.5cm, pch=21, col="#005826", xlab="log [DBH (cm)]", ylab="log [Canopy diameter (m)]", cex=1.5)
points(log(avecanopy.m)~log(dbh.cm), qula.5cm, pch=21, col="#f26522", cex=1.5)

#	Add trendlines for Pipa and Qula to above plot
p2.lm = lm(log(avecanopy.m)~log(dbh.cm), pipa.5cm)
abline(p2.lm, col="#005826")
q2.lm = lm(log(avecanopy.m)~log(dbh.cm), qula.5cm)
abline(q2.lm, col="#f26522")

summary(p2.lm)

Call:
lm(formula = log(avecanopy.m) ~ log(dbh.cm), data = pipa.5cm)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.65079 -0.15097  0.00354  0.14342  0.53184 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -1.45085    0.13467  -10.77 1.83e-15 ***
log(dbh.cm)  0.98579    0.04929   20.00  < 2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.2414 on 58 degrees of freedom
Multiple R-squared: 0.8734,	Adjusted R-squared: 0.8712 
F-statistic:   400 on 1 and 58 DF,  p-value: < 2.2e-16 

summary(q2.lm)

Call:
lm(formula = log(avecanopy.m) ~ log(dbh.cm), data = qula.5cm)

Residuals:
    Min      1Q  Median      3Q     Max 
-1.3772 -0.1309  0.0367  0.2129  0.5332 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  -2.0324     0.3117  -6.521 4.36e-08 ***
log(dbh.cm)   1.1653     0.1291   9.029 7.76e-12 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.3765 on 47 degrees of freedom
Multiple R-squared: 0.6343,	Adjusted R-squared: 0.6265 
F-statistic: 81.53 on 1 and 47 DF,  p-value: 7.764e-12




#	Plot height vs. average canopy diameter
plot(log(height.m)~log(avecanopy.m), pipa.5cm, pch=21, col="#005826", xlab="log [Canopy diameter (m)]", ylab="log [Tree height (m)]", cex=1.5)
points(log(height.m)~log(avecanopy.m), qula.5cm, pch=21, col="#f26522", cex=1.5)

#	Add trendlines for Pipa and Qula to above plot
p3.lm = lm(log(height.m)~log(avecanopy.m), pipa.5cm)
abline(p3.lm, col="#005826")
q3.lm = lm(log(height.m)~log(avecanopy.m), qula.5cm)
abline(q3.lm, col="#f26522")

summary(p3.lm)

Call:
lm(formula = log(height.m) ~ log(avecanopy.m), data = pipa.5cm)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.45233 -0.13477  0.00006  0.15102  0.39405 

Coefficients:
                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)       1.86629    0.05011   37.25   <2e-16 ***
log(avecanopy.m)  0.52517    0.03722   14.11   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.1923 on 58 degrees of freedom
Multiple R-squared: 0.7744,	Adjusted R-squared: 0.7705 
F-statistic: 199.1 on 1 and 58 DF,  p-value: < 2.2e-16 

summary(q3.lm)

Call:
lm(formula = log(height.m) ~ log(avecanopy.m), data = qula.5cm)

Residuals:
     Min       1Q   Median       3Q      Max 
-1.07066 -0.14955 -0.02901  0.15743  0.65303 

Coefficients:
                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)       1.70883    0.07013  24.368  < 2e-16 ***
log(avecanopy.m)  0.47571    0.07316   6.503 4.65e-08 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.3122 on 47 degrees of freedom
Multiple R-squared: 0.4736,	Adjusted R-squared: 0.4624 
F-statistic: 42.28 on 1 and 47 DF,  p-value: 4.653e-08


==========================================================================================
#	Create 4-panel plot of veg structure parameters versus species, using only Alive individuals with DBH ≥ 5 cm

#	Select only "Alive" individuals
vs = read.csv("D3_VS_OS_pathfinder.csv", header=T)
vs1 = vs[vs$status=='Alive',]

#	Select values of DBH ≠ NA
vs1 = vs1[is.na(vs1$dbh.cm)=='FALSE',]

#	Select values of DBH ≥ 5 cm
vs1 = vs1[vs1$dbh.cm >=5,]

#	Remove unused levels from "species" categorical variable
vs1$species = factor(vs1$species, levels=c("Pipa","Qula"), labels=c("Pipa","Qula"))

#	Create 4-panel plot
par(mfrow=c(2,2))
plot(dbh.cm~species, data=vs1, xlab="Species code", ylab="DBH (cm)")
plot(height.m~species, data=vs1, xlab="Species code", ylab="Height (m)")
plot(canopyrad.m~species, data=vs1, xlab="Species code", ylab="Canopy radius (m)")
plot(firstbranch.m~species, data=vs1, xlab="Species code", ylab="Height to first branch (m)")





