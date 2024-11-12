###	Semivariogram analysis of LAI-2200 data collected every 10 m along 8 transects in Ordway-Swisher in August/Sept. 2010.

###	Aspect, elevation, and slope for each LAI measurement point come from a 10 m resolution DEM managed by M. Slater.

###	As determined in "D3_LAI_spatial_analysis_v1.R", use sqrt(LAI) rather than untransformed LAI, due to non-normality of untransformed LAI values.

###	Use data file with values of LAI=NA removed manually. This makes resid(model) and model$residuals equivalent when performing semivariogram analysis below.

os = read.csv("D3_LAI_OS_pathfinder_NArm.csv", header=T)

	#	transectID = unique ID assigned to each 500 m transect
	#	team = NEON team that collected the data. FSU collected data mid-Aug, AOP 				collected data late-Aug/early-Sept.
	#	position.m = the position along the transect center-line (in meters) at which individual LAI data points were acquired.
	#	pos.bin = the 50 m segment of transect to which a LAI datum belongs.
	#	easting = Easting coordinates associated with individual LAI measurements, in UTM Zone 17N WGS 1984.
	#	northing = Northing coordinates associated with individual LAI measurements, in UTM Zone 17N WGS 1984.
	#	lai = Leaf area index values, calculated using LI-COR FV2200 software that employed a masking and transmittance clipping procedure.
	#	elevation = elevation (m) from a 10 m resolution DEM.
	#	slope = slope (%) from a 10 m resolution DEM.
	#	aspect = aspect (deg) from a 10 m resolution DEM.


###	Use trigonometric transformation to create "northness" and "eastness" variables from "aspect" variable, in order to account for circular nature of aspect - i.e. 1˚ and 359˚ are virtually identical. These two variables will take the place of "aspect" in the multiple regression model.

radians = function(degrees) {
	rad = (degrees*pi)/180
	return(rad) 	
	}

os$northness = cos(radians(os$aspect))
os$eastness = sin(radians(os$aspect))


#	Plot DEM variables to visually determine whether they are more or less independent from each other.

par(mfrow=c(3,2))
plot(northness~elevation, os, xlab="Elevation (m)", ylab="Northness (deg)", main="DEM output for LAI transects at Ordway")
plot(eastness~elevation, os, xlab="Elevation (m)", ylab="Eastness (deg)", main="DEM output for LAI transects at Ordway")
plot(northness~slope, os, xlab="Slope (%)", ylab="Northness (deg)", main="DEM output for LAI transects at Ordway")
plot(eastness~slope, os, xlab="Slope (%)", ylab="Eastness (deg)", main="DEM output for LAI transects at Ordway")
plot(elevation~slope, os, xlab="Slope (%)", ylab="Elevation (m)", main="DEM output for LAI transects at Ordway")


#	Examine Pearson correlation coefficients between DEM variables

cor(os$northness, os$elevation)
[1] 0.07734176
cor(os$eastness, os$elevation)
[1] -0.09234932
cor(os$northness, os$slope)
[1] 0.1273855
cor(os$eastness, os$slope)
[1] -0.1488953
cor(os$elevation, os$slope)
[1] -0.3045087

	==> Some collinearity between elevation and slope, which is perhaps expected.


#	Create model with all interaction terms, and test whether assumptions are satisfied.
m1 = lm(sqrt(lai)~northness + eastness + elevation + slope + northness:elevation + eastness:elevation + northness:slope + eastness:slope + elevation:slope, data=os)

plot(fitted(m1), resid(m1))
qqnorm(resid(m1))
qqline(resid(m1))
library(nortest)
ad.test(resid(m1))

	Anderson-Darling normality test

data:  resid(m1) 
A = 0.2955, p-value = 0.5939

	==>	Satisfies assumptions.
	
#	Use stepAIC to remove terms with little predictive power from model. The "upper" and "lower" terms in the "scope" argument specify which terms should be considered for elimination, and which must be kept. Function stepAIC respects the heirarchy principle when determining which terms to keep in the model.

library(MASS)
m2 = stepAIC(m1, scope=list(upper=~., lower=~1), trace=F)

plot(fitted(m2), resid(m2))
qqnorm(resid(m2))
qqline(resid(m2))
ad.test(resid(m2))

	Anderson-Darling normality test

data:  resid(m2) 
A = 0.3114, p-value = 0.5508

	==>	Satisfies assumptions.


summary(m2)

Call:
lm(formula = sqrt(lai) ~ northness + elevation + slope + northness:elevation + 
    elevation:slope, data = os)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.67533 -0.16741  0.01276  0.16261  0.70530 

Coefficients:
                     Estimate Std. Error t value Pr(>|t|)    
(Intercept)          1.681509   0.286224   5.875 9.35e-09 ***
northness            0.602195   0.334926   1.798  0.07298 .  
elevation           -0.019358   0.006481  -2.987  0.00300 ** 
slope               -0.228105   0.084674  -2.694  0.00738 ** 
northness:elevation -0.012602   0.007477  -1.686  0.09272 .  
elevation:slope      0.005252   0.001998   2.629  0.00892 ** 
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.2544 on 375 degrees of freedom
Multiple R-squared: 0.03168,	Adjusted R-squared: 0.01877 
F-statistic: 2.454 on 5 and 375 DF,  p-value: 0.03316





#	Compare model "m2" to a new model in which "aspect" is treated as a factor with 8 levels. Each level consists of those points in the dataset that fall into the following degree bins: 0, 45, 90, 135, 180, 225, 270, 315

max(os$aspect)
[1] 359.59
min(os$aspect)
[1] 0.91

	==>	above bins will work

#	Create new aspect variable, to be factored later.
os$aspectcat = NA

#	Make first level within categorical variable "aspectcat" for 0˚± 22.5˚, second level for 45˚± 22.5˚, and so on. The "||" symbol below is a boolean "or" operator. Apparently "|" is also an "or" operator, but "||" is preferred in "if" statements.

for (i in 1:nrow(os)){
	if (os$aspect[i] <= 22.5 || os$aspect[i] > 337.5) os$aspectcat[i] = 1
	else if (os$aspect[i] > 22.5 && os$aspect[i] <= 67.5) os$aspectcat[i] = 2
	else if (os$aspect[i] > 67.5 && os$aspect[i] <= 112.5) os$aspectcat[i] = 3
	else if (os$aspect[i] > 112.5 && os$aspect[i] <= 157.5) os$aspectcat[i] = 4
	else if (os$aspect[i] > 157.5 && os$aspect[i] <= 202.5) os$aspectcat[i] = 5
	else if (os$aspect[i] > 202.5 && os$aspect[i] <= 247.5) os$aspectcat[i] = 6
	else if (os$aspect[i] > 247.5 && os$aspect[i] <= 292.5) os$aspectcat[i] = 7
	else os$aspectcat[i] = 8
	}
	
#	Define os$aspectcat as a categorical variable
os$aspectcat = factor(os$aspectcat, levels=1:8)

#	Create linear model using "aspectcat" variable and others
m3 = lm(sqrt(lai)~aspectcat*elevation*slope, data=os)

#	Use stepAIC to remove unnecessary terms
m4 = stepAIC(m3, scope=list(upper=~., lower=~1), trace=F)

plot(fitted(m4, resid(m4)))
qqnorm(resid(m4))
qqline(resid(m4))
ad.test(resid(m4))

	Anderson-Darling normality test

data:  resid(m4) 
A = 0.2915, p-value = 0.6062

	==>	Satisfies assumptions.

summary(m4)

Call:
lm(formula = sqrt(lai) ~ elevation + slope + elevation:slope, 
    data = os)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.67436 -0.16397  0.00736  0.16728  0.71547 

Coefficients:
                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)      1.467603   0.271426   5.407 1.14e-07 ***
elevation       -0.014660   0.006167  -2.377   0.0179 *  
slope           -0.173684   0.080729  -2.151   0.0321 *  
elevation:slope  0.004052   0.001915   2.116   0.0350 *  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.2558 on 377 degrees of freedom
Multiple R-squared: 0.01568,	Adjusted R-squared: 0.007847 
F-statistic: 2.002 on 3 and 377 DF,  p-value: 0.1133


#	Compare AIC for models "m2" and "m4"
AIC(m2,m4)
   df      AIC
m2  7 46.21445
m4  5 48.45987

	==>	Model "m2" is better.


### Create plots summarizing output of deterministic analysis

#	Plot data to show interacting effects of "northness" and "elevation" as indicated by summary(m2)
plot(os$northness, os$elevation, pch="", xlab="Northness: sin(aspect)", ylab="Elevation (m)", main="Interacting effects of aspect, elevation on LAI")

for (i in 1:nrow(os)){
	points(os$northness[i], os$elevation[i], cex=os$lai[i], pch=21, bg=0)
	}


#	Plot data to show interacting effects of "elevation" and "slope" as indicated by summary(m2)
plot(os$slope, os$elevation, pch="", xlab="Slope (%)", ylab="Elevation (m)", main="Interacting effects of elevation, slope on LAI")

for (i in 1:nrow(os)){
	points(os$slope[i], os$elevation[i], cex=os$lai[i], pch=21, bg=0)
	}

#	Create partial residual plots for "northness", "elevation", and "slope" based on model "m2"
adj.y = resid(m2) + coef(m2)['northness']*os$northness
plot(adj.y~os$northness, xlab="northness: sin(aspect) [deg]", ylab="Partial residuals sqrt(LAI)", main="Residual LAI as a function of northness")
abline(0, coef(m2)['northness'], col=4)

adj.y = resid(m2) + coef(m2)['elevation']*os$elevation
plot(adj.y~os$elevation, xlab="Elevation (m)", ylab="Partial residuals sqrt(LAI)", main="Residual LAI as a function of elevation")
abline(0, coef(m2)['elevation'], col=4)

adj.y = resid(m2) + coef(m2)['slope']*os$slope
plot(adj.y~os$slope, xlab="Slope (%)", ylab="Partial residuals sqrt(LAI)", main="Residual LAI as a function of slope")
abline(0, coef(m2)['slope'], col=4)



==========================================================================================
Spatial semivariogram analysis on residuals, after removing effects of aspect (eastness), elevation, and slope.

#	Create geodata for use in semivariogram analyses. Pull "easting" and "northing" values from "os" dataframe, and use residuals from model "m2"
library(geoR)
tmp.geo = as.geodata(data.frame(cbind(os[,c(5,6)],m2$residuals)))

#	Create diagnostic plots for data
plot(tmp.geo)

	Plot 1: Easting vs. Northing, with points colored according to LAI quartiles
	Does not appear to be too much obvious spatial pattern to the LAI values, though there is some clustering within transects that is obvious.
	
	Plot 2:	Northing (y-axis) versus data (x-axis).
	Data appear to be more or less randomly distributed.
	
	Plot 3:	Easting (x-axis) versus data (y-axis).
	Data are more or less random, though OS08 and OS09 transects toward the eastern edge of the sampling area are obvious due to the fact that these transects were oriented only slightly off of True North.
	
	Plot 4:	Hist, density, and rug plots of the data.
	Data appear to be normally distributed after sqrt() transformation.


#	Create directional variograms with tolerance = pi/4, to reduce impact of OS08 and OS09 orientation.

plot(variog4(tmp.geo, tolerance=pi/4),xlab="Distance (m)",ylab="Semivariance")

	==>	Seems that there is no real issue with direction of the variograms.


#	Determine maximum acceptable lag distance. This distance should be adjusted so that the value is no greater than the shortest distance between 2 points on an ellipse that contains the actual data. 
#	Plot of the data using plot(tmp.geo) above indicates that this distance corresponds roughly to range of the Northing coordinates.

max(os$northing)-min(os$northing)
[1] 597.421

	==>	To be safe, use 500 m as the value for the "max.dist" argument in function "variog"


###	Following "D3_LAI_spatial_analysis_v1.R", perform variogram analysis, using max.dist = 500, with 20 m breaks, and check to see if output has ~ 20 points on graph

variog.1 = variog(tmp.geo, max.dist=500, option="bin", breaks=seq(0,500,20))
plot(variog.1, pch=21, xlab="Distance (m)", ylab="Semivariance", ylim=c(0.04,0.08))

#	Visual examination of graph
	==>	tausq (nugget):				0.05
		sigmasq (partial sill):		0.025
		phi:						160

	==>	Number of points on graph = 25



###	Examine variofits with a number of different initial "phi" values to determine whether fits are stable. "phi" is distance at which fit asymptotes to sill. Vary phi between 100 m and 360 m, by 20 m intervals.


##	Summary comments for variofit analyses:
#	For fit.1, fit.8, and fit.9 the solutions for phi are more or less identical, and the minimised weighted sum of squares is lowest for these three fits. All other fits have solution for phi = input for phi, and model fits show higher minimised weighted sum of squares. Based on these results, I am assuming that the solution for phi given by fit.1, fit.8, and fit.9 are optimal.


#	Variofit, with phi=100 m
fit.1 = variofit(variog.1, cov.model="spherical", ini.cov.pars=c(0.025,100), nugget=0.05, fix.nugget=F, weights="cressie")

fit.1
variofit: model parameters estimated by WLS (weighted least squares):
covariance model is: spherical
parameter estimates:
   tausq  sigmasq      phi 
  0.0436   0.0270 151.7738 
Practical Range with cor=0.05 for asymptotic range: 151.7738

variofit: minimised weighted sum of squares = 143.7912

lines(fit.1, col=1)

	==>	fit.1 has solution for phi ≠ input for phi. Fitting process appears to be functioning normally.


#	Variofit with phi=120 m
fit.2 = variofit(variog.1, cov.model="spherical", ini.cov.pars=c(0.025,120), nugget=0.05, fix.nugget=F, weights="cressie")

fit.2
variofit: model parameters estimated by WLS (weighted least squares):
covariance model is: spherical
parameter estimates:
   tausq  sigmasq      phi 
  0.0417   0.0287 120.1167 
Practical Range with cor=0.05 for asymptotic range: 120.1167

variofit: minimised weighted sum of squares = 155.2983

lines(fit.2, col=2)

	==>	fit.2 has solution for phi = input for phi.

#	Variofit with phi=140 m
fit.3 = variofit(variog.1, cov.model="spherical", ini.cov.pars=c(0.025,140), nugget=0.05, fix.nugget=F, weights="cressie")

fit.3
variofit: model parameters estimated by WLS (weighted least squares):
covariance model is: spherical
parameter estimates:
   tausq  sigmasq      phi 
  0.0429   0.0277 140.0849 
Practical Range with cor=0.05 for asymptotic range: 140.0849

variofit: minimised weighted sum of squares = 145.3113

lines(fit.3, col=3)

	==>	fit.3 has solution for phi = input for phi. 


#	Variofit with phi=160 m
fit.4 = variofit(variog.1, cov.model="spherical", ini.cov.pars=c(0.025,160), nugget=0.05, fix.nugget=F, weights="cressie")

fit.4
variofit: model parameters estimated by WLS (weighted least squares):
covariance model is: spherical
parameter estimates:
   tausq  sigmasq      phi 
  0.0442   0.0265 160.0488 
Practical Range with cor=0.05 for asymptotic range: 160.0488

variofit: minimised weighted sum of squares = 144.6915

lines(fit.4, col=4)

	==>	fit.4 has solution for phi = input for phi.

#	Variofit with phi=180 m
fit.5 = variofit(variog.1, cov.model="spherical", ini.cov.pars=c(0.025,180), nugget=0.05, fix.nugget=F, weights="cressie")

fit.5
variofit: model parameters estimated by WLS (weighted least squares):
covariance model is: spherical
parameter estimates:
   tausq  sigmasq      phi 
  0.0457   0.0252 180.0260 
Practical Range with cor=0.05 for asymptotic range: 180.026

variofit: minimised weighted sum of squares = 152.1174

lines(fit.5, col=5)

	==>	fit.5 has solution for phi = input for phi.


#	Variofit with phi=200 m
fit.6 = variofit(variog.1, cov.model="spherical", ini.cov.pars=c(0.025,200), nugget=0.05, fix.nugget=F, weights="cressie")

fit.6
variofit: model parameters estimated by WLS (weighted least squares):
covariance model is: spherical
parameter estimates:
   tausq  sigmasq      phi 
  0.0474   0.0237 200.0127 
Practical Range with cor=0.05 for asymptotic range: 200.0127

variofit: minimised weighted sum of squares = 163.418

lines(fit.6, col=6)

	==>	fit.6 has solution for phi = input for phi.


#	Variofit with phi=220 m
fit.7 = variofit(variog.1, cov.model="spherical", ini.cov.pars=c(0.025,220), nugget=0.05, fix.nugget=F, weights="cressie")

fit.7
variofit: model parameters estimated by WLS (weighted least squares):
covariance model is: spherical
parameter estimates:
   tausq  sigmasq      phi 
  0.0490   0.0223 220.0056 
Practical Range with cor=0.05 for asymptotic range: 220.0056

variofit: minimised weighted sum of squares = 175.6519

lines(fit.7, col=7)

	==>	fit.7 has solution for phi = input for phi.


#	Variofit with phi=240 m
fit.8 = variofit(variog.1, cov.model="spherical", ini.cov.pars=c(0.025,240), nugget=0.05, fix.nugget=F, weights="cressie")

fit.8
variofit: model parameters estimated by WLS (weighted least squares):
covariance model is: spherical
parameter estimates:
   tausq  sigmasq      phi 
  0.0437   0.0269 151.2330 
Practical Range with cor=0.05 for asymptotic range: 151.233

variofit: minimised weighted sum of squares = 143.798

lines(fit.8, col=8)

	==>	Solution for phi ≠ input for phi. fit.8 identical in output to fit.1


#	Variofit with phi=260 m
fit.9 = variofit(variog.1, cov.model="spherical", ini.cov.pars=c(0.025,260), nugget=0.05, fix.nugget=F, weights="cressie")

fit.9
variofit: model parameters estimated by WLS (weighted least squares):
covariance model is: spherical
parameter estimates:
   tausq  sigmasq      phi 
  0.0436   0.0270 151.7843 
Practical Range with cor=0.05 for asymptotic range: 151.7843

variofit: minimised weighted sum of squares = 143.7905

lines(fit.9, col=9)

	==>	Solution for phi ≠ input for phi. fit.9 identical in output to fit.1, fit.8


#	Variofit with phi=280 m
fit.10 = variofit(variog.1, cov.model="spherical", ini.cov.pars=c(0.025,280), nugget=0.05, fix.nugget=F, weights="cressie")

fit.10
variofit: model parameters estimated by WLS (weighted least squares):
covariance model is: spherical
parameter estimates:
   tausq  sigmasq      phi 
  0.0534   0.0183 280.0000 
Practical Range with cor=0.05 for asymptotic range: 280

variofit: minimised weighted sum of squares = 201.9132

lines(fit.10, col=2)

	==>	Solution for phi = input for phi.
	

#	Variofit with phi=300 m
fit.11 = variofit(variog.1, cov.model="spherical", ini.cov.pars=c(0.025,300), nugget=0.05, fix.nugget=F, weights="cressie")

fit.11
variofit: model parameters estimated by WLS (weighted least squares):
covariance model is: spherical
parameter estimates:
   tausq  sigmasq      phi 
  0.0545   0.0173 300.0008 
Practical Range with cor=0.05 for asymptotic range: 300.0008

variofit: minimised weighted sum of squares = 206.9048

lines(fit.11, col=3)

	==>	Solution for phi = input for phi.


#	Variofit with phi=320 m
fit.12 = variofit(variog.1, cov.model="spherical", ini.cov.pars=c(0.025,320), nugget=0.05, fix.nugget=F, weights="cressie")

fit.12
variofit: model parameters estimated by WLS (weighted least squares):
covariance model is: spherical
parameter estimates:
   tausq  sigmasq      phi 
  0.0555   0.0165 320.0041 
Practical Range with cor=0.05 for asymptotic range: 320.0041

variofit: minimised weighted sum of squares = 211.7776

lines(fit.12, col=4)

	==>	Solution for phi = input for phi.
	

#	Variofit with phi=340 m
fit.13 = variofit(variog.1, cov.model="spherical", ini.cov.pars=c(0.025,340), nugget=0.05, fix.nugget=F, weights="cressie")

fit.13
variofit: model parameters estimated by WLS (weighted least squares):
covariance model is: spherical
parameter estimates:
   tausq  sigmasq      phi 
  0.0564   0.0157 340.0096 
Practical Range with cor=0.05 for asymptotic range: 340.0096

variofit: minimised weighted sum of squares = 216.6086

lines(fit.13, col=5)

	==>	Solution for phi = input for phi.
	

#	Variofit with phi=360 m
fit.14 = variofit(variog.1, cov.model="spherical", ini.cov.pars=c(0.025,360), nugget=0.05, fix.nugget=F, weights="cressie")

fit.14
variofit: model parameters estimated by WLS (weighted least squares):
covariance model is: spherical
parameter estimates:
   tausq  sigmasq      phi 
  0.0573   0.0150 360.0173 
Practical Range with cor=0.05 for asymptotic range: 360.0173

variofit: minimised weighted sum of squares = 221.4422

lines(fit.14, col=6)

	==>	Solution for phi = input for phi.
	



Notes from PD on 2011-09-30:
#	Could compare spherical model versus exponential model - exponential does not reach a	 flat asymptote.

#	Could compare different weighting methods as well - compare Cressie to npairs; don't want to use "equal".


