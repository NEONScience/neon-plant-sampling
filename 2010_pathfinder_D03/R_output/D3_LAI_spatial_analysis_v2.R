###	Semivariogram analysis of LAI-2200 data collected every 10 m along 8 transects in Ordway-Swisher in August/Sept. 2010.

###	Aspect, elevation, and slope for each LAI measurement point come from a 10 m resolution DEM managed by M. Slater.

###	As determined in "D3_LAI_spatial_analysis_v1.R", use sqrt(LAI) rather than untransformed LAI, due to non-normality of untransformed LAI values.

###	First remove deterministic effects on sqrt(LAI) associated with aspect, elevation, and slope using a multiple regression model with all interactions possible. This analysis differs from that in "D3_LAI_spatial_analysis_v1.R" since elevation is not treated here as a 2˚ polynomial, and all interactions are specified here and were not in the other analysis. The "stepAIC" function is then used to determine which variables should remain in the model.

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


#	Plot DEM variables to visually determine whether they are more or less independent from each other.

par(mfrow=c(2,2))
plot(aspect~elevation, os, xlab="Elevation (m)", ylab="Aspect (deg)", main="DEM output for LAI transects at Ordway")
plot(aspect~slope, os, xlab="Slope (%)", ylab="Aspect (deg)", main="DEM output for LAI transects at Ordway")
plot(elevation~slope, os, xlab="Slope (%)", ylab="Elevation (m)", main="DEM output for LAI transects at Ordway")

#	Examine Pearson correlation coefficients between DEM variables

cor(os$aspect, os$elevation)
[1] -0.05373783
cor(os$aspect, os$slope)
[1] 0.09415197
cor(os$elevation, os$slope)
[1] -0.3045087

	==>	Some covariance between Elevation and slope.



#	Create model with all interaction terms, and test whether assumptions satisfied
m1 = lm(sqrt(lai)~aspect*elevation*slope, data=os)
plot(fitted(m1), resid(m1))
qqnorm(resid(m1))
qqline(resid(m1))
library(nortest)
ad.test(resid(m1))

	Anderson-Darling normality test

data:  resid(m1) 
A = 0.3, p-value = 0.581

	==>	Satisfies assumptions

summary(m1)

Call:
lm(formula = sqrt(lai) ~ aspect * elevation * slope, data = os)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.68953 -0.16018  0.00698  0.18205  0.69682 

Coefficients:
                         Estimate Std. Error t value Pr(>|t|)   
(Intercept)             1.616e+00  6.147e-01   2.628  0.00893 **
aspect                 -1.200e-03  3.921e-03  -0.306  0.75970   
elevation              -1.690e-02  1.381e-02  -1.225  0.22153   
slope                  -1.775e-01  1.752e-01  -1.013  0.31170   
aspect:elevation        1.930e-05  8.763e-05   0.220  0.82580   
aspect:slope            6.722e-05  9.316e-04   0.072  0.94252   
elevation:slope         3.945e-03  4.090e-03   0.965  0.33540   
aspect:elevation:slope -2.512e-07  2.184e-05  -0.012  0.99083   
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.2562 on 373 degrees of freedom
Multiple R-squared: 0.02312,	Adjusted R-squared: 0.004784 
F-statistic: 1.261 on 7 and 373 DF,  p-value: 0.2687

#	Use stepAIC to remove terms with little predictive power from model. The "upper" and "lower" terms in the "scope" argument specify which terms should be considered for elimination, and which must be kept. Function stepAIC respects the heirarchy principle when determining which terms to keep in the model.

m2 = stepAIC(m1, scope=list(upper=~aspect*elevation*slope, lower=~1), trace=F)

plot(fitted(m2), resid(m2))
qqnorm(resid(m2))
qqline(resid(m2))
ad.test(resid(m2))

	Anderson-Darling normality test

data:  resid(m2) 
A = 0.2823, p-value = 0.6352

	==>	Satisfies assumptions


summary(m2)

Call:
lm(formula = sqrt(lai) ~ aspect + elevation + slope + elevation:slope, 
    data = os)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.69561 -0.16829  0.00395  0.17727  0.69613 

Coefficients:
                  Estimate Std. Error t value Pr(>|t|)    
(Intercept)      1.4656616  0.2710278   5.408 1.14e-07 ***
aspect          -0.0002032  0.0001397  -1.455   0.1466    
elevation       -0.0139917  0.0061748  -2.266   0.0240 *  
slope           -0.1580402  0.0813240  -1.943   0.0527 .  
elevation:slope  0.0036969  0.0019273   1.918   0.0558 .  
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.2555 on 376 degrees of freedom
Multiple R-squared: 0.02119,	Adjusted R-squared: 0.01078 
F-statistic: 2.035 on 4 and 376 DF,  p-value: 0.08889

	==>	Very little variation in the data is explained by DEM variables, but keep model "m2" for generating residuals for use with spatial semivariogram analysis.
	
	==>	PD suggests categorizing "aspect" data into 4 categories (N,E,S,W - with each centered on 0, 90, 180, 270 ± 45˚ to get categories). This is because there are issues with aspect being a circular variable.
	
	==>	DB transforms raw aspect numbers and computes:
		northness = cos(aspect)
		eastness = sin(aspect)
	to get deviation from N, and deviation from E for each location.


#	Create partial residual plot for aspect
adj.y = resid(m2) + coef(m2)['aspect']*os$aspect
plot(adj.y~os$aspect, xlab="Aspect (deg)", ylab="Partial residuals sqrt(LAI)", main="Residual LAI as a function of aspect")
abline(0, coef(m2)['aspect'], col=4)

#	Create partial residual plot for elevation
adj.y = resid(m2) + coef(m2)['elevation']*os$elevation
plot(adj.y~os$elevation, xlab="Elevation (m)", ylab="Partial residuals sqrt(LAI)", main="Residual LAI as a function of elevation")
abline(0, coef(m2)['elevation'], col=4)

#	Create partial residual plot for slope
adj.y = resid(m2) + coef(m2)['slope']*os$slope
plot(adj.y~os$slope, xlab="Slope (%)", ylab="Partial residuals sqrt(LAI)", main="Residual LAI as a function of slope")
abline(0, coef(m2)['slope'], col=4)


==========================================================================================
Spatial semivariogram analysis on residuals, after removing effects of aspect, elevation, and slope.

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
plot(variog.1, pch=21, xlab="Distance (m)", ylab="Semivariance")

#	Visual examination of graph
	==>	tausq (nugget):				0.05
		sigmasq (partial sill):		0.025
		phi:						160

	==>	Number of points on graph = 25


###	Examine variofits with a number of different initial "phi" values to determine whether fits are stable. "phi" is distance at which fit asymptotes to sill.

plot(variog.1, pch=21, xlab="Distance (m)", ylab="Semivariance", ylim=c(0.04,0.08))

fit.1 = variofit(variog.1, cov.model="spherical", ini.cov.pars=c(0.025,160), nugget=0.05, fix.nugget=F, weights="cressie")

fit.1
variofit: model parameters estimated by WLS (weighted least squares):
covariance model is: spherical
parameter estimates:
   tausq  sigmasq      phi 
  0.0437   0.0281 160.0369 
Practical Range with cor=0.05 for asymptotic range: 160.0369

variofit: minimised weighted sum of squares = 156.0607

lines(fit.1, col=1)

	==>	Optimized phi very similar to value supplied in ini.cov.pars argument that was visually estimated from the plot above.


fit.2 = variofit(variog.1, cov.model="spherical", ini.cov.pars=c(0.025,200), nugget=0.05, fix.nugget=F, weights="cressie")

fit.2
variofit: model parameters estimated by WLS (weighted least squares):
covariance model is: spherical
parameter estimates:
   tausq  sigmasq      phi 
  0.0470   0.0253 200.0001 
Practical Range with cor=0.05 for asymptotic range: 200.0001

variofit: minimised weighted sum of squares = 176.9997

lines(fit.2, col=2)

	==>	Optimized phi very similar to value supplied in ini.cov.pars argument.


fit.3 = variofit(variog.1, cov.model="spherical", ini.cov.pars=c(0.025,250), nugget=0.05, fix.nugget=F, weights="cressie")

fit.3
variofit: model parameters estimated by WLS (weighted least squares):
covariance model is: spherical
parameter estimates:
   tausq  sigmasq      phi 
  0.0514   0.0213 249.9980 
Practical Range with cor=0.05 for asymptotic range: 249.998

variofit: minimised weighted sum of squares = 210.4349

lines(fit.3, col=3)

	==>	Optimized phi very similar to value supplied in ini.cov.pars argument.


fit.4 = variofit(variog.1, cov.model="spherical", ini.cov.pars=c(0.025,300), nugget=0.05, fix.nugget=F, weights="cressie")

fit.4
variofit: model parameters estimated by WLS (weighted least squares):
covariance model is: spherical
parameter estimates:
   tausq  sigmasq      phi 
  0.0432   0.0286 152.3391 
Practical Range with cor=0.05 for asymptotic range: 152.3391

variofit: minimised weighted sum of squares = 155.2032

lines(fit.4, col=4)

	==>	Results of fit.1 and fit.4 are quite similar; semivariance appears to stabilize at about 300 m, which may account for optim function working properly?


fit.5 = variofit(variog.1, cov.model="spherical", ini.cov.pars=c(0.025,100), nugget=0.05, fix.nugget=F, weights="cressie")

fit.5
variofit: model parameters estimated by WLS (weighted least squares):
covariance model is: spherical
parameter estimates:
   tausq  sigmasq      phi 
  0.0432   0.0286 152.2753 
Practical Range with cor=0.05 for asymptotic range: 152.2753

variofit: minimised weighted sum of squares = 155.2031

lines(fit.5, col=5)

	==>	fit.1, fit.4, and fit.5 are all quite similar.


fit.6 = variofit(variog.1, cov.model="spherical", ini.cov.pars=c(0.025,75), nugget=0.05, fix.nugget=F, weights="cressie")

fit.6
variofit: model parameters estimated by WLS (weighted least squares):
covariance model is: spherical
parameter estimates:
  tausq sigmasq     phi 
 0.0381  0.0331 75.1985 
Practical Range with cor=0.05 for asymptotic range: 75.19852

variofit: minimised weighted sum of squares = 209.3659

lines(fit.6, col=6)

	==>	Problem resurfaces if phi becomes too small. Optimized phi very similar to value supplied in ini.cov.pars argument.
	

#	Based on values from fit.4 and fit.5, appears that phi = 152 meters.

##	Create semivariogram plot of "optimal" model, with point-size weighted according to number of pairs in each bin.

plot(variog.1, pch="", xlab="Distance (m)", ylab="Semivariance", main="Semivariogram plot for Ordway LAI", ylim=c(0.04,0.09))

weights.size = sqrt((variog.1$n/sum(variog.1$n)))*15

for(j in 1:length(variog.1$n)){
points(variog.1$uvec[j],variog.1$v[j],cex=weights.size[j],pch=21,bg=0)
}

#	Add minor tick marks to x-axis
#	Need to load library(Hmisc)
require(Hmisc)
minor.tick(nx=10, ny=0, tick.ratio=0.5)

#	Add non-linear regression line from "optimal" fit.5 output
lines(fit.5, col=4)

#	Add vertical line indicating lag distance (phi) at which variofit regression line asymptotes to sill.
# Specifies "phi" from variofit output
v.x = c(fit.5$cov.pars[2], fit.5$cov.pars[2])	
# First value comes from visual inspection of graph so that vertical line intersects the y-axis; second value derived from variofit output.
v.y = c(0.035, I(fit.5$cov.pars[1]+fit.5$nugget))
lines(v.x, v.y, col="gray60", lty=2)

#	Add text to plot indicating value of "phi"
text(170, 0.045, labels="phi = 152.3 meters", adj=0, cex=0.8)