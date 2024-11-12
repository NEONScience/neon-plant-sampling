###	Semivariogram analysis of LAI-2200 data collected every 10 m along 8 transects in Ordway-Swisher in August/Sept. 2010.

###	First examine which fixed effects influence LAI values. Fixed effects are elevation, slope, and aspect, calculated at each meausurement point along the transect using a Digital Elevation Model (DEM).

	==>	Only elevation has a significant influence on LAI values.

###	Second, create an offset of 20 m for elevation, slope, and aspect. The offset accounts for the fact that the area sampled by the LAI-2200 lens at each UTM pair was actually in front of the lens direction of travel by approximately 20 m on average. The offsetting procedure means there are NA values for elevation, slope, and aspect at the 490 and 500 m transect points.

	==>	Offset does not improve analysis in any way. Discard this additional procedure from the analysis.

###	Third, perform semivariogram analysis on residuals of LAI data after influence of any significant fixed effects has been removed.

	==>	Final analysis uses max.dist=500 m, and breaks=20 m, which results in a total of 25 points used in the variofit analysis.

==========================================================================================

os = read.csv("D3_LAI_OS_pathfinder.csv", header=T)

##	Examine distribution of LAI values across all transects

hist(os$lai)

	==>	Distribution appears Gaussian rather than normal. What to do?

##	Examine effects of elevation, slope, aspect on LAI

plot(lai~elevation, os)
plot(lai~slope, os)
plot(lai~aspect, os)

	==>	Looks like a 2˚ polynomial fit for lai~elevation.
	==>	No relationship between lai and slope.
	==>	No relationship between lai and aspect.
	
m1 = lm(lai~poly(elevation,2), os, na.action=na.exclude)
plot(fitted(m1), resid(m1))
qqnorm(resid(m1))
qqline(resid(m1))
library(nortest)
ad.test(resid(m1))

	Anderson-Darling normality test

data:  resid(m1) 
A = 3.2049, p-value = 4.821e-08

	==>	Fails assumptions. What to do?


##	Try log() transformation of lai
m2 = lm(log(lai)~poly(elevation,2), os, na.action=na.exclude)
plot(fitted(m2), resid(m2))
qqnorm(resid(m2))
qqline(resid(m2))
ad.test(resid(m2))

	Anderson-Darling normality test

data:  resid(m2) 
A = 6.6275, p-value = 2.84e-16

	==>	Worse. Different transformation?


##	Try sqrt() transformation
m3 = lm(sqrt(lai)~poly(elevation,2), os, na.action=na.exclude)
plot(fitted(m3), resid(m3))
qqnorm(resid(m3))
qqline(resid(m3))
ad.test(resid(m3))

	Anderson-Darling normality test

data:  resid(m3) 
A = 0.2407, p-value = 0.7731

	==>	Satisfies assumptions

hist(sqrt(os$lai))

	==>	Appears normally distributed.

#	Plot relationship with regression output line
plot(sqrt(lai)~elevation, os)
ele = seq(min(os$elevation), max(os$elevation), len=200)
pr.m3 = predict(m3, list(elevation=ele))
lines(ele,pr.m3, col=4)

summary(m3)

Call:
lm(formula = sqrt(lai) ~ poly(elevation, 2), data = os, na.action = na.exclude)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.60852 -0.18310  0.00196  0.17092  0.74947 

Coefficients:
                    Estimate Std. Error t value Pr(>|t|)    
(Intercept)          0.81831    0.01283  63.784  < 2e-16 ***
poly(elevation, 2)1 -0.42544    0.26235  -1.622    0.106    
poly(elevation, 2)2 -1.32836    0.26270  -5.057 6.67e-07 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.2488 on 378 degrees of freedom
  (27 observations deleted due to missingness)
Multiple R-squared: 0.06634,	Adjusted R-squared: 0.0614 
F-statistic: 13.43 on 2 and 378 DF,  p-value: 2.32e-06

	==>	Significant effect of elevation on sqrt(lai).
	==>	Highest elevation points (possibly > 47 m elevation) appear to have somewhat lower LAI than other points.
	
	
##	Test for significant effects of slope on LAI
m4 = lm(sqrt(lai)~poly(elevation,2)+slope, data=os, na.action=na.exclude)
plot(fitted(m4), resid(m4))
qqnorm(resid(m4))
qqline(resid(m4))
ad.test(resid(m4))

	Anderson-Darling normality test

data:  resid(m4) 
A = 0.2437, p-value = 0.7633

	==>	Satisfies assumptions.

summary(m4)

Call:
lm(formula = sqrt(lai) ~ poly(elevation, 2) + slope, data = os, 
    na.action = na.exclude)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.61276 -0.18564  0.00119  0.17012  0.75234 

Coefficients:
                     Estimate Std. Error t value Pr(>|t|)    
(Intercept)          0.826088   0.018217  45.348  < 2e-16 ***
poly(elevation, 2)1 -0.475844   0.275592  -1.727   0.0851 .  
poly(elevation, 2)2 -1.329404   0.262925  -5.056 6.69e-07 ***
slope               -0.003503   0.005817  -0.602   0.5474    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.2491 on 377 degrees of freedom
  (27 observations deleted due to missingness)
Multiple R-squared: 0.06724,	Adjusted R-squared: 0.05982 
F-statistic: 9.059 on 3 and 377 DF,  p-value: 8.347e-06
	
	==>	No effect of slope.	
	
	
	
	==>	Does it make sense to perform semivariogram analysis on residuals of sqrt() transformed LAI values from model "m3" above? Model "m3" selected rather than "m4" since "slope" not significant. Try this path, and consult P. Duffy afterwards.



==========================================================================================
Spatial semivariogram analysis on residuals, after removing effects of elevation.

#	Create geodata for use in semivariogram analyses. Pull "easting" and "northing" values from "os" dataframe, and use residuals from model "m3"

os = read.csv("D3_LAI_OS_pathfinder.csv", header=T)
m3 = lm(sqrt(lai)~poly(elevation,2), os, na.action=na.exclude)
library(geoR)
tmp.geo<-as.geodata(data.frame(cbind(os[,c(5,6)],resid(m3))))

	==>	as.geodata: 27 points removed due to NA in the data
	
	#	Note: When creating tmp.geo above, could not use m3$residuals argument, since length(m3$residuals) differs from length(resid(m3)). Unclear why this is, but I think it is due to the way in which the two methods of describing the residuals differ in how they handle NA values for LAI in the "os" dataframe.
	
		==> The length difference does appear to be due to the NAs, as difference is removed once NAs are manually removed from .csv file.


#	Create diagnostic plots for data
plot(tmp.geo)

	Plot 1: Easting vs. Northing, with points colored according to LAI quartiles
	Does not appear to be too much obvious spatial pattern to the LAI values, though there is some clustering within transects that is obvious.
	
	Plot 2:	Northing (y-axis) versus data (x-axis).
	Data appear to be more or less randomly distributed.
	
	Plot 3:	Easting (x-axis) versus data (y-axis).
	Data are more or less random, though OS08 and OS09 transects toward the eastern edge of the sampling area are obvious due to the fact that these transects were oriented only slightly off of True North.
	
	Plot 4:	Hist, density, and rug plots of the data.
	Data appear to be normally distributed after sqrt() transformation, as shown previously above.



#	Create directional variograms for 4 directions specified by the user. Default corresponds to 0, 45, 90, and 135 degrees, with a tolerance of ± 22.5 degrees (pi/8).

plot(variog4(tmp.geo),xlab="Distance (m)",ylab="Semivariance")	

	==>	0 deg variogram is quite different from others at large lag distance, likely due to transects OS08 and OS09 being oriented nearly North/South.

#	Create directional variograms with tolerance = pi/4, to reduce impact of OS08 and OS09 orientation.

plot(variog4(tmp.geo, tolerance=pi/4),xlab="Distance (m)",ylab="Semivariance")

	==>	Seems that there is no real issue with direction of the variograms.
	==>	Double-check this interpretation with P.Duffy


#	Determine maximum acceptable lag distance. Note that P.Duffy recommends this distance should be adjusted so that the value is no greater than the shortest distance between 2 points on an ellipse that contains the actual data.

	Plot of the data using plot(tmp.geo) above indicates that this distance corresponds roughly to range of the Northing coordinates.

max(os$northing)-min(os$northing)
[1] 692.351

	==>	Use 700 m as the value for the "max.dist" argument in function "variog"


###	Perform variogram analysis, using max.dist=700, with 10 m breaks as first cut.
variog.1 = variog(tmp.geo, max.dist=700, option="bin", breaks=seq(0,700,10))

#	Plot the output from the variogram analysis to visually estimate the nugget, sill, and range for the data.
plot(variog.1, pch=21, xlab="Distance (m)", ylab="Semivariance")

	==>	tausq:		0.05	# Semivariance at which nugget occurs.
		sigmasq:	0.025	# Partial sill - difference b/t nugget and sill.
		phi:		150		# Lag distance at which points asymptote to sill.

	==>	Want to retry with breaks = 20, 50, as number of points = 70

#	Fit a variogram model, using parameters estimated graphically above for ini.cov.pairs
fit.1 = variofit(variog.1, cov.model="spherical",ini.cov.pars=c(0.025, 150), nugget=0.05, fix.nugget=F, weights="cressie")

fit.1
variofit: model parameters estimated by WLS (weighted least squares):
covariance model is: spherical
parameter estimates:
   tausq  sigmasq      phi 
  0.0465   0.0206 150.0551 
Practical Range with cor=0.05 for asymptotic range: 152.1613

variofit: minimised weighted sum of squares = 567.4985

#	Calculate sill
0.0465+0.0206
[1] 0.0671




###	Perform variogram analysis, using max.dist=700, with 20 m breaks.
variog.2 = variog(tmp.geo, max.dist=700, option="bin", breaks=seq(0,700,20))
plot(variog.2, pch=21, xlab="Distance (m)", ylab="Semivariance")

	==>	tausq:		0.045
		sigmasq:	0.025
		phi:		150

	==>	Number of points = 30

fit.2 = variofit(variog.2, cov.model="spherical",ini.cov.pars=c(0.025, 150), nugget=0.045, fix.nugget=F, weights="cressie")

fit.2
variofit: model parameters estimated by WLS (weighted least squares):
covariance model is: spherical
parameter estimates:
   tausq  sigmasq      phi 
  0.0468   0.0203 149.9976 
Practical Range with cor=0.05 for asymptotic range: 149.9976

variofit: minimised weighted sum of squares = 513.5643

#	Calculate sill
0.0468+0.0203
[1] 0.0671




###	Perform variogram analysis, using max.dist = 700, with 50 m breaks.
variog.3 = variog(tmp.geo, max.dist=700, option="bin", breaks=seq(0,700,50))
plot(variog.3, pch=21, xlab="Distance (m)", ylab="Semivariance")

	==>	tausq:		0.05
		sigmasq:	0.02
		phi:		120
	
	==>	Number of points = 14

fit.3 = variofit(variog.3, cov.model="spherical",ini.cov.pars=c(0.02, 120), nugget=0.05, fix.nugget=F, weights="cressie")

fit.3
variofit: model parameters estimated by WLS (weighted least squares):
covariance model is: spherical
parameter estimates:
   tausq  sigmasq      phi 
  0.0452   0.0215 127.8241 
Practical Range with cor=0.05 for asymptotic range: 127.8241

variofit: minimised weighted sum of squares = 324.9838

#	Calculate sill
0.0452+0.0215
[1] 0.0667




###	Perform variogram analysis, using max.dist = 500, with 20 m breaks
variog.4 = variog(tmp.geo, max.dist=500, option="bin", breaks=seq(0,500,20))
plot(variog.4, pch=21, xlab="Distance (m)", ylab="Semivariance")

	==>	tausq:		0.05
		sigmasq:	0.02
		phi:		130

	==>	Number of points = 25; perfect.

fit.4 = variofit(variog.4, cov.model="spherical", ini.cov.pars=c(0.02,130), nugget=0.05, fix.nugget=F, weights="cressie")

fit.4
variofit: model parameters estimated by WLS (weighted least squares):
covariance model is: spherical
parameter estimates:
   tausq  sigmasq      phi 
  0.0442   0.0249 130.0349 
Practical Range with cor=0.05 for asymptotic range: 130.0349

variofit: minimised weighted sum of squares = 187.6688

lines(fit.4, col=4)

#	Calculate sill
0.0442+0.0249
[1] 0.0691


###	Make plots comparing output from all four variofit models
par(mfrow=c(2,2))

plot(variog.1, pch=21, xlab="Distance (m)", ylab="Semivariance", main="Ordway LAI: max.dist=700, breaks=10")
lines(fit.1, col=4)

plot(variog.2, pch=21, xlab="Distance (m)", ylab="Semivariance", main="Ordway LAI: max.dist=700, breaks=20")
lines(fit.2, col=4)

plot(variog.3, pch=21, xlab="Distance (m)", ylab="Semivariance", main="Ordway LAI: max.dist=700, breaks=50")
lines(fit.3, col=4)

plot(variog.4, pch=21, xlab="Distance (m)", ylab="Semivariance", main="Ordway LAI: max.dist=500, breaks=20")
lines(fit.4, col=4)




### Summary observations of different variofits analyzed above #####################
-	Nugget and partial sill are nearly identical for all 4 fits.
-	Phi ranges between 127.8 m (fit.3; max.dist=700, breaks=50) and 150.1 m (fit.1; max.dist=700, breaks=10)
-	Across the board, output from different variofit models is similar.
-	Choose fit.4 as optimal model (max.dist=500, breaks=20, total number points=25)




###	Make summary plots of deterministic factor analysis, and semivariogram analysis
par(mfrow=c(2,2))

#	Plot 1: Histogram of raw LAI data, with normal distribution overlaid
hist(os$lai, prob=T, xlab="LAI", main="Distribution of measured LAI\nat Ordway-Swisher")
mu = mean(os$lai, na.rm=T)
sdev = sd(os$lai, na.rm=T)
tmp.x = seq(-0.5, max(os$lai, na.rm=T), len=200)
tmp.y = dnorm(tmp.x, mu, sdev)
lines(tmp.x, tmp.y, col=4)


#	Plot 2: Histogram of sqrt(lai) data, with normal distribution overlaid.
hist(sqrt(os$lai), prob=T, ylim=c(0,1.6), xlab="Square-root transformed LAI", main="Distribution of sqrt(LAI)\nat Ordway-Swisher")
mu = mean(sqrt(os$lai), na.rm=T)
sdev = sd(sqrt(os$lai), na.rm=T)
tmp.x = seq(-0.5, 2, len=200)
tmp.y = dnorm(tmp.x, mu, sdev)
lines(tmp.x, tmp.y, col=4)


#	Plot 3: Plot of sqrt(lai) vs. elevation, with lines drawn from "m3" above.
plot(sqrt(lai)~elevation, os, xlab="Elevation (m)", ylab="Square-root transformed LAI", main="LAI response to elevation\nat Ordway-Swisher", xlim=c(min(os$elevation)-1,max(os$elevation)+1))
ele = seq(min(os$elevation), max(os$elevation), len=200)
pr.lai = predict(m3, list(elevation=ele))
lines(ele, pr.lai, col=4)
#	Add text to plot indicating statistical summary output
text(34.5, 1.45, labels="R2 = 0.066\nF=13.4, p<0.0001", adj=0, cex=0.8)


#	Plot 4: Semivariogram plot of "optimal" model, with point-size weighted according to number of pairs in each bin.
plot(variog.4, pch="", xlab="Distance (m)", ylab="Semivariance", main="Semivariogram plot for\nOrdway-Swisher LAI", ylim=c(0.04,0.08))

weights.size = sqrt((variog.4$n/sum(variog.4$n)))*15

for(j in 1:length(variog.4$n)){
points(variog.4$uvec[j],variog.4$v[j],cex=weights.size[j],pch=21,bg=0)
}

#	Add non-linear regression line from variofit output
lines(fit.4, col=4)

#	Add minor tick marks to x-axis
library(Hmisc)
minor.tick(nx=10, ny=0, tick.ratio=0.5)

#	Add vertical line indicating lag distance (phi) at which variofit regression line asymptotes to sill.
# Specifies "phi" from variofit output
v.x = c(fit.4$cov.pars[2], fit.4$cov.pars[2])	
# First value comes from visual inspection of graph so that vertical line intersects the y-axis; second value derived from variofit output.
v.y = c(0.035, I(fit.4$cov.pars[1]+fit.4$nugget))
lines(v.x, v.y, col="gray60", lty=2)

#	Add text to plot indicating value of "phi"
text(150, 0.045, labels="phi = 130.03 meters", adj=0, cex=0.8)