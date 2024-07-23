Assessment of effects of transect ID and time/team on LAI across four transects in a Sandhill ecosystem dominated by P. palustris and Q. laevis in the Ordway-Swisher Biological Station near Gainesville, FL. Transects were located in the airshed of the proposed tower site.

LAI was recorded every 10 m along each of four 500 m transects, once on 2010-08-14 (by FSU team) and on 2010-08-31 and 2010-09-02 (by AOP team). FSU collected all data pre-dawn, while AOP collected data at dusk. Analyses below have not incorporated spatial dependence of individual datapoints, and was solely meant to test for effects of "Transect" and "Time/Team" on average LAI values across transects.

Objectives 1 and 2: Is there an effect of "Team" (and by proxy, an effect of two weeks time in the latter half of August) on LAI, and does LAI vary by "Position" and "Transect" in the tower airshed of the D3 Ordway-Swisher site?

Summary:	Statistically significant but small effect of "Transect" on LAI. Effect of "Team" not significant. Significant, but very small, linear increase in LAI with "Position", but will need fancier analyses that can account for spatial auto-correlation in the data. Most variation in the data remains unexplained.



Objective 3: Is LAI significantly different between the Donaldson and Ordway sites?

Summary:	Analyses not done yet.




Objective 4: Create density plots of LAI data for each transect at Ordway-Swisher and for each transect at Donaldson. For each density plot, pair it with a "corrected" plot that scales the LAI-2200 data according the DHP data that account for both overstory and understory. These plots will be useful in comparing to AOP pixels from the same transects to see whether AOP distributions for LAI match up better with just canopy LAI from the ground, or canopy+understory LAI from the ground.

Summary:	Based on visual comparisons, it appears that "corrected" distributions potentially match up better with AOP distributions, but K.Krause needs to isolate transect boxes from whole AOP dataset to get something that compares better.




Objective 5: Create uncorrected and "corrected" density plots of LAI data by pooling values from all Ordway-Swisher transects, in order to get a distribution that is as representative of the entire Sandhill ecosystem as possible.

Summary:	


Get data into R:

FL = read.csv("D3_LAI_pathfinder.csv", header=T)

str(FL)
'data.frame':	408 obs. of  4 variables:
 $ Transect: Factor w/ 4 levels "OS01","OS02",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ Position: int  0 10 20 30 40 50 60 70 80 90 ...
 $ Team    : Factor w/ 2 levels "AOP","FSU": 2 2 2 2 2 2 2 2 2 2 ...
 $ LAI     : num  NA NA NA NA NA NA NA NA NA NA ...


==========================================================================================
Objective 1 and 2:

Visually check for outliers, transect effects, "team" effects:

plot(LAI~Transect, subset(FL, Team=="FSU"), main="FSU data")
plot(LAI~Transect, subset(FL, Team=="AOP"), main="AOP data")

library(lattice)
xyplot(LAI~Position|Transect+Team, data=FL)




Create linear model exploring effects of "Transect" and "Team" on variation in LAI:

m1 = lm(LAI~Transect+Team, data=FL, na.action=na.omit)



Check for homogeneity of variance and normality of residuals:

par(mfrow=c(2,2))
plot(fitted(m1), resid(m1))
qqnorm(resid(m1))
qqline(resid(m1))

	==>	Homogeneity of variance is good.
	==>	Normality of residuals also looks good.
	



Summary of linear model output:

summary(m1)

Call:
lm(formula = LAI ~ Transect + Team, data = FL, na.action = na.omit)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.84598 -0.34193 -0.05681  0.27413  2.18038 

Coefficients:
             Estimate Std. Error t value Pr(>|t|)    
(Intercept)   0.72032    0.05603  12.855   <2e-16 ***
TransectOS02 -0.07268    0.07114  -1.022   0.3076    
TransectOS03  0.13026    0.07114   1.831   0.0679 .  
TransectOS04  0.14566    0.07664   1.901   0.0582 .  
TeamFSU       0.07904    0.05124   1.542   0.1239    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.4659 on 347 degrees of freedom
  (56 observations deleted due to missingness)
Multiple R-squared: 0.0436,	Adjusted R-squared: 0.03257 
F-statistic: 3.954 on 4 and 347 DF,  p-value: 0.003749

	==>	Most variation in the data is not explained by the chosen independent variables. Would likely need to do something fancier nesting "Position" within "Transect" in order to get at significant spatial effects: Not necessary here. However, the model is significant, and there appears to be some variation in LAI across transects (see ANOVA output below).
	


anova(m1)
Analysis of Variance Table

Response: LAI
           Df Sum Sq Mean Sq F value   Pr(>F)   
Transect    3  2.917 0.97217  4.4794 0.004217 **
Team        1  0.516 0.51631  2.3790 0.123889   
Residuals 347 75.310 0.21703                    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

	==>	Statistically significant but small effect of "Transect" on LAI. Effect of "Team" not significant.






Model accounting for "Position", "Transect" and "Team":

m2 = lm(LAI~Position+Transect+Team, data=FL)


Check model assumptions:

plot(fitted(m2), resid(m2))
qqnorm(resid(m2))
qqline(resid(m2))

	==> good.



anova(m2)
Analysis of Variance Table

Response: LAI
           Df Sum Sq Mean Sq F value    Pr(>F)    
Position    1  3.530  3.5302 16.9150 4.889e-05 ***
Transect    3  2.847  0.9490  4.5470   0.00385 ** 
Team        1  0.155  0.1554  0.7445   0.38882    
Residuals 346 72.210  0.2087                      
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 


summary(m2)

Call:
lm(formula = LAI ~ Position + Transect + Team, data = FL)

Residuals:
     Min       1Q   Median       3Q      Max 
-0.90166 -0.32936 -0.05214  0.28680  2.21098 

Coefficients:
               Estimate Std. Error t value Pr(>|t|)    
(Intercept)   0.5399968  0.0721707   7.482 6.09e-13 ***
Position      0.0006552  0.0001700   3.854 0.000139 ***
TransectOS02 -0.0386759  0.0703125  -0.550 0.582635    
TransectOS03  0.1642653  0.0703125   2.336 0.020050 *  
TransectOS04  0.1437079  0.0751560   1.912 0.056685 .  
TeamFSU       0.0440593  0.0510631   0.863 0.388823    
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 0.4568 on 346 degrees of freedom
  (56 observations deleted due to missingness)
Multiple R-squared: 0.08296,	Adjusted R-squared: 0.06971 
F-statistic:  6.26 on 5 and 346 DF,  p-value: 1.414e-05 

	==>	Significant effect of "Position" and "Transect" on LAI, but no effect of "Team/Time"

	==>	Effect sizes of these variables are quite small.
	
	
	
	

==========================================================================================
Objective 3:





==========================================================================================
Objective 4:

###	Create distributions of corrected and uncorrected LAI values for each transect at DT and OS. "Corrected" values are based on DHP that include upward and downward facing images.

#	Correction factor for LAI-2200 at Donaldson Tract, calculated by taking the average difference between LAI calculated from DHP (upward & downward images) and LAI from the LAI-2200
correction = 1.33

#	Get data into R
lai = read.csv("D3_LAI_DT_OS_pathfinder.csv", header=T)

str(lai)
'data.frame':	612 obs. of  9 variables:
 $ transectID: Factor w/ 12 levels "DT03","DT04",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ site      : Factor w/ 2 levels "DT","OS": 1 1 1 1 1 1 1 1 1 1 ...
 $ group     : Factor w/ 5 levels "east","lower",..: 2 2 2 2 2 2 2 2 2 2 ...
 $ position.m: int  0 10 20 30 40 50 60 70 80 90 ...
 $ pos.bin   : int  0 0 0 0 0 50 50 50 50 50 ...
 $ easting   : num  387628 387619 387610 387601 387591 ...
 $ northing  : num  3292042 3292046 3292051 3292055 3292059 ...
 $ lai       : num  NA NA NA NA NA NA NA NA NA NA ...
 $ lai.quant : num  NA NA NA NA NA NA NA NA NA NA ...

par(mfrow=c(2,2))

##	Subset data to only look at LAI values from DT03
dt03 = lai[lai$transectID=="DT03",8]

#	Plot corrected data
plot(density(dt03+1.33, na.rm=TRUE), xlim=c(0,max(dt03, na.rm=TRUE)+2), xlab="Leaf area index", main="Distribution of LAI along DT03", col=2)

#	Overlay uncorrected data onto density plot above
lines(density(dt03, na.rm=TRUE), col=4)

##	Subset data to only look at LAI values from DT04
dt04 = lai[lai$transectID=="DT04",8]

#	Plot corrected data
plot(density(dt04+1.33, na.rm=TRUE), xlim=c(0,max(dt04, na.rm=TRUE)+2), xlab="Leaf area index", main="Distribution of LAI along DT04", col=2)

#	Overlay uncorrected data onto density plot above
lines(density(dt04, na.rm=TRUE), col=4)

##	Subset data to only look at LAI values from DT05
dt05 = lai[lai$transectID=="DT05",8]

#	Plot corrected data
plot(density(dt05+1.33, na.rm=TRUE), xlim=c(0,max(dt05, na.rm=TRUE)+2), xlab="Leaf area index", main="Distribution of LAI along DT05", col=2)

#	Overlay uncorrected data onto density plot above
lines(density(dt05, na.rm=TRUE), col=4)

##	Subset data to only look at LAI values from DT06
dt06 = lai[lai$transectID=="DT06",8]

#	Plot corrected data
plot(density(dt06+1.33, na.rm=TRUE), xlim=c(0,max(dt06, na.rm=TRUE)+2), xlab="Leaf area index", main="Distribution of LAI along DT06", col=2)

#	Overlay uncorrected data onto density plot above
lines(density(dt06, na.rm=TRUE), col=4)



###	Plot distributions from Ordway-Swisher
Correction from OS = 0.85

par(mfrow=c(2,4))

##	Subset data to only look at LAI values from OS01
os01 = lai[lai$transectID=="OS01",8]

#	Plot corrected data
plot(density(os01+0.85, na.rm=TRUE), xlim=c(0,max(os01, na.rm=TRUE)+1.5), xlab="Leaf area index", main="Distribution of LAI along OS01", col=2)

#	Overlay uncorrected data onto density plot above
lines(density(os01, na.rm=TRUE), col=4)

##	Subset data to only look at LAI values from OS02
os02 = lai[lai$transectID=="OS02",8]

#	Plot corrected data
plot(density(os02+0.85, na.rm=TRUE), xlim=c(0,max(os02, na.rm=TRUE)+1.5), xlab="Leaf area index", main="Distribution of LAI along OS02", col=2)

#	Overlay uncorrected data onto density plot above
lines(density(os02, na.rm=TRUE), col=4)

##	Subset data to only look at LAI values from os03
os03 = lai[lai$transectID=="OS03",8]

#	Plot corrected data
plot(density(os03+0.85, na.rm=TRUE), xlim=c(0,max(os03, na.rm=TRUE)+1.5), xlab="Leaf area index", main="Distribution of LAI along OS03", col=2)

#	Overlay uncorrected data onto density plot above
lines(density(os03, na.rm=TRUE), col=4)

##	Subset data to only look at LAI values from OS04
OS04 = lai[lai$transectID=="OS04",8]

#	Plot corrected data
plot(density(OS04+0.85, na.rm=TRUE), xlim=c(0,max(OS04, na.rm=TRUE)+1.5), xlab="Leaf area index", main="Distribution of LAI along OS04", col=2)

#	Overlay uncorrected data onto density plot above
lines(density(OS04, na.rm=TRUE), col=4)

##	Subset data to only look at LAI values from OS08
OS08 = lai[lai$transectID=="OS08",8]

#	Plot corrected data
plot(density(OS08+0.85, na.rm=TRUE), xlim=c(0,max(OS08, na.rm=TRUE)+1.5), xlab="Leaf area index", main="Distribution of LAI along OS08", col=2)

#	Overlay uncorrected data onto density plot above
lines(density(OS08, na.rm=TRUE), col=4)

##	Subset data to only look at LAI values from OS11
OS11 = lai[lai$transectID=="OS11",8]

#	Plot corrected data
plot(density(OS11+0.85, na.rm=TRUE), xlim=c(0,max(OS11, na.rm=TRUE)+1.5), xlab="Leaf area index", main="Distribution of LAI along OS11", col=2)

#	Overlay uncorrected data onto density plot above
lines(density(OS11, na.rm=TRUE), col=4)

##	Subset data to only look at LAI values from OS12
OS12 = lai[lai$transectID=="OS12",8]

#	Plot corrected data
plot(density(OS12+0.85, na.rm=TRUE), xlim=c(0,max(OS12, na.rm=TRUE)+1.5), xlab="Leaf area index", main="Distribution of LAI along OS12", col=2)

#	Overlay uncorrected data onto density plot above
lines(density(OS12, na.rm=TRUE), col=4)



==========================================================================================
Objective 5: Create uncorrected and "corrected" density plots of LAI data by pooling values from all Ordway-Swisher transects, in order to get a distribution that is as representative of the entire Sandhill ecosystem as possible.


#	Read data into R
temp.df = read.csv("D3_LAI_OS_pathfinder.csv", header=T)

#	Create 3-panel plot of uncorrected, corrected, and overlay distributions
par(mfrow=c(3,1))

#	Create Panel 1 = uncorrected distribution
hist(temp.df$lai, xlab="Leaf Area Index", main="Distribution of LAI using LAI-2200 across all\nOrdway-Swisher transects", col=8, xlim=c(0,max(temp.df$lai, na.rm=TRUE)+0.5), freq=FALSE)

#	Create Panel 2 = DHP corrected distribution
hist(temp.df$lai+0.85, xlab="Leaf Area Index", main="DHP corrected distribution of LAI using across\n all Ordway-Swisher transects", col=8, xlim=c(0,max(temp.df$lai, na.rm=TRUE)+1.5), freq=FALSE)

#	Create Panel 3 = overlay of 1 & 2
hist(temp.df$lai, xlab="Leaf Area Index", main="Uncorrected (gray) and DHP corrected LAI distributions (blue)\nfrom all Ordway-Swisher transects", col=8, xlim=c(0,max(temp.df$lai, na.rm=TRUE)+1.5), freq=FALSE)

lines(density(temp.df$lai+0.85, na.rm=TRUE), col=4)



