###	Sample number analysis of LAI-2200 data collected for 8 transects in Ordway-Swisher in August/Sept. 2010

###	Approach 1: Use sample number function in conjunction with mean and SD of LAI calculated from data collected every 10 m along 8 transects. Bear in mind that results from spatial analysis indicates there is spatial autocorrelation not accounted for when one uses all the values spaced 10 m apart.

#	Assumption of "Sample_number_function.R" is that variable for which sample number estimation is required is normally distributed.

	==>	Use sqrt(LAI) values, which are normally distributed, based on above-mentioned analyses.
	
	==>	Use LAI values as they are, which are not normally distributed (based on analyses performed in "D3_LAI_spatial_analysis.R"

par(mfrow=c(2,1))

#	Sample number analysis with sqrt(LAI) values; acceptable error set at 0.2 (20%)
mu.1 = mean(sqrt(os$lai), na.rm=T)
mu.1 = 0.8245893
std.1 = sd(sqrt(os$lai), na.rm=T)
std.1 = 0.2568533
samplenum(std.1, mu.1, 0.2)
[1] "The number of samples required to estimate this variable to within 10% of the mean and with 90% (2-tailed) confidence is 26.351745535888"
text(0.075, 140, adj=0, cex=0.8, labels="Square-root transformed LAI data")


#	Sample number analysis with untransformed LAI values; acceptable error set at 0.2
mu.2 = mean(os$lai, na.rm=T)
mu.2 = 0.745748
std.2 = sd(os$lai, na.rm=T)
std.2 = 0.4230619
samplenum(std.2, mu.2, 0.2)
[1] "The number of samples required to estimate this variable to within 10% of the mean and with 90% (2-tailed) confidence is 87.405337746279"
text(0.075, 460, adj=0, cex=0.8, labels="Un-transformed LAI data")


#	Summary comments:
-	Shape of distribution for input data (normal, non-normal) has large effect on the number of samples required.





### Approach 2: Use sample number function in conjunction with mean LAI and SD of LAI from all 8 of the transects measured at Ordway

###	This approach gives the number of 500 m transects required to estimate LAI to within 10% of the mean with 90% confidence.

# Values are mean LAI from each transect measured at Ordway-Swisher

lai.mean = c(0.78,0.71,0.75,0.86,0.94,0.63,0.67,0.57)

#	Check to see whether values are normally distributed and meet assumptions of "samplenum" function
ad.test(lai.mean)

	Anderson-Darling normality test

data:  lai.mean 
A = 0.1297, p-value = 0.9674

	==>	Satisfies assumptions.

# Calculate mean, SD, and coefficient of variation for all 8 values
mu.3 = mean(lai.mean)
mu.3 = 0.73875
std.3 = sd(lai.mean)
std.3 = 0.1212362
cv.lai = sd(lai)/mean(lai)
cv.lai = 0.1641099
samplenum(std.3, mu.3, 0.2)
[1] "The number of samples required to estimate this variable to within 10% of the mean and with 90% (2-tailed) confidence is 7.31448917682733"

	==>	7.3 transects at 500 m length required.






### Approach 3: Resampling LAI from 8 transects to visually inspect how values of LAI can be affected by using different numbers of transects.





## Plot a histrogram of LAI values from all 8 transects, showing mean and +/- 1 sd
hist(lai, main="LAI values from all transects", xlab="Leaf area index")
abline(v=mean(lai), col=4, lwd=2)
abline(v=mean(lai)+sd(lai), col=2, lty=2, lwd=2)
abline(v=mean(lai)-sd(lai), col=2, lty=2, lwd=2)



## Generate 10 random samples of 4 values from the 8 values in the "lai" vector
x = replicate(10, sample(lai, size=4, replace=F))
x
     [,1] [,2] [,3] [,4] [,5] [,6] [,7] [,8] [,9] [,10]
[1,] 0.57 0.94 0.86 0.78 0.86 0.86 0.75 0.86 0.57  0.71
[2,] 0.67 0.75 0.75 0.94 0.63 0.94 0.94 0.71 0.86  0.57
[3,] 0.78 0.67 0.67 0.67 0.94 0.75 0.78 0.67 0.63  0.67
[4,] 0.86 0.78 0.94 0.57 0.75 0.63 0.86 0.63 0.78  0.78


## Calculate the mean for each of the 10 draws of 4 LAI values
temp = 0
for(j in 1:ncol(x)){
	temp[j] = mean(x[,j])
	}

## Calculate the average mean value, based on the 10 random draws of 4 LAI values from the original 8

mean(temp) = 0.75825

## Calculate the mean coefficient of variation based on the 10 random draws of 4 LAI values of 8 total

temp.cv = 0
for(j in 1:ncol(x)){
	temp.cv[j] = sd(x[,j])/mean(x[,j])
	}

mean(temp.cv) = 0.1580524



## Calculate mean and CV for transects located in tower airshed

os.tower = mean(lai[1:4])
os.tower = 0.775

os.tower.cv = sd(lai[1:4])/mean(lai[1:4])
os.tower.cv = 0.08194649




## Perform a t.test to determine whether first for OS transects are significantly different than all 8 transects

t.test(lai[1:4], lai)

	Welch Two Sample t-test

data:  lai[1:4] and lai 
t = 0.6795, df = 9.861, p-value = 0.5124
alternative hypothesis: true difference in means is not equal to 0 
95 percent confidence interval:
 -0.0828354  0.1553354 
sample estimates:
mean of x mean of y 
  0.77500   0.73875

	==>	Based on these results, the 4 tower airshed transects are not significantly different in terms of LAI compared to all 8 transects.







## Using the "prob" package, can use function "urnsamples" to determine the number of distinct combinations possible when sampling 4,5,6, and 7 transects w/o replacement from the 8 total transects.

nrow(urnsamples(1:8, size=4, replace=F, ordered=F))
[1] 70

# The "replace=F" argument means once a transect is selected for a given draw, it cannot be selected again.

# The "ordered=F" argument means that the order of the 4 transects drawn does not contribute to making a draw distinct - i.e. 1,2,3,4 is the same as 2,1,3,4 when ordered=F.



## Use the LAI data above to create a matrix with "urnsamples" that has all distinct permutations as above.

lai.4 = urnsamples(lai, size=4, replace=F, ordered=F)

## Calculate the mean for each row of "lai.4" and store in a new "temp" vector
## Following function requires that "lai.4" be a matrix

lai.4 = as.matrix(lai.4)

rowmean = function(urn){
temp = 0
for(i in 1:nrow(urn)){
	temp[i] = mean(urn[i,])
	}
	return(temp)
}

# Unclear why above function gives "NULL" as output when "print" line omitted.

## Plot values for distinct permutations of 4 transects & overlay mean +/- SD from all 8
meanlai.4 = rowmean(lai.4)

hist(meanlai.4, main="LAI values from distinct permutations of 4 transects", xlab="Leaf area index", xlim=c(0.5,1))

# Overlay lines associated with mean +/- 1 SD from all 8 transects 
abline(v=mean(lai), col=2, lwd=2)
abline(v=mean(lai)+sd(lai), col=4, lty=2, lwd=2)
abline(v=mean(lai)-sd(lai), col=4, lty=2, lwd=2)



## Repeat with sampling 5 of 8 total transects

lai.5 = urnsamples(lai, size=5, replace=F, ordered=F)
lai.5 = as.matrix(lai.5)
meanlai.5 = rowmean(lai.5)
hist(meanlai.5, main="LAI values from distinct permutations of 5 transects", xlab="Leaf area index", xlim=c(0.5,1))

# Overlay lines associated with mean +/- 1 SD from all 8 transects 
abline(v=mean(lai), col=4, lwd=2)
abline(v=mean(lai)+sd(lai), col=2, lty=2, lwd=2)
abline(v=mean(lai)-sd(lai), col=2, lty=2, lwd=2)



## Repeat with sampling for 6 of 8 total transects

lai.6 = urnsamples(lai, size=6, replace=F, ordered=F)
lai.6 = as.matrix(lai.6)
meanlai.6 = rowmean(lai.6)
hist(meanlai.6, main="LAI values from distinct permutations of 6 transects", xlab="Leaf area index", xlim=c(0.5,1))

# Overlay lines associated with mean +/- 1 SD from all 8 transects 
abline(v=mean(lai), col=4, lwd=2)
abline(v=mean(lai)+sd(lai), col=2, lty=2, lwd=2)
abline(v=mean(lai)-sd(lai), col=2, lty=2, lwd=2)



## Repeat with sampling for 7 of 8 total transects

lai.7 = urnsamples(lai, size=7, replace=F, ordered=F)
lai.7 = as.matrix(lai.7)
meanlai.7 = rowmean(lai.7)
hist(meanlai.7, main="LAI values from distinct permutations of 7 transects", xlab="Leaf area index", xlim=c(0.5,1))

# Overlay lines associated with mean +/- 1 SD from all 8 transects 
abline(v=mean(lai), col=4, lwd=2)
abline(v=mean(lai)+sd(lai), col=2, lty=2, lwd=2)
abline(v=mean(lai)-sd(lai), col=2, lty=2, lwd=2)

