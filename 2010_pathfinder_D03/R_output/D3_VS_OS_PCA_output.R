###	PCA with scale=T and scale=F in order to determine whether Pipa and Qula individuals at Ordway-Swisher can be separated on the basis of vegetation structure parameters that can be obtained via airborne LIDAR.

temp.df = read.csv("D3_VS_OS_PCA_input.csv", header=T)
str(temp.df)
'data.frame':	115 obs. of  5 variables:
 $ speciesID    : Factor w/ 2 levels "Pipa","Qula": 1 1 1 1 1 1 1 1 1 1 ...
 $ maxcanopy.m  : num  0.5 2 2 1.2 3.1 2.7 1 1.4 2 3 ...
 $ mincanopy.m  : num  0.5 0.6 0.7 0.8 0.8 0.8 0.9 1 1 1 ...
 $ firstbranch.m: num  3 2.7 4.2 2.5 4.3 3.8 3 1.7 4.7 4.4 ...
 $ height.m     : num  3.8 7 8.8 5.3 9 10.9 4 5.2 11.5 11.6 ...

#	Remove "speciesID" column for PCA
pc.df = temp.df[,2:5]


==========================================================================================

###	PCA using VS data from all alive "Pipa" and "Qula" individuals for which data were available for maximum canopy diameter, minimum canopy diameter, first branch height, and total height.
###	Center=T
###	Scale=T

pca.st = prcomp(pc.df, scale=T, center=T)
summary(pca.st)
Importance of components:
                          PC1    PC2     PC3     PC4
Standard deviation     1.7874 0.6978 0.41962 0.37711
Proportion of Variance 0.7987 0.1217 0.04402 0.03555
Cumulative Proportion  0.7987 0.9204 0.96445 1.00000


#	Write scores to .csv file
write.csv(pca.st$x, file="PCA_scores_sT.csv")

#	Write loadings to .csv file
write.csv(pca.st$rotation, file="PCA_loadings_sT.csv")

#	Create new .csv datafile created in Excel in order to compile speciesID and PC scores
temp.df = read.csv("D3_VS_OS_PCA_scores.csv", header=T)

str(temp.df)
'data.frame':	115 obs. of  5 variables:
 $ speciesID: Factor w/ 2 levels "Pipa","Qula": 1 1 1 1 1 1 1 1 1 1 ...
 $ PC1      : num  -2.086 -1.511 -1.027 -1.818 -0.776 ...
 $ PC2      : num  0.417 0.167 0.651 0.14 0.508 ...
 $ PC3      : num  -0.411 0.143 0.18 -0.195 0.267 ...
 $ PC4      : num  0.1922 0.1099 0.0406 0.0716 0.264 ...
 

###	Create empty plot to contain PC scores for Pipa and Qula individuals
plot(temp.df$PC1, temp.df$PC2, xlim=c(min(temp.df$PC1)*1.05, max(temp.df$PC1)*1.05), ylim=c(min(temp.df$PC2)*1.05, max(temp.df$PC2)*1.05), xlab="PC1 (79.9%)", ylab="PC2 (12.2%)", main="PCA of Pine (green) and Oak (orange) based on VS data", type="n")

#	Add horizontal and vertical grid-lines to the plot
abline(v=0, col=8)
abline(h=0, col=8)

#	Add green symbols for Pipa individuals
pipa = temp.df[temp.df$speciesID==c("Pipa"),]
points(pipa$PC1, pipa$PC2, pch=21, col="#005826")

#	Add orange symbols for Qula individuals
qula = temp.df[temp.df$speciesID==c("Qula"),]
points(qula$PC1, qula$PC2, pch=21, col="#f26522")


###	Create empty plot for PC loadings from PC1 and PC2 - use same plot dimensions as for scores plot above
#	Create vector with variances for each PC axis
pca.var = pca.st$sdev^2
#	Compute proportional variance for each PC axis (as in summary PCA output)
pca.pvar = pca.var/sum(pca.var)

#	Create axis labels, "sep" argument removes unwanted spaces from default "paste" output
x.label = paste("PC1 (",(round(pca.pvar[1], digits=3)*100),"%)", sep="")
y.label = paste("PC2 (",(round(pca.pvar[2], digits=3)*100),"%)", sep="")

plot(pca.st$x, xlim=c(min(pca.st$x[,1])*1.05, max(pca.st$x[,1])*1.05), ylim=c(min(pca.st$x[,2])*1.05, max(pca.st$x[,2])*1.05), xlab=x.label, ylab=y.label, type="n")

#	Add axis lines
abline(h=0, col=8)
abline(v=0, col=8)

##	Add lines for each loading vector
#	Create vector of loading names
load.names = row.names(pca.st$rotation)

#	Use "for" loop to plot loading vectors and name them
for (i in 1:nrow(pca.st$rotation)){
	#	Create coordinates to plot loading vector i
	x.pos = c(0, pca.st$rotation[i,1])
	y.pos = c(0, pca.st$rotation[i,2])
	#	Plot loading vector i
	lines(x.pos, y.pos ,col="black", lty=1)
	#	Create offsets for label of loading vector i. If/else statement required because offset will be positive or negative depending on loading being positive or negative.
	x.offset = NA
	y.offset = NA
	if (pca.st$rotation[i,1] >= 0) x.offset = 0.25 else x.offset = -0.25
	if (pca.st$rotation[i,2] >= 0) y.offset = 0.25 else y.offset = -0.25
	#	Place text with loading vector label, using offsets above to determine proper location of label
	text(pca.st$rotation[i,1]+x.offset, pca.st$rotation[i,2]+y.offset, labels=paste(load.names[i]), cex=0.8)	
	}



==========================================================================================

###	PCA using VS data from all alive "Pipa" and "Qula" individuals for which data were available for maximum canopy diameter, minimum canopy diameter, first branch height, and total height.
###	Center=T
###	Scale=F

pca.sf = prcomp(pc.df, scale=F, center=T)
summary(pca.sf)
Importance of components:
                          PC1     PC2     PC3     PC4
Standard deviation     6.0717 1.86016 1.44111 0.94182
Proportion of Variance 0.8516 0.07993 0.04797 0.02049
Cumulative Proportion  0.8516 0.93153 0.97951 1.00000

#	Write scores to .csv file
write.csv(pca.sf$x, file="PCA_scores_sF.csv")

#	Write loadings to .csv file
write.csv(pca.sf$rotation, file="PCA_loadings_sF.csv")

#	Create new .csv datafile created in Excel in order to compile speciesID and PC scores
temp.df = read.csv("D3_VS_OS_PCA_sF_scores.csv", header=T)
str(temp.df)
'data.frame':	115 obs. of  5 variables:
 $ speciesID: Factor w/ 2 levels "Pipa","Qula": 1 1 1 1 1 1 1 1 1 1 ...
 $ PC1      : num  -7.92 -4.84 -2.9 -6.53 -2.2 ...
 $ PC2      : num  -0.955 -0.398 -1.702 -0.379 -1.054 ...
 $ PC3      : num  -1.388 0.142 0.164 -0.489 -0.207 ...
 $ PC4      : num  0.199 -0.499 -0.53 0.113 -1.006 ...


#	Create empty plot to contain PC scores for Pipa and Qula individuals
plot(temp.df$PC1, temp.df$PC2, xlim=c(min(temp.df$PC1)*1.05, max(temp.df$PC1)*1.05), ylim=c(min(temp.df$PC2)*1.05, max(temp.df$PC2)*1.05), xlab="PC1 (85.2%)", ylab="PC2 (8.0%)", main="PCA of Pine (green) and Oak (orange) based on VS data, scale=F", type="n")


#	Add horizontal and vertical grid-lines to the plot
abline(v=0, col=8)
abline(h=0, col=8)


#	Add green symbols for Pipa individuals
pipa = temp.df[temp.df$speciesID==c("Pipa"),]
points(pipa$PC1, pipa$PC2, pch=21, col="#005826")

#	Add orange symbols for Qula individuals
qula = temp.df[temp.df$speciesID==c("Qula"),]
points(qula$PC1, qula$PC2, pch=21, col="#f26522")


