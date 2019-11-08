### Calculate limiting distances (Dlim) for a given log round diameter (RD), and calculate minimum round diameters (RDmin) for a given distance (D) across various LIDS volume factors (F, m3 ha-1) for a three-segment transect configuration.

# 1) Use chosen F and M (M=transect segment number) to calculate the constant kM
# 2) Calculate Dlim for various values of RD
# 3) Calculate RDmin for various values of D

# Define F and M
M = 3
F = c(5,8,10,15,20)

# Define vectors of RD and D for which Dlim and RDmin are desired at each F
RD = c(2,3,4,5,6,7,8,10,12,14,16,20,25,30,35,40,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200)
D = c(0.5,1,2,4,6,8,10,15,20,25,30,35,40,45,50,60,70,80,90,100,110,120,130,140,150,160,170,180,190,200,210,220,230,240,250,260,270,280,290,300)


##  Calculate Dlim
# Create a dataframe to hold the calculated values of Dlim
dlim.df = data.frame(matrix(data=NA, nrow=length(RD), ncol=length(F)+1))
cNamesDlim = c("RD_cm")

for (i in 1:length(F)){
  name = paste("F=",F[i],sep="")
  cNamesDlim = append(cNamesDlim, name)
}

colnames(dlim.df) = cNamesDlim
dlim.df$RD_cm = RD

# Calculate Dlim for a given log RD across each value of F
for (i in 1:length(F)){
  dlim = round(((pi^2)*(RD^2))/(8*M*F[i]), digits=2)
  dlim.df[,i+1] = dlim
}

# Write Dlim output to .csv
<<<<<<< HEAD:cdw_protocolDev/cdw_protocolAppendices.R
#write.csv(dlim.df, file="cdw_lidsDLim.csv", row.names=FALSE)
=======
write.csv(dlim.df, file="cdw_lidsDLim_v2.csv", row.names=FALSE)
>>>>>>> 19e0b05e7f2bb5e656d4a8d86d7f7bd6195c5281:cdwProtocolDev/cdw_protocolAppendices.R


##  Calculate RDmin
# Create a dataframe to hold the calculated values of RDmin
rdmin.df = data.frame(matrix(data=NA, nrow=length(D), ncol=length(F)+1))
cNamesRDmin = c("D_m")

for (i in 1:length(F)){
  name = paste("F=",F[i],sep="")
  cNamesRDmin = append(cNamesRDmin, name)
}

colnames(rdmin.df) = cNamesRDmin
rdmin.df$D_m = D

# Calculate RDmin for a given distance D across each value of F
for (i in 1:length(F)){
  rdmin = round(sqrt((8*M*F[i]*D)/pi^2), digits=1)
  rdmin.df[,i+1] = rdmin
}

# Write RDmin output to .csv
<<<<<<< HEAD:cdw_protocolDev/cdw_protocolAppendices.R
#write.csv(rdmin.df, file="cdw_lidsRDmin.csv", row.names=FALSE)
=======
write.csv(rdmin.df, file="cdw_lidsRDmin_v2.csv", row.names=FALSE)
>>>>>>> 19e0b05e7f2bb5e656d4a8d86d7f7bd6195c5281:cdwProtocolDev/cdw_protocolAppendices.R


##### Round Diameter Equivalent Calculation #####
### Require a table to relate multiple round diameters of split CDW pieces to equivalent single diameter values. 
# The LIDS protocol selects CDW based on cross-sectional area (or aggregate cross-sectional area in the case of split 
# pieces), and thus it is difficult for techs in the field to know when to select and tally a split piece since there 
# is no easy mental calculation to relate the multiple diameters measured to one diameter.

## Use library(prob) and the 'urnsamples' function to generate all possible unique combinations of 2 elements 
# drawn from the supplied vector of RD values
# Define the vector 'rd' that contains input round diameter values from individual splits on a piece of CDW

rd = c(1:10, seq(from=12, to=45, by=2))

# Generate all unique combinations from 'rd', and store as a data frame
require(prob)
rds = data.frame(urnsamples(rd, 2, replace=FALSE, ordered=FALSE))
colnames(rds) = c("RDa", "RDb")
rds$RDab = paste(rds$RDa, rds$RDb, sep=", ")

# Calculate the equivalent round diameter from the cross-sectional area of the two input diameters using the 'mutate' function from library(dplyr)
require(dplyr)
rds = mutate(rds, rdEquiv = 2*sqrt((RDa/2)^2 + (RDb/2)^2))
rds$rdEquiv = round(rds$rdEquiv, digits=1)

# Write data frame to .csv
#write.csv(rds, file="cdw_appendixF_diameterEquivalents.csv", row.names=FALSE)
