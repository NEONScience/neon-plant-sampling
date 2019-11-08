# Functions to calculate Dlim and RDmax for CDW sampling

##  Dlim calculation
### transectNum = number of transects to be established at each sampling point
### RD = round diameter(s) of log(s) for which Dlim is required (cm)
### F = F-value(s) for which Dlim is required; typically the value in use at a given site

dlim <- function(RD, transectNum, fValue){
  d = round(((pi^2)*(RD^2))/(8*transectNum*fValue), digits=2)
  return(d)
}

##  RDmin calculation
### transectNum = number of transects to be established at each sampling point
### Dlim = limiting distance for which RDmin is required (meters)
### F = F-value(s) for which RDmin is required; typically the value in use at a given site

rdmin <- function(Dlim, transectNum, fValue){
  rd = round(sqrt((8*transectNum*fValue*Dlim)/pi^2), digits=1)
  return(rd)
}
