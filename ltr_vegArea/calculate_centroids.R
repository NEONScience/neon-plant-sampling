library(neonUtilities)
library(geoNEON)
library(tidyverse)
library(restR2)

dat <- loadByProduct(dpID="DP1.10033.001",
                     tabl="ltr_pertrap",
                     #site = "HEAL",
                     check.size = FALSE,
                     token = Sys.getenv('NEON_KEY'))

# unlist all data frames
list2env(dat ,.GlobalEnv)

tSites <- unique(ltr_pertrap$siteID[ltr_pertrap$trapPlacement=="Targeted"])

ltrPlots <- ltr_pertrap%>%
  filter(siteID%in%tSites)%>% #only targeted sites
  mutate(def.extr.geo.os(data=., locCol = "namedLocation", 
                         locOnly = F))%>% #add spatial data
  select(siteID, namedLocation, subplotID, targetTaxaPresent, easting,
         northing, utmZone)%>% #fields of interest
  distinct()%>% # remove dupes
  arrange(namedLocation) #organize


table(ltrPlots$siteID)
table(ltrPlots$subplotID, useNA='always')

sp <- get.lov(lovName = "vst.subplot")
sort(unique(ltrPlots$subplotID))
sort(unique(sp$lovElementCode))

table(ltrPlots$siteID, ltrPlots$subplotID, useNA = 'always')

# NIWO and KONZ have non-standard or NA subplotIDs

ltrPlots$subplotID[ltrPlots$siteID%in%c("NIWO", "KONZ")] <- 31
table(ltrPlots$subplotID, useNA = 'always')

#calculate centroid for subplots
for(i in 1:nrow(ltrPlots)){
  if (ltrPlots$subplotID[i] == "31"){
    ltrPlots$E_add[i] <- 0
    ltrPlots$N_add[i] <- 0
    }else if ((ltrPlots$subplotID[i] == "21")){
      ltrPlots$E_add[i] <- -10
      ltrPlots$N_add[i] <- -10      
    }else if ((ltrPlots$subplotID[i] == "23")){ 
      ltrPlots$E_add[i] <- 10
      ltrPlots$N_add[i] <- -10      
    }else if ((ltrPlots$subplotID[i] == "39")){
      ltrPlots$E_add[i] <- -10
      ltrPlots$N_add[i] <- 10      
    }else if ((ltrPlots$subplotID[i] == "41")){
      ltrPlots$E_add[i] <- 10
      ltrPlots$N_add[i] <- 10      
    }
}

options(digits=13) #necessary to support converting UTMs from character to numeric

ltrPlots$easting_calc <- as.numeric(ltrPlots$easting) + ltrPlots$E_add
ltrPlots$northing_calc <- as.numeric(ltrPlots$northing) + ltrPlots$N_add

ltrPlots$plotArea <- 400 #plot or subplot, always 20x20m
ltrPlots$CHM_area <- NA
ltrPlots$CHM_percent <- NA


write.csv(ltrPlots, paste0(getwd(), "/ltr_vegArea/allLitterPlots.csv"), row.names = F)

write.csv(ltrPlots, paste0(getwd(), "/ltr_vegArea/allTargetPlots.csv"), row.names = F)

test <- ltrPlots[ltrPlots$siteID%in%c("HEAL", "YELL"),]
#look <- ltrPlots[ltrPlots$siteID=="YELL",]

write.csv(test, paste0(getwd(), "/ltr_vegArea/testDat.csv"), row.names = F)

# Check subplots are in correct direction
sub_pt <- "39"

plot(x = as.numeric(test$easting[test$subplotID == sub_pt][1]),
       y = as.numeric(test$northing[test$subplotID == sub_pt][1]),
     xlim=c(as.numeric(test$easting[test$subplotID == sub_pt][1])-15,
            as.numeric(test$easting[test$subplotID == sub_pt][1])+15),
     ylim=c(as.numeric(test$northing[test$subplotID == sub_pt][1])-15,
            as.numeric(test$northing[test$subplotID == sub_pt][1])+15),
       pch = 16,
       col = "black")

points(x = test$easting_calc[test$subplotID == sub_pt][1],
       y = test$northing_calc[test$subplotID == sub_pt][1],
       pch = 16,
       col = "orange")

