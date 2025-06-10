### CDW bulk density: Convert log locations from UTM to decimal degrees ####
library(neonUtilities)
library(sf)
library(tidyverse)



### Load Portal data: Retrieve for 



#--> D05 is UTM zone 16


### Example code from stack overflow
df <- data.frame(longitude=c(4585434,4588904,4586694),
                 latitude=c(430060.8,430960.8,432427.2),
                 street=c("74 JOSEP SERRANO", "30 LLOBERA","4 SANT ILDEFONS"),
                 number=c(45,68,34))

#Conversion of data frame to sf object
df_sf <- st_as_sf(x = df,                         
                  coords = c("longitude", "latitude"),
                  crs = "+proj=utm +zone=10")

#Projection transformation
sfc = st_transform(df_sf, crs = "+proj=longlat +datum=WGS84")

#Convert it to data frame
sfc_df <- as.data.frame(sfc)