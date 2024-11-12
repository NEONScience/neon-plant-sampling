
library(tidyverse)
library(dplyr, quietly=T)
library(lubridate)
library(ggforce)

##pull all available data for a site from the NEON API

library(neonUtilities)
sitesOfInterest <- c("TREE", "UNDE")
## 
dpid <- as.character('DP1.10055.001') #phe data
 
pheDat <- loadByProduct(dpID="DP1.10055.001",
                     site = sitesOfInterest,
                     #startdate="2024-01",
                     #enddate="2023-12",
                     package = "basic",
                     release = "LATEST",
                     include.provisional = TRUE,
                     check.size = FALSE, 
                     token = Sys.getenv('latestTok'))
                     token=Sys.getenv("NEON_KEY"))

# unlist all data frames
list2env(pheDat ,.GlobalEnv)

## unlist dataframes individually
# perind <- pheDat$phe_perindividual
# stint <- pheDat$phe_statusintensity 
# annual <- pheDat$phe_perindividualperyear

#remove duplicate records
phe_statusintensity <- select(phe_statusintensity, -uid)
phe_statusintensity <- distinct(phe_statusintensity)

#Format dates (native format is 'factor' silly R)
phe_statusintensity$date <- as.Date(phe_statusintensity$date, "%Y-%m-%d")
phe_statusintensity$editedDate <- as.Date(phe_statusintensity$editedDate, "%Y-%m-%d")
phe_statusintensity$year <- substr(phe_statusintensity$date, 1, 4)
phe_statusintensity$monthDay <- format(phe_statusintensity$date, format="%m-%d")


si_last <- phe_statusintensity %>%
  group_by(individualID, date, phenophaseName) %>%
  filter(editedDate==max(as.Date(editedDate)))

### remove first year of data from each site ### Typically partial season
## optional ##
# si_last <- si_last %>%
#   group_by(siteID) %>%
#   filter(year!=min(year))

### add perindividual info (taxonID etc...)
phe_perindividual <- select(phe_perindividual, individualID, growthForm, scientificName, taxonID, nativeStatusCode, editedDate)
phe_perindividual <- distinct(phe_perindividual)

ind_last <- phe_perindividual %>%
  group_by(individualID) 

ind_last <- select(ind_last, -editedDate)

si_last <- left_join(si_last, ind_last)

early <- si_last[si_last$dayOfYear < 166,]

head(si_last$siteID)

si_last$d_site <- paste(si_last$domainID, si_last$siteID, sep="-")

###### calculte bout intervals

siteDates <- phe_statusintensity %>%
  select(domainID, siteID, date, dayOfYear)%>%
  distinct()%>%
  arrange(siteID, date)

siteDates$index <- 1:nrow(siteDates)
siteDates$year <- substr(siteDates$date, 1,4)
siteDates$interval <- NA

out <- data.frame()

for(i in unique(siteDates$siteID)){
  print(i)
  sub <- siteDates[siteDates$siteID==i,]
  #for(y in unique(sub$year)){
  #print(y)
  temp <- sub#[sub$year==y,]
  for(j in 2:nrow(temp)){
    temp$interval[j] <- temp$date[j]-temp$date[j-1]
  }
  out <- bind_rows(out, temp)
  print(nrow(out))
}
#} 


############### Plot it

### use this to subset to any set of variables you may want to plot
df <- filter(si_last, 
             #siteID=='HARV'
             year=='2023'
             & phenophaseStatus=='yes' #, #)
             #& taxonID%in%c('MEPO5', 'ACKO')
             #growthForm=='Forb',
              #phenophaseName%in%c('Breaking leaf buds', 'Initial growth', 'Breaking needle buds',
              #                     'Emerging needles') #, 'Colored leaves')#  ,
                           #        , 'Colored needles')
             #phenophaseName=="Open flowers"
             #& dayOfYear < 130
             #& dayOfYear > 80#& !is.na(growthForm)
             #& phenophaseIntensity !=""
)
df <- distinct(df)

df$monthDay <- format(df$date, format="%b-%d")

# reorganize levels to optimize plot - by placing 'leaves' first in order, this send this to the back of the plot. 
# otherwise, may layer in front of and obscure other phenophases.

df$phenophaseName <- factor(df$phenophaseName, levels = c("Leaves", "Colored leaves", "Falling leaves",   "Young leaves", "Young needles",  "Initial growth", "Increasing leaf size","Breaking leaf buds", "Breaking needle buds", "Open flowers", "Open pollen cones"))

df$phenophaseIntensity <- factor(df$phenophaseIntensity, levels = c("NA", ">= 95%", "< 5%", "5-24%", "25-49%", "50-74%", "75-94%", NA))

### density plot  - layered curves
ggplot(df, aes(x=dayOfYear, y = stat(count), color = taxonID, fill=phenophaseName)) +
  #ggplot(df, aes(x=dayOfYear, ..count.., fill=phenophaseIntensity, color=phenophaseIntensity)) +  
  geom_density(position="stack")+  # stacks data vertically
  geom_density(alpha=0.5)+  # sensitivity of the curves
  ggtitle("" )+
  geom_density()+  
  facet_wrap(~year, scale="free_y") #+ # places taxonID in separate windows
  #xlim(min(df$date)-15, max(df$date)+15) # x-axis scales by date

## histogram
p1 <-  ggplot(df, aes(x=dayOfYear, fill=phenophaseName)) +
  ggtitle("")+
   guides(fill = guide_legend(size = 1))+
  geom_histogram(binwidth = 2)+ 
  #geom_vline(xintercept = min(df$dayOfYear))+
  #labs(title=str_wrap("Phenophase status for Spring (Breaking Leaf Buds & Initial growth) phenophases by day of year between March 21 (DOY = 80) and May 10 (DOY = 130) at DELA)"), 60)+
  #     capti on="2021-01-14: Summarized  by day of year for all available data at each site. Black line indicates DOY = 151, June 1.")+
   # facet_wrap(~d_site, scale="free_y") # scale="free_y"
  facet_wrap(~taxonID)+ # scale="free_y",  scale="free_y"
  scale_x_continuous(breaks=seq(0,360, by=60))
#ylim(0,500)
#xlim(0, 365)
print(p1)


ggplot()+
  geom_histogram(data=df, aes(x=dayOfYear, fill=taxonID, position="stack"), binwidth = 2 )+
  geom_point(data=out, aes(x=dayOfYear, y=interval*5, color = "sampling interval"))+
  scale_color_manual(NULL, values = "black")+
  scale_y_continuous(name='count of individuals',
                     sec.axis =sec_axis(~./5, name="sampling interval (days)", breaks = c(0:14)),
                     minor_breaks = seq(0, 70, 5))+ 
  labs(title = paste0("Spring and fall transitions with sampling frequency for ", unique(df$siteID), ", ", sort(unique(df$year))))

  

ggsave(filename = paste('H:/Phenology/Scheduling/activePheTransitionPhases_bySite.pdf', sep='_'))
his