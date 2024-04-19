library(tidyverse)
library(neonUtilities)

source(paste0(getwd(), "/phe_functions/calculate_transition_dates.R"))

dat <- loadByProduct(dpID="DP1.10055.001",
                     #tabl="phe_statusintensity", 
                     site = c("MOAB", "ONAQ"),
                     #startdate = "2022-01",
                     #enddate = "2016-10",
                     #package = "basic",
                     release = "LATEST",
                     check.size = FALSE, 
                     include.provisional = T,
                     token = Sys.getenv('latestTok')) 
#token = Sys.getenv('NEON_KEY'))

# unlist all data frames
list2env(dat ,.GlobalEnv)

loc <- phe_perindividual%>%
  group_by(individualID)%>%
  filter(editedDate==max(editedDate))%>%
  select(individualID, taxonID, growthForm, transectMeter)%>%
  distinct()

df <- phe_statusintensity

out <- df%>% #
  calculate_transition_dates()%>%
  left_join(loc, by="individualID")

# create dfs to plot, subset or filter as needed

a <- out[#out$siteID=="ONAQ" &
           out$date>"2023-10-01 GMT" & 
           out$transitionType%in%c("no-yes") &
           out$phenophaseName%in%c('Initial growth', 'Young leaves'),]#

# set factor order
a$phenophaseName <- factor(a$phenophaseName, c("Breaking leaf buds", "Initial growth",
 "Young leaves", "Leaves", "Colored leaves", "Falling leaves", "Open flowers", "Fruits", "Ripe fruits"))


b <- df%>%
  filter(date>"2023-10-01 GMT" & phenophaseName%in%c("Initial growth", "Young Leaves"))%>%
  left_join(loc)
          
#set factor order
b$phenophaseStatus <- factor(b$phenophaseStatus, c("no", "yes", "missed", "uncertain"))
b$phenophaseName <- factor(b$phenophaseName, c("Breaking leaf buds", "Initial growth",
                                               "Young leaves", "Leaves", "Colored leaves", "Falling leaves", "Open flowers", "Fruits", "Ripe fruits"))

##### Plot it #####

p <- ggplot(a, aes(x=transition_date, y=reorder(individualID, transectMeter), color=phenophaseName))+
  geom_errorbarh(aes(xmin=transition_date - uncertainty, xmax=transition_date + uncertainty), color='black')+
  geom_point(size=1)

p


g <- ggplot()+
  geom_point(data=b[!b$phenophaseStatus=='uncertain',], 
             aes(x=as.Date(date), y=reorder(individualID, transectMeter), 
                         color=phenophaseStatus))+
  geom_point(data=b[b$phenophaseStatus=='uncertain',],
             aes(x=as.Date(date), y=reorder(individualID, transectMeter),
              color = "uncertain"), shape=1,)+ #preferred color = dark gray
  geom_point(data=a[a$individualID%in%b$individualID,],
             aes(x=transition_date, y=reorder(individualID, transectMeter),
                 color = "onset date"),
             shape=8) + #preferred color = black
  geom_errorbarh(data=a[a$individualID%in%b$individualID,], 
                 aes(xmin=transition_date - uncertainty, xmax=transition_date + uncertainty,
                             y=reorder(individualID, transectMeter),
                     color='onset uncertainty window')
                 )+ #preferred color = black & overlaped on 
  labs(x = "date", y = "IndividualID (ordered by transectMeter)", 
       #title = "Initial Growth and Young Leaves phenology", 
       subtitle = paste0(min(b$date), " through ", max(b$date)))+
  facet_grid(siteID~phenophaseName)+
  scale_color_brewer(palette = "Paired")

g


##### Summarize it #####

c <- out%>%
  filter(year%in%c(2023,2024) & phenophaseName%in%c("Initial growth", "Young leaves", "Leaves") & 
           transitionType=="no-yes")%>%
  group_by(year,  phenophaseName, transitionType)%>% #taxonID,
  summarise(count=n(), meanUncertainty=mean(uncertainty), meanDate=mean(transition_date), meanInterval=mean(samplingInterval))
