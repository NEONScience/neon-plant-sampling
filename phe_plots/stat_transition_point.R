library(tidyverse)
library(neonUtilities)


# load function
#source(paste0(getwd(), "/phe_functions/calculate_transition_dates.R")) #load locally from relative path
devtools::source_url("https://raw.githubusercontent.com/NEONScience/neon-plant-sampling/main/phe_functions/calculate_transition_dates.R")

# download data from neon data portal
dat <- loadByProduct(dpID="DP1.10055.001",
                     site = "PUUM",
                     startdate = "2022-01",
                     enddate = "2024-05",
                     check.size = FALSE, 
                     include.provisional = T,
                     token = Sys.getenv('NEON_KEY')) 

# unlist all data frames
list2env(dat ,.GlobalEnv)

# de-dupe and refine perindividual df to fields of interest
loc <- phe_perindividual%>%
  group_by(individualID)%>%
  filter(editedDate==max(editedDate))%>%
  select(individualID, taxonID, growthForm, transectMeter)%>%
  distinct()


df <- phe_statusintensity

# calculate transitions and attach loc variables
out <- df%>% #
  calculate_transition_dates()%>%
  left_join(loc, by="individualID")


# create dfs to plot, subset or filter as needed

# df with transition dates and uncertainties
a <- out[out$date>"2023-10-01 GMT" & 
         out$transitionType%in%c("no-yes") &
         out$phenophaseName%in%c('Initial growth', 'Young leaves'),] # spring phenophases in example 


# set factor order
a$phenophaseName <- factor(a$phenophaseName, c("Breaking leaf buds", "Initial growth",
 "Young leaves", "Leaves", "Colored leaves", "Falling leaves", "Open flowers", "Fruits", "Ripe fruits"))

# subseted df with phenophase status values from original download, joined with location
b <- df%>%
  filter(date>"2022-01-01 GMT" & phenophaseName%in%c("Initial growth", "Young Leaves"))%>%
  left_join(loc)
          
#set factor order
b$phenophaseStatus <- factor(b$phenophaseStatus, c("no", "yes", "missed", "uncertain"))
b$phenophaseName <- factor(b$phenophaseName, c("Breaking leaf buds", "Initial growth",
                                               "Young leaves", "Leaves", "Colored leaves", "Falling leaves", "Open flowers", "Fruits", "Ripe fruits"))

##### Plot it #####

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
       title = paste0(unlist(unique(b$phenophaseName)), " phenology at ", unlist(unique(b$siteID))), 
       subtitle = paste0(min(b$date), " through ", max(b$date)))+
  facet_grid(siteID~phenophaseName)+
  scale_color_brewer(palette = "Paired")

g


##### Summarize it #####

c <- out%>%
  filter(year%in%c(2022, 2023,2024) &  
           transitionType=="no-yes")%>% #phenophaseName%in%c("Initial growth", "Young leaves", "Leaves")
  group_by(year,  phenophaseName, transitionType, nth_transition)%>% #taxonID,
  summarise(count=n(), meanUncertainty=mean(uncertainty), meanDate=mean(transition_date), meanInterval=mean(samplingInterval))
