#assemble covariates for spatial prediction
library(terra)
library(tidyverse)
library(geosphere)
library(lubridate)

#----1.  Temperatures ----
myTemp<-readRDS("eu_weeklymedianT_2008_2022.RDS")
myTemp<-as.data.frame(myTemp, xy=TRUE)

myTemp<-myTemp %>%
  mutate(tile_ID=1:n()) %>% 
  pivot_longer(!c("x", "y", "tile_ID"))  %>% 
  rename(temporal_ID=name, 
         medianTweek=value) %>%  
  mutate(year=substr(temporal_ID, 3,6), 
         week=sub(".*_", "", temporal_ID) 
  )  %>% 
  group_by(tile_ID) %>% 
  mutate(medianTweek.lag2=zoo::rollmean(medianTweek,2, align="right", fill=NA), 
         medianTweek.lag3=zoo::rollmean(medianTweek,3, align="right",  fill=NA)
  ) %>% 
  filter(year>=2010)


#----2. Cumulative precipitations ----
myPrec<-readRDS("eu_weeklymedianPrec_2008_2022.RDS")
myPrec<-as.data.frame(myPrec, xy=TRUE)

myPrec<-myPrec %>% 
  mutate(tile_ID=1:n()) %>% 
  pivot_longer(!c("x", "y", "tile_ID")) %>% 
  rename(temporal_ID=name, 
         cumPrecweek=value ) %>% 
  mutate(year=substr(temporal_ID, 3,6), 
         week=sub(".*_", "", temporal_ID) 
  )  %>% 
  group_by(tile_ID) %>% 
  mutate(cumPrecweek.lag2=zoo::rollmean(cumPrecweek,2, align="right", fill=NA), 
         cumPrecweek.lag3=zoo::rollmean(cumPrecweek,3, align="right",  fill=NA)
  ) %>% 
  filter(year>=2010)

myPrec

#----2. Photoperiod----
ph <- myPrec %>% 
  dplyr::select(tile_ID, y) %>%
  rename(lat=y) %>% 
  distinct()

myDates<-data.frame(date=seq.Date(as.Date('2008-12-01'), as.Date('2022-12-31'), by="day"))
myDates$week<-week(myDates$date)
myDates$year<-year(myDates$date)

phOut<-list()
for(i in 1:nrow(ph)){
  # i=1
  message(i, "/", nrow(ph))
  tmpDat<-myDates
  phOut[[i]]<-tmpDat %>%
    mutate(photop=geosphere::daylength(lat=ph[i, 2]$lat, doy=as.Date(myDates$date))) %>% 
    group_by(year, week) %>% 
    summarise(medianPhotoweek=median(photop, na.rm=TRUE)) %>% 
    ungroup() %>% 
    mutate( medianPhotoweek.lag2=zoo::rollmean(medianPhotoweek,2, align="right", fill=NA), 
            medianPhotoweek.lag3=zoo::rollmean(medianPhotoweek,3, align="right",  fill=NA),
            # temporalID=paste0(year, "_", str_pad(week, 2, pad="0")), 
            ID=ph[i, 1]$tile_ID
            # join.id=paste0(ID, "_", temporalID)
    ) %>% 
    filter(year>=2010)
} 
phOut<-do.call(rbind.data.frame, phOut)
phOut

#----3. bind together ----
myTemp$join.ID<-paste0(myTemp$tile_ID, "_", myTemp$temporal_ID)
myPrec$join.ID<-paste0(myPrec$tile_ID, "_", myPrec$temporal_ID)
phOut$join.ID<-paste0(phOut$ID, "_w_",  phOut$year, "_", phOut$week)

head(myTemp)
head(myPrec)
head(phOut)

spatial_cov <- myTemp %>% 
  select(x, y, tile_ID, temporal_ID, join.ID, medianTweek, medianTweek.lag2, medianTweek.lag3) %>% 
  left_join(myPrec %>% 
              select(join.ID, cumPrecweek, cumPrecweek.lag2, cumPrecweek.lag3), 
            by="join.ID") %>% 
  left_join(phOut %>% 
              select(join.ID, medianPhotoweek, medianPhotoweek.lag2, medianPhotoweek.lag3), 
            by="join.ID") %>% 
  select(-"tile_ID.y" ) %>% 
  rename(tile_ID = tile_ID.x)

names(spatial_cov)

# load harmonics
spatial_cov <-readRDS("spatialPredictors_ITA_2022_2023-08-28.RDS")
bio.matrix<-readRDS("ovitrapsAllObservations_2023-08-28.RDS")
myL <- length(unique(spatial_cov$tile_ID))
myHarm<-bio.matrix %>%
  mutate(year=lubridate::year(date), 
         week=lubridate::week(date)
  ) %>% 
  filter(year==2022, ID ==14428) %>% # I chose one of them for a given year because they are all identical
  select(year, week, S1.52, C1.52, S2.52,  C2.52)

myHarm<-bio.matrix %>% select(S1.52, C1.52, S2.52,  C2.52)
myHarm<-myHarm[rep(seq_len(nrow(myHarm)), myL), ]

spatial_cov <- cbind(spatial_cov, myHarm[, -c(1)])

head( spatial_cov)
outname<-paste0("spatialPredictors_ITA_2022_", Sys.Date(), ".RDS")
saveRDS(spatial_cov, outname)

