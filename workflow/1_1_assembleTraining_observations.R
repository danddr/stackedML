library(tidyverse)
library(ggplot2)

# compute photoperiod
library(geosphere)

# spatial aspect
library(sf)
library(terra)
library(exactextractr)
library(leaflet)
library(htmltools)

# process temporal aspect
library(lubridate)
library(zoo)
library(forecast)


outdir<-"/your/output/directory"

#---- 1 load and preprocess observations and covariates ----
list.files("ovitraps", pattern="RDS", full.names = TRUE)
tmp<-lapply(list.files("ovitraps", pattern="RDS", full.names = TRUE), readRDS)
tmp <-  do.call(bind_rows, tmp)

### 1.1 explorative analysis ----
hist(tmp$value)
summary(tmp$value)
quantile(tmp$value, c(.50,.70, .80, .90, .99), na.rm=TRUE)
length(which(tmp$value==0))/nrow(tmp)

round(table(tmp$year)/nrow(tmp),3)
hist(tmp$year)

tmp %>% 
  filter(value >0) %>% 
  group_by( Region) %>% 
  summarise(median=median(value, na.rm=TRUE),
            iqrl=quantile(value, probs=0.25, na.rm=TRUE),
            iqru=quantile(value, probs=0.75, na.rm=TRUE)) %>%
  summary()


tmp %>% 
  group_by(year, Region) %>% 
  tally() %>% 
  ggplot(aes(Region, n, fill=Region))+
  geom_bar(stat="identity")+
  facet_wrap(~year)+
  theme_classic()+
  theme(legend.position = "bottom")


# we select observations from 2010 onwards
tmp<-tmp %>%
  filter(year>=2010)
table(tmp$year)

## 1.2 create ID raster for spatial aggregation ----
# extract coordinates
myCoord<-tmp %>% 
  dplyr::select(ID, long, lat, Country, Region) %>%
  drop_na %>% 
  distinct()  

leaflet(data = myCoord) %>%
  addTiles() %>%
  addMarkers(~long, ~lat, label = ~htmlEscape(ID)) 

myCoord <- myCoord %>% 
  st_as_sf(coords=c("long", "lat"), crs=4326)

# extract biogeographic regions from Cervellini et al. 2020
bgr_shp <-st_read("BiogeoRegions2016.shp") %>% 
  mutate(new_name=str_replace(name, pattern=" Bio-geographical Region", "")) %>% 
  st_transform(crs=4326)%>%
  filter(new_name=="Alpine" | new_name=="Mediterranean" | new_name=="Continental" | new_name=="Pannonian")

myCoord <- myCoord %>% 
  st_join(bgr_shp[,"code"], left = FALSE) %>% 
  rename(bgr="code" ) 


# load raster to create raster id
tileID<-readRDS("eu_weeklymedianT_2008_2022.RDS")
tileID<-tileID$w_2008_1
names(tileID)<-"tile_ID"
values(tileID)<-1:(ncol(tileID)*nrow(tileID))
myCoord_tileID<-terra::as.data.frame(tileID, xy=TRUE)
tileID<-terra::extract(tileID, myCoord, df=TRUE)
tileID$ID<-myCoord$ID
myCoord_tileID<-myCoord_tileID[myCoord_tileID$tile_ID %in% tileID$tile_ID,]
names(myCoord_tileID)<-c("long", "lat", "tile_ID")

tileID <- tileID %>% 
  left_join(myCoord_tileID, by="tile_ID")

myCoord<-myCoord %>% 
  left_join(tileID, by="ID")

OvitrapsCheat_tab<-cbind.data.frame(st_drop_geometry(myCoord), 
                                    ovitrapLong =st_coordinates(myCoord)[,1], 
                                    ovitrapLat = st_coordinates(myCoord)[,2]) %>% 
                    rename(tileLong=long, tileLat=lat)

outname <-paste0(outdir, "ovitrapsID_cheatsheet_", Sys.Date(), ".csv" )
write.csv(OvitrapsCheat_tab, outname)

#check for missing coordinates
mySub<-tmp[which(is.na(tmp$lat)),]
table(mySub$year)
table(mySub$ContactPerson)
unique(mySub$ID)
summary(mySub$value)

## 1.3 aggregate observed eggs value by raster cell id ----
#group by tile id 
aggr.df <- tmp %>%
  dplyr::select(ID, temporalID, value,  Country,  Region) %>% 
  dplyr::left_join(tileID, by="ID") %>% 
  as_tibble() %>%  
  dplyr::group_by(tile_ID, temporalID, lat, Country,  Region) %>% 
  dplyr::summarise(value=median(value, na.rm=TRUE)#, 
            # value.lci=quantile(value, probs=0.025, na.rm=TRUE),
            # value.uci=quantile(value, probs=0.975, na.rm=TRUE)
  ) %>% 
  dplyr::mutate(join.id=paste0(tile_ID, "_", temporalID)
         ) 
   
aggr.df


summary(aggr.df)

# visualize location of aggregated ovitraps
tiles.loc<-read_csv("ovitrapsID_cheatsheet_2023-08-28.csv")
length(unique(tiles.loc$ID))
length(unique(tiles.loc$tile_ID))

tiles.loc %>% 
  group_by(bgr) %>% 
  summarise(length(unique(tile_ID)))

tiles.loc_Sel<-tiles.loc %>% 
  filter(tile_ID %in% unique(aggr.df$tile_ID)) 

leaflet(data = tiles.loc_Sel) %>%
  addTiles() %>%
  addMarkers(~tileLong, ~tileLat, label = ~htmlEscape(tile_ID)) 

#---- 1.4 GAP filling of eggs observations ----
aggr.df
full.df <- aggr.df%>%
  mutate(year=substr(temporalID, 1,4)) %>% 
  dplyr::select(tile_ID, temporalID, year,  value,  Country,  Region  ) %>%
  drop_na()
summary(full.df)
uID<-unique(full.df$tile_ID)

mySeq<-data.frame("mydate"=seq.Date(as.Date('2000-01-01'), as.Date('2022-12-31'), by="week"))
mySeq$temporalID<-paste0(year(mySeq$mydate), "_",    str_pad(week(mySeq$mydate), 2, pad="0"))
mySeq$year<-year(mySeq$mydate)

myOut<-list()
for(i in 1:length(uID)){
  # i=1
  message(uID[i])
  full.cal <- subset(full.df, tile_ID==uID[i])
  myYears <- unique(full.cal$year)
  full.cal<-merge(x = full.cal, y =  mySeq[which(mySeq$year %in%  myYears), ], by = "temporalID", all.y = TRUE)
  full.cal$tile_ID<-unique(full.cal$tile_ID)[!is.na(unique(full.cal$tile_ID))]
  full.cal$Country<-unique(full.cal$Country)[!is.na(unique(full.cal$Country))]
  full.cal$Region<-unique(full.cal$Region)[!is.na(unique(full.cal$Region))]
  full.cal<-full.cal %>% 
    dplyr::select(tile_ID,Country, Region,  temporalID, mydate, value ) %>% #value.lci,  value.uci  
    rename(date=mydate)
  
  #quality check
  if (sum(!is.na(full.cal$value), na.rm = TRUE)<=2){
    full.cal$value <- NA
    full.cal$S1.52 <- NA    
    full.cal$C1.52 <- NA     
    full.cal$S2.52 <- NA    
    full.cal$C2.52 <- NA
  } else {
    ### interpolation of missing data
    full.cal$value <- as.numeric(forecast::na.interp(full.cal$value))
    # full.cal$value.lci <- as.numeric(forecast::na.interp(full.cal$value.lci))
    # full.cal$value.uci <- as.numeric(forecast::na.interp(full.cal$value.uci))
    ### add fourier decomp
    yt.ts <- ts(full.cal$value, start = 1, frequency = 52) ### for computing the sine/cosine waves is the same as zoo above - start = 1 is ok even if the first starting time/day is at 
    aaarrr <- data.frame(fourier(yt.ts, K=2)) ### on average enough
    full.cal<-cbind.data.frame(full.cal, aaarrr)
  }
  myOut[[i]]<-full.cal
}

bio.matrix<-do.call(rbind.data.frame, myOut)
head(bio.matrix)
dim(bio.matrix)
summary(bio.matrix)

#----2. process covariates ----
## 2.1 load and process temperatures ----
myT<-readRDS("eu_weeklymedianT_2008_2022.RDS") 
myT

#crop to the the buffer around the observations and NAs fill with moving window to avoid NAs in the sea
buff<-terra::buffer(terra::vect(myCoord), width=15000) #width udm is [m]
buff<-aggregate(buff)
myT<-terra::mask(myT, buff)

#fill NAs close to the coast with a moving window gap filling 
w <- matrix(1, 3, 3)
myT <- terra::focal(myT, w, mean, na.rm=TRUE, NAonly=TRUE, pad=TRUE)

# extract temperatures 
temp.obs<-terra::extract(myT, myCoord, df=TRUE)
temp.obs<-temp.obs[,2:ncol(temp.obs)]
temp.obs$tile_ID<-myCoord$tile_ID
temp.obs$lat<-myCoord$lat
head(temp.obs)

#pivot_longer and compute lagged temp 
temp.obs <- temp.obs %>%
  pivot_longer(cols = -c(tile_ID, lat)) %>% 
  rename(date=name, 
         medianTweek=value)  %>% 
  mutate(medianTweek=zoo::na.approx(medianTweek)) %>% # na.approx to interpolate missing values for temperature observations in Palermo stations for week 26 and 52  
  # slice(1:10) %>% 
  group_by(tile_ID) %>% 
  mutate(medianTweek.lag2=zoo::rollmean(medianTweek,2, align="right", fill=NA), 
         medianTweek.lag3=zoo::rollmean(medianTweek,3, align="right",  fill=NA),
         medianTweek.poly1=poly(medianTweek, 2)[,1],
         medianTweek.poly2=poly(medianTweek, 2)[,2],
         week=as.numeric(substr(date, 8, nchar(date))),
         year=as.numeric(substr(date, 3, 6)),
         temporalID=paste0(year, "_", str_pad(week, 2, pad="0")), 
         join.id=paste0(tile_ID, "_", temporalID))

temp.obs

# convert temperature to threshold-like variable
myT_thresh<-15
temp.obs$medianTweek<-ifelse(temp.obs$medianTweek<myT_thresh, 0, temp.obs$medianTweek)
temp.obs$medianTweek.lag2<-ifelse(temp.obs$medianTweek.lag2<myT_thresh, 0, temp.obs$medianTweek.lag2)
temp.obs$medianTweek.lag3<-ifelse(temp.obs$medianTweek.lag3<myT_thresh, 0, temp.obs$medianTweek.lag3)
hist(temp.obs$medianTweek)

## 2.2 compute weekly photoperiod for each trap ----
library(geosphere)
ph<-myCoord %>% 
  dplyr::select(tile_ID, lat) %>% 
  st_drop_geometry() %>% 
  distinct()

#create dates df
myDates<-data.frame(date=seq.Date(as.Date("2009-01-01"), as.Date("2022-12-31"), by="day"))
myDates$week<-week(myDates$date)
myDates$year<-year(myDates$date)
myDates$cut_date<-floor_date(myDates$date, "weeks", week_start = 1)
myDates$temporalID<-paste0(myDates$year, "_", myDates$week)
dim(myDates)

phOut<-list()
for(i in 1:nrow(ph)){
  # i=1
  message(i)
  tmpDat<-myDates
  phOut[[i]]<-tmpDat %>%
    mutate(photop=geosphere::daylength(lat=ph[i, 2], doy=as.Date(myDates$date)))  %>% 
    group_by(year, week) %>% 
    summarise(medianPhotoweek=median(photop, na.rm=TRUE))  %>% 
    mutate( medianPhotoweek.lag2=zoo::rollmean(medianPhotoweek,2, align="right", fill=NA), 
            medianPhotoweek.lag3=zoo::rollmean(medianPhotoweek,3, align="right",  fill=NA), 
            temporalID=paste0(year, "_", str_pad(week, 2, pad="0")), 
           ID=ph[i, 1], 
           join.id=paste0(ID, "_", temporalID)) %>%  
    dplyr::select(join.id, medianPhotoweek, medianPhotoweek.lag2, medianPhotoweek.lag3)

  } 
phOut<-do.call(rbind.data.frame, phOut)
phOut

## 2.3 process cumulative precipitation ----
myPrec<-readRDS("eu_weeklymedianPrec_2008_2022.RDS")
plot(myPrec$w_2022_32)

# crop to the the buffer around the observations and NAs fill with moving window to avoid NAs in the sea
myPrec<-terra::mask(myPrec, myT)

#fill NAs close to the coast with a moving window gap filling 
myPrec <- terra::focal(myPrec, w, mean, na.rm=TRUE, NAonly=TRUE, pad=TRUE)
# myPrec

prec.obs<-terra::extract(myPrec, myCoord, df=TRUE)
prec.obs<-prec.obs[,2:ncol(prec.obs)]
# names(prec.obs)<-unique(myDates$cut_date)[1:terra::nlyr(myT)]
prec.obs$tile_ID<-myCoord$tile_ID
prec.obs$lat<-myCoord$lat
head(prec.obs)

prec.obs <- prec.obs %>%
  pivot_longer(cols = -c(tile_ID, lat)) %>% 
  rename(date=name, 
         cumPrecweek=value)  %>% 
   # slice(1:10) %>%
  group_by(tile_ID) %>% 
  mutate(cumPrecweek.lag2=zoo::rollsum(cumPrecweek,2, align="right", fill=NA), 
         cumPrecweek.lag3=zoo::rollsum(cumPrecweek,3, align="right",  fill=NA),
         week=as.numeric(substr(date, 8, nchar(date))),
         year=as.numeric(substr(date, 3, 6)),
         temporalID=paste0(year, "_", str_pad(week, 2, pad="0")), 
         join.id=paste0(tile_ID, "_", temporalID))

prec.obs


##----3.  assemble covariates dataset ----
wda <- temp.obs %>%
  left_join(phOut, by="join.id") %>% 
  distinct(join.id, .keep_all= TRUE)

summary(wda$year.x)

wda <- wda %>% 
  left_join(prec.obs, by="join.id") %>% 
  distinct(join.id, .keep_all= TRUE) 

wda <-  wda %>% 
  dplyr::select( tile_ID.x , join.id,  lat.x, date.x, year.x, week.x,  
                medianTweek, medianTweek.lag2, medianTweek.lag3,  #medianTweek.poly1, medianTweek.poly2, 
                medianPhotoweek,  medianPhotoweek.lag2, medianPhotoweek.lag3,
                "cumPrecweek", "cumPrecweek.lag2", "cumPrecweek.lag3" #,
                # "RainyDaysweek", "RainyDaysweek.lag2", "RainyDaysweek.lag3",
                # "weeklyMort", "weeklyMort.lag2", "weeklyMort.lag3"
                )  %>% 
    rename(year=year.x, 
           tile_ID = tile_ID.x,
           lat= lat.x,
           date= date.x, 
           year= year.x, 
           week = week.x)  %>% 
  filter(year>=2010) 

names(wda)
wda
summary(wda)

#export
outname<-paste0(outdir, "/climaticCovariates_", Sys.Date(), ".RDS")
saveRDS(wda, outname)



#---- 4.1 Assemble training dataset ----
bio.matrix
bio.matrix$join.id<-paste0(bio.matrix$tile_ID, "_", bio.matrix$temporalID)
wda<-readRDS("climaticCovariates_2023-08-28.RDS")

bio.matrix <-  bio.matrix %>%
  left_join(wda, by="join.id")  %>% 
    dplyr::select( -tile_ID.y, -date.y, -join.id   ) %>% 
   rename(tile_ID=tile_ID.x , date=date.x) %>% 
  arrange( tile_ID) %>% 
  as_tibble() %>% 
  group_by(tile_ID) %>% 
  mutate(value.lag1 = dplyr::lag(value, n=1, order_by = tile_ID), # add lagged eggs value as proxy of auto regressive behaviour
         year =  lubridate::year(date)) %>% 
  rename(ID=tile_ID)

table(lubridate::year(bio.matrix$date))
# skimr::skim(bio.matrix)
summary(bio.matrix)

#----4.2. Partitioning training and testing dataset ----
# external validation: remove one station from each region, except Sicily
# internal validation: train: 2010-2021, test : 2022
table(bio.matrix$Region)

set.seed(666)

tiles.loc<-read_csv("ovitrapsID_cheatsheet_2023-08-28.csv") %>% 
  select(tile_ID, bgr) %>% 
  rename(ID ="tile_ID") %>% 
  distinct()

bio.matrix<-bio.matrix %>% 
  left_join(tiles.loc, by = "ID")
names(bio.matrix)

#exclude stations with less than 3 years of observations, belonging to NUTS3 and bgr
ext.valID<-bio.matrix %>%
  select(ID, Country,  Region, bgr) %>% 
  group_by(ID, Country,  Region, bgr) %>%
  tally() %>% 
  filter(n >= 52*3) %>%  # at least three years of observation to be selected as a valid station for external validation 
  group_by(Region, bgr) %>% 
  slice_sample(n=2) %>% 
  filter(Region !="Sicily") %>%  # remove Sicily because we have only one station there and it's the southernmost one
  arrange(bgr)
  
bio.matrix <- bio.matrix %>% 
  mutate(externalVal = ifelse(ID %in% ext.valID$ID, 1, 0), 
         internalVal = ifelse(year < 2022, 1, 0))

table(subset(bio.matrix, internalVal==1)$year)
table(subset(bio.matrix, internalVal==0)$year)
table(subset(bio.matrix, externalVal==1)$year)

subset(bio.matrix, externalVal==1, select=ID)

bio.matrix %>% 
  filter(externalVal==1) %>% 
  select(Region, bgr, ID) %>% 
  group_by(Region, bgr) %>% 
  summarise(length(unique(ID)))


bio.matrix %>% 
  filter(externalVal==1) %>% 
  ggplot(aes(date, value))+ #
  geom_bar( col='darkgray', stat='identity', position="dodge", width = 0.3)+
  labs(y="N. of eggs", x="Date")+
  facet_wrap(~Region+bgr, scales="free")+
  theme_classic()+
  theme(legend.background=element_blank(),
        panel.grid = element_blank(),
        legend.position = 'bottom',
        text = element_text(size=16), 
        strip.text = element_text(size=16),
        legend.text = element_text(size=16,angle = 0), legend.title = element_text(size=14),
        legend.key.size = unit(1.5, 'cm'))


outname<-paste0(outdir, "ovitrapsAllObservations_", Sys.Date(), ".RDS")
saveRDS(bio.matrix, outname)