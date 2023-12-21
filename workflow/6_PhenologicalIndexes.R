#  phenological indexes 
library(ggplot2)
library(dplyr)
library(lubridate)
library(terra)
library(tidyverse)
library(stars)
library(viridis)
library(geodata)
library(sf)
library(lme4)
library(ggh4x)
library(ggpmisc)
outdir<-"figures/" 


#-- 1. spatial phenological indexes ----

# load predictions
my_spPred<-readRDS("spatialPredictions_ITA_2022_2023-08-31.RDS")
head(my_spPred)
my_spPred$week<-paste0("w_", sub(".*_", "", my_spPred$temporal_ID))

my_spPred<-my_spPred %>%  
  select(x, y, temporal_ID, spatial_pred) %>% 
  pivot_wider(names_from = "temporal_ID", values_from = "spatial_pred")

spat.pred<-terra::rast(my_spPred, type="xyz")
spat.pred[spat.pred<0]<-0
plot(spat.pred$w_2010_40)

spat.pred.df<-as.data.frame(spat.pred, xy=TRUE)
spat.pred.df <- spat.pred.df %>% 
  mutate(ID=1:n()) %>% 
  pivot_longer(-c(ID, x, y)) %>% 
  rename(date=name)%>%
  mutate(partition="Predicted",
         model="stacked", 
         year=substr(date, 3,6), 
         week=substr(date, 8, nchar(date))) %>% 
    filter(year==2022)%>% 
  mutate(date=as.Date(paste(year, week, 1, sep="-"), "%Y-%U-%u")
         )

head(spat.pred.df)
tail(spat.pred.df)
spat.pred.df<-na.omit(spat.pred.df)

# load pseudo-seanonal index function
source("script/functions/phenologicalIndexes.R")
myEggThresh<- 55 
phenInd.sp<-phenologicalIndexes(df = spat.pred.df,  
                      value.field = "value",  
                      field.ID = "ID",  
                      field.partition = "partition", 
                      field.strata="model",
                      field.date= "date",  
                      field.year = "year", 
                      egg.thresh = myEggThresh,  
                      method = "threshold" )

onset.sp <- phenInd.sp$Onset %>% 
  mutate(week=week(date), 
         year=year(date)) %>%  
  select(x, y, week) %>% 
  terra::rast( type="xyz")
plot(onset.sp)

peak.sp <- phenInd.sp$Peak %>% 
  mutate(week=week(date), 
         year=year(date)) %>%  
  select(x, y, week) %>% 
  terra::rast( type="xyz")
plot(peak.sp)

offset.sp <- phenInd.sp$Offset %>% 
  mutate(week=week(date), 
         year=year(date)) %>%  
  select(x, y, week) %>% 
  terra::rast( type="xyz")
plot(offset.sp)

season.length<-(offset.sp-onset.sp)+1 # +1 because the differece is 20-20 is zero, but the mosquito was actually active one week
plot(season.length)

##----2.1 plotting ----

#pannonian mask 
pan <- st_read("BiogeoRegions2016.shp") %>%
  mutate(new_name=str_replace(name, pattern=" Bio-geographical Region", "")) %>% 
  st_transform(crs=4326)%>%
  filter(new_name=="Pannonian") 

#north africa mask
tun <- geodata::gadm(country=c("TUN", "DZA"), level=0, path = "/home/.")
tun <- st_as_sf(tun) %>% 
  mutate(group = "A") %>% 
  group_by(group) %>% 
  summarize()

#regional borders
countries <- geodata::gadm(country=c("ITA", "CH", "FRA", "ALB"), level=1, path = "/home/.")
# terra::plot(countries)
countries <- st_as_sf(countries)


p <- ggplot() +
  geom_stars(data = stars::st_as_stars(season.length), inherit.aes = FALSE)+
  geom_sf(data= pan, fill="darkgrey")+
  geom_sf(data= tun, fill="darkgrey")+
  geom_sf(data = countries, fill=NA, 
          color="#E53F71", linewidth=0.5, # #"" 
          alpha=0.8)+
  labs(x="Longitude",y="Latitude", fill=paste("POT (weeks) \nThreshold =", myEggThresh))+
  scale_fill_viridis_b(option="viridis", direction = 1,  limits = c(0,30),
                       # breaks=c(1, 4, 8, 12, 20, 28),
                       breaks=c(0, 5, 10, 15, 20, 30),
                     oob = scales::squish, na.value = "transparent")+
  ggspatial::annotation_scale(location = "br", #bottom right 
                              width_hint = 0.25 # proportion of the plot
  ) +
  ylim(36, 48)+
  theme_classic()+
  theme(legend.position = "bottom",  
        text = element_text(size=16),
        legend.key.size = unit(1, 'cm')
  )+
  coord_sf(xlim = c(4, 22), ylim = c(35.95, 48), expand = FALSE)

p
outname <- paste0(outdir, "POT_Lenght_thresh", myEggThresh, "_", Sys.Date(), ".png")
ggsave(p, filename = outname, width = 16, height = 10, device='png', dpi=320)


# derive POT IQR
myEggThresh<- c(20, 125)
POT_IQR<-list()
for(i in 1:length(myEggThresh)){
  # i=1
  phenInd.sp<-phenologicalIndexes(df = spat.pred.df,  
                                  value.field = "value",  
                                  field.ID = "ID",  
                                  field.partition = "partition", 
                                  field.strata="model",
                                  field.date= "date",  
                                  field.year = "year", 
                                  egg.thresh = myEggThresh[i],  
                                  method = "threshold" )
  
  onset.sp <- phenInd.sp$Onset %>% 
    mutate(week=week(date), 
           year=year(date)) %>%  
    select(x, y, week) %>% 
    terra::rast( type="xyz")
  
  
  peak.sp <- phenInd.sp$Peak %>% 
    mutate(week=week(date), 
           year=year(date)) %>%  
    select(x, y, week) %>% 
    terra::rast( type="xyz")
  
  offset.sp <- phenInd.sp$Offset %>% 
    mutate(week=week(date), 
           year=year(date)) %>%  
    select(x, y, week) %>% 
    terra::rast( type="xyz")
  
  POT_IQR[[i]] <-(offset.sp-onset.sp)+1
    

}

POT_IQR <- do.call(c, POT_IQR)
names(POT_IQR)<- paste("Threshold =", myEggThresh)

p <- ggplot() +
  geom_stars(data = stars::st_as_stars(POT_IQR), inherit.aes = FALSE)+
  geom_sf(data= pan, fill="darkgrey")+
  geom_sf(data= tun, fill="darkgrey")+
  geom_sf(data = countries, fill=NA, 
          color="#E53F71", linewidth=0.5, # #"" 
          alpha=0.8)+
 
  labs(x="Longitude",y="Latitude", fill="POT (weeks)")+
  scale_fill_viridis_b(option="viridis", direction = 1,  limits = c(0,30),
                       # breaks=c(1, 4, 8, 12, 20, 28),
                       breaks=c(0, 5, 10, 15, 20, 30),
                       oob = scales::squish, na.value = "transparent")+
  ggspatial::annotation_scale(location = "br", #bottom right 
                              width_hint = 0.25 # proportion of the plot
  ) +
  ylim(36, 48)+
  facet_wrap(~band)+
  theme_classic()+
  theme(legend.position = "bottom",  
        text = element_text(size=16),
        legend.key.size = unit(1, 'cm')
  )+
  coord_sf(xlim = c(4, 22), ylim = c(35.95, 48), expand = FALSE)


p
outname <- paste0(outdir, "POT_Lenght_threshIQR_", Sys.Date(), ".png")
ggsave(p, filename = outname, width = 16, height = 10, device='png', dpi=320)


#---- 2. Linear Modelling -----

##---- 2.1 load observations and predictions----
myObs <-readRDS("mlr3_stack_Obs_2023-08-30.RDS")

#add biogeographical regions
tiles <-read_csv("ovitraps/ovitrapsID_cheatsheet_2023-08-28.csv") %>%
  select(tile_ID, bgr) %>%
  distinct() %>%  
  rename( ID=tile_ID) %>% 
  mutate(ID=as.factor(ID))
  
myObs<-myObs %>% 
  left_join(tiles, by="ID") %>% 
  mutate(partition = "Train",
         year = year(date))

length(unique(subset(myObs, partition = "Train")$ID))


# load predictions 
myPred <-readRDS("/mlr3_stack_Pred_2023-08-30.RDS")
myPred<-myPred %>% 
  mutate(partition=as.factor(partition), 
         ID=as.factor(ID), 
         value=ifelse(value<0, 0, value), 
         value=round(value)) %>% 
  left_join(tiles, by="ID") %>% 
  mutate(partition = "Predicted",
         year = year(date))

##---- 2.2 compute indexes ----
# bind observed and predicted together
MyAll <-  bind_rows(myObs, myPred) %>% 
  mutate(model=recode(model, "stacked"="Predicted")) %>% 
  select(-se, -externalVal, -internalVal)

unique(MyAll$ID)
table(MyAll$model)
table(MyAll$partition)
table(MyAll$bgr)

myEggThresh <- 55
#load phenological index function 
source("script/functions/phenologicalIndexes.R")
phenInd<-phenologicalIndexes(df = MyAll,  
                             value.field = "value",  
                             field.ID = "ID",  
                             field.partition = "partition",
                             field.strata="bgr",
                             field.date= "date",  
                             field.year = "year", 
                             egg.thresh = myEggThresh,  
                             method = "threshold" )

start.s <- phenInd$Onset %>%
  mutate( week=week(date), 
          year=year(date),
          # model=factor(model, levels = c("Observations", "Estimated")), 
          join.id = paste0(ID, bgr, partition, year)) %>% 
  select(join.id, ID, Region, Country, bgr,  partition, year, week) %>%  
  rename(onset=week)

table(start.s$partition)

end.s <- phenInd$Offset %>%
  mutate( week=week(date), 
          year=year(date),
          model=factor(model, levels = c("Observations", "Estimated")), 
          join.id = paste0(ID, bgr, partition, year)) %>% 
  select(join.id, ID, Region, Country, bgr, bgr, partition, year, week) %>%  
  rename(offset=week)

# table(end.s$model)
table(end.s$partition)

##---- 2.3 Modelling ----
#overall model on the CP length
mod.df<-start.s %>% 
  select(join.id, onset) %>% 
  left_join(end.s, by="join.id") %>% 
  mutate(sLength=offset-onset,
         partition=recode(partition, "Train"="Observed")) %>% 
  select(ID, year, bgr, sLength, partition)

dim(mod.df)
summary(mod.df)
table(mod.df$bgr)
hist(mod.df$sLength)

mod.df %>% 
  drop_na() %>% 
  group_by(partition, bgr, year) %>% 
  summarise(median = quantile(sLength, 0.5), 
            IQR_l = quantile(sLength, 0.25), 
            IQR_u = quantile(sLength, 0.75 )) %>% 
  group_by(partition, bgr) %>% 
  summarise(median = round(mean(median)), 
            IQR_l = round(mean(IQR_l)), 
            IQR_u = round(mean(IQR_u)))

# GLM model 
mod.df %>%   
  split(.$partition) %>% 
  map(~summary(glm(.$sLength~.$year*.$bgr, family= "poisson"))) 

# GLM vs GLMM model
tdf<-subset(mod.df, partition == "Predicted") #Observed
nre<-glm(sLength~year*bgr , family= "poisson", data=tdf)
re<-glmer(sLength~year*bgr + (1|ID), family= "poisson", data=tdf)
anova(nre, re, test = "F")
AIC(nre, re)

# ok GLMM works better, let's keep the GLMM
mod.df %>%
  split(.$partition) %>%
  map(~ {
    .data <- .
    summary(glmer(sLength ~ year * bgr + (1|ID), family = "poisson", data = .data))
  })

#train
train.coeff<-list(alpine = paste("Alpine: CP","~", (47.94),  "+", (-0.02), "* year"), 
                  cont = paste("Continental: CP","~", (47.94 -98.28),"+",  (-0.02+  0.050), "* year"),
                  med = paste("Mediterranean: CP","~", (47.94-81.90),"+",  (-0.02+  0.041), "* year")
)
#pred
pred.coeff<-list(alpine = paste("Alpine: CP","~", (-361.98),  "+", (0.18), "* year"),
                 cont = paste("Continental: CP","~", (-361.98 +   318.68),"+",  (0.18  -0.16), "* year"),
                 med = paste("Mediterranean: CP","~", (-361.98 +  263.64),"+",  (0.18  -0.13), "* year")
)

coef.tab <- data.frame(Observed=do.call(rbind, train.coeff), 
           Predicted=do.call(rbind, pred.coeff), 
           bgr=c("Alpine", "Continental", "Mediterranean")) 
  # pivot_longer(!bgr)

coef.tab$Observed

#predict regression line
myclass<-c("Observed", "Predicted")
predList<-list()
tabList<-list()

for(i in 1:length(myclass)){
  subdf<-subset(mod.df, partition==myclass[i])
  trainMod<-glmer(sLength~year*bgr + (1|ID), family= "poisson", data=subdf)
  pred.df <- expand.grid(
    year = sort(unique(subdf$year)),
    bgr = unique(subdf$bgr)
  )
  
  #prediction db matrix
  X <- model.matrix(~ year * bgr , data = pred.df)
    #fixed eff 
  beta <- fixef(trainMod)
  #variance-covariance matrix
  VCV <- vcov(trainMod)
  # compute standard error
  pred.se <- sqrt(diag(X %*% VCV %*% t(X)))
  # model predictions
  fit <- predict(trainMod, newdata=pred.df, re.form = NA) #[ puoi anche fare fit <- X %*% beta in notazione matriciale]
  pred.df <- cbind(pred.df, 
                   sLength =  exp(fit), 
                   lo.ci = exp(fit - 1.96*pred.se), 
                   hi.ci = exp(fit + 1.96*pred.se),
                   partition = myclass[i]
  )
  predList[[i]] <- pred.df
  
  trainMod<-summary(trainMod)
  tabList[[i]]<-data.frame(cov=rownames(trainMod$coefficients), 
                           regr.est=paste0(round(trainMod$coefficients[,1],3), 
                                           " (", round(trainMod$coefficients[,2],3), ")") ,
                           p.val = paste0(" (", ifelse(round(trainMod$coefficients[,4],3) < 0.05, "p < 0.05", paste0("p = ", as.character(round(trainMod$coefficients[,4],3))))
                                          , ")"), 
                           partition=myclass[i])
  
}

tabList <- do.call(rbind, tabList)
outname <- paste0(outdir, "CriticalPeriod_ModelCoeff_Tab", Sys.Date(), ".csv")
write_csv(tabList, outname)

predList<-do.call(rbind, predList)
predList<-predList %>% 
  as_tibble() %>% 
  drop_na()

##---- 2.4 plotting ----
strip <- ggh4x::strip_themed(background_x = ggh4x::elem_list_rect(fill = c("#FBE5B6", "#D99527",  "#CE4B3C")))

# colors
cols<-c(c("Alpine" = "#FBE5B6", "Continental" = "#D99527",  "Mediterranean"= "#CE4B3C"))

# Add facet-specific labels using annotate()
coef.tab
facet_labels <- c("Alpine: ln(POT) = 47.94 -0.02*year \rContinental: ln(POT) = -50.34 + 0.03*year \rMediterranean: ln(POT) = -33.96 + 0.021*year", 
                  "Alpine: ln(POT) = -361.98 + 0.18*year \rContinental: ln(POT) = -43.3 + 0.02*year \rMediterranean: ln(POT) = -98.34 + 0.05*year"
)

data_text <- data.frame(label = facet_labels,  # Create data for text
                        partition = names(table(mod.df$partition)),
                        year = c(2014, 2014),
                        sLength = c(35, 35), 
                        bgr=c(NA, NA))

p<-mod.df%>%
  drop_na() %>% 
  ggplot(aes(year, sLength, col=bgr))+
  geom_point(size=1.5)+
  geom_ribbon(data=predList, aes(ymin=lo.ci, ymax=hi.ci, fill=bgr), alpha= 0.5) +
  # geom_point(data=predList)+
  geom_line(data=predList)+
  labs(y="POT (weeks)", x="Year", 
       col="Biogeographical regions", 
       fill="Biogeographical regions")+
  geom_text(data = data_text, size=6, show.legend = FALSE, mapping = aes(x = year,
                                                                         y = sLength,
                                                                         label = label)
            )+
  scale_color_manual(values=cols)+
  scale_fill_manual(values=cols)+
  scale_x_continuous(breaks = 2010:2022)+
  guides(colour = guide_legend(override.aes = list(size=2)))+
  theme_classic()+
  facet_wrap(~partition)+
  theme(legend.background=element_blank(),
        panel.grid = element_blank(),
        legend.position = 'bottom',
        text = element_text(size=16), 
        strip.text = element_text(size=16),
        legend.text = element_text(size=16,angle = 0), legend.title = element_text(size=16),
        legend.key.size = unit(1.5, 'cm'))


p
outname <- paste0(outdir, "POT_Model", Sys.Date(), ".png")
ggsave(p, filename = outname, width = 20, height = 16, device='png', dpi=320)