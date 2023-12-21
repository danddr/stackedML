# spatial prediction 
library(terra)
library(sf)
library(tidyverse)
library(stars)
library(viridis)
library(geodata)

outdir<-"figures/" 

# load data
my_spPred<-readRDS("stacked_outputs/mlr3_mosquitoes_2023-08-31/spatialPredictions_ITA_2022_2023-08-31.RDS")
my_spPred$week<-paste0("w_", sub(".*_", "", my_spPred$temporal_ID))
my_spPred$year<-substr(my_spPred$temporal_ID, 3,6)

#subset for 2022 only
my_spPred<-my_spPred %>%  
  filter(year== "2022") %>% 
  select(x, y, week, spatial_pred) %>% 
  pivot_wider(names_from = "week", values_from = "spatial_pred") 

spat.pred<-terra::rast(my_spPred, type="xyz")
spat.pred[spat.pred<0]<-0
plot(spat.pred$w_10)

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
countries <- st_as_sf(countries)

# source: https://github.com/paleolimbot/ggspatial/issues/25
stars.pred<-spat.pred[[c(10, 15, 20, 25, 30, 35, 40, 45)]]
names(stars.pred)<-LETTERS[seq( from = 1, to = nlyr(stars.pred) )]
stars.pred<-st_as_stars(stars.pred) 


p <- ggplot() +
  geom_stars(data = stars.pred, inherit.aes = FALSE)+
  geom_sf(data= pan, fill="darkgrey")+
  geom_sf(data= tun, fill="darkgrey")+
  geom_sf(data = countries, fill=NA, 
          color="#2B1608", linewidth=0.2, 
          alpha=0.6)+
  ggspatial::annotation_scale(data=data.frame(band = c("H"),
                                              location = c("br")), 
                              aes(location = location),  
                              width_hint = 0.25 # proportion of the plot
  ) +
   scale_fill_viridis_b(option="rocket", direction = -1,
    limits = c(0, 500), breaks=c(0,  50, 100, 200, 400, 500),
    oob = scales::squish, na.value = "transparent")+
  labs(x="Longitude",y="Latitude", fill="Median number of eggs")+
  ylim(36, 48)+
  facet_wrap(~band, ncol = 4, 
             labeller = labeller(band = c("A" = "Week 10 (7-13 March)", 
                                          "B" = "Week 15 (11-17 April)", 
                                          "C" = "Week 20 (16-22 May)",
                                          "D" = "Week 25 (20-26 June)", 
                                          "E" = "Week 30 (25-31 July)", 
                                          "F" = "Week 35 (29 August - 4 September)",
                                          "G" = "Week 40 (3-8 October)", 
                                          "H" = "Week 45 (7-13 November)")
                                 ))+ 
  theme_classic()+
  theme(legend.position = "bottom",  
        text = element_text(size=12),
        legend.text = element_text( size = 10),
        legend.key.size = unit(1, 'cm')
  )+
  coord_sf(xlim = c(4, 22), ylim = c(35.95, 48), expand = FALSE)

p
outname <- paste0(outdir, "SpatialPrediction_", Sys.Date(), ".png")
ggsave(p, filename = outname, width = 16, height = 10, device='png', dpi=320)