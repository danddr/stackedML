#Plotting observed and predicted values

library(ggplot2)
library(tidyverse)
library(ggh4x)
library(sf)

#plotting only external validation ----
#graphical parametest
cols <- c("Observations" = "darkgray", "Train" = "lightblue", "Test_int" = "darkorange", "Test_ext"= 	"#8B008B")
# ltypes<- c("Observations" = 2, "Train" = 1, "Test" = 1)
Autoregr_model = 1 # add an autoregressive component to the model (1: Yes; 0: No)

if(Autoregr_model == 1) {
  myObs<-readRDS("/mlr3_stack_Obs_2023-08-30.RDS")
  myPred<-readRDS("mlr3_stack_AutoRegrPred_2023-08-30.RDS")
} else {
  myObs<-readRDS("mlr3_stack_Obs_2023-08-30.RDS")
  myPred<-readRDS("mlr3_stack_Pred_2023-08-30.RDS")
}

# organize prediction data.frame
myPred<-myPred %>% 
  mutate(partition=as.factor(partition), 
         ID=as.factor(ID), 
         value=ifelse(value<0, 0, value), 
         value=round(value), 
         lci= value -(1.96*se), 
         uci= value +(1.96*se))  

# external validation datates
Ext_test.df <-   myObs %>%
    filter(externalVal == 1) %>% 
    select(ID, Region, value, date) %>% 
  mutate(partition = "Observed")
     
#figure paper
myCols<-c("Observed" = "darkgray",  "Predicted" = "#8B008B")

Ext_test.df <-   myObs %>%
  filter(externalVal == 1) %>% 
  select(ID, Region, value, date) %>% 
  mutate(partition = "Observed")

Ext_test.df %>% 
  select(ID, Region) %>% 
  distinct()

# ovitraps locations
tiles <-read_csv("ovitraps/ovitrapsID_cheatsheet_2023-08-28.csv") %>% 
  select(tile_ID, bgr) %>% 
  distinct() %>% 
  rename(ID=tile_ID) %>% 
  mutate(ID=as.factor(ID))

strip <- ggh4x::strip_themed(background_x = ggh4x::elem_list_rect(fill = c("#FBE5B6", "#FBE5B6", "#FBE5B6",
                                                                           "#D99527", "#D99527", 
                                                                           "#CE4B3C", "#CE4B3C" , "#CE4B3C", "#CE4B3C", "#CE4B3C")))
p <- myPred %>% 
  filter(partition == "Test_ext") %>%
  mutate(partition= dplyr::recode(partition, Test_ext = "Predicted")) %>% 
  bind_rows(Ext_test.df) %>% 
  left_join(tiles, by="ID") %>% 
  as_tibble() %>% 
  group_by(Region, date, bgr, model, partition) %>% 
  mutate(value = median(value), 
         lci = median(lci),  
         uci = median(uci),
         Region = factor(Region, levels=c("Autonomous Province of Trento", "Canton of Ticino", 
                                          "Emilia-Romagna", "Veneto", 
                                          "Cote Azur",  "Lazio", "Tirane", "Tuscany", "Puglia"))) %>% 
  ggplot(aes(date, value, col=partition))+ 
  # geom_point(size= 0.3)+
  geom_line(linewidth=0.5)+
  geom_ribbon(aes(ymin = lci, ymax = uci, fill=partition), alpha = 0.5)+
  scale_colour_manual(values = myCols)+
  scale_fill_manual(values = myCols)+
  labs(y="Median number of eggs", x="Date", col="", fill="")+
  facet_wrap2(bgr~Region, scales="free", strip = strip, ncol=3) +
  theme_classic()+
  theme(legend.background=element_blank(),
        panel.grid = element_blank(),
        legend.position = 'bottom',
        text = element_text(size=16), 
        strip.text = element_text(size=16),
        legend.text = element_text(size=16,angle = 0), legend.title = element_text(size=14),
        legend.key.size = unit(1.5, 'cm'))

p
if(Autoregr_model == 1) {
  outname <- paste0(outdir, "autoregr_temporalPlot_Externalvalidation", Sys.Date(), ".png")
  ggsave(p, filename = outname, width = 28, height = 12, device='png', dpi=320)
} else {
  outname <- paste0(outdir, "regr_temporalPlot_Externalvalidation", Sys.Date(), ".png")
  ggsave(p, filename = outname, width = 28, height = 12, device='png', dpi=320)
}


#---- aggregation by biogeographical regions ----

outdir<-"figures/" 

# ovitraps locations
tiles <-read_csv("ovitrapsID_cheatsheet_2023-08-28.csv") %>%
  select(tile_ID, tileLong, tileLat, bgr) %>% 
  distinct() %>%  
  st_as_sf(coords=c("tileLong", "tileLat"), crs=4326) %>% 
  # st_join(bgr_shp[,"code"], left = FALSE) %>% 
  rename( ID="tile_ID") %>%
  st_drop_geometry() %>% 
  mutate(ID=as.factor(ID))

#graphical parameters
cols <- c("Observed" = "darkgray", "Train" = "lightblue", "Internal validation" = "darkorange", "External validation"= 	"#8B008B")

Autoregr_model = 0 # add an autoregressive component to the model (1: Yes; 0: No)

if(Autoregr_model == 1) {
  myObs<-readRDS("stacked_outputs/mlr3_mosquitoes_2023-08-31//mlr3_stack_Obs_2023-08-30.RDS")
  myPred<-readRDS("stacked_outputs/mlr3_mosquitoes_2023-08-31/mlr3_stack_AutoRegrPred_2023-08-30.RDS")
} else {
  myObs<-readRDS("stacked_outputs/mlr3_mosquitoes_2023-08-31/mlr3_stack_Obs_2023-08-30.RDS")
  myPred<-readRDS("stacked_outputs/mlr3_mosquitoes_2023-08-31/mlr3_stack_Pred_2023-08-30.RDS")
}

myPred<-myPred %>% 
  mutate(partition=as.factor(partition), 
         ID=as.factor(ID), 
         value=ifelse(value<0, 0, value), 
         value=round(value))  

# create dataset for plotting
aggr.Pred <- myPred %>% 
  bind_rows(myObs) %>% 
  as_tibble() %>% 
  left_join(tiles, by="ID") %>% 
  group_by(bgr, date, model, partition) %>% 
  summarise(median=quantile(value, probs=0.5), 
            IQR_l=quantile(value, probs=0.25), 
            IQR_u=quantile(value, probs=0.75)) %>% 
  mutate(model=recode(model, stacked="Predicted"), 
         partition=recode(partition, Test_int="Internal validation", Test_ext="External validation") )%>% 
  ungroup() %>% 
  mutate(colorscheme = paste0(model,  "_", partition)) %>% 
  group_by(colorscheme) %>% 
  mutate(colorscheme = recode(colorscheme, 
                             `Observations_External validation`="Observed", 
                             `Observations_Internal validation`="Observed", 
                             `Observations_Train`="Observed", 
                             `Predicted_External validation`="External validation",
                             `Predicted_Internal validation` =  "Internal validation",
                             `Predicted_Train` = "Train"
                             ) 
         ) %>% 
  ungroup() %>% 
  mutate(plotscheme = paste0(model,  "_", partition)) %>% 
  group_by(plotscheme) %>% 
  mutate(plotscheme = recode(plotscheme, 
                              `Observations_External validation`="External validation", 
                              `Observations_Internal validation`="Internal validation", 
                              `Observations_Train`="Internal validation", 
                              `Predicted_External validation`="External validation",
                              `Predicted_Internal validation` =  "Internal validation",
                              `Predicted_Train` = "Internal validation"
  ) 
  )


strip <- ggh4x::strip_themed(background_x = ggh4x::elem_list_rect(fill = c("#FBE5B6", "#D99527", "#CE4B3C")))

#plot
p <- aggr.Pred %>% 
  drop_na() %>% # CHECK this out next time 
  mutate(colorscheme=factor(colorscheme, levels = c("Observed", "Train",  "Internal validation", "External validation")),
         plotscheme=factor(plotscheme, levels = c("Internal validation", "External validation"))) %>% 
  ggplot(aes(date, median, col=colorscheme, fill=colorscheme))+ 
  geom_ribbon(aes(ymin = IQR_l, ymax = IQR_u, fill=colorscheme, col=NULL), alpha=0.3) + 
    geom_line(linewidth=0.5)+
    scale_colour_manual(values = cols)+
    scale_fill_manual(values = cols)+
    labs(y="Number of eggs (IQR)", x="Date", col="", fill="")+
    ylim(0,750)+
    # facet_wrap(~bgr, scales="free")+
    # facet_wrap2(~ bgr, scales="free", strip = strip) +
    facet_grid2(plotscheme ~ bgr, scales="free", strip = strip) +
    theme_classic()+
    theme(legend.background=element_blank(),
          panel.grid = element_blank(),
          legend.position = 'bottom',
          text = element_text(size=16), 
          strip.text = element_text(size=16),
          legend.text = element_text(size=16,angle = 0), legend.title = element_text(size=14),
          legend.key.size = unit(1.5, 'cm'))

if(Autoregr_model == 1) {
  outname <- paste0(outdir, "autoregr_temporalPlot_validation", Sys.Date(), ".png")
  ggsave(p, filename = outname, width = 28, height = 12, device='png', dpi=320)
} else {
  outname <- paste0(outdir, "regr_temporalPlot_validation", Sys.Date(), ".png")
  ggsave(p, filename = outname, width = 28, height = 12, device='png', dpi=320)
}