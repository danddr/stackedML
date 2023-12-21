# Model validation 
library(ggplot2)
library(tidyverse)
library(ggpubr)
library(mlr3verse)

outdir<-"figures/" 
cols <- c("Observations" = "darkgray", "Train" = "lightblue", "Internal validation" = "darkorange", "External validation"=  "#8B008B")

#load predictions and observations
Autoregr_model = 0 # add an autoregressive component to the model (1: Yes; 0: No)

if(Autoregr_model == 1) {
  myObs<-readRDS("mlr3_stack_Obs_2023-08-30.RDS")
  myPred<-readRDS("mlr3_stack_AutoRegrPred_2023-08-30.RDS")
    
  myObs<-myObs %>% 
    mutate(join.id=paste0(ID, date))
  
  table(myPred$partition) # I might have multiple associations with the left join
  
  gof_m <- myPred %>%
    mutate(partition=as.factor(partition), 
           ID=as.factor(ID), 
           value=ifelse(value<0, 0, value), 
           value=round(value),
           join.id=paste0(ID, date)) %>% 
    left_join(myObs, by="join.id") %>% 
    select(ID.x,  date.x, partition.x, value.x, value.y) %>% 
    rename(ID=ID.x, 
           predicted = value.x, 
           date= date.x, 
           partition = partition.x, 
           observed= value.y) %>% 
    mutate(observed=round(observed)) %>% 
    group_by(ID, partition  ) %>% 
    summarise(RMSE=Metrics::rmse(observed, predicted),
              MAE=Metrics::mae(observed, predicted), 
              RMSE.std=Metrics::rmse(observed, predicted)/mean(observed),
              MAE.std=Metrics::mae(observed, predicted)/mean(observed))   %>% 
    pivot_longer(-c(ID, partition)) %>% 
    mutate(partition=recode(partition, "Test_int"  = "Internal validation", 
                            "Test_ext" = "External validation"), 
           partition=factor(partition, levels=c("Train", "Internal validation", "External validation")))
  
  autoregr_gof <- gof_m %>% 
    filter(name %in% c("RMSE", "MAE")) %>% 
    # filter(value<80) %>%
    mutate(name=factor(name, levels=c("RMSE", "MAE"))) %>% 
    ggplot(aes(partition, value, col=partition))+
    geom_boxplot()+
    scale_colour_manual(values = cols)+
    labs(y="Value", col="", x="")+
    ylim(0,80)+
    facet_wrap(~name, scales="free_y")+
    theme_classic()+
    theme(legend.background=element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          panel.grid = element_blank(),
          legend.position = 'bottom',
          text = element_text(size=16), 
          strip.text = element_text(size=16),
          legend.text = element_text(size=16,angle = 0), legend.title = element_text(size=16),
          legend.key.size = unit(1.5, 'cm'))
  
  autoregr_std.gof<-gof_m %>% 
    filter(name %in% c("RMSE.std", "MAE.std")) %>% 
    filter(value<15) %>%
    mutate(name=recode(name, "RMSE.std"  = "RMSE", 
                       "MAE.std" = "MAE"),
           name=factor(name, levels=c("RMSE", "MAE"))) %>% 
    ggplot(aes(partition, value, col=partition))+
    geom_boxplot()+
    scale_colour_manual(values = cols)+
    labs(y="Value", col="", x="")+
    facet_wrap(~name, scales="free_y")+
    theme_classic()+
    theme(legend.background=element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          panel.grid = element_blank(),
          legend.position = 'bottom',
          text = element_text(size=16), 
          strip.text = element_text(size=16),
          legend.text = element_text(size=16,angle = 0), legend.title = element_text(size=16),
          legend.key.size = unit(1.5, 'cm'))
  } else {
    myObs<-readRDS("mlr3_stack_Obs_2023-08-30.RDS")
    myPred<-readRDS("mlr3_stack_Pred_2023-08-30.RDS")
    
  myObs<-myObs %>% 
    mutate(join.id=paste0(ID, date))
  
  table(myPred$partition) # I might have multiple associations with the left join
  
  gof_m <- myPred %>%
    mutate(partition=as.factor(partition), 
           ID=as.factor(ID), 
           value=ifelse(value<0, 0, value), 
           value=round(value),
           join.id=paste0(ID, date)) %>% 
    left_join(myObs, by="join.id") %>% 
    select(ID.x,  date.x, partition.x, value.x, value.y) %>% 
    rename(ID=ID.x, 
           predicted = value.x, 
           date= date.x, 
           partition = partition.x, 
           observed= value.y) %>% 
    mutate(observed=round(observed)) %>% 
    group_by(ID, partition  ) %>% 
    summarise(RMSE=Metrics::rmse(observed, predicted),
              MAE=Metrics::mae(observed, predicted), 
              RMSE.std=Metrics::rmse(observed, predicted)/mean(observed),
              MAE.std=Metrics::mae(observed, predicted)/mean(observed))   %>% 
    pivot_longer(-c(ID, partition)) %>% 
    mutate(partition=recode(partition, "Test_int"  = "Internal validation", 
                            "Test_ext" = "External validation"), 
           partition=factor(partition, levels=c("Train", "Internal validation", "External validation")))
  
  regr_gof <- gof_m %>% 
    filter(name %in% c("RMSE", "MAE")) %>% 
    # filter(value<80) %>%
    mutate(name=factor(name, levels=c("RMSE", "MAE"))) %>% 
    ggplot(aes(partition, value, col=partition))+
    geom_boxplot()+
    scale_colour_manual(values = cols)+
    labs(y="Value", col="", x="")+
    ylim(0,80)+
    facet_wrap(~name, scales="free_y")+
    theme_classic()+
    theme(legend.background=element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          panel.grid = element_blank(),
          legend.position = 'bottom',
          text = element_text(size=16), 
          strip.text = element_text(size=16),
          legend.text = element_text(size=16,angle = 0), legend.title = element_text(size=16),
          legend.key.size = unit(1.5, 'cm'))
  
  regr_std.gof <- gof_m %>% 
    filter(name %in% c("RMSE.std", "MAE.std")) %>% 
    filter(value<20) %>%
    mutate(name=recode(name, "RMSE.std"  = "RMSE", 
                       "MAE.std" = "MAE"),
           name=factor(name, levels=c("RMSE", "MAE"))) %>% 
    ggplot(aes(partition, value, col=partition))+
    geom_boxplot()+
    scale_colour_manual(values = cols)+
    labs(y="Value", col="", x="")+
    facet_wrap(~name, scales="free_y")+
    theme_classic()+
    theme(legend.background=element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          panel.grid = element_blank(),
          legend.position = 'bottom',
          text = element_text(size=16), 
          strip.text = element_text(size=16),
          legend.text = element_text(size=16,angle = 0), legend.title = element_text(size=16),
          legend.key.size = unit(1.5, 'cm'))
  
  
}

pp<-ggarrange(regr_gof, autoregr_gof, #regr_std.gof, autoregr_std.gof
          ncol=2, nrow=1, 
          labels = c("A", "B"),
          common.legend = TRUE, legend="bottom")

pp
outname <- paste0(outdir, "GOF_boxplot_", Sys.Date(), ".png")
ggsave(pp, filename = outname, width = 36, height = 12, device='png', dpi=320)

#---- internal validation CV----
cv_list.autoregr<-readRDS("stacked_autoregrModel_train_internalCV_2023-08-30.RDS")
cv_list.regr<-readRDS("stacked_Model_train_internalCV_2023-08-30.RDS")

best.learner_regr <- readRDS("stacked_Model_train_2023-08-30.RDS")
best.learner_regr <- summary(best.learner_regr$model$lm_ensemble$model)

best.learner_autoregr <- readRDS("stacked_autoregrModel_train_2023-08-30.RDS")
best.learner_autoregr <- summary(best.learner_autoregr$model$lm_ensemble$model)

#summary tables
tab2<-tibble(Model = c("Regressive", "Autoregressive"), 
           "R2" = round(c(best.learner_regr$adj.r.squared, best.learner_autoregr$adj.r.squared),3),
           "Residual standard error" = round(c(best.learner_regr$sigma, best.learner_autoregr$sigma),3),
           "10-fold CV RMSE" = round(c(cv_list.regr$aggregate(msr("regr.rmse")), 
                                      cv_list.autoregr$aggregate(msr("regr.rmse"))
                                      ),3),
"10-fold CV MAE" = round(c(cv_list.regr$aggregate(msr("regr.mae")), 
                                      cv_list.autoregr$aggregate(msr("regr.mae"))
                                      ),3)
           )

outname <- paste0(outdir, "GOF_Tab", Sys.Date(), ".csv")
write_csv(tab2, outname)

# summary table
t3<-bind_cols(
  cov=rownames(best.learner_regr$coefficients), 
  regr.est=paste0(round(best.learner_regr$coefficients[,1],3), 
                  " (",round(best.learner_regr$coefficients[,2],3), ")") ,
  regr.pval=ifelse(round(best.learner_regr$coefficients[,4],3) < 0.05, "p < 0.05", paste0("p = ", as.character(round(best.learner_regr$coefficients[,4],3)))),
  
  autoregr.est=paste0(round(best.learner_autoregr$coefficients[,1],3), 
                      " (", round(best.learner_autoregr$coefficients[,2],3)
                      , ")") ,
  autoregr.stderr= ifelse(round(best.learner_autoregr$coefficients[,4],3) < 0.05, "p < 0.05", as.character(round(best.learner_autoregr$coefficients[,4],3)))
)


t3
outname <- paste0(outdir, "ModelCoeff_Tab", Sys.Date(), ".csv")
write_csv(t3, outname)


#---- variable importance ----
best.learner_regr <- readRDS("stacked_Model_train_2023-08-30.RDS")
vimpRF<-data.frame(importance=best.learner_regr$model$regr.ranger$model$variable.importance)
vimpRF$variables<-rownames(vimpRF)
regrRF <-
  vimpRF %>%
  ggplot(aes(x=reorder(variables, +importance), y=importance))+
  geom_bar(position='dodge', stat='identity')+
  labs(x="Predictors", y="Importance")+
  coord_flip()+
  theme_classic()+
  theme(legend.background=element_blank(),
        panel.grid = element_blank(),
        legend.position = 'bottom',
        text = element_text(size=16),
        strip.text = element_text(size=16),
        legend.text = element_text(size=16,angle = 0), legend.title = element_text(size=14),
        legend.key.size = unit(1.5, 'cm'))

best.learner_autoregr <- readRDS("stacked_autoregrModel_train_2023-08-30.RDS")
vimpRF<-data.frame(importance=best.learner_autoregr$model$regr.ranger$model$variable.importance)
vimpRF$variables<-rownames(vimpRF)
autoregrRF <-
  vimpRF %>%
  ggplot(aes(x=reorder(variables, +importance), y=importance))+
  geom_bar(position='dodge', stat='identity')+
  labs(x="Predictors", y="Importance")+
  coord_flip()+
  theme_classic()+
  theme(legend.background=element_blank(),
        panel.grid = element_blank(),
        legend.position = 'bottom',
        text = element_text(size=16),
        strip.text = element_text(size=16),
        legend.text = element_text(size=16,angle = 0), legend.title = element_text(size=14),
        legend.key.size = unit(1.5, 'cm'))

pp<-ggarrange(regrRF, autoregrRF,
              ncol=2, nrow=1, 
              labels = c("A", "B"),
              common.legend = TRUE, legend="bottom")
pp
outname <- paste0(outdir, "VarImp_", Sys.Date(), ".png")
ggsave(pp, filename = outname, width = 28, height = 12, device='png', dpi=320)