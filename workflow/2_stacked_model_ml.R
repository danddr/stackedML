# Stacked Machine learning model for Aedes albopictus seasonality forecast
# Daniele Da Re, Carmelo Bonannella
library("mlr3pipelines")
library("mlr3tuning")
library("mlr3viz")
library("mlr3extralearners")
library("mlr3learners")
library("mlr3fselect")
library("mlr3temporal")
library("paradox")
library("data.table")
library("dplyr")
library("lubridate")
library("skimr")
out.dir<-"./outputs/"

#---- 1. load all observations ----
bio.matrix<-readRDS("ovitrapsAllObservations_2023-08-28.RDS")
head(bio.matrix)
names(bio.matrix)

#---- 2. modelling ----
Autoregr_model = 0 # add an autoregressive component to the model (1: Yes; 0: No)

if(Autoregr_model == 1) {
  vars <- unique(make.names(unlist(sapply(c("value.lag1", 
                                            "medianTweek.lag2", "medianTweek.lag3",
                                            "cumPrecweek.lag2", "cumPrecweek.lag3",
                                            "medianPhotoweek.lag2", "medianPhotoweek.lag3",
                                            "S1.52", "C1.52", "S2.52", "C2.52"), function(i){names(bio.matrix)[grep(i, names(bio.matrix))]}))))
} else {
  vars <- unique(make.names(unlist(sapply(c("medianTweek.lag2", "medianTweek.lag3",
                                            "cumPrecweek.lag2", "cumPrecweek.lag3",
                                            "medianPhotoweek.lag2", "medianPhotoweek.lag3",
                                            "S1.52", "C1.52", "S2.52", "C2.52"), function(i){names(bio.matrix)[grep(i, names(bio.matrix))]}))))
  }

formula <- as.formula(paste('value ~ ', paste(vars, collapse = "+")))
r.sel <- stats::complete.cases(bio.matrix[,all.vars(formula)])

#assigning id
df.s <- bio.matrix[which(r.sel),c(all.vars(formula),"ID", "year", "date",  "Country", "Region", "externalVal", "internalVal")]
# skim(df.s) 

# Subset the dataset into train and test df
## train df
train.df<- df.s %>% 
  filter(internalVal == 1 & externalVal == 0)
date.train<-train.df[,c("ID",  "date", "Country", "Region", "externalVal", "internalVal")]  
id <- train.df$ID
train.df<-train.df %>% 
  select(-ID, -year, -date, -Region, -Country, -externalVal, -internalVal)  
train.df<-train.df[,-1] # ugly but functional solution to avoid the grouping variable

## internal test df
test.df<- df.s %>% 
  filter(internalVal == 0 & externalVal == 0)
date.test<-test.df[,c("ID", "date", "Country", "Region", "externalVal", "internalVal")]
id.test<- test.df$ID
test.df<-test.df %>%
  select(-ID, -year, -date, -Region, -Country, -externalVal, -internalVal) 
test.df<-test.df[,-1] 

## external test df
Ext_test.df<- df.s %>% 
  filter(externalVal == 1)
date.Ext_test<-Ext_test.df[,c("ID", "date", "Country", "Region", "externalVal", "internalVal")]
id.Ext_test<- Ext_test.df$ID
Ext_test.df<-Ext_test.df %>%
  select(-ID, -year, -date, -Region, -Country, -externalVal, -internalVal) 
Ext_test.df<-Ext_test.df[,-1]

## 2.1 Create task + Tile_ID for grouping ----
#define response variable
tv <- all.vars(formula)[1]
task <- as_task_regr(train.df, target = tv,  id = "eggs_regr")

# Associate vector with blocking factor
task$cbind(data.table("ID" = id))

# Tell the task about the groups
task$set_col_roles("ID", roles = "group")

## 2.2 Tuning ----
fts<-names(train.df[,2:ncol(train.df)])

stacked_graph <- gunion(
  list(po("learner_cv", lrn("regr.xgboost", predict_type = "response", nthread = 5)), #importance for xgboos?
       po("learner_cv", lrn('regr.ranger', predict_type = "response", num.threads = 5, importance="permutation")),
       po("learner_cv", lrn('regr.gbm', predict_type = "response")), 
       po("learner_cv", lrn('regr.cubist', predict_type = "response"))
  )) %>>%
  po("featureunion") %>>%
  lrn("regr.lm", id = "lm_ensemble", predict_type = "se")
# lrn("regr.glmnet", id = "glm_ensemble", predict_type = "response", family="poisson")

# stacked_graph$plot(html = FALSE)
stacked_graph$keep_results <- T
stacked_learner <- as_learner(stacked_graph)

stacked.search_space <- paradox::ps(
  regr.ranger.num.trees =  p_int(lower = 200, upper = 500),
  regr.ranger.mtry =  p_int(lower = round((length(fts)-1) * 0.5), upper = (length(fts)-3) ),
  regr.ranger.min.node.size = p_int(lower = 5, upper = 10),
  regr.xgboost.nrounds = p_int(10, 30),
  regr.xgboost.max_depth = p_int(4, 10),
  regr.xgboost.eta = p_dbl(0.2, 0.4),
  regr.xgboost.subsample = p_dbl(0.9, 1),
  regr.xgboost.min_child_weight = p_int(1, 4),
  regr.xgboost.colsample_bytree = p_dbl(0.5, 0.6),
  regr.gbm.n.trees =p_int(lower = 200, upper = 10000),
  # regr.gbm.shrinkage =p_dbl(lower = 0.001, upper = 5),
  regr.gbm.interaction.depth =p_int(lower = 1, upper = 5),
  regr.gbm.cv.folds = p_int(lower = 5, upper = 5), 
  #regr.cubist.neighbors = p_int(lower = 0, upper = 2), 
  regr.cubist.committees = p_int(lower = 1, upper = 10)
)

#define tuning setting
tune.eml <- TuningInstanceSingleCrit$new(
  task = task,
  learner = stacked_learner,
  resampling = rsmp("cv", folds = 10),
  measure = msr("regr.rmse"),
  search_space = stacked.search_space,
  terminator = trm("evals", n_evals = 10)
)

set.seed(255)
tuner <- tnr("random_search")
future::plan("multisession", workers = 10)
tuner$optimize(tune.eml) #run tuning

#save tuned hyperparameters
if(Autoregr_model == 1) {
  outname<-paste0(out.dir, "/TunedHyperParam_autoregrModel_", Sys.Date(), ".RDS")
  saveRDS(tune.eml, outname)
} else {
  outname<-paste0(out.dir, "/TunedHyperParam_Model_", Sys.Date(), ".RDS")
  saveRDS(tune.eml, outname)
}

#create new learner just to separate the two learners
best.learner <- stacked_learner
best.learner$param_set$values <- tune.eml$result$learner_param_vals[[1]]

## 2.3 Stacking ----
# Create task + Tile_ID for groupin = "value",  id = "eggs_regr")
task <-  as_task_regr(train.df, target = "value",  id = "eggs_regr")
task$cbind(data.table("ID" = id))

# Tell the task about the groups
task$set_col_roles("ID", roles = "group")

# train the best model
best.learner$train(task)
summary(best.learner$model$lm_ensemble$model)
AIC(best.learner$model$lm_ensemble$model)

if(Autoregr_model == 1) {
  outname<-paste0(out.dir, "/stacked_autoregrModel_train_", Sys.Date(), ".RDS")
  saveRDS(best.learner, outname)
} else {
  outname<-paste0(out.dir, "/stacked_Model_train_", Sys.Date(), ".RDS")
  saveRDS(best.learner, outname)
}

# model diagnostics
# car::vif(best.learner$model$lm_ensemble$model)
# car::marginalModelPlots(best.learner$model$lm_ensemble$model)
# car::residualPlots(best.learner$model$lm_ensemble$model) 
#
##----2.4 internal cross validation ----
myresmpl <- rsmp("cv",folds=10L)

cv_pedictions <- resample(
  task = task,
  learner = best.learner,
  resampling = myresmpl,
)

# as.list(cv_pedictions$predictions()) #extract prediction from each iteration in a list, we can the tile id by joining back the column from the original task
# cv_pedictions$aggregate(msr("regr.rmse"))
# cv_pedictions$data$response
# cv_pedictions$score(msr("regr.rmse"))

if(Autoregr_model == 1) {
  outname<-paste0(out.dir, "/stacked_autoregrModel_train_internalCV_", Sys.Date(), ".RDS")
  saveRDS(cv_pedictions, outname)
} else {
  outname<-paste0(out.dir, "/stacked_Model_train_internalCV_", Sys.Date(), ".RDS")
  saveRDS(cv_pedictions, outname)
}

#---- 2.5 variable importance ----
# best.learner<-readRDS("tacked_Model_train_2023-08-28.RDS")
# best.learner<-readRDS("stacked_autoregrModel_train_2023-08-28.RDS")
# vimpRF<-data.frame(importance=best.learner$model$regr.ranger$model$variable.importance)
# vimpRF$variables<-rownames(vimpRF)
# vimpRF %>% 
#   ggplot(aes(x=reorder(variables, +importance), y=importance))+
#   geom_bar(position='dodge', stat='identity')+
#   labs(x="Predictors", y="Importance")+
#   coord_flip()+
#   theme_classic()+
#   theme(legend.background=element_blank(),
#         panel.grid = element_blank(),
#         legend.position = 'bottom',
#         text = element_text(size=16), 
#         strip.text = element_text(size=16),
#         legend.text = element_text(size=16,angle = 0), legend.title = element_text(size=14),
#         legend.key.size = unit(1.5, 'cm'))

#---- 3. Prediction ----
# estimate ci https://gist.github.com/thengl/eafb19b99b20042c689c9ea899435f57
# best.learner<-readRDS("/stacked_Model_train_2023-08-28.RDS")

# Note from Carmelo:
# To add the confidence interval in the model predictions, the algorithm needs to support
# the following condition (check each algorithm/learner page, i.e. ranger:https://mlr3learners.mlr-org.com/reference/mlr_learners_regr.ranger.html) 
#
# Predict Types: “se”
#
# without that, it's not possible to generate the confidence interval.
# To generate it in the prediction set (not on the task) you need to specify "predict_type = "<Prediction>"
# in the predict method.
# See: https://mlr3.mlr-org.com/reference/predict.Learner.html

# predict on the training dataset
pred.train <- best.learner$predict(task)
# predict on the internal testing dataset
pred.test = predict(best.learner, newdata = test.df, predict_type = "<Prediction>")
# predict on the external testing dataset
pred.test_Ext <- predict(best.learner, newdata = Ext_test.df, predict_type = "<Prediction>")

fitted.stack <- data.frame("ID"= date.train$ID,
                           "Region"= date.train$Region,
                           "Country"= date.train$Country,
                           "value"= round(pred.train$response, 3),
                           "se" = round(pred.train$se, 3),
                           "date"= date.train$date,
                           "model" = "stacked", 
                           "partition"="Train")

test.stack <- data.frame(  "ID"= date.test$ID,
                           "Region"= date.test$Region,
                           "Country"= date.test$Country,
                           "value"= round(pred.test$response, 3),
                           "se" = round(pred.test$se, 3),
                           "date"= date.test$date,
                           "model" = "stacked", 
                           "partition"="Test_int")

test.stack_Ext <- data.frame("ID"= date.Ext_test$ID,
                             "Region"= date.Ext_test$Region,
                             "Country"= date.Ext_test$Country,
                             "value"= round(pred.test_Ext$response, 3),
                             "se" = round(pred.test_Ext$se, 3),
                             "date"= date.Ext_test$date,
                             "model" = "stacked", 
                             "partition"="Test_ext")


myPred <- rbind(fitted.stack, test.stack, test.stack_Ext)

if(Autoregr_model == 1) {
  outname<-paste0(out.dir, "/mlr3_stack_AutoRegrPred_", Sys.Date(), ".RDS")
  saveRDS(myPred, outname)
} else {
  outname<-paste0(out.dir, "/mlr3_stack_Pred_", Sys.Date(), ".RDS")
  saveRDS(myPred, outname)
}

# export also observations for comparison
myObs<-bind_rows(cbind(date.train, value=train.df$value, partition="Train", model="Observations"), 
                 cbind(date.test, value=test.df$value, partition="Test_int", model="Observations"), 
                 cbind(date.Ext_test, value=Ext_test.df$value, partition="Test_ext", model="Observations")
) %>% 
  mutate(ID = as.factor(ID))


if(Autoregr_model == 1) {
  outname<-paste0(out.dir, "/mlr3_stack_AutoRegrObs_", Sys.Date(), ".RDS")
  saveRDS(myObs, outname)
} else {
  outname<-paste0(out.dir, "/mlr3_stack_Obs_", Sys.Date(), ".RDS")
  saveRDS(myObs, outname)
}

# 4. Spatial predictions -----
# https://mlr.mlr-org.com/articles/tutorial/predict.html
harm = T

if(Autoregr_model == 1) {
  stop("load NOT autoregressive model")
} else {
  spatial_cov <-readRDS("./remlr3script/spatialPredictors_ITA_2022_2023-08-28.RDS")
  if(harm==T){
    
    # Note from Carmelo: 
    # Harmonic values are the same in 1yr, but there are 13yrs in the dataset
    # Better use unique that + unique tile_ID for repetition
    # The spatial_cov dataset is ordered by tile_ID anyway, so it's easier
    
    myL <- length(unique(bio.matrix$year))*length(unique(spatial_cov$tile_ID))
    
    myHarm<-bio.matrix %>%
      mutate(year=lubridate::year(date),
             week=lubridate::week(date)
      ) %>%
      filter(year==2022, ID ==4228) %>% # I chose one of them for a given year because they are all identical
      select(year, week, S1.52, C1.52, S2.52,  C2.52)
    
    myHarm = as.data.frame(sapply(myHarm[,4:7], rep.int, times=myL))
    spatial_cov <- cbind(spatial_cov, myHarm)
    
    outname<-paste0(out.dir, "spatialPredictors_2022_", Sys.Date(), ".RDS")
    saveRDS(spatial_cov, outname)
  }
  spatial_cov<-na.omit(spatial_cov)
  test.sp.id<-spatial_cov$tile_ID
  test.sp<-spatial_cov %>% 
    select(-x, -y, -tile_ID, -temporal_ID,  -join.ID)
  
  test.sp1 = test.sp[1:5400726, ]
  test.sp2 = test.sp[5400727:10801453, ]
  
  spPred1 <- predict(best.learner,  newdata = test.sp1)
  spPred2 <- predict(best.learner,  newdata = test.sp2)
  spPred = c(spPred1, spPred2)
  
  my_spPred<-spatial_cov %>% 
    select(x, y, tile_ID, temporal_ID,  join.ID) %>% 
    bind_cols(spatial_pred = round(spPred))
  
  outname<-paste0(out.dir, "spatialPredictions_2022_", Sys.Date(), ".RDS")
  saveRDS(my_spPred, outname)
  }
