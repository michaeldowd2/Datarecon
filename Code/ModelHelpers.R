train_or_load_models <- function(Dateranges, Features, Labels, Training_Days) {
  model_lists = list()
  if (ANALYSIS_MODE == 0 | ANALYSIS_MODE == 1) { # train models for each date
    for (i in 1 : length(Dateranges)) {
      daterange = Dateranges[[i]]
      model_lists[i] = extract_training_data(daterange, Features, Labels, Mode = 'MODEL', Training_Days) %>%
        train_models() %>%
        list()
    }
  }
  return(model_lists)
}

train_or_load_ensembles <- function(Dateranges, Features, Labels, Model_Lists, Training_Days) {
  ensembles = list()
  if (ANALYSIS_MODE == 0 | ANALYSIS_MODE == 1) { # train models for each date
    for (i in 1 : length(Dateranges)) {
      daterange = Dateranges[[i]]
      model_list = Model_Lists[[i]]
      ensembles[i] = extract_training_data(daterange, Features, Labels, Mode = 'ENSEMBLE', Training_Days) %>%
        train_ensemble(model_list) %>%
        list()
    }
  }
  return(ensembles)
}

train_models <- function(Dataset) {

  ma_crossover <- train_model(Dataset$Features, Dataset$Labels, generate_MA_crossover_data)
  dates <- train_model(Dataset$Features, Dataset$Labels, generate_dates_data)
  
  model_list <- list(ma_crossover, dates)
  names(model_list) <- c('ma_crossover', 'dates')
  return(model_list)
}

train_model <- function(Features, Labels, Feature_Function) {
  names(Labels)[2] <- "Label"
  
  engineered_training = Features %>% Feature_Function() %>%
                        merge(Labels, by = "Date", all = FALSE)
  
  # the scaling factors will be used on new data and out of training data
  scaling_means <- engineered_training %>% summarise_at(vars(-Date), mean)
  scaling_sds <- engineered_training %>% summarise_at(vars(-Date), sd)

  training <- engineered_training %>% select(-Date) %>%
              standardise(scaling_means, scaling_sds)
  
  xgbTree_imp <- xgb_tree_importance(training)
  ggplot_imp <- ggplot(xgbTree_imp, top = 10)
  
  var_list <- xgbTree_imp$importance %>% rownames_to_column(var = "Variable") %>%
              mutate(Variable = str_replace_all(Variable,"`", "")) %>%
              filter(Overall > 0.01) %>% top_n(10, Overall)
  
  # Include the generated in the output if in Debug
  curated_training <- 'ENABLE DEBUG'
  if (DEBUG == 1) {
    curated_training <- engineered_training %>%
                        select(one_of(c('Date', var_list$Variable, 'Label')))
  }
  
  # Return the model and other properties
  model_properties = list(as.character(substitute(Feature_Function)),
                          ggplot_imp, 
                          var_list$Variable,
                          curated_training,
                          scaling_means,
                          scaling_sds)
  
  names(model_properties) = c('Feature_Function',
                              'Feature_Importance_Plot', 
                              'Chosen_Features_Vector',
                              'Curated_Training_Set',
                              'scaling_means',
                              'scaling_sds')
  
  return(model_properties)
}

xgb_tree_importance <- function(Training_Data) {
  xgb_grid_1 <- expand.grid(nrounds = 1, eta = 0.3, max_depth = 5, 
                            gamma = 0, colsample_bytree=1, 
                            min_child_weight=1, subsample = 1)
  
  xgb_tree <-  train(Label ~ ., data = Training_Data,
                     trControl = trainControl(method="none"),
                     metric="logLoss", tuneGrid = xgb_grid_1, method = "xgbTree")
  
  return(varImp(xgb_tree, scale = FALSE))
}

standardise <- function(Dataset, Means, SDs) {
  res <- data.frame(mapply('-', Dataset, Means, SIMPLIFY = FALSE))
  res <- data.frame(mapply('/', res, SDs, SIMPLIFY = FALSE))
  return(res)
}

de_standardise <- function(Dataset, Means, SDs) {
  res <- data.frame(mapply('*', res, SDs, SIMPLIFY = FALSE))
  res <- data.frame(mapply('+', Dataset, Means, SIMPLIFY = FALSE))
  return(res)
}

train_ensemble <- function(Dataset, Models) {
  ensemble = 'Ensemble_1'
  return(ensemble)
}