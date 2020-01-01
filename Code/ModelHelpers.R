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
  
  curated_training <- training %>%
                      select(one_of(c(var_list$Variable, 'Label')))
  
  nn_model <- caret_model(curated_training, "nnet", expand.grid(size = (5:10), decay = c(1.0e-3, 1.0e-2, 1.0e-1)))
  ggplot_nn_model <- plot(nn_model)
  
  knn_model <- caret_model(curated_training, "kknn", expand.grid(kmax = seq(4, 20, by = 2), kernel = c('rectangular', 'gaussian', 'optimal'), distance = 2))
  ggplot_knn_model <- plot(knn_model)
  
  linear_model <- caret_model(curated_training, "gbm", expand.grid(interaction.depth = c(1,5,9), n.trees = (1:30)*50, shrinkage = 0.1, n.minobsinnode=20))
  ggplot_linear_model <- plot(linear_model)
  
  # Return the model and other properties
  model_properties = list(as.character(substitute(Feature_Function)),
                          ggplot_imp, 
                          var_list$Variable,
                          curated_training,
                          scaling_means,
                          scaling_sds,
                          nn_model,
                          ggplot_nn_model,
                          knn_model,
                          ggplot_knn_model,
                          linear_model,
                          ggplot_linear_model)
  
  names(model_properties) = c('Feature_Function',
                              'Feature_Importance_Plot', 
                              'Chosen_Features_Vector',
                              'Curated_Training_Set',
                              'scaling_means',
                              'scaling_sds',
                              'nn_model',
                              'nn_model_plot',
                              'knn_model',
                              'knn_model_plot',
                              'linear_model',
                              'linear_model_plot')
  
  return(model_properties)
}

xgb_tree_importance <- function(Training_Data) {
  parameter_grid <- expand.grid(nrounds = 1, eta = 0.3, max_depth = 5, 
                            gamma = 0, colsample_bytree=1, 
                            min_child_weight=1, subsample = 1)
  
  model <-  train(Label ~ ., data = Training_Data,
                  trControl = trainControl(method="none"),
                  metric="logLoss", 
                  tuneGrid = parameter_grid, 
                  method = "xgbTree")
  
  return(varImp(model, scale = FALSE))
}

caret_model <- function(Training_Data, Method, Tuning_Grid) {
  fitControl <- trainControl(method = "repeatedcv",
                             number = 3,
                             repeats = 3)

  if (Method == 'nnet') {
    model <- train(Label ~ ., data = Training_Data, 
                   method = Method, 
                   trControl = fitControl,
                   verbose = FALSE, 
                   tuneGrid = Tuning_Grid,
                   trace = FALSE)
  }
  else {
    model <- train(Label ~ ., data = Training_Data, 
                   method = Method, 
                   trControl = fitControl,
                   verbose = FALSE, 
                   tuneGrid = Tuning_Grid)
  }
  
  return(model)
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