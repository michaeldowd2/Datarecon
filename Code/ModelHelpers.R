train_or_load_models <- function(Dateranges, Features, Labels, Training_Days) {
  model_lists = list()
  if (ANALYSIS_MODE == 0 | ANALYSIS_MODE == 1) { # train models for each date
    for (i in 1 : length(Dateranges)) {
      daterange = Dateranges[[i]]
      model_lists[i] = extract_training_data(daterange, Features, Labels, Mode = 'MODEL', Training_Days) %>%
                       train_feature_models() %>%
                       list()
    }
  }
  return(model_lists)
}

train_or_load_ensembles <- function(Dateranges, Features, Labels, Expansion_Sets, Training_Days) {
  ensembles = list()
  for (i in 1 : length(Dateranges)) {
    daterange = Dateranges[[i]]
    expansion_set = Expansion_Sets[[i]]
    ensembles[i] = extract_training_data(daterange, Features, Labels, Mode = 'ENSEMBLE', Training_Days) %>%
                   train_ensemble_models(expansion_set) %>%
                   list()
  }
  return(ensembles)
}

train_feature_models <- function(Dataset) {
  feature_models_list <- list()
  for (feature in FEATURE_GENERATORS) {
    feature_models <- train_model(Dataset$Features, Dataset$Labels, feature)
    feature_models_list <- list.append(feature_models_list, gen = feature_models)
  }
  return(feature_models_list)
}

train_model <- function(Features, Labels, Feature_Function) {
  names(Labels)[2] <- "Label"
  
  engineered_training = Features %>% 
                        match.fun(Feature_Function)() %>%
                        merge(Labels, by = "Date", all = FALSE)
  
  # the scaling factors will be used on new data and out of training data
  scaling_means <- engineered_training %>% summarise_at(vars(-Date), mean)
  scaling_sds <- engineered_training %>% summarise_at(vars(-Date), sd)

  training <- engineered_training %>%
              standardise(scaling_means, scaling_sds) %>%
              select(-Date)
  
  xgbTree_imp <- xgb_tree_importance(training)
  ggplot_imp <- ggplot(xgbTree_imp, top = 10) + xlab(Feature_Function)
  
  var_list <- xgbTree_imp$importance %>% rownames_to_column(var = "Variable") %>%
              mutate(Variable = str_replace_all(Variable,"`", "")) %>%
              filter(Overall > 0.01) %>% top_n(10, Overall)
  
  curated_training <- training %>%
                      select(one_of(c(var_list$Variable, 'Label')))

  model_properties = list('Feature_Function' = Feature_Function,
                          'Feature_Importance_Plot' = ggplot_imp, 
                          'Chosen_Features_Vector' = var_list$Variable,
                          'Curated_Training_Set' = curated_training,
                          'scaling_means' = scaling_means,
                          'scaling_sds' = scaling_sds)
  
  for (i in 1 : length(CARET_MODELS)) {
    grid <- CARET_MODELS[[i]]
    mod_name <- names(CARET_MODELS)[i]
    model <- train_caret_model(curated_training, mod_name, grid)
    mod_plot_name <- paste(mod_name, 'plot', sep = '_')
    #model_properties <- list.append(model_properties, model)
    #model_properties <- list.append(model_properties, plot(model, ylab = "RMSE"))
    #names(model_properties)[length(model_properties) - 1] <- mod_name
    #names(model_properties)[length(model_properties)] <- paste(mod_name, 'plot', sep = '_')
    
    model_properties[[mod_name]] <- model
    model_properties[[mod_plot_name]] <- plot(model, ylab = "RMSE")
  }

  return(model_properties)
}

test_models <- function(Dateranges, Features, Labels, Models, Training_Days) {
  i <- 1
  x <- 1
  model_tests <- list()
  for (i in range(1:length(Models))) {
    test_data <- extract_training_data(daterange, Features, Labels, Mode = 'ENSEMBLE', Training_Days)
    for (model in Models[[i]]) {
      model_tests[x] <-  test_model(test_data, model) %>% list()
    }
  }
  return(model_tests)
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

train_caret_model <- function(Training_Data, Method, Tuning_Grid) {
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
  dates <- Dataset$Date
  res <- Dataset %>% select(-Date)
  res <- data.frame(mapply('-', res, Means, SIMPLIFY = FALSE))
  res <- data.frame(mapply('/', res, SDs, SIMPLIFY = FALSE))
  res <- cbind(res, Date = dates)
  return(res)
}

de_standardise <- function(Dataset, Means, SDs) {
  res <- data.frame(mapply('*', res, SDs, SIMPLIFY = FALSE))
  res <- data.frame(mapply('+', Dataset, Means, SIMPLIFY = FALSE))
  return(res)
}

train_ensemble_models <- function(Dataset, Expansion_Set) {
  labels <- Dataset$Labels
  names(labels)[2] <- "Label"
  
  model_predictions <- model_predict(Dataset$Features, labels, Expansion_Set) %>%
                       select(-Date)
  
  xgbTree_imp <- xgb_tree_importance(model_predictions)
  ggplot_imp <- ggplot(xgbTree_imp, top = 10) + xlab('Model')
  
  ensemble_model_list <- list(ggplot_imp)
  
  for (i in 1 : length(CARET_MODELS)) {
    grid <- CARET_MODELS[[i]]
    ensemble_name <- names(CARET_MODELS)[i]
    model <- train_caret_model(model_predictions, ensemble_name, grid)
    ensemble_plot_name <- paste(ensemble_name, 'plot', sep = '_')
    ensemble_model_list[[ensemble_name]] <- model
    ensemble_model_list[[ensemble_plot_name]] <- plot(model, ylab = "RMSE")
  }
  
  return(ensemble_model_list)
}
# Predicion Features for ensemble
model_predict <- function(Features, Labels, Expansion_Set) {
  end_results <- Labels
  for (i in 1 : length(Expansion_Set)) {
    expansion <-  Expansion_Set[[i]]
    
    engineered <- Features %>% 
      match.fun(expansion$Feature_Function)() %>%
      merge(Labels, by = "Date", all = FALSE) %>%
      standardise(expansion$scaling_means, expansion$scaling_sds) %>%
      select(one_of(c(expansion$Chosen_Features_Vector, 'Label', 'Date')))
    
    if (i == 1) { end_results <- engineered %>% select(Date, Label) }
    results <- engineered %>% select(Date)
    training_data <- engineered %>% select(-c(Label, Date))
    
    for (i in seq(7, length(expansion), by = 2))  {
      model <- expansion[[i]]
      predictions <- predict(model, newdata = training_data)
      column_name <- paste(names(expansion)[i], expansion$Feature_Function, sep='_')
      results[[column_name]] <- predictions
    }
    end_results <- end_results %>%
      merge(results, by = "Date", all = FALSE)
  }
  return(end_results)
}

model_summary_dataframe <- function(Dataset, Expansion_Set) {
  labels <- Dataset$Labels
  names(labels)[2] <- "Label"
  predictions <- model_predict(Dataset$Features, labels, Expansion_Set)
  df <- data.frame(matrix(ncol=4, nrow=0, dimnames=list(NULL, c('date', 'model_type', 'feature_type', 'test_accuracy'))))
}