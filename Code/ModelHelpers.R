train_models <- function(Dates, Features, Labels) {
  models <- extract_training_data(Dates, Features, Labels, Mode = 'MODEL', MODEL_TRAINING_DAYS) %>%
    train_feature_models()
  return(models)
}

train_ensembles <- function(Dates, Features, Labels, Models) {
  ensembles = extract_training_data(Dates, Features, Labels, Mode = 'ENSEMBLE', ENSEMBLE_TRAINING_DAYS) %>%
      train_ensemble_models(Models)
  return(ensembles)
}

train_feature_models <- function(Dataset) {
  feature_models_list <- list()
  for (feature in FEATURE_GENERATORS) {
    feature_models_list[[feature]] <-  train_model(Dataset$Features, Dataset$Labels, feature)
   # feature_models_list <- list.append(feature_models_list, feature$Feature_Function = feature_models)
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

train_ensemble_models <- function(Dataset, Models) {
  labels <- Dataset$Labels
  names(labels)[2] <- "Label"
  
  model_predictions <- model_predict(Dataset$Features, labels, Models) %>%
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
model_predict <- function(Features, Labels, Models) {
  end_results <- Labels
  for (i in 1 : length(Models)) {
    expansion <-  Models[[i]]
    
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
    end_results <- end_results %>% merge(results, by = "Date", all = FALSE)
  }
  return(end_results)
}

#ensemble predictions
ensemble_predict <- function(Model_Predictions, Ensembles) {
  end_results <- Model_Predictions %>% select(Date, Label)
  
  for (i in seq(2, length(Ensembles), by = 2))  {
    model <- Ensembles[[i]]
    predictions <- predict(model, newdata = training_data)
    column_name <- paste(names(expansion)[i], expansion$Feature_Function, sep='_')
    results[[column_name]] <- predictions
  }
  end_results <- end_results %>% merge(results, by = "Date", all = FALSE)
  return(end_results)
  
}

summary_dataframe <- function(Dates, Features, Labels, Models, Ensembles) {
  res <- data.frame(matrix(ncol=6, nrow=0, dimnames=list(NULL, c('Date','Model_Name', 'Feature_Name','Dataset', 'RMSE', 'Rsquared'))))
  date <- Dates$Date[1]
  feature_replacer <- vector(mode='character',length=length(FEATURE_GENERATORS))
  names(feature_replacer) <- FEATURE_GENERATORS
  model_replacer <- vector(mode='character',length=length(CARET_MODELS))
  names(model_replacer) <- names(CARET_MODELS)
  
  model_training_data <- extract_training_data(Dates, Features, Labels, Mode = 'MODEL',MODEL_TRAINING_DAYS)
  names(model_training_data$Labels)[2] <- "Label"
  ensemble_training_data <- extract_training_data(Dates, Features, Labels, Mode = 'ENSEMBLE', ENSEMBLE_TRAINING_DAYS)
  names(ensemble_training_data$Labels)[2] <- "Label"
  
  model_training_predictions <- model_predict(model_training_data$Features, model_training_data$Labels, Models)
  model_test_predictions <- model_predict(ensemble_training_data$Features, ensemble_training_data$Labels, Models)
  
  for (i in 3:ncol(model_test_predictions)) {
    model_name <-  str_replace_all(colnames(model_test_predictions)[i], feature_replacer) %>% str_replace_all("_", " ") %>% trimws()
    feature_name <- str_replace_all(colnames(model_test_predictions)[i], model_replacer) %>% str_replace_all("_", " ") %>% trimws()
    
    model_train_acc <- postResample(pred = model_training_predictions[i], obs = model_training_predictions[2])
    train_row <- data.frame(date, model_name, feature_name, "Train", model_train_acc['RMSE'], model_train_acc['Rsquared'])
    names(train_row) <- c('Date', 'Model_Name', 'Feature_Name', 'Dataset', 'RMSE', 'Rsquared')
    res <- rbind(res, train_row)
    
    model_test_acc <- postResample(pred = model_test_predictions[i], obs = model_test_predictions[2])
    test_row <- data.frame(date, model_name, feature_name, "Test", model_test_acc['RMSE'], model_test_acc['Rsquared'])
    names(test_row) <- c('Date', 'Model_Name', 'Feature_Name', 'Dataset', 'RMSE', 'Rsquared')
    res <- rbind(res, test_row)
  }
  
  
  # ensemble_predictions <- ensemble_predict(ensemble_test_data$Features, ensemble_test_data$Labels, Models)
  # for (i in 3:ncol(predictions)) {
  #   model_name <-  str_replace_all(colnames(predictions)[i], '_ensemble')
  #   feature_name <- 'ensemble'
  #   acc <- postResample(pred = ensemble_predictions[i], obs = ensemble_predictions[2])
  #   rmse <- acc['RMSE']
  #   rsquared <- acc['Rsquared']
  #   df <- data.frame(date, model_name, feature_name, "Test", rmse, rsquared)
  #   names(df) <- c('Date', 'Model_Name', 'Feature_Name', 'Dataset', 'RMSE', 'Rsquared')
  #   res <- rbind(res, df)
  # }
  
  return(res)
}