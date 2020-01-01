chart_models_list <- function(Models) {
  last_index <-  length(Models)
  plist <- list()
  i <- 1
  j <- 1
  for (j in 1:length(Models[[last_index]])) {
    
    plist[[i]] <-   Models[[last_index]][[j]]$Feature_Importance_Plot
    plist[[i+1]] <- Models[[last_index]][[j]]$nn_model_plot
    plist[[i+2]] <- Models[[last_index]][[j]]$knn_model_plot
    plist[[i+3]] <- Models[[last_index]][[j]]$linear_model_plot
    i <- i + 4
  }
  return(plist)
}
