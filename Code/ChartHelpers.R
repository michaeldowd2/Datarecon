chart_models_list <- function(Models) {
  plist <- list()
  i <- 1
  date <- Models[[ length(Models)]]
  for (feature in date) {
    plist[[i]] <- feature[[2]]
    i <- i + 1
    for (j in seq(8, length(feature), by = 2)) {
      plist[[i]] <-   feature[[j]]
      i <- i + 1
    }
  }
  return(plist)
}
