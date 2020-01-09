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

chart_ensembles_list <- function(Ensembles) {
  plist <- list()
  i <- 1
  date <- Ensembles[[length(Ensembles)]]
  plist[[i]] <- date[[1]]
  i <- i + 1
  for (j in seq(3, length(date), by = 2)) {
    plist[[i]] <-   date[[j]]
    i <- i + 1
  }
  return(plist)
}
