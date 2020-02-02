chart_models_list <- function(Models) {
  plist <- list()
  i <- 1
  for (feature in Models) {
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
  plist[[i]] <- Ensembles[[1]]
  i <- i + 1
  for (j in seq(3, length(Ensembles), by = 2)) {
    plist[[i]] <-   Ensembles[[j]]
    i <- i + 1
  }
  return(plist)
}
