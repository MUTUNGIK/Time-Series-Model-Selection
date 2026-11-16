
recommend_model <- function(features, seasonal = TRUE) {
  
  if (seasonal) {
    tree <- readRDS("tree_seasonal.rds")
  } else {
    tree <- readRDS("tree_nonseasonal.rds")
  }
  
  predict(tree, newdata = features, type = "class")
}
