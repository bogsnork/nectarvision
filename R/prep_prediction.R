#' Convert prediction output into dataframes. 
#'
#' \code{prep_prediction} converts prediction output into dataframes
#'
#' 
#' @param object a prediction object
#' 


prep_prediction <- function(object){
  
  pred <- object
  pred_cat <- pred[[1]] #extract predicted categories
  pred_box <- pred[[2]] #extract predicted boxes
  
  #read into data frames
  pred_box <- matrix(pred_box, nrow = 16, ncol = 4, byrow = F) %>% data.frame()
  names(pred_box) <- c("pred_xl", "pred_yt", "pred_xr", "pred_yb")
  
  pred_cat <- matrix(pred_cat, nrow = 16, ncol = 28, byrow = F) %>% data.frame()
  names(df_cat) <- 1:28
  
  assign("pred_cat", pred_cat, envir=globalenv())
  assign("pred_box", pred_box, envir=globalenv())
  
}