#' Print a sample image with bounding boxes and labels. 
#'
#' \code{output_image_print} prints a sample image with bounding boxes and labels to a graphics device.
#'
#' 
#' @param img_data requires a dataframe or tibble containing data for image.  I must contain at least the following variables: 
#' "file_name" (name of the image), 
#' "categories" (vector of category codes in image), 
#' "name" (vector of category names in image), 
#' "xl_orig", "yt_orig", "xr_orig", "yb_orig" (vectors of left, top, right and bottom coordinate values of the bounding boxes in the image)
#' @param pred_data requires a dataframe containing prediction data.  
#' @param img_name either filename of an image or "random".
#' @param pred_data either "none" (default) or a dataframe containing prediction data 
#' for a single image which must include:
#' "pred_xl", "pred_yt", "pred_xr", "pred_yb".  
#' @param img_dir name of the image directory if it is in the working directory, or alternatively a filepath. Defaults to "source_photos".  
#' @param label either "categories" or "name" indicating whether to print the category name or the category code. 
#' @param cex a numeric determining the size of the label text.
#' @param img_save_name path and filename to save the image (defaults to "none")  
#' @param anchors whether to draw anchor boxes (defaults to F)
#' @param ground_truth whether to show ground truth boxes (defaults to F)

output_image_print <- function(
  img_data, pred_data = "none", img_name = "random",  img_dir = "source_photos",
  label = "categories", cex = 4, img_save_name = "none", 
  anchors = F, ground_truth = F){

  #select image and get data 
  ifelse(img_name == "random", 
         imginfo <- img_data[sample(nrow(img_data), 1),], #select a random image
         imginfo <- img_data[which(img_data$file_name == img_name),]) #or select chosen image
  
  ifelse(label == "name", 
         label <- (imginfo$name %>% str_split(pattern = ", "))[[1]], #prep category name as label
         label <- (imginfo$categories %>% str_split(pattern = ", "))[[1]]) #prep category code as label
  
  ifelse(img_name == "random", 
         img_name <- imginfo$file_name, #filename of random image
         img_name <- img_name) #or stated image name
  
  #read image
  img <- image_read(file.path(here(), img_dir, img_name))
  
    #get image attributes
    img_w <- image_info(img)[[1,"width"]]; img_h <- image_info(img)[[1,"height"]]
  
  #split out individual coordinates
  orig_xl <- (imginfo$xl_orig %>% str_split(pattern = ", "))[[1]]
  orig_xr <- (imginfo$xr_orig %>% str_split(pattern = ", "))[[1]]
  orig_yt <- (imginfo$yt_orig %>% str_split(pattern = ", "))[[1]]
  orig_yb <- (imginfo$yb_orig %>% str_split(pattern = ", "))[[1]]
  
  #prep labels
  pad <- 0.1 #proportion to pad textbox by

  #prep anchors
  anks <- data.frame(a_xr = c(0, 0, 0, 0, 0.25, 0.25, 0.25, 0.25, 0.5, 0.5, 0.5, 0.5, 0.75, 0.75, 0.75, 0.75),
                     a_yb = c(0, 0.25, 0.5, 0.75, 0, 0.25, 0.5, 0.75, 0, 0.25, 0.5, 0.75, 0, 0.25, 0.5, 0.75),
                     a_xl = c(0.25, 0.25, 0.25, 0.25, 0.5, 0.5, 0.5, 0.5, 0.75, 0.75, 0.75, 0.75, 1, 1, 1, 1), 
                     a_yt = c(0.25, 0.5, 0.75, 1, 0.25, 0.5, 0.75, 1, 0.25, 0.5, 0.75, 1, 0.25, 0.5, 0.75, 1))
    #rescale
    anks$a_xl <- anks$a_xl * img_w; anks$a_xr <- anks$a_xr * img_w
    anks$a_yt <- anks$a_yt * img_h; anks$a_yb <- anks$a_yb * img_h
    
  #plot 
  img <- image_draw(img)  
    
    #label attributes
    if(ground_truth){
      textHeight <- graphics::strheight(label, cex = cex)
      textWidth <- graphics::strwidth(label, cex = cex)}
  
  #plot anchors
  if(anchors){
    for(m in 1:nrow(anks)){
    rect(xleft = anks$a_xl[m], ybottom = anks$a_yb[m], 
         xright = anks$a_xr[m], ytop = anks$a_yt[m],
         border = "red", lwd = 5)}}
  
  #plot ground truth boxes
  if(ground_truth){
  for (i in 1:imginfo$cnt) {
    rect(xleft = orig_xl[i], ybottom = orig_yb[i],
         xright = orig_xr[i], ytop = orig_yt[i],
         border = "blue", lwd = 5)
    
    rect(xleft = as.integer(orig_xr[i]) - (textWidth[i] * (1 + pad)), 
         ybottom = as.integer(orig_yt[i]) - (textHeight[i] * (1 + pad)), 
         xright = as.integer(orig_xr[i]) + (textWidth[i] * pad), 
         ytop = as.integer(orig_yt[i]) + (textHeight[i] * pad),
         col = "white", border = NA) 
    
    text(x = as.integer(orig_xr[i]), y = as.integer(orig_yt[i]), labels = label[i], 
         cex = cex, 
         adj = c(1,1), 
         col = "blue", vfont = c("sans serif", "bold"))
  }}
  
  #plot predicted boxes
  if(is.data.frame(pred_data)){
    
    #rescale
    pred_data$pred_xl <- pred_data$pred_xl * img_w 
    pred_data$pred_xr <- pred_data$pred_xr * img_w
    pred_data$pred_yt <- pred_data$pred_yt * img_h
    pred_data$pred_yb <- pred_data$pred_yb * img_h
    
    for (n in 1:nrow(pred_data)){
      rect(xleft = pred_data$pred_xl[n], 
           xright = pred_data$pred_xr[n], 
           ytop = pred_data$pred_yt[n],
           ybottom = pred_data$pred_yb[n], 
           border = "white", lwd = 5)
    }
  }
  
  
  #save and plot
  dev.off()
  
  if(img_save_name != "none"){
    image_write(image = img, path = file.path(here(), img_save_name))}
  
  print(img)
}




