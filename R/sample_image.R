#' Print a sample image with bounding boxes and labels. 
#'
#' \code{sample_image} prints a sample image with bounding boxes and labels to a graphics device.
#'
#' 
#' @param img_data requires a dataframe or tibble containing data for image.  I must contain at least the following variables: 
#' "file_name" (name of the image), 
#' "categories" (vector of category codes in image), 
#' "name" (vector of category names in image), 
#' "xl_orig", "yt_orig", "xr_orig", "yb_orig" (vectors of left, top, right and bottom coordinate values of the bounding boxes in the image)
#' @param img_name either filename of an image or "random".
#' @param img_dir name of the image directory if it is in the working directory, or alternatively a filepath. Defaults to "source_photos".  
#' @param label either "categories" or "name" indicating whether to print the category name or the category code. 
#' @param cex a numeric determining the size of the label text.

sample_image <- function(img_data, img_name = "random",  img_dir = "source_photos", label = "categories", cex = 4){
  
  ifelse(img_name == "random", 
         imginfo <- img_data[sample(nrow(img_data), 1),], #select a random image
         imginfo <- img_data[which(img_data$file_name == img_name),]) #or select chosen image
  
  ifelse(label == "name", 
         label <- (imginfo$name %>% str_split(pattern = ", "))[[1]], #prep category name as label
         label <- (imginfo$categories %>% str_split(pattern = ", "))[[1]]) #prep category code as label
  
  x_left <- (imginfo$xl_orig %>% str_split(pattern = ", "))[[1]]
  x_right <- (imginfo$xr_orig %>% str_split(pattern = ", "))[[1]]
  y_top <- (imginfo$yt_orig %>% str_split(pattern = ", "))[[1]]
  y_bottom <- (imginfo$yb_orig %>% str_split(pattern = ", "))[[1]]
  
  pad <- 0.1 #proportion to pad box by
  
  img <- image_read(file.path(img_dir, imginfo$file_name))
  img <- image_draw(img)
  textHeight <- graphics::strheight(label, cex = cex)
  textWidth <- graphics::strwidth(label, cex = cex)
  for (i in 1:imginfo$cnt) {
    rect(xleft = x_left[i], ybottom = y_bottom[i],
         xright = x_right[i], ytop = y_top[i],
         border = "white", lwd = 5)
    rect(xleft = as.integer(x_right[i]) - (textWidth[i] * (1 + pad)), 
         ybottom = as.integer(y_top[i]) - (textHeight[i] * (1 + pad)), 
         xright = as.integer(x_right[i]) + (textWidth[i] * pad), 
         ytop = as.integer(y_top[i]) + (textHeight[i] * pad),
         col = "white", border = NA) 
    text(x = as.integer(x_right[i]), y = as.integer(y_top[i]), labels = label[i], 
         cex = cex, 
         adj = c(1,1), 
         col = "black", vfont = c("sans serif", "bold"))
  }
  
  dev.off()
  print(img)
}


