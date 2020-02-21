library(tidyverse)
library(keras)
img_name <- "IMG_4664.JPG"
img_dir = "example_photos"
 
pred <- model$predict(x = load_and_preprocess_image(image_name = img_name, 
                                                    target_height = 224, 
                                                    target_width = 224, 
                                                    img_dir = img_dir))
# saveRDS(pred, "data/pred.rds")

pred <- read_rds("data/pred.rds")

str(pred)

pred.df <- pred %>% enframe(name = "type", value = "value") %>% unnest(cols = value)

pred_cat <- pred[[1]]
pred_box <- pred[[2]]

str(pred_cat)

hist(pred_box, 100)
hist(pred_cat, 100)

str(pred_cat)
str(pred_box)

pred_cat[,2,1]
dimnames(pred_cat)

#the output is always 16 boxes (although some may have 0 coordinates).  pred_box
#is the four coordinates, 16 times, pred_cat is the ?probability? of each of the
#28 classes matching a given box.
df_box <- matrix(pred_box, nrow = 16, ncol = 4, byrow = T) %>% data.frame()
names(df_box) <- c("xl", "yt", "xr", "yb")

df_cat <- matrix(pred_cat, nrow = 16, ncol = 28, byrow = T) %>% data.frame()
names(df_cat) <- 1:28


#plot it ----
library(magick)

pad <- 0.1 #proportion to pad box by

img <- image_read(file.path(img_dir, img_name))

#rescale
x_left <- df_box$xl * image_info(img)[[1,"width"]]
x_right <- df_box$xr * image_info(img)[[1,"width"]]
y_top <- df_box$yt * image_info(img)[[1,"height"]]
y_bottom <- df_box$yb * image_info(img)[[1,"height"]]


img <- image_draw(img)
# textHeight <- graphics::strheight(label, cex = cex)
# textWidth <- graphics::strwidth(label, cex = cex)
for (i in 1:nrow(df_box)) {
  rect(xleft = x_left[i], ybottom = y_bottom[i],
       xright = x_right[i], ytop = y_top[i],
       border = "white", lwd = 5)
  # rect(xleft = as.integer(x_right[i]) - (textWidth[i] * (1 + pad)), 
  #      ybottom = as.integer(y_top[i]) - (textHeight[i] * (1 + pad)), 
  #      xright = as.integer(x_right[i]) + (textWidth[i] * pad), 
  #      ytop = as.integer(y_top[i]) + (textHeight[i] * pad),
  #      col = "white", border = NA) 
  # text(x = as.integer(x_right[i]), y = as.integer(y_top[i]), labels = label[i], 
  #      cex = cex, 
  #      adj = c(1,1), 
  #      col = "black", vfont = c("sans serif", "bold"))
}

dev.off()
print(img)