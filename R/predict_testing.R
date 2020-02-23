library(tidyverse)
library(keras)
img_name <- "IMG_4664.JPG"
img_dir = "example_photos"
 
# pred <- model$predict(x = load_and_preprocess_image(image_name = img_name, 
#                                                     target_height = 224, 
#                                                     target_width = 224, 
#                                                     img_dir = img_dir))
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
#is the four coordinates, 16 times, 
df_box_raw <- matrix(pred_box, nrow = 16, ncol = 4, byrow = T) %>% data.frame()
names(df_box_raw) <- c("xl", "yt", "xr", "yb")

# #or is it actually the offset from the anchor centre? or anchor corner?  Lets try: 
# df_anchor_corners <- data.frame(anchor_corners)
# names(df_anchor_corners) <- c("a_xl", "a_yt", "a_xr", "a_yb")
# df_box_ac <- df_box_raw + df_anchor_corners

#try anchor centres instead
df_anchor_centers <- data.frame(anchor_centers)
df_box_ac <- df_box_raw + cbind(df_anchor_centers, df_anchor_centers)

#pred_cat is the ?probability? of each of the 28 classes matching a given box.
df_cat <- matrix(pred_cat, nrow = 16, ncol = 28, byrow = T) %>% data.frame()
names(df_cat) <- 1:28


#plot it ----
library(magick)

pad <- 0.1 #proportion to pad box by
img <- image_read(file.path(img_dir, img_name))
df_box <- df_box_ac

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