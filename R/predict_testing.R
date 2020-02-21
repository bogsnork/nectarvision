library(tidyverse)
library(keras)
img_name <- "IMG_4664.JPG"
img_dir = "source_photos"
 
# pred <- model$predict(x = load_and_preprocess_image(image_name = img_name, target_height = 224, target_width = 224))
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
