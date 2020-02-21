library(tidyverse)

img_name <- "IMG_4664.JPG"
img_dir = "source_photos"

pred <- model$predict(x = load_and_preprocess_image(image_name = img_name, target_height = 224, target_width = 224))

saveRDS(pred, "data/pred.rds")

str(pred)

pred.df <- pred %>% enframe(name = "type", value = "value") %>% unnest(cols = value)

pred_boxes <- pred[[1]]

str(pred_boxes)

pred_boxes[4]

