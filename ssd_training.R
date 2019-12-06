# NectarVision - single shot detector training script

#based on https://blogs.rstudio.com/tensorflow/posts/2018-12-18-object-detection-concepts/ 

#load packages ----
library(tidyverse)
library(keras)
library(rjson)
library(magick) #may need terminal command: sudo apt-get install libmagick++-dev
library(repurrrsive)
library(listviewer)


#image directory
img_dir <- "source_photos"

#descriptor - some descriptor to organise adn separate runs using this code from others
descriptor <- "ssd"

#import prepped data - using import script
# source("R/data_prep.R")

#or import prepped data from folder
boxinfo <- read_csv("data/boxinfo.csv")
imageinfo <- read_csv("data/imageinfo.csv")
catinfo <- read_csv("data/catinfo.csv")

# Scaling---

# scale all bounding box coordinates according to the actual image size we’ll
# use when we pass it to our network.

target_height <- 224
target_width <- 224

imageinfo <- imageinfo %>% 
  mutate(
    x_left_scaled = (x_left / image_width * target_width) %>% round(),
    x_right_scaled = (x_right / image_width * target_width) %>% round(),
    y_top_scaled = (y_top / image_height * target_height) %>% round(),
    y_bottom_scaled = (y_bottom / image_height * target_height) %>% round(),
    bbox_width_scaled =  (bbox_width / image_width * target_width) %>% round(),
    bbox_height_scaled = (bbox_height / image_height * target_height) %>% round()
  )


# Aggregate information ----
#aggregate all information on a single image into a single row.

imageinfo4ssd <- imageinfo %>%
  select(category_id, file_name, category,
         x_left, y_top, x_right, y_bottom,
         ends_with("scaled"))

imageinfo4ssd <- imageinfo4ssd %>%
  group_by(file_name) %>%
  summarise(
    categories = toString(category_id),
    category = toString(category),
    xl = toString(x_left_scaled),
    yt = toString(y_top_scaled),
    xr = toString(x_right_scaled),
    yb = toString(y_bottom_scaled),
    xl_orig = toString(x_left),
    yt_orig = toString(y_top),
    xr_orig = toString(x_right),
    yb_orig = toString(y_bottom),
    cnt = n()
  )

# check

example <- imageinfo4ssd[5,]
img <- image_read(file.path(img_dir, example$file_name))
category <- (example$category %>% str_split(pattern = ", "))[[1]]
x_left <- (example$xl_orig %>% str_split(pattern = ", "))[[1]]
x_right <- (example$xr_orig %>% str_split(pattern = ", "))[[1]]
y_top <- (example$yt_orig %>% str_split(pattern = ", "))[[1]]
y_bottom <- (example$yb_orig %>% str_split(pattern = ", "))[[1]]

img <- image_draw(img)
for (i in 1:example$cnt) {
  rect(x_left[i],
       y_bottom[i],
       x_right[i],
       y_top[i],
       border = "white",
       lwd = 2)
  text(
    x = as.integer(x_right[i]),
    y = as.integer(y_top[i]),
    labels = category[i],
    offset = 1,
    pos = 2,
    cex = 1,
    col = "white"
  )
}
dev.off()
print(img)
