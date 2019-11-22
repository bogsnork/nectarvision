# NectarVision - data prep

#based on https://blogs.rstudio.com/tensorflow/posts/2018-11-05-naming-locating-objects/ 

#load packages ----
library(tidyverse)
library(keras)
library(rjson)
# library(magick) #may need terminal command: sudo apt-get install libmagick++-dev
library(repurrrsive)
library(listviewer)
# library(magrittr)


#data sources ----

#define image directories 
img_dir <- "~/compvis_flowers_1/data/quadrats/images_voc"
annot_coco_file <- "../data/quadrats/export-coco-2019-03-05T14_31_52.619981.json"

#image attributes----

#image annotations
annotations <- fromJSON(file = annot_coco_file)
#str(annotations, max.level = 1)

imageinfo <- annotations$images %>% {
  tibble(
    id = map_chr(., "id"),
    file_name = map_chr(., "file_name"),
    image_height = map_dbl(., "height"),
    image_width = map_dbl(., "width")
  )
}

#bounding box coordinates
boxinfo <- annotations$annotations %>% {
  tibble(
    image_id = map_chr(., "image_id"),
    category_id = map_dbl(., "category_id"),
    bbox = map(., "bbox")
  )
}

boxinfo <- boxinfo %>% 
  mutate(bbox = unlist(map(.$bbox, function(x) paste(x, collapse = " "))))
boxinfo <- boxinfo %>% 
  separate(bbox, into = c("x_left", "y_top", "bbox_width", "bbox_height"))
boxinfo <- boxinfo %>% mutate_at(vars(-image_id), as.numeric) 
#For the bounding boxes, the annotation file provides x_left and y_top coordinates, as well as width and height. We will mostly be working with corner coordinates, so we create the missing x_right and y_top.

#As usual in image processing, the y axis starts from the top.
boxinfo <- boxinfo %>% 
  mutate(y_bottom = y_top + bbox_height - 1, x_right = x_left + bbox_width - 1)

# match class ids to class names.
catinfo <- annotations$categories %>%  {
  tibble(id = map_dbl(., "id"), name = map_chr(., "name"))
}
catinfo

#merge image info with bounding box and class info
imageinfo <- imageinfo %>%
  inner_join(boxinfo, by = c("id" = "image_id")) %>%
  inner_join(catinfo, by = c("category_id" = "id"))

#get image name
annot_csv <- read.csv(annot_csv_file) %>% 
  select(ID, External.ID)

imageinfo <- imageinfo %>%
  inner_join(annot_csv, by = c("id" = "ID")) %>% 
  rename(file_name_original = External.ID,
         url = file_name) %>% 
  mutate(file_name = paste0(id, str_sub(file_name_original, start = -4)))

#clean data----
#remove all rows where we don't have an image
img_files <- list.files(img_dir)
img_files <- img_files[which(str_sub(img_files, start = -4) != ".xml")]
length(img_files)

imageinfo <- imageinfo %>% 
  filter(file_name %in% img_files) %>% 
  droplevels()

#tidy up and export----


#write.csv(imageinfo, "../data/quadrats/imageinfo.csv")





























