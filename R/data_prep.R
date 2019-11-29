# NectarVision - data prep

#based on https://blogs.rstudio.com/tensorflow/posts/2018-11-05-naming-locating-objects/ 

#load packages ----
library(tidyverse)
library(rjson)
library(repurrrsive)
library(listviewer)


#data sources ----

#define image directories 
img_dir <- "source_photos"
annot_coco_file <- "annotations/nectarvision_proj-export.json"
annot_csv_file <- "annotations/nectarvision_proj-export.csv"

#image annotations - from json
annotations <- fromJSON(file = annot_coco_file)

imageinfo_orig <- annotations$assets %>% map("asset") %>% {
  tibble(
  image_id = map_chr(., "id"),
  file_name = map_chr(., "name"),
  image_height = map_dbl(map(., "size"), "height"),
  image_width = map_dbl(map(., "size"), "width"),
  )}

#add a new file_name field to fix issue of spaces in filenames beign replaced with `%02` in VOTT:

imageinfo_orig <- mutate(imageinfo_orig, 
                    file_name_vott = file_name,
                    file_name = str_replace_all(file_name, "%20", " "))

  #report on overlap between json file and actual image list
source_photo_list <- list.files(img_dir)
cat(
  length(source_photo_list[!(imageinfo_orig$file_name %in% source_photo_list)]), 
  "source photos are not listed in the JSON file", "\n",
  length(imageinfo_orig$file_name[!(source_photo_list %in% imageinfo_orig$file_name)]),
  "photos listed in the JSON file are not in the source photo folder"
)

  #for belt and braces we can remove lines from the json doc that don't have photos in the source folder
imageinfo_orig <- filter(imageinfo_orig, file_name %in% source_photo_list)

#regions, rectangles and tags

regions <- annotations$assets %>% map("regions") %>% 
  tibble(image_id = names(annotations$assets), regions = .) %>% 
  unnest_longer(data = ., col = regions) %>% 
  unnest_wider(data = ., col = regions)
               
regions <- select(regions, image_id, region_id = "id", tags, boundingBox, points)               
regions_bkp <- regions
regions <- regions %>% unnest_wider(data = ., col = boundingBox) 
regions <- select(regions, -`...1`) 
regions_bkp <- regions
regions <- unnest_longer(regions, tags)
regions_bkp <- regions

#bounding boxes
boxinfo <- regions %>% 
  mutate(x_right = width + left,
         y_bottom = top - height) %>% 
  select(region_id, image_id,
         category = tags,
         x_left = left,
         x_right,
         y_top = top,
         y_bottom,
         bbox_width = width,
         bbox_height = height)


#create a category id field

category <- unique(boxinfo$category)
category_reordered <- category[order(category)]

boxinfo %>% count(category) %>% data.frame()

catinfo <- data.frame(category = category[order(category)], 
                      category_id = 1:length(category))

#merge them

imageinfo <- imageinfo_orig %>%
  inner_join(boxinfo, by = "image_id") %>%
  inner_join(catinfo, by = c("category"))
 



# #clean data----
# #remove all rows where we don't have an image
# img_files <- list.files(img_dir)
# img_files <- img_files[which(str_sub(img_files, start = -4) != ".xml")]
# length(img_files)
# 
# imageinfo <- imageinfo %>% 
#   filter(file_name %in% img_files) %>% 
#   droplevels()

#tidy up and export----
write_rds(boxinfo, "data/boxinfo.rds")
write_rds(imageinfo_orig, "data/imageinfo_orig.rds")
write_rds(imageinfo, "data/imageinfo.rds")
write_rds(regions, "data/regions.rds")

write.csv(imageinfo, "data/imageinfo.csv", row.names = F)
write.csv(boxinfo, "data/boxinfo.csv", row.names = F)
write.csv(catinfo, "data/catinfo.csv", row.names = F)


