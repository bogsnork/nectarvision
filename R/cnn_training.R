# NectarVision - CNN training script

#based on https://blogs.rstudio.com/tensorflow/posts/2018-11-05-naming-locating-objects/ 

#load packages ----
library(tidyverse)
library(keras)
library(rjson)
# library(magick) #may need terminal command: sudo apt-get install libmagick++-dev
library(repurrrsive)
library(listviewer)


#image directory
img_dir <- "source_photos"

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


#multiple object classification ----

#Multi-hot-encode  data

  #For every image make vector of length `ncats` 
  #0 indicates absence, 1 means presence of the respective object class.
  
ncats <-  imageinfo %>% select(category_id) %>% pull() %>% max() 

image_cats <- imageinfo %>% 
  select(category_id) %>% #category ids are numbered from 1
  mutate(category_id = category_id - 1) %>%  #this converts it to zero based indexing
  pull() %>% to_categorical(num_classes = ncats)

image_cats <- data.frame(image_cats) %>%
  add_column(file_name = imageinfo$file_name, .before = TRUE)
#at this stage we have a row per box, and 0 or 1 in each column showing which spp 
#the box is tagged as.  There should only be one 1 as boxes only have one tag 

#reduce to one line per image, multi 1 or 0 for spp
image_cats <- image_cats %>% 
  group_by(file_name) %>% 
  summarise_all(.funs = funs(max))
#we now have a row for each image, and a 0 or 1 for each spp.  there may be multiple 1's as 
#images can have several species


n_samples <- nrow(image_cats)
train_indices <- sample(1:n_samples, 0.8 * n_samples)
train_data <- image_cats[train_indices,]
validation_data <- image_cats[-train_indices,]

#image generator
batch_size <- 10

load_and_preprocess_image <- function(image_name, target_height, target_width) {
  img_array <- image_load(
    file.path(img_dir, image_name),
    target_size = c(target_height, target_width)
  ) %>%
    image_to_array() %>%
    xception_preprocess_input() 
  dim(img_array) <- c(1, dim(img_array))
  img_array
}



#classification generator

  #generator returns target of dimensions batch_size * ncats
classification_generator <- 
  function(data, target_height, target_width, shuffle, batch_size) {
    i <- 1
    function() {
      if (shuffle) {indices <- sample(1:nrow(data), size = batch_size)} 
      else {if (i + batch_size >= nrow(data))
        i <<- 1
      indices <- c(i:min(i + batch_size - 1, nrow(data)))
      i <<- i + length(indices)
      }
      x <- array(0, dim = c(length(indices), target_height, target_width, 3))
      y <- array(0, dim = c(length(indices), ncats))
      
      for (j in 1:length(indices)) {
        x[j, , , ] <- load_and_preprocess_image(data[[indices[j], "file_name"]], 
                                                target_height, target_width)
        y[j, ] <- data[indices[j], 2:(ncats+1)] %>% as.matrix()
      }
      x <- x / 255
      list(x, y)
    }
  }

train_gen <- classification_generator(
  train_data, target_height = target_height, target_width = target_width,
  shuffle = TRUE, batch_size = batch_size)

valid_gen <- classification_generator(
  validation_data, target_height = target_height, target_width = target_width,
  shuffle = FALSE, batch_size = batch_size)


#define model ----
feature_extractor <-
  application_xception(
    include_top = FALSE,
    input_shape = c(224, 224, 3),
    pooling = "avg"
  )

feature_extractor %>% freeze_weights()

model <- keras_model_sequential() %>%
  feature_extractor %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.25) %>%
  layer_dense(units = 512, activation = "relu") %>%
  layer_batch_normalization() %>%
  layer_dropout(rate = 0.5) %>%
  layer_dense(units = ncats, activation = "sigmoid")

#Compile model ----
model %>% compile(optimizer = "adam",
                  loss = "binary_crossentropy",
                  metrics = list("accuracy"))


#fit model ----
model %>% fit_generator(
  train_gen,
  epochs = 20,
  steps_per_epoch = nrow(train_data) / batch_size,
  validation_data = valid_gen,
  validation_steps = nrow(validation_data) / batch_size,
  callbacks = list(
    callback_model_checkpoint(
      file.path("models/multiclass", "weights.{epoch:02d}-{val_loss:.2f}.hdf5")
    ),
    callback_early_stopping(patience = 2)
  )
)










































