# NectarVision - single shot detector training script

#based on https://blogs.rstudio.com/tensorflow/posts/2018-12-18-object-detection-concepts/ 

#load packages ----
library(tidyverse)
library(keras)
library(rjson)
library(magick) #if magick installation fails, may need terminal command: sudo apt-get install libmagick++-dev
                #if running docker container, need to run this from command line (of the virtual computer):
                # sudo nvidia-docker exec [containername] sudo apt-get install libmagick++-dev -y
                #then run install.packages("magick") again
library(repurrrsive)
library(listviewer)
library(reticulate)


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
n_classes <- nrow(catinfo)

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

example <- imageinfo4ssd[50,]
img <- image_read(file.path(img_dir, example$file_name))
category <- (example$category %>% str_split(pattern = ", "))[[1]]
x_left <- (example$xl_orig %>% str_split(pattern = ", "))[[1]]
x_right <- (example$xr_orig %>% str_split(pattern = ", "))[[1]]
y_top <- (example$yt_orig %>% str_split(pattern = ", "))[[1]]
y_bottom <- (example$yb_orig %>% str_split(pattern = ", "))[[1]]

img <- image_draw(img)
# for (i in 1:example$cnt) {
#   rect(xleft = x_left[i], ybottom = y_bottom[i], 
#        xright = x_right[i], ytop = y_top[i],
#        border = "white", lwd = 2)
#   text(x = as.integer(x_right[i]), y = as.integer(y_top[i]), labels = category[i], 
#        offset = 1, pos = 2, cex = 1, col = "white")
# }
# dev.off()
# print(img)


# define anchors

cells_per_row <- 4
gridsize <- 1/cells_per_row
anchor_offset <- 1 / (cells_per_row * 2) 

anchor_xs <- seq(anchor_offset, 1 - anchor_offset, length.out = 4) %>%
  rep(each = cells_per_row)
anchor_ys <- seq(anchor_offset, 1 - anchor_offset, length.out = 4) %>%
  rep(cells_per_row)

# ggplot(data.frame(x = anchor_xs, y = anchor_ys), aes(x, y)) + geom_point() +
#   coord_cartesian(xlim = c(0,1), ylim = c(0,1)) + theme(aspect.ratio = 1)

anchor_centers <- cbind(anchor_xs, anchor_ys)
anchor_height_width <- matrix(1 / cells_per_row, nrow = 16, ncol = 2)

#defines anchor centres, height and width
anchors <- cbind(anchor_centers, anchor_height_width)

#defines grid cell corners
# cells are indicated by (xl, yt, xr, yb)
# successive rows first go down in the image, then to the right
hw2corners <- function(centers, height_width) {
  cbind(centers - height_width / 2, centers + height_width / 2) %>% unname()
}
anchor_corners <- hw2corners(anchor_centers, anchor_height_width)


#visualise an example: 
example <- imageinfo4ssd[50, ]
category <- (example$category %>% str_split(pattern = ", "))[[1]]
x_left <- (example$xl %>% str_split(pattern = ", "))[[1]]
x_right <- (example$xr %>% str_split(pattern = ", "))[[1]]
y_top <- (example$yt %>% str_split(pattern = ", "))[[1]]
y_bottom <- (example$yb %>% str_split(pattern = ", "))[[1]]


img <- image_read(file.path(img_dir, example$file_name))
img <- image_resize(img, geometry = "224x224!")
img <- image_draw(img)

# for (i in 1:example$cnt) {
#   rect(x_left[i], y_bottom[i], x_right[i], y_top[i],
#        border = "white", lwd = 2)
#   text(x = as.integer(x_right[i]), y = as.integer(y_top[i]), labels = category[i],
#     offset = 0, pos = 2, cex = 0.4, col = "white")
# }
# for (i in 1:nrow(anchor_corners)) {
#   rect(anchor_corners[i, 1] * 224, anchor_corners[i, 4] * 224,
#     anchor_corners[i, 3] * 224, anchor_corners[i, 2] * 224,
#     border = "cyan", lwd = 1, lty = 3)
# }
# dev.off()
# print(img)

#choose which bounding box is evaluated in which anchor box (using Intersection over Union)
# overlaps shape is: number of ground truth objects * number of grid cells
map_to_ground_truth <- function(overlaps) {
  
  # for each ground truth object, find maximally overlapping cell (crit. 1)
  # measure of overlap, shape: number of ground truth objects
  prior_overlap <- apply(overlaps, 1, max)
  # which cell is this, for each object
  prior_idx <- apply(overlaps, 1, which.max)
  
  # for each grid cell, what object does it overlap with most (crit. 2)
  # measure of overlap, shape: number of grid cells
  gt_overlap <-  apply(overlaps, 2, max)
  # which object is this, for each cell
  gt_idx <- apply(overlaps, 2, which.max)
  
  # set all definitely overlapping cells to respective object (crit. 1)
  gt_overlap[prior_idx] <- 1.99
  
  # now still set all others to best match by crit. 2
  # actually it's other way round, we start from (2) and overwrite with (1)
  for (i in 1:length(prior_idx)) {
    # iterate over all cells "absolutely assigned"
    p <- prior_idx[i] # get respective grid cell
    gt_idx[p] <- i # assign this cell the object number
  }
  
  # return: for each grid cell, object it overlaps with most + measure of overlap
  list(gt_overlap, gt_idx)
  
}


# compute IOU
jaccard <- function(bbox, anchor_corners) {
  bbox <- k_constant(bbox)
  anchor_corners <- k_constant(anchor_corners)
  intersection <- intersect(bbox, anchor_corners)
  union <-
    k_expand_dims(box_area(bbox), axis = 2)  + k_expand_dims(box_area(anchor_corners), axis = 1) - intersection
  res <- intersection / union
  res %>% k_eval()
}

# compute intersection for IOU
intersect <- function(box1, box2) {
  box1_a <- box1[, 3:4] %>% k_expand_dims(axis = 2)
  box2_a <- box2[, 3:4] %>% k_expand_dims(axis = 1)
  max_xy <- k_minimum(box1_a, box2_a)
  
  box1_b <- box1[, 1:2] %>% k_expand_dims(axis = 2)
  box2_b <- box2[, 1:2] %>% k_expand_dims(axis = 1)
  min_xy <- k_maximum(box1_b, box2_b)
  
  intersection <- k_clip(max_xy - min_xy, min = 0, max = Inf)
  intersection[, , 1] * intersection[, , 2]
  
}

box_area <- function(box) {
  (box[, 3] - box[, 1]) * (box[, 4] - box[, 2]) 
}


#data generator ----
batch_size <- 16
image_size <- target_width # same as height

threshold <- 0.4

class_background <- 21 #the index of the class which indicates background (i.e. not a target feature)

  #fix for OSError: image file is truncated
PIL <- reticulate::import("PIL")
PIL$ImageFile$LOAD_TRUNCATED_IMAGES <- TRUE

#image generator
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


#data generator ctd

ssd_generator <-
  function(data,
           target_height,
           target_width,
           shuffle,
           batch_size) {
    i <- 1
    function() {
      if (shuffle) {
        indices <- sample(1:nrow(data), size = batch_size)
      } else {
        if (i + batch_size >= nrow(data))
          i <<- 1
        indices <- c(i:min(i + batch_size - 1, nrow(data)))
        i <<- i + length(indices)
      }
      
      x <-
        array(0, dim = c(length(indices), target_height, target_width, 3))
      y1 <- array(0, dim = c(length(indices), 16))
      y2 <- array(0, dim = c(length(indices), 16, 4))
      
      for (j in 1:length(indices)) {
        x[j, , , ] <-
          load_and_preprocess_image(data[[indices[j], "file_name"]], target_height, target_width)
        
        class_string <- data[indices[j], ]$categories
        xl_string <- data[indices[j], ]$xl
        yt_string <- data[indices[j], ]$yt
        xr_string <- data[indices[j], ]$xr
        yb_string <- data[indices[j], ]$yb
        
        classes <-  str_split(class_string, pattern = ", ")[[1]]
        xl <-
          str_split(xl_string, pattern = ", ")[[1]] %>% as.double() %>% `/`(image_size)
        yt <-
          str_split(yt_string, pattern = ", ")[[1]] %>% as.double() %>% `/`(image_size)
        xr <-
          str_split(xr_string, pattern = ", ")[[1]] %>% as.double() %>% `/`(image_size)
        yb <-
          str_split(yb_string, pattern = ", ")[[1]] %>% as.double() %>% `/`(image_size)
        
        # rows are objects, columns are coordinates (xl, yt, xr, yb)
        # anchor_corners are 16 rows with corresponding coordinates
        bbox <- cbind(xl, yt, xr, yb)
        overlaps <- jaccard(bbox, anchor_corners)
        
        c(gt_overlap, gt_idx) %<-% map_to_ground_truth(overlaps)
        gt_class <- classes[gt_idx]
        
        pos <- gt_overlap > threshold
        gt_class[gt_overlap < threshold] <- 21
        
        # columns correspond to objects
        boxes <- rbind(xl, yt, xr, yb)
        # columns correspond to object boxes according to gt_idx
        gt_bbox <- boxes[, gt_idx]
        # set those with non-sufficient overlap to 0
        gt_bbox[, !pos] <- 0
        gt_bbox <- gt_bbox %>% t()
        
        y1[j, ] <- as.integer(gt_class) - 1
        y2[j, , ] <- gt_bbox
        
      }
      
      x <- x %>% imagenet_preprocess_input()
      y1 <- y1 %>% to_categorical(num_classes = class_background)
      list(x, list(y1, y2))
    }
  }


#training generator ----

train_gen <- ssd_generator(
  imageinfo4ssd,
  target_height = target_height,
  target_width = target_width,
  shuffle = TRUE,
  batch_size = batch_size
)

#model ----

feature_extractor <- application_resnet50(
  include_top = FALSE,
  input_shape = c(224, 224, 3))


#outputs ----
input <- feature_extractor$input

common <- feature_extractor$output %>%
  layer_conv_2d(filters = 256, kernel_size = 3, padding = "same",
    activation = "relu", name = "head_conv1_1") %>%
  layer_batch_normalization() %>%
  layer_conv_2d(filters = 256, kernel_size = 3, padding = "same",
    activation = "relu", name = "head_conv1_2") %>%
  layer_batch_normalization() %>%
  layer_conv_2d(filters = 256, kernel_size = 3, padding = "same",
    activation = "relu", name = "head_conv1_3") %>%
  layer_batch_normalization() %>%
  layer_conv_2d(filters = 256, kernel_size = 3, strides = 2,
    padding = "same", activation = "relu", name = "head_conv2") %>%
  layer_batch_normalization()

class_output <-
  layer_conv_2d(common, filters = 21, kernel_size = 3,
    padding = "same", name = "class_conv") %>%
  layer_reshape(target_shape = c(16, 21), name = "class_output")

bbox_output <-
  layer_conv_2d(common, filters = 4, kernel_size = 3,
    padding = "same", name = "bbox_conv") %>%
  layer_reshape(target_shape = c(16, 4), name = "bbox_flatten") %>%
  layer_activation("tanh") %>%
  layer_lambda(f = function(x) {
      activation_centers <-
        (x[, , 1:2] / 2 * gridsize) + k_constant(anchors[, 1:2])
      activation_height_width <-
        (x[, , 3:4] / 2 + 1) * k_constant(anchors[, 3:4])
      activation_corners <-
        k_concatenate(
          list(
            activation_centers - activation_height_width / 2,
            activation_centers + activation_height_width / 2))
      activation_corners
    },
    name = "bbox_output"
  )

model <- keras_model(inputs = input, outputs = list(class_output, bbox_output))

#loss function ----
tf <- import("tensorflow")
  # shapes are batch_size * 16 * 21
class_loss <- function(y_true, y_pred) {
  class_loss  <-
    tf$nn$sigmoid_cross_entropy_with_logits(labels = y_true, logits = y_pred)
  class_loss <-
    tf$reduce_sum(class_loss) / tf$cast(n_classes + 1, "float32")
  class_loss
}


  # shapes are batch_size * 16 * 4
bbox_loss <- function(y_true, y_pred) {
  # calculate localization loss for all boxes where ground truth was assigned some overlap
  # calculate mask
  pos <- y_true[, , 1] + y_true[, , 3] > 0
  pos <- pos %>% k_cast(tf$float32) %>% k_reshape(shape = c(batch_size, 16, 1))
  pos <- tf$tile(pos, multiples = k_constant(c(1L, 1L, 4L), dtype = tf$int32))
  diff <- y_pred - y_true
  # mask out irrelevant activations
  diff <- diff %>% tf$multiply(pos)
  
  loc_loss <- diff %>% tf$abs() %>% tf$reduce_mean()
  loc_loss * 100
}


# compile model ----
model %>% freeze_weights()
model %>% unfreeze_weights(from = "head_conv1_1")
model
model %>% compile(
  loss = list(class_loss, bbox_loss),
  optimizer = "adam",
  metrics = list(
    class_output = custom_metric("class_loss", metric_fn = class_loss),
    bbox_output = custom_metric("bbox_loss", metric_fn = bbox_loss))
)


# train model ----

steps_per_epoch <- nrow(imageinfo4ssd) / batch_size

model %>% fit_generator(
  train_gen,
  steps_per_epoch = steps_per_epoch,
  epochs = 5,
  callbacks = callback_model_checkpoint(
    "weights.{epoch:02d}-{loss:.2f}.hdf5", 
    save_weights_only = TRUE
  )
)






