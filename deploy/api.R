#deploy computer vision model using plumber api

library(keras)


#define the model exactly as in training ----



# define anchors
cells_per_row <- 4
gridsize <- 1/cells_per_row
anchor_offset <- 1 / (cells_per_row * 2) 

anchor_xs <- seq(anchor_offset, 1 - anchor_offset, length.out = 4) %>%
  rep(each = cells_per_row)
anchor_ys <- seq(anchor_offset, 1 - anchor_offset, length.out = 4) %>%
  rep(cells_per_row)

anchor_centers <- cbind(anchor_xs, anchor_ys)
anchor_height_width <- matrix(1 / cells_per_row, nrow = 16, ncol = 2)

anchors <- cbind(anchor_centers, anchor_height_width)

feature_extractor <- application_resnet50(
  include_top = FALSE,
  input_shape = c(224, 224, 3))

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
  layer_conv_2d(common, filters = 28, kernel_size = 3,
                padding = "same", name = "class_conv") %>%
  layer_reshape(target_shape = c(16, 28), name = "class_output")

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

# load the model weights ----
load_model_weights_tf(model, "model_weights_directory_path") # [need to change this]


# decode and reshape input ----

#* Predicts **the number** [need to change this] in an image
#* @param enc a base64  encoded 28x28 image
#* @post /cnn-mnist
function(enc) {
  # decode and read the jpeg image
  img <- jpeg::readJPEG(source = base64enc::base64decode(enc))
  
  # reshape 
  img <- img %>% 
    array_reshape(., dim = c(1, dim(.), 1)) # [need to change this]
  
# make the prediction ----
  predict_classes(model, img) # [need to change this]
}





