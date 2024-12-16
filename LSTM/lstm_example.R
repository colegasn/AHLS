##### EXAMPLE LSTM ####
### Last Update: 2/6/2024

# Set up tensorflow and keras
# devtools::install_github("rstudio/tensorflow")
# devtools::install_github("rstudio/keras")

# tensorflow::install_tensorflow()
# tensorflow::tf_config()

# use_condaenv(“keras-tf”, required = T)

# Load packages
library(dplyr)
library(tibble)
library(tsibble)
library(lubridate)
library(ggplot2)
library(keras)
library(tensorflow)

# Load data
imdb <- dataset_imdb(num_words = 500)
c(c(train_x, train_y), c(test_x, test_y)) %<-% imdb
length(train_x); length(test_x)

# Balance between good (0) and bad (1) movies in both training and test sets
table(train_y)
table(test_y)

# Example review scores
train_x[[10]]

# Ensure movie review length is 90 words or less
train_x <- pad_sequences(train_x, maxlen = 90)
test_x <- pad_sequences(test_x, maxlen = 90)

# Example review scores
train_x[[10]]

# Initiate model
model <- keras_model_sequential()

# Embed the recurrent neural network layers
model %>%
  layer_embedding(input_dim = 500, output_dim = 32) %>%
  layer_simple_rnn(units = 32) %>% 
  layer_dense(units = 1, activation = "sigmoid")

# Compile the model
model %>%
  compile(optimizer = "rmsprop", loss = "binary_crossentropy", metrics = c("acc"))

# Fit the model
history <- model %>%
  fit(train_x, train_y, epochs = 25, batch_size = 128, validation_split = 0.2)
plot(history)

# Model prediction
model %>% evaluate(train_x, train_y) 
model %>% evaluate(test_x, test_y) 

#####

# Make changes to model
model %>%
  layer_embedding(input_dim = 500, output_dim = 32) %>%
  layer_simple_rnn(units = 32,return_sequences = TRUE,activation = 'relu') %>% 
  layer_simple_rnn(units = 32,return_sequences = TRUE,activation = 'relu') %>% 
  layer_simple_rnn(units = 32) %>% 
  layer_dense(units = 1, activation = "sigmoid")

# Compile the model
model %>%
  compile(optimizer = "rmsprop", loss = "binary_crossentropy", metrics = c("acc"))

# Fit the model
history <- model %>%
  fit(train_x, train_y, epochs = 25, batch_size = 128, validation_split = 0.2)
plot(history)

# Model prediction
model %>% evaluate(train_x, train_y) 
model %>% evaluate(test_x, test_y)

#####

# Pad sequences
train_x <- pad_sequences(train_x, maxlen = 200)
test_x <- pad_sequences(test_x, maxlen = 200)

# Rerun the model and check the accuracy again
model %>% evaluate(train_x, train_y)
model %>% evaluate(test_x, test_y)
