#' @title ml_model
#' 
#' @description Function called by lgw to hook the final DF into the machine learning model
#' 
#' @param data DF with data from all datasets, witn the appropiate shape
#' @param config List of configuration parameters 
#'
#' @import MLmetrics
#' @import logging
#' 

ml_model <- function(data_df, config){
  
  #Data used for training
  data_df <- selec_column(data_df, config)
  
  num_train <- round(nrow(data_df) * 0.7) #70% of data to train & 30% to test
  position_train <- sample(1:nrow(datos), size = num_train) #random positions for data for training subset
  
  train <- data_df[position_train,] 
  test <- data_df[-position_train,] 
  
  #Multiple LinearRegression model
  linear_mod <- lm(train$ener_pers ~ ., data = train) 
  test$ener_pers_predic <- predict(linear_mod, test)
  linear_mod_acc <- abs(sum(test$ener_pers - test$ener_pers_predic)/nrow(test))
  
  
  # Simple linear regression model (lm means linear model)
  model_1 <- train( train_df[,3:(ncol(train_df)-1)], train_df[, ncol(train_df)], method = 'lm')
  
  loginfo("Resumen de modelo de ML", logger = 'log')
  
  predict_test <- predict(model_1, test_df[,3:(ncol(test_df)-1)])
  
  loginfo("Resumen de modelo de ML terminado", logger = 'log')
  
  RMSE <- sqrt(mean((predict_test - test_df[, ncol(train_df)])^2))
  
  resultado <- predict(model_1, predict_y) 
  predict_y[, ncol(predict_y)] <- resultado
  output <- predict_y
  return(output)
}