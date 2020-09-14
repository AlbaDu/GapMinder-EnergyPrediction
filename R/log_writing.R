#' @title log_writing == lgw
#' @description Main package function
#' 
#' @param path Project's environment
#' 
#' @return 
#' 
#' @import logging
#' 
 
lgw <- function (path){
  tryCatch(expr = {
    
    library(logging)
    
    addHandler(writeToFile, logger = 'log', file = paste0(path, "/Log output/logfile.log"))
    loginfo("Programme start", logger = 'log')
    
    #Reading config
    loginfo("Reading config...", logger = 'log')
    config <- read_config(path)
    loginfo("Config reading concluded", logger = 'log')
    
    #Data are readed into a dataframe
    loginfo("Reading data...", logger = 'log')
    df_1 <- read_data(config, path)
    loginfo("Data reading concluded", logger = 'log')
    
    #Raw data are cleaned and reshaped, to be prepared for the ML model
    loginfo("Reshaping data...", logger = 'log')
    df_1 <- reshape_data(config, df_1)
    loginfo("Data reshaping concluded", logger = 'log')
    
    #Loading of the machine learning model chosen for making the prediction
    loginfo("Machine Learning model loading...", logger = 'log')
    output <- to_ML(df_1, config)
    loginfo("ML model loading completed", logger = 'log')
    
    #Output generation
    loginfo("Generating data output", logger = 'log')
    output(output, config, path)
    loginfo("Output generated", logger = 'log')
    
  }, error = function(e){
    
    logerror("ERROR!", logger = 'log')
    stop()
    
  }, finally = {
    
    loginfo("End of programme execution", logger = 'log')
    removeHandler(writeToFile, logger = 'log')
    
  })
}