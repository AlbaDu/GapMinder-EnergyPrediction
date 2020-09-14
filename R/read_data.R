#'@title read_data
#'@description Function called by lgw to read the data
#'
#'@param config List of configuration parameters
#'@param path Project's environment
#'
#'@import data.table
#'@import logging
#'
#'@return datasets_list
#'

read_data <- function(config, path){
  
    datasets_list <- list() #empty list
    
    for (i in config$data$predictors){
      tryCatch(expr = {
        path_data <- paste0(path, 'data/', i)
        data_ <- data.table::fread(path_data, sep = config$sep, encoding = 'UTF-8',
                                  data.table = FALSE, header = TRUE)
      }, error = function(e){
        logerror("Data was not found on the path. Please check the direction and the config",
                 logger = 'log')
        stop()
      })
      
      if(nrow(data_) == 0 | ncol(data_) == 0){ #checking files with date truly contain both rows & columns
      
        logerror("Data was incorrectly recorded or readed. Please, check data format",
                 logger = 'log')
        stop()
      }
      
      datasets_list[[i]] <- data_
    }
    
    loginfo("Correct action", logger = 'log')
    
    return(datasets_list)
    
}