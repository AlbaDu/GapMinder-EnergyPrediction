#' @title read_config
#' @description Function called by lgw to read the config
#' 
#' @param path Project's environment
#'
#' @return
#' 
#' @import XML
#' @import logging
#'

read_config <- function (path){
  
  library(XML)
  
  configPath <- paste0(path, "config/config.xml")
  
  
  tryCatch(expr = {
    
    #Read xml and converts it to a list
    config <- XML::xmlToList(xmlParse(configPath))
    
    
  }, error = function(e){
    
    logerror("Config was not find on the path. Check if it´s call config.xml",
             logger = 'log')
    stop()
  })
  
  loginfo("Config readed", logger = 'log')
  
  validate_config_nodes(config)
  
  config$data$predictors <- trimws(strsplit(config$data$predictors, ",")[[1]])
  
  check_target <- is.null(config$data$target)
  if(check_target){
    logerror("Target is empty, choose another one", logger = 'log')
    stop()
  }
  
  check_country <- is.null(config$data$prediction$country)
  if(check_country){
    logerror("Please, you need to choose the prediction country", logger = 'log')
    stop()
  }
  
  check_year <- is.null(config$data$prediction$year)
  
  if(check_year){
    logerror("Please, you need to choose the prediction year", logger = 'log')
    stop()
    
  }else{
    
    config$data$prediction$year <- as.numeric(config$data$prediction$year)
    
  }
  
  check_year_na <- is.na(config$data$prediction$year)
  
  if(check_year_na){
    logerror("The year should be a number", logger = 'log')
    stop()
    
  } 
  check_test_rate <- is.null(config$test_rate)
  
  if(check_test_rate){
    
    logerror("Please, you need to choose the test rate", logger = 'log')
    stop()
    
  }else{
    
    config$test_rate <- as.numeric(config$test_rate)
  } 
  
  check_less_one <- config$test_rate < 1
  check_greater_zero <- config$test_rate > 0
  
  if(!(check_less_one && check_greater_zero)){
    
    logerror("The test rate should be a number between 0 and 1 ", logger = 'log')
    stop()    
    
  }
  
  check_test_rate_na <- is.na(config$test_rate)
  
  if(check_test_rate_na){
    
    logerror("The test rate should be a number", logger = 'log')
    stop()
    
  }
  
  check_output_file <- is.null(config$output_file)
  if(check_output_file){
    
    logerror("No output file, create it", logger = 'log')
    
    stop()
  }
  
  accepted_spacers <- config$sep %in% c(",", ";")
  if(!accepted_spacers){
    loggeror("Sorry, but spacers can only be , or ;", logger = "log")
    stop()
  }
  
  return(config)
  
} 

#Validation of the configuration nodes for the prediction
validate_config_nodes <- function(config){
  
  main_node <- identical(names(config), c("sep", "data", "test_rate", "output_file"))
  data_node <- identical(names(config$data), c("predictors", "target", "prediction"))
  prediction_node <- identical(names(config$data$prediction), c("country", "year"))
  
  nodes <- c("main_node" = main_node, "data_node" = data_node, 
             "prediction_node" = prediction_node)
  
  check <- all(nodes)
  
  if(!check){
    
    bad_nodes <- names(nodes)[!nodes]
    
    logerror(paste0("The nodes: ", paste(bad_nodes, collapse = ", "),
                    " are badly structure!"), logger = 'log')
    stop()
    
  }
  
}