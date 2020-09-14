#' @title reshape_data
#' 
#' @description Function called by lgw to reshape the data df to a [n, 2] dimension
#' 
#' @param data List of datasets
#' @param config List of configuration parameters 
#'
#' @return
#' 
#' @import reshape2
#' @import logging
#' 

reshape_data <- function(config, datasets_list){
  
  
  for (i in 1:length(datasets_list$predictors)){
    
    col = colnames(datasets_list$predictors[[i]])
    datasets_list$predictors[[i]] <- reshape2::melt(data = datasets_list$predictors[[i]], 
                                                   id.vars = 'country', measure.vars = col[!(col %in% "country")],
                                                   value.name = config$data$predictors[i])
  }
  
  col = colnames(datasets_list$target)
  datasets_list$target <- reshape2::melt(data = datasets_list$target,
                                       id.vars = 'country', measure.vars = col[!(col %in% "country")],
                                       value.name = config$data$target)
  
  
  
  df <- datasets_list$predictors[[1]]
  
  for (i in 2:length(datasets_list$predictors)){
    
    df <- merge(df, datasets_list$predictors[[i]], by = c('country', 'variable'))
  }
  
  df <- merge(df, datasets_list$target, by = c('country', 'variable'))
  
  predict_Y <- df[(df$variable == config$data$prediction$year & df$country == config$data$prediction$country), ]
  
  data_df <- na.omit(df)
  
  return(data_df, predict_Y)
  
}