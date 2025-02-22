###
### --- Helping functions
###

# Replace missing values with mean of remaining values
mean_replace <- function(arg){#requires a numeric vector as argument
  vec <- arg
  
  # Compute mean value for the present values
  vec[!is.na(vec)]|> mean() |> round(2) -> m
  
  # Do replacement
  replacement_counter <- 0
  replaced_values <- rep(NA,length(vec)) # vector catching just replaced values
  for(i in 1:length(vec)){
    if(is.na(vec[[i]])){
      replacement_counter <- replacement_counter + 1
      vec[[i]] <- m
      replaced_values[[i]] <- m
    }
  }
  print(paste0('Percentage of missing values replaced with mean values for series: ',round(replacement_counter/length(vec),2),'%'))
  return(list(new_series = vec,replaced_series = replaced_values))
}


# Wrap function for replacement with mean value
apply_replacement_to_df <- function(df,col_names_vector){
  new_df <- df
  replacement_df <- df[col_names_vector]
  for(name in col_names_vector){
    ret <- mean_replace(new_df[[name]])
    new_df[[name]] <- ret$new_series
    replacement_df [[name]] <- ret$replaced_series
  }
  return(list(completed_df=new_df,replacement_df=replacement_df))
}


# Min-max normalization
min_max_norm <- function(series){
  m <- min(series)
  d <- max(series)-m
  return((series-m)/d)
}

# Z_score normalization
z_norm <- function(series){
  m <- mean(series)
  s <- sd(series)
  r <- (series-m)/s
  return(r)
}