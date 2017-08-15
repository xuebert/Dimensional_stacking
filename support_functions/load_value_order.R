load_value_order <- function(lines_obj) {
  
  split_list = strsplit(lines_obj, ":")
  var_names = sapply(split_list, function(i) i[[1]])
  unique_values = sapply(split_list, function(i) strsplit(i[[2]], ","))
  names(unique_values) = var_names
  
  return(unique_values)
  
}