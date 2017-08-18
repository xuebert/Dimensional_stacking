formatting <- function(data_mat, response, col_vars, row_vars, value_order_obj) {
  
  if (is.null(value_order_obj)) {value_order_obj = lapply(data_mat, unique)}
  
  list.of.packages <- "prodlim"
  new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new.packages)
  library(prodlim)
  
  var_names = colnames(data_mat)
  num_col = ncol(data_mat)
  
  if (length(col_vars) == 0 & length(row_vars) == 0) {
    col_vars = num_col:(floor(ncol(data_mat) / 2) + 1)
    row_vars = floor(num_col / 2):1
  }
  if (length(col_vars) == 0) {
    col_vars = rev((1:num_col)[!(var_names %in% row_vars)])
  }
  if (length(row_vars) == 0) {
    row_vars = rev((1:num_col)[!(var_names %in% col_vars)])
  }
  
  # optimize ordering
  # source("support_functions/optimize_order.R")
  # return_list = optimize_order(data_mat, response)
  # col_vars = return_list[[1]]
  # row_vars = return_list[[2]]
  
  ####################
  # get full value ordering information
  value_order = lapply(1:num_col, function(i) unique(data_mat[,i]))
  names(value_order) = var_names
  for (n_var in var_names) {
    if (n_var %in% names(value_order_obj) & !any(!(value_order_obj[[n_var]] %in% value_order[[n_var]]))) { # check that variable exists and that all values are specified properly
      value_order[[n_var]] = value_order_obj[[n_var]]
    }
  }
  
  # create storage
  grid_row = expand.grid(lapply(row_vars, function(i) value_order[[i]]))
  grid_col = expand.grid(lapply(col_vars, function(i) value_order[[i]]))
  
  axis1 = row.match(data_mat[,row_vars], grid_row)
  axis2 = row.match(data_mat[,col_vars], grid_col)
  
  bubble_size = matrix(NA, ncol = nrow(grid_row), nrow = nrow(grid_col))
  bubble_color = matrix(NA, ncol = nrow(grid_row), nrow = nrow(grid_col))
  
  # only record in bubble size/color if there is no NA
  for (n in 1:length(axis1)) {
    if ((!is.na(axis1[[n]])) & (!is.na(axis2[[n]]))) {
      bubble_size[matrix(c(axis2[[n]], axis1[[n]]), ncol = 2)] = response[[n]]
      bubble_color[matrix(c(axis2[[n]], axis1[[n]]), ncol = 2)] = response[[n]]
    }
  }
  
  return(list(bubble_color, bubble_size, col_vars, row_vars, grid_col, grid_row, value_order))
  
}