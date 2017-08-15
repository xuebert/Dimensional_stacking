formatting <- function(data_mat, response, col_vars, row_vars, value_order_obj) {
  
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
  full_grid1 = expand.grid(lapply(row_vars, function(i) value_order[[i]]))
  full_grid2 = expand.grid(lapply(col_vars, function(i) value_order[[i]]))
  grid_size1 = length(row_vars)
  grid_size2 = length(col_vars)
  
  browser()
  all_combo_strings1 = apply(full_grid1, 1, paste, collapse = "")
  combo_strings1 = apply(data_mat[,row_vars], 1, paste, collapse = "")
  all_combo_strings2 = apply(full_grid2, 1, paste, collapse = "")
  combo_strings2 = apply(data_mat[,col_vars], 1, paste, collapse = "")
  
  grid_to_axis1 = rep(all_combo_strings1, length(all_combo_strings2))
  grid_to_axis2 = rep(all_combo_strings2, each = length(all_combo_strings1))
  
  axis1 = match(combo_strings1, all_combo_strings1)
  axis2 = match(combo_strings2, all_combo_strings2)
  has_NA = is.na(axis1) | is.na(axis2)
  
  bubble_size = matrix(NA, ncol = nrow(full_grid2), nrow = nrow(full_grid1))
  bubble_color = matrix(NA, ncol = nrow(full_grid2), nrow = nrow(full_grid1))
  
  for (n in 1:length(axis1)) {
    if ((!is.na(axis1[[n]])) & (!is.na(axis2[[n]]))) {
      bubble_size[matrix(c(axis1[[n]], axis2[[n]]), ncol = 2)] = response[[n]]
      bubble_color[matrix(c(axis1[[n]], axis2[[n]]), ncol = 2)] = response[[n]]
    }
  }
  
  return(list(bubble_color, bubble_size, col_vars, row_vars))
  
}