formatting <- function(data_mat, response, col_vars, row_vars) {
  
  if (length(col_vars) == 0 & length(row_vars) == 0) {
    col_vars = ncol(data_mat):(floor(ncol(data_mat) / 2) + 1)
    row_vars = floor(ncol(data_mat) / 2):1
  }
  if (length(col_vars) == 0) {
    col_vars = rev((1:ncol(data_mat))[!(colnames(data_mat) %in% row_vars)])
  }
  if (length(row_vars) == 0) {
    row_vars = rev((1:ncol(data_mat))[!(colnames(data_mat) %in% col_vars)])
  }
  
  # optimize ordering
  # source("support_functions/optimize_order.R")
  # return_list = optimize_order(data_mat, response)
  # col_vars = return_list[[1]]
  # row_vars = return_list[[2]]
  
  ####################
  # create storage
  full_grid1 = expand.grid(lapply(row_vars, function(i) unique(data_mat[,i])))
  full_grid2 = expand.grid(lapply(col_vars, function(i) unique(data_mat[,i])))
  grid_size1 = length(row_vars)
  grid_size2 = length(col_vars)
  
  all_combo_strings1 = apply(full_grid1, 1, paste, collapse = "")
  combo_strings1 = apply(data_mat[,row_vars], 1, paste, collapse = "")
  all_combo_strings2 = apply(full_grid2, 1, paste, collapse = "")
  combo_strings2 = apply(data_mat[,col_vars], 1, paste, collapse = "")
  
  grid_to_axis1 = rep(all_combo_strings1, length(all_combo_strings2))
  grid_to_axis2 = rep(all_combo_strings2, each = length(all_combo_strings1))
  
  axis1 = match(combo_strings1, all_combo_strings1)
  axis2 = match(combo_strings2, all_combo_strings2)
  
  bubble_size = matrix(NA, ncol = nrow(full_grid2), nrow = nrow(full_grid1))
  bubble_color = matrix(NA, ncol = nrow(full_grid2), nrow = nrow(full_grid1))
  
  bubble_size[matrix(c(axis1, axis2), ncol = 2)] = response
  bubble_color[matrix(c(axis1, axis2), ncol = 2)] = response
  
  return(list(bubble_color, bubble_size, col_vars, row_vars))
  
}