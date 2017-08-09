optimize_order <- function(data_mat, response) {
  library(party)
  
  data_frame = cbind(data_mat, response)
  
  tree_obj = ctree(response ~ ., data_frame)
  top_node = tree_obj@tree$psplit$variableName
  
  find_vars <- function(node_obj, storage = NULL, current_level = 0) {
    
    # initialize storage
    if (is.null(storage)) {storage = matrix(NA, nrow = 0, ncol = 3, dimnames = list(NULL, c("var_name", "level", "stat")))}
    
    current_level = current_level + 1
    if (node_obj$terminal) { # don't update if leaf node
      return(storage)
    } else {
      if (!is.null(node_obj$left)) {
        storage = find_vars(node_obj$left, storage, current_level)
      }
      if (!is.null(node_obj$right)) {
        storage = find_vars(node_obj$right, storage, current_level)
      }
    }
    storage = rbind(storage, c(node_obj$psplit$variableName, current_level, node_obj$criterion$statistic[node_obj$psplit$variableName]))
    return(storage)
    
  }
  
  storage = find_vars(tree_obj@tree)
  
  storage = storage[order(as.numeric(storage[,2]), as.numeric(storage[,3])),]
  var_order = unique(c(storage[,1], colnames(data_mat))) # include colnames(data_mat) in case a variable wasn't included
  
  # first is actually last
  var_order = rev(var_order)
  
  col_vars = var_order[seq(2, length(var_order), by = 2)]
  row_vars = var_order[seq(1, length(var_order), by = 2)]
  
  return(list(col_vars, row_vars))
  
}