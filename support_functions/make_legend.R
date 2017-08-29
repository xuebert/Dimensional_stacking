make_legend <- function(list_args) {
  
  # get dimension of bubble plot
  list_args$return_formatted_data = T
  list_args$legend_title = NULL # remove legend title
  list_args$width = NULL
  list_args$height = NULL
  return_list = do.call(make_dimensional_stacking, list_args)
  num_col = ncol(return_list[[1]])
  num_row = nrow(return_list[[1]])
  orig_range = return_list[[2]]
  
  legend_color = matrix(c(seq(0.01, 1, by = 0.1), 1), ncol = 11)
  legend_color = rbind(legend_color, matrix(rep(NA, 11 * (num_row - 1)), ncol = 11))
  legend_color = cbind(legend_color, matrix(rep(NA, nrow(legend_color) * (num_col - 11)), nrow = nrow(legend_color)))
  
  par(mar = c(4,7,6,2))
  par(xpd = NA)
  
  if (list_args$diverge) {
    list_args$selected_color = "diverge"
    legend_size = matrix(c(seq(1,0.2,by=-0.2), 0.01, seq(0.2,1,by=0.2))*0.9, nrow = 1)
    legend_size = rbind(legend_size, matrix(rep(NA, 11 * (num_row - 1)), ncol = 11))
    legend_size = cbind(legend_size, matrix(rep(NA, nrow(legend_size) * (num_col - 11)), nrow = nrow(legend_size)))
  } else {
    legend_size = legend_color
  }
  
  bubble_chart(legend_color, legend_size * list_args$bubble_size_rescale, selected_color = list_args$selected_color, bty = "n")
  
  text(c(1, 11), y = rep(nrow(legend_color) - 2, 2), labels = orig_range, cex = 1)
  if (list_args$diverge) {text(6, nrow(legend_color) - 2, labels = 0)}
  text(6.5, nrow(legend_color) + 2, labels = list_args$legend_title, cex = 1.5, adj = c(0.5, 0.5))
}