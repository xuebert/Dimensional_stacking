make_legend <- function(list_args) {
  
  # get dimension of bubble plot
  list_args$return_formatted_data = T
  list_args$legend_title = NULL # remove legend title
  formatted_data = do.call(make_dimensional_stacking, list_args)
  num_col = ncol(formatted_data)
  num_row = nrow(formatted_data)
  
  formatted_data = matrix(c(seq(0.01, 1, by = 0.1), 1), ncol = 11)
  formatted_data = rbind(formatted_data, matrix(rep(NA, 11 * (num_row - 1)), ncol = 11))
  formatted_data = cbind(formatted_data, matrix(rep(NA, nrow(formatted_data) * (num_col - 11)), nrow = nrow(formatted_data)))
  
  par(mar = c(4,7,6,2))
  par(xpd = NA)
  
  bubble_chart(formatted_data, formatted_data * list_args$bubble_size_rescale, selected_color = list_args$selected_color, bty = "n")
  text(c(1, 11), y = rep(nrow(formatted_data) - 2, 2), labels = c(min(formatted_data, na.rm = T), max(formatted_data, na.rm = T)), cex = 1)
  text(6.5, nrow(formatted_data) + 2, labels = list_args$legend_title, cex = 1.5, adj = c(0.5, 0.5))
}