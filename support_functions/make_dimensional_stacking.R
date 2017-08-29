make_dimensional_stacking <- function(data_mat_file, response_file, row_vars = NULL, col_vars = NULL, value_order_obj = NULL, bubble_size_rescale = 1, cex_col = character(0), cex_row = character(0), lwd = 1, var_label_size = 0.8, selected_color = "red", normalize = T, log_data = T, diverge = F, return_formatted_data = F) {
  
  source("support_functions/bubble_chart.R")
  source("support_functions/formatting.R")
  source("support_functions/load_data.R")
  
  return_list = load_data(data_mat_file, response_file)
  data_mat = return_list[[1]]
  response = return_list[[2]]
  orig_range = range(response, na.rm = T)
  
  # response changes
  if (log_data) {
    response[response > 0] = log10(response[response > 0] + 1)
    response[response < 0] = -log10(-response[response < 0] + 1)
  }
  if (normalize) {
    max_value = max(abs(response))
    response = response / max_value
  }
  if (diverge) {
    response[response >= 0] = response[response >= 0] / 2 + 0.5
    response[response < 0] = (response[response < 0] / 2) + 0.5
    selected_color = "diverge"
  }
  
  # format bubble_color and bubble_size.  Also sets defaults for col_vars and row_vars if they weren't specified
  return_list = formatting(data_mat, response, col_vars, row_vars, value_order_obj)
  if (return_formatted_data) {return(list(return_list[[1]], orig_range))}
  bubble_color = return_list[[1]]
  bubble_size = return_list[[2]]
  col_vars = return_list[[3]]
  row_vars = return_list[[4]]
  grid_col = return_list[[5]]
  grid_row = return_list[[6]]
  
  # if diverging, convert sizes properly
  if (diverge) {
    bubble_size[bubble_color < 0.5 & !is.na(bubble_color)] = bubble_size[bubble_color < 0.5 & !is.na(bubble_color)] * -2 + 1
    bubble_size[bubble_color >= 0.5 & !is.na(bubble_color)] = (bubble_size[bubble_color >= 0.5 & !is.na(bubble_color)] - 0.5) * 2
  }
  
  # initialize null parameters
  if (length(cex_col)==0 | identical(cex_col, "default")) {cex_col = seq(1, 0.4, length.out = max(c(length(row_vars), length(col_vars))))}
  if (length(cex_row)==0 | identical(cex_row, "default")) {cex_row = seq(1, 0.4, length.out = max(c(length(row_vars), length(col_vars))))}
  
  var_names = colnames(data_mat)
  names(var_names) = var_names
  
  # support function
  line2user <- function(line, side) {
    lh <- par('cin')[2] * par('cex') * par('lheight')
    x_off <- diff(grconvertX(0:1, 'inches', 'user'))
    y_off <- diff(grconvertY(0:1, 'inches', 'user'))
    switch(side,
           `1` = par('usr')[3] - line * y_off * lh,
           `2` = par('usr')[1] - line * x_off * lh,
           `3` = par('usr')[4] + line * y_off * lh,
           `4` = par('usr')[2] + line * x_off * lh,
           stop("side must be 1, 2, 3, or 4", call.=FALSE))
  }
  
  ################################################################################
  # setup and make bubble plot
  par(mar = c(4,7,6,2))
  par(xpd = NA)
  plot(1, type = "n", xlab = "", ylab = "", main = "", ylim = c(0,nrow(bubble_color)), xlim = c(0,ncol(bubble_color)), bty = "n", axes = F)
  par(new = T)
  
  p <- bubble_chart(bubble_colors = bubble_color, bubble_sizes = bubble_size * bubble_size_rescale, bty = "n", white_buffer = F, lwd = lwd, selected_color = selected_color)
  
  # get text placement
  get_placement <- function(labels, number_range = c(0, ncol(bubble_color))) {
    
    center_points = vector("list", length(labels))
    text_labels = vector("list", length(labels))
    
    boundaries = seq(number_range[[1]], number_range[[2]], length.out = 1 + length(labels[[1]]))
    center_points[[1]] = sapply(2:length(boundaries), function(i) mean(c(boundaries[[i]], boundaries[[i-1]]))) + 0.5
    text_labels[[1]] = rep(labels[[1]], length.out = length(center_points[[1]]))
    
    for (n_level in 2:length(labels)) {
      num_labels = length(labels[[n_level]])
      boundaries = seq(number_range[[1]], number_range[[2]], length.out = 1 + length(center_points[[n_level-1]]) * num_labels)
      center_points[[n_level]] = sapply(2:length(boundaries), function(i) mean(c(boundaries[[i]], boundaries[[i-1]]))) + 0.5
      text_labels[[n_level]] = rep(labels[[n_level]], length.out = length(center_points[[n_level]]))
    }
    return(list(text_labels, center_points))
  }
  
  ####################
  # make text
  # make column information
  return_list = get_placement(rev(lapply(grid_col, unique)))
  text_values = return_list[[1]]
  coord_values = return_list[[2]]
  par(xpd = NA)
  counter = 0
  for (n_level in length(text_values):1) {
    counter = counter + 1
    line_diff = line2user(2, 3) - line2user(1, 3)
    # variable values
    text(x = coord_values[[n_level]], y = line2user(counter, 3) + line_diff * 0.1, labels = text_values[[n_level]], cex = cex_col[[n_level]], offset = 0, adj = c(0.5, 0))
    # variable names
    text(x = 0.3, y = line2user(counter, 3) + line_diff * 0.1, labels = var_names[[col_vars[[counter]]]], cex = var_label_size, offset = 0, adj = c(1, 0))
    
    for (n_line in 1:length(coord_values[[n_level]])) {
      first_midpoint = coord_values[[n_level]][[1]] - 0.5
      line_coords = c((n_line - 1) * 2 * first_midpoint, 0) + 0.5
      line_coords[[2]] = line_coords[[1]] + 2 * first_midpoint
      
      # adjust border of line
      line_coords[[1]] = line_coords[[1]] + 0.25
      line_coords[[2]] = line_coords[[2]] - 0.25
      lines(x = line_coords, y = rep(line2user(counter, 3), 2), lwd = 0.8)
    }
  }
  
  # make row information
  return_list = get_placement(labels = rev(lapply(grid_row, unique)), number_range = c(0,nrow(bubble_color)))
  text_values = return_list[[1]]
  coord_values = return_list[[2]]
  counter = 0
  for (n_level in length(return_list[[1]]):1) {
    counter = counter + 1
    line_diff = line2user(1, 2) - line2user(2, 2)
    # variable values
    text(x = line2user(counter, 2) - line_diff * 0.1, y = rev(coord_values[[n_level]]), labels = text_values[[n_level]], cex = cex_row[[n_level]], adj = c(0.5, 0), srt = 90)
    # variable names
    text(x = line2user(counter, 2) - line_diff * 0.1, y = 0.3, labels = var_names[[row_vars[[counter]]]], pos = 2, cex = var_label_size, offset = 0, srt = 45, adj = c(1, 0))
    
    for (n_line in 1:length(coord_values[[n_level]])) {
      first_midpoint = coord_values[[n_level]][[1]] - 0.5
      line_coords = c((n_line - 1) * 2 * first_midpoint, 0) + 0.5
      line_coords[[2]] = line_coords[[1]] + 2 * first_midpoint
      
      # adjust border of line
      line_coords[[1]] = line_coords[[1]] + 0.25
      line_coords[[2]] = line_coords[[2]] - 0.25
      lines(x = rep(line2user(counter, 2), 2), y = line_coords, lwd = 0.8)
    }
  }
  
  return(p)
}
