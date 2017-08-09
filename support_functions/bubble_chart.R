bubble_chart <- function(bubble_colors, bubble_sizes, color_scale = NULL, xlab = "", ylab = "", x_axis_opt = NULL, y_axis_opt = NULL, bty = NULL, white_buffer = T, lwd = 1, main = "", selected_color = "red") {
  # color_colors are assumed to be between 0-1 and linearly scale within color_scale
  # colors and sizes are assumed to be between 0-1 and linearly scale within color_scale_list
  load("support_functions/color_scale.RData")
  color_scale = color_scale_list[[selected_color]]
  
  # initialize common plot parameters
  x_axis = c(0, ncol(bubble_colors) + 1)
  y_axis = c(0, nrow(bubble_colors) + 1)
  if (!white_buffer) {
    x_axis = c(1, ncol(bubble_colors))
    y_axis = c(1, nrow(bubble_colors))
  }
  
  y_grid = rep(1:nrow(bubble_colors), each = ncol(bubble_colors))
  x_grid = rep(1:ncol(bubble_colors), nrow(bubble_colors))
  
  # map bubble_colors to the proper colors
  mapped_colors = color_scale[matrix(t(round(bubble_colors * (length(color_scale) - 1))), ncol = 1, byrow = T) + 1]
  
  p <- symbols(x = x_grid, y = rev(y_grid), circles = (matrix(t(bubble_sizes), ncol = 1, byrow = T)/pi)^0.5, bg = mapped_colors, fg = "black", xlim = x_axis, ylim = y_axis, inches = F, xlab = xlab, ylab = ylab, xaxt = "n", yaxt = "n", bty = bty, lwd = lwd, main = main)
  
  if (!is.null(x_axis_opt)) {
    suppressWarnings(do.call(axis, x_axis_opt))
  }
  if (!is.null(y_axis_opt)) {
    y_axis_opt$labels = rev(y_axis_opt$labels)
    suppressWarnings(do.call(axis, y_axis_opt))
  }
  
  assign("dim_global", list(x_axis, y_axis), envir = .GlobalEnv)
  return(p)
}

