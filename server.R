# server.R

rm(list=ls())

source("support_functions/load_library_data.R")
source("support_functions/bubble_chart.R")
source("support_functions/setup_color_scale.R")
source("support_functions/make_dimensional_stacking.R")

shinyServer(function(input, output) {
  
  #################### setup ####################
  
  get_col_vars <- reactive({col_vars = rev(strsplit(input$col_vars, ",")[[1]])})
  get_row_vars <- reactive({rev(strsplit(input$row_vars, ",")[[1]])})
  
  get_normalize <- reactive({input$normalize})
  get_log_data <- reactive({input$log_data})
  get_cex_col <- reactive({as.numeric(strsplit(input$cex_col, ",")[[1]])})
  get_cex_row <- reactive({as.numeric(strsplit(input$cex_row, ",")[[1]])})
  get_bubble_size_rescale <- reactive({as.numeric(input$bubble_size_rescale)})
  get_legend_title <- reactive({input$legend_title})
  get_w <- reactive({as.numeric(input$w)})
  get_h <- reactive({as.numeric(input$h)})
  get_var_label_size <- reactive({as.numeric(input$var_label_size)})
  get_color <- reactive({input$color})
  
  # get data functions
  get_data_mat_file <- reactive({
    ifelse(is.null(input$data_mat_file), data_mat_file <- "experiment_variables.csv", data_mat_file <- input$data_mat_file$datapath)
  })
  get_response_file <- reactive({
    ifelse(is.null(input$response_file), response_file <- "SEAP.csv", response_file <- input$response_file$datapath)
  })
  
  get_update <- reactive({input$update})
  get_data <- reactive({load_library_data(data_mat_file = get_data_mat_file(), response_file = get_response_file())})
  
  #################### get all ####################
  get_all <- reactive({
    if (get_update() == 0) {
      return_list = NA
    } else {
      # only output file when button is pressed (I don't get this logic)
      isolate({
        list(
          data_mat = get_data()[[1]], 
          response = get_data()[[2]], 
          row_vars = get_row_vars(), 
          col_vars = get_col_vars(), 
          normalize = get_normalize(), 
          log_data = get_log_data(), 
          cex_col = get_cex_col(), 
          cex_row = get_cex_row(), 
          selected_color = get_color(), 
          bubble_size_rescale = get_bubble_size_rescale(), 
          var_label_size = get_var_label_size()
        )
      })
    }
  })
  #################### plotting ####################
  
  
  # this function makes the plots
  make_plot <- function() {
    return_list = get_all()
    if (identical(NA, return_list)) {return()}

    do.call(make_dimensional_stacking, return_list)
    # make_dimensional_stacking(data_mat = data_mat, 
    #                           response = response, 
    #                           row_vars = get_row_vars(), 
    #                           col_vars = get_col_vars(), 
    #                           normalize = get_normalize(), 
    #                           log_data = get_log_data(), 
    #                           cex_col = get_cex_col(), 
    #                           cex_row = get_cex_row(), 
    #                           selected_color = get_color(), 
    #                           bubble_size_rescale = get_bubble_size_rescale(), 
    #                           var_label_size = get_var_label_size())
  }
  
  make_legend <- function() {
    return_list = get_all()
    if (identical(NA, return_list)) {return()}
    data_mat = return_list$data_mat
    response = return_list$response
    col_vars = return_list$col_vars
    row_vars = return_list$row_vars
    bubble_size_rescale = return_list$bubble_size_rescale
    selected_color = return_list$selected_color
    
    # get dimension of bubble plot
    return_list = formatting(data_mat, response, col_vars, row_vars)
    num_col = ncol(return_list[[1]])
    num_row = nrow(return_list[[1]])
    
    plot_storage = matrix(c(seq(0.01, 1, by = 0.1), 1), ncol = 11)
    plot_storage = rbind(plot_storage, matrix(rep(NA, 11 * (num_row - 1)), ncol = 11))
    plot_storage = cbind(plot_storage, matrix(rep(NA, nrow(plot_storage) * (num_col - 11)), nrow = nrow(plot_storage)))
    
    par(mar = c(4,7,6,2))
    par(xpd = NA)
    
    bubble_chart(plot_storage, plot_storage * bubble_size_rescale, selected_color = selected_color, bty = "n")
    text(c(1, 11), y = rep(nrow(plot_storage) - 2, 2), labels = c(min(response), max(response)), cex = 1)
    text(6.5, nrow(plot_storage) + 2, labels = get_legend_title(), cex = 1.5, adj = c(0.5, 0.5))
  }
  
  output$plot.ui <- renderUI({plotOutput("plot", width = get_w() * 128, height = get_h() * 128)})
  output$plot <- renderPlot({make_plot()}, res = 128)

  output$legend.ui <- renderUI({plotOutput("legend", width = get_w() * 128, height = get_h() * 128)})
  output$legend <- renderPlot({make_legend()}, res = 128)
  
  #################### table ####################
  # make output table
  output_table <- observe({
    if (input$table_output == 0) return()
    
    return_list = get_data()
    bubble_colors = return_list[[1]]
    bubble_sizes = return_list[[2]]
    
    write.table(bubble_colors, file = paste(input$outfile, "_color_table.csv", sep = ""), sep = ",", quote = F, col.names = T)
    write.table(bubble_sizes, file = paste(input$outfile, "_size_table.csv", sep = ""), sep = ",", quote = F, col.names = T)
  })
  
  #################### output figure ####################
  # make figure
  output_graphic <- observe({
    if (input$graph_output == 0) return()
    
    # only output file when button is pressed (I don't get this logic)
    isolate({
      # store number form of outfile_format
      outfile_format = as.numeric(input$outfile_format)
      
      # get figure print out function
      figure_func_list = list("pdf" = pdf, "png" = png, "jpg" = jpeg, "bmp" = bmp)
      figure_func <- figure_func_list[[outfile_format]]
      
      # pdf (single file with multi pages)
      if (outfile_format == 1) { 
        
        pdf(paste(input$outfile, ".pdf", sep = ""), width = get_w(), height = get_h(), useDingbats = F)
        make_plot()
        make_legend()
        dev.off()
        
      } else { # png jpg and bmp
        
        figure_func(paste(input$outfile, ".", names(figure_func_list)[[as.numeric(input$outfile_format)]], sep = ""), width = get_w(), height = get_h(), units = "in", res = 300)
        make_plot()
        dev.off()
      }
    })
  })
})