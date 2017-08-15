# server.R

rm(list=ls())

source("support_functions/load_data.R")
source("support_functions/make_dimensional_stacking.R")
source("support_functions/make_legend.R")

shinyServer(function(input, output, session) {
  
  #################### setup ####################
  
  get_col_vars <- reactive({col_vars = rev(strsplit(input$col_vars, ",")[[1]])})
  get_row_vars <- reactive({rev(strsplit(input$row_vars, ",")[[1]])})
  
  get_normalize <- reactive({input$normalize})
  get_log_data <- reactive({input$log_data})
  get_cex_col <- reactive({as.numeric(strsplit(input$cex_col, ",")[[1]])})
  get_cex_row <- reactive({as.numeric(strsplit(input$cex_row, ",")[[1]])})
  get_bubble_size_rescale <- reactive({as.numeric(input$bubble_size_rescale)})
  get_legend_title <- reactive({input$legend_title})
  get_width <- reactive({input$width})
  get_height <- reactive({input$height})
  get_var_label_size <- reactive({as.numeric(input$var_label_size)})
  get_color <- reactive({input$color})
  
  # get data functions
  get_data_mat_file <- reactive({
    ifelse(is.null(input$data_mat_file), data_mat_file <- "experiment_variables.csv", data_mat_file <- input$data_mat_file$datapath)
  })
  get_response_file <- reactive({
    ifelse(is.null(input$response_file), response_file <- "SEAP.csv", response_file <- input$response_file$datapath)
  })
  get_value_order_file <- reactive({
    ifelse(is.null(input$value_order_file), value_order_file <- "value_order_example.csv", value_order_file <- input$value_order_file)
  })
  # get_value_order_file <- function() {
  #   ifelse(is.null(input$value_order_file), value_order_file <- "value_order_example.csv", value_order_file <- input$value_order_file)
  #   return(value_order_file)
  # }
  get_value_order <- reactive({
    read_value_order = reactiveFileReader(1000, session, get_value_order_file(), readLines)
    read_value_order()
  })
  
  get_update <- reactive({input$update})
  get_data <- reactive({return_var = load_data(data_mat_file = get_data_mat_file(), response_file = get_response_file())})
  
  get_outfile <- reactive({input$outfile})
  
  #################### get all ####################
  # this is required to load all variables for plotting.  It is needed for the isolate command so that new plots are only generated when the update button is pressed
  get_all <- reactive({
    A = get_value_order()
    browser()
    if (get_update() == 0) { # don't execute plotting on startup, not until update button is pressed
      return_list = NA
    } else {
      # only output file when button is pressed (I don't get this logic)
      isolate({
        list(
          data_mat_file = get_data_mat_file(), 
          response_file = get_response_file(), 
          row_vars = get_row_vars(), 
          col_vars = get_col_vars(), 
          normalize = get_normalize(), 
          log_data = get_log_data(), 
          cex_col = get_cex_col(), 
          cex_row = get_cex_row(), 
          selected_color = get_color(), 
          bubble_size_rescale = get_bubble_size_rescale(), 
          var_label_size = get_var_label_size(),
          legend_title = get_legend_title(),
          width = get_width(),
          height = get_height()
        )
      })
    }
  })
  #################### plotting ####################
  
  # this function makes the plots
  master_dim_stack <- function() {
    return_list = get_all()
    if (identical(NA, return_list)) {return()}
    
    # remove irrelevant stuff
    return_list$legend_title = NULL
    return_list$width = NULL
    return_list$height = NULL
    do.call(make_dimensional_stacking, return_list)
  }
  
  master_legend <- function() {
    return_list = get_all()
    if (identical(NA, return_list)) {return()}
    
    return_list$width = NULL
    return_list$height = NULL
    do.call(make_legend, list(list_args = return_list))
  }
  
  # dim stack plot
  output$plot.ui <- renderUI({
    return_list = get_all()
    if (identical(NA, return_list)) {return()}
    width = return_list$width
    height = return_list$height
    plotOutput("plot", width = width * 128, height = height * 128)
  })
  output$plot <- renderPlot({master_dim_stack()}, res = 128)
  
  # legend plot
  output$legend.ui <- renderUI({
    return_list = get_all()
    if (identical(NA, return_list)) {return()}
    width = return_list$width
    height = return_list$height
    plotOutput("legend", width = width * 128, height = height * 128)
  })
  output$legend <- renderPlot({master_legend()}, res = 128)
  
  #################### table ####################
  # make output table
  output_table <- observe({
    if (input$table_output == 0) return()
    return_list = get_all()
    if (identical(NA, return_list)) {return()}
    
    # get data_mat of bubble plot
    return_list$return_formatted_data = T
    return_list$legend_title = NULL # remove legend title
    return_list$width = NULL
    return_list$height = NULL
    formatted_data = do.call(make_dimensional_stacking, return_list)
    
    write.table(formatted_data, file = paste(get_outfile(), ".csv", sep = ""), sep = ",", quote = F, col.names = T)
  })
  
  #################### output figure ####################
  # make figure
  output_graphic <- observe({
    if (input$graph_output == 0) return()
    
    # only output file when button is pressed (I don't get this logic)
    isolate({
      # get width/height
      return_list = get_all()
      if (identical(NA, return_list)) {return()}
      width = return_list$width
      height = return_list$height
      
      # store number form of outfile_format
      outfile_format = as.numeric(input$outfile_format)
      
      # get figure print out function
      figure_func_list = list("pdf" = pdf, "png" = png, "jpg" = jpeg, "bmp" = bmp)
      figure_func <- figure_func_list[[outfile_format]]
      
      # pdf (single file with multi pages)
      if (outfile_format == 1) { 
        
        pdf(paste(input$outfile, ".pdf", sep = ""), width = width, height = height, useDingbats = F)
        master_dim_stack()
        master_legend()
        dev.off()
        
      } else { # png jpg and bmp
        
        figure_func(paste(input$outfile, ".", names(figure_func_list)[[as.numeric(input$outfile_format)]], sep = ""), width = width, height = height, units = "in", res = 300)
        master_dim_stack()
        dev.off()
      }
    })
  })
})