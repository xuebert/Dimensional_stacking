# rm(list=ls())

library(shiny)
library(gridExtra)

shinyUI(
  fluidPage(
    titlePanel("Dimension stacking application"),
    
    fluidRow(
      ####################################################
      column(4, 
             # sidebarLayout(
             
             # load data
             wellPanel(
               titlePanel("File inputs and outputs"),
               fileInput("data_mat_file", "Choose data file"),
               fileInput("response_file", "Choose response file"),
               textInput("outfile", "Choose output file name", value = "example_output", placeholder = "example_output"),
               actionButton("table_output", label = "Make table file"),
               br(),
               br(),
               selectInput("outfile_format", label = "Figure format", choices = list("pdf" = 1, "png" = 2, "jpg" = 3, "bmp" = 4), selected = 1),
               actionButton("graph_output", label = "Make figure file")
             )
      ),
      
      ####################################################
      column(4, 
             # plotting parameters
             wellPanel(
               titlePanel("Plotting parameters"),
               
               textInput("w", "width (in)", value = "10", placeholder = "10"),
               textInput("h", "height (in)", value = "5", placeholder = "5"),
               
               textAreaInput("col_vars", "Choose variables in columns", "", width = "230px", height = "50px"),
               textAreaInput("row_vars", "Choose variables in rows", "", width = "230px", height = "50px"),
               checkboxInput('normalize', 'Normalize response?', TRUE),
               checkboxInput('log_data', 'Log transform response?', TRUE),
               
               br(),
               actionButton("update", "Update plot")
             )
      ),
      
      ####################################################
      column(4, 
             wellPanel(
               titlePanel("Tinkering parameters"),
               
               textInput("legend_title", label = "Legend title", value = "Legend title", placeholder = "Legend title"),
               
               textInput("cex_col", label = "Column label sizes", value = "1,0.8,0.6,0.4", placeholder = "1,0.8,0.6,0.4"),
               textInput("cex_row", label = "Row label sizes", value = "1,0.8,0.6,0.4", placeholder = "1,0.8,0.6,0.4"),
               
               textInput("bubble_size_rescale", label = "Bubble size scaling factor", value = "1", placeholder = "1"),
               textInput("var_label_size", label = "Variable label size", value = "1", placeholder = "1"),
               selectInput("color", label = "Select color scale", choices = list("Red" = "red", "Blue" = "blue", "Green" = "green", "Orange" = "orange", "Purple" = "purple"), selected = "red")
             )
      )),
    
    mainPanel(
      
      tabPanel("Plot",
               fluidRow(
                 column(10, uiOutput("plot.ui")),
                 column(10, uiOutput("legend.ui"))
               ))
    )
  )
)