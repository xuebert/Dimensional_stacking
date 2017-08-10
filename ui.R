rm(list=ls())

library(shiny)
library(gridExtra)

shinyUI(
  fluidPage(
    titlePanel("Dimensional stacking application"),
    
    fluidRow(
      ####################################################
      column(4, 
             
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
               
               sliderInput("width", label = "Width (in)", min=3, max=20, value=10, ticks=F),
               sliderInput("height", label = "Height (in)", min=3, max=20, value=5, ticks=F),
               
               textAreaInput("col_vars", "Choose variables in columns", "", width = "230px", height = "50px"),
               textAreaInput("row_vars", "Choose variables in rows", "", width = "230px", height = "50px"),
               checkboxInput('normalize', 'Normalize response?', TRUE),
               checkboxInput('log_data', 'Log10 transform response?', FALSE),
               
               br(),
               tags$head(tags$style(HTML('#update{background-color:lightgreen}'))),
               actionButton("update", "Update plot")
             )
      ),
      
      ####################################################
      column(4, 
             wellPanel(
               titlePanel("Tinkering parameters"),
               
               textInput("legend_title", label = "Legend title", value = "Legend title", placeholder = "Legend title"),
               
               textInput("cex_col", label = "Column label sizes", value = "", placeholder = ""),
               textInput("cex_row", label = "Row label sizes", value = "", placeholder = ""),
               
               numericInput("bubble_size_rescale", label = "Bubble size scaling factor", value = 1, min = 0, step = 0.05),
               numericInput("var_label_size", label = "Variable label size", value = 1, min = 0, step = 0.05),
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