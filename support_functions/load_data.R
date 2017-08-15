load_data <- function(data_mat_file, response_file) {
  
  # read the file
  data_mat = read.csv(data_mat_file, header = T, colClasses = "character")
  response = read.csv(response_file, header = T)
  
  # make response not an array
  if (is.matrix(response) | is.data.frame(response)) {response = response[,1]}
  
  return(list(data_mat, response))
  
}