# test_make_dimensional_stacking

setwd("~/Dropbox/Albert Xue/Research/Deployment/dimensional_stacking/")

source("support_functions/make_dimensional_stacking.R")
source("support_functions/load_library_data.R")

load("../../SNA/data/20170206/20170206_Sigma_SEAP_Results_Processed_SNAonly.RData")

# data_mat = experiment_variables
# response = SEAP_storage[,3, drop = F]
# 
# response = response / max(response)

# make_dimensional_stacking(data_mat, response)
# setup_dimensional_stacking(data_mat, response)

return_list = load_library_data(data_mat_file = "experiment_variables.csv", response_file = "SEAP.csv")
data_mat = return_list[[1]]
response = return_list[[2]]

make_dimensional_stacking(data_mat, response, )