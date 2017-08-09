# test_make_dimensional_stacking

setwd("~/Dropbox/Albert Xue/Research/Deployment/dimensional_stacking/")


load("../../SNA/data/20170206/20170206_Sigma_SEAP_Results_Processed_SNAonly.RData")

data_mat = experiment_variables
response = SEAP_storage[,3]

# change up long names in data_mat
switch_value <- function(full_vector, orig_value, new_value) {
  full_vector = as.character(full_vector)
  full_vector[full_vector == orig_value] = new_value
  return(full_vector)
}
data_mat[,"Lipid"] = switch_value(data_mat[,"Lipid"], "100% DOPC", "100%")
data_mat[,"Lipid"] = switch_value(data_mat[,"Lipid"], "80:20 DOPC:DOPE", "80%")
data_mat[,"OligoDensity"] = switch_value(data_mat[,"OligoDensity"], "Very High", "VHigh")

write.csv(data_mat, file = "experiment_variables.csv", row.names = F, quote = F)

write.csv(response, file = "SEAP.csv", row.names = F, quote = F)