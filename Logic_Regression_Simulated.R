rm(list=ls())
clc <- function() cat(rep("\n", 50))
clc()
## -------------------------------------------------------------------------- ##
# REQUIRED PACKAGES
req_packages <- c("survival", "LogicReg")
# DOWNLOADING LIBRARIES
installed_packages <- installed.packages()
packages <- installed_packages[, 1]
for(i in req_packages)
{
  if(! is.element(i, installed_packages))
  {
    install.packages(i)
  }
}

# IMPORTING LIBRARIES
lapply(req_packages, library, character.only = TRUE)

# CLEAR
rm(req_packages, installed_packages, packages, i)
## -------------------------------------------------------------------------- ##
# FUNCTIONS DEFINITION

## -------------------------------------------------------------------------- ##
# DOWNLOADING & IMPORTING DATA
script_dir <- dirname(sys.frame(1)$ofile)
setwd(script_dir)
data_name <- "Simulated_Data.csv"
if (file.exists(data_name))
{
  my_data <- read.csv(data_name)
  print("Data has been imported!")
} else {
  my_data <- matrix(0, nrow = 500, ncol = 8)
  for (i in 1:(dim(my_data)[2]-2)) 
  {
    my_data[, i] <- rbinom(n = 500, size = 1, prob = 0.5)
  }
  my_data[, dim(my_data)[2]-1] <- ((my_data[, 3] | !my_data[, 1]) & !my_data[, 4]) | (my_data[, 5] & my_data[, 2])
  my_data[, dim(my_data)[2]] <- !xor(my_data[, dim(my_data)[2]-1], 
                                     rbinom(n = 500, size = 1, prob = 2/3))
  # dimnames(my_data) <- list(c(0), c("X1", "X2", "X3", "X4", "X5", "X6", "X7", "X8"))
  write.csv(my_data, file = data_name, row.names = F)
  print("Data has been created and saved!")
  
  # CLEAR
  rm(i)
}

# CLEAR
rm(script_dir, data_name)
## -------------------------------------------------------------------------- ##
#SETTING UP THE PREDICTORS & RESPONSE
predictors_data <- my_data[, c(1, 2, 3, 4, 5, 6)]
response_data <- my_data[, 8]
## -------------------------------------------------------------------------- ##
# METHOD IMPLEMENTATION
annealing_params <- logreg.anneal.control(start = 1, end = -5, iter = 500000,
                                          update = 10000)
tree_params <- logreg.tree.control(treesize = 16, opers = c(1,2), minmass = 25)
my_model <- logreg(resp = response_data, bin = predictors_data, type = 3, select = 1,
                   ntrees = c(5), nleaves = c(16), kfold = 5, nrep = 10,
                   anneal.control = annealing_params, tree.control = tree_params)
## -------------------------------------------------------------------------- ##
# ERRORS & RESULTS
plot(my_model, pscript = FALSE, title = TRUE)
predicted_response <- predict(my_model)
# predicted_response <- as.integer(predicted_response >= 0.5)
# error <- response_data - predicted_response
plot(response_data, predicted_response)
aaa <- data.frame(response_data, predicted_response)
