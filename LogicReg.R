rm(list=ls())
clc <- function() cat(rep("\n", 50))
clc()
## -------------------------------------------------------------------------- ##
# REQUIRED PACKAGES
req_packages <- c("survival", "funModeling", "tidyverse", "Hmisc", "LogicReg", 
                  "clusteval", "corrplot", "DMwR")
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
# PREPARING DATA
CleanMyData <- function(my_data, what_symptoms)
{
  # RESULT AND SYMPTOMS COLUMNS + NAMES
  covid_idx <- c(4)
  symptoms_idx <- c(24, 25, 26, 27, 29, 31, 32, 34, 35, 36, 37, 38, 39, 40, 41)
  all_symptoms <- c("CTAB", "LB", "Rhonchi", "Wheezes", "Cough", "Fever", "SOB",
                    "Diarrhea", "Fatigue", "Headache", "LOS", "LOT", "RN", "MS",
                    "ST")
  my_data <- my_data[, c(covid_idx, symptoms_idx)]
  # REMOVING NA/MISSING VALUES
  my_data[my_data == ""] <- NA
  my_data <- filter_at(my_data, all_of(1:ncol(my_data)), all_vars(!is.na(.)))
  # UPPER-CASING DATA
  my_data <- as.data.frame(sapply(my_data, toupper))
  # CREATING BINARY DATA
  my_data[, 1] <- my_data[, 1] == "POSITIVE"
  my_data <- as.data.frame(sapply(my_data, as.logical))
  my_data <- as.data.frame(sapply(my_data, as.integer))
  # CHOOSING DESIRED SYMPTOMS
  idx <- match(what_symptoms, all_symptoms)
  my_data <- my_data[, c(1, idx+1)]
  # ASSIGNING COLUMNS NAMES
  colnames(my_data) <- c("Result", what_symptoms)
  # CHANING THE DATA TYPE OF RESPONSE VECTOR
  my_data[, 1] <- sapply(my_data[, 1], as.factor)
  return(my_data)
}

# FIXING UNBALANCING ISSUE
BalanceMySample <- function(my_data, k, over, under, N)
{
  my_data <- filter_at(my_data, all_of(2:ncol(my_data)), any_vars(as.logical(.)))
  my_data <- SMOTE(Result ~ ., my_data, k = k, perc.over = over, perc.under = under)
  my_data[, 1] <- sapply(my_data[, 1], as.numeric) - 1
  my_data[my_data >= 0.5] <- 1
  my_data[my_data < 0.5] <- 0
  my_data <- as.data.frame(sapply(my_data, as.logical))
  pos_data <- as.data.frame(matrix(rep(T, N*ncol(my_data)), nrow = N, byrow = F))
  colnames(pos_data) <- colnames(my_data)
  my_data <-rbind(my_data, pos_data)
  
  return(my_data)
}

# PIE PLOT
ShowPiePlot <- function(my_data)
{
  vec <- as.numeric(table(my_data[, 1])) / nrow(my_data) * 100
  names <- c("Negative\n", "Positive\n")
  names <- paste(names, paste(round(vec, digits = 2), "%", sep = ""))
  colors = c("deepskyblue4","deepskyblue1")
  pie(vec, labels = names, col = colors)
}

# BAR PLOT
ShowBarPlot <- function(my_data)
{
  N <- ncol(my_data) - 1
  M <- nrow(my_data)
  mat <- matrix(0, nrow = 2, ncol = N)
  for (i in 1L:N)
  {
    mat[2, N-i+1] <- as.numeric(table(my_data[, i+1])[2]) / M * 100
    mat[1, N-i+1] <- as.numeric(table(my_data[, 1], my_data[, i+1])[2, 2]) / M * 100
  }
  colors = c("deepskyblue1","deepskyblue4")
  par(oma=c(0,1,0,1))
  barplot(mat, horiz = T, beside = T, names.arg = colnames(my_data)[(N+1):2], las = 1,
          xlab = "Percentage of patients showing each symptom",
          xlim = c(0, 60), ylim = c(0, 40), col = colors)
  par(oma=c(0,1,0,1), xpd = F)
  grid(nx = 6, ny = 0, col = "lightgray")
  par(oma=c(1,0,0,1))
  legend("bottomright", c("COVID-positive patients", "All patients"), bty = "n",
         cex = .6, fill = colors)
}

# CORRELATION
ShowCorrelation <- function(my_data)
{
  # CORRELATION MATRIX - JACCARD INDEX FOR PREDICTORS
  pred_data <- my_data[, -1]
  N <- ncol(my_data) - 1
  corr_mat <- matrix(0, nrow = N, ncol = N)
  for (i in 1L:(N-1))
  {
    for (j in (i+1):N)
    {
      corr_mat[i, j] <- cluster_similarity(pred_data[, i], pred_data[, j],
                                           similarity = "jaccard", 
                                           method = "independence")
      corr_mat[j, i] <- corr_mat[i, j]
    }
  }
  diag(corr_mat) <- 1
  dimnames(corr_mat) <- list(colnames(pred_data), colnames(pred_data))
  corrplot(corr_mat, method = "color", type = "upper", cl.lim = c(0, 1),
           addCoefasPercent = T, addCoef.col = "black", tl.col = "black", diag = FALSE)
  
  # CORRELATION VECTOR - JACCARD INDEX FOR RESPONSE
  resp_data <- my_data[, 1]
  corr_vec <- matrix(0, nrow = 1, ncol = N)
  for (i in 1L:N)
  {
    corr_vec[i] <- cluster_similarity(resp_data, pred_data[, i],
                                      similarity = "jaccard", 
                                      method = "independence")
  }
  dimnames(corr_vec) <- list("Result", colnames(pred_data))
  corrplot(corr_vec, method = "color", cl.lim = c(0, 1),
           addCoefasPercent = T, addCoef.col = "black", tl.col = "black",
           cl.pos = "b", cl.ratio = 1)
}
## -------------------------------------------------------------------------- ##
# DOWNLOADING & IMPORTING DATA
script_dir <- dirname(sys.frame(1)$ofile)
setwd(script_dir)
data_name <- "Carbon_Health_Data.csv"
if (file.exists(data_name))
{
  my_data <- read.csv(data_name)
  print("Data has been imported!")
} else {
  parent_URL <- "https://github.com/mdcollab/covidclinicaldata/raw/master/data/"
  dates1 <- c("10-20", "10-13", "10-06", "09-29", "09-22", "09-15", "09-08", "09-01", 
              "08-25", "08-18", "08-11", "08-04", "07-28", "07-21", "07-14", "07-07",
              "06-30", "06-23")
  dates2 <- c("06-16", "06-09", "06-02", "05-26", "05-19", "05-12", "05-05", "04-28",
              "04-21", "04-14", "04-07")
  file_name <- "_carbonhealth_and_braidhealth.csv"
  list <- paste(parent_URL, c(dates1, dates2), file_name, sep = "", collapse = NULL)
  data_list <- lapply(list, read.csv)
  for (i in (length(dates1) + 1:length(dates2)))
  {
    data_list[[i]] <- data_list[[i]][-ncol(data_list[[i]])]
  }
  my_data <- do.call(rbind, data_list)
  write.csv(my_data, file = data_name, row.names = F)
  print("Data has been downloaded and saved!")
  
  # CLEAR
  rm(parent_URL, dates1, dates2, file_name, list, data_list, i)
}

# CLEAR
rm(script_dir, data_name)
## -------------------------------------------------------------------------- ##
# PREPARING THE DATA
all_symptoms <- c("CTAB", "LB", "Rhonchi", "Wheezes", "Cough", "Fever", "SOB",
                  "Diarrhea", "Fatigue", "Headache", "LOS", "LOT", "RN", "MS",
                  "ST")
what_symptoms <- c("CTAB", "LB", "Rhonchi", "Cough", "Fever", "SOB",
                   "Diarrhea", "Fatigue", "Headache", "LOS", "RN", "MS",
                   "ST")
my_data <- CleanMyData(my_data, what_symptoms)
all_data <- my_data
my_data <- BalanceMySample(my_data, k = 2, over = 300, under = 600, N = 100)
# ShowPiePlot(my_data)
# ShowBarPlot(my_data)
# ShowCorrelation(my_data)

pred_data <- my_data[, -1]
resp_data <- my_data[, 1]
weight_vec <- 0.1*resp_data + rep(1, length(resp_data))
## -------------------------------------------------------------------------- ##
# METHOD IMPLEMENTATION
annealing_params <- logreg.anneal.control(start = 2, end = -3, iter = 500000,
                                          update = 50000)
tree_params <- logreg.tree.control(treesize = 16, opers = c(1,2))
my_model <- logreg(resp = resp_data, bin = pred_data, wgt = weight_vec,
                   type = 3, select = 1, ntrees = c(4), nleaves = c(12),
                   kfold = 10, nrep = 10,
                   anneal.control = annealing_params, tree.control = tree_params)
## -------------------------------------------------------------------------- ##
# ERRORS & RESULTS
plot(my_model, pscript = F, title = F)
pred_resp <- predict(my_model)
pred_resp_bin <- as.logical(pred_resp >= 0.5)
compare1 <- data.frame(resp_data, pred_resp)
acc_train <- cluster_similarity(resp_data, pred_resp_bin, similarity = "jaccard", 
                          method = "independence")
pred_resp_bin2 <- as.logical(predict(my_model, newbin = all_data[, -1]) >= 0.5) 
acc_all <- cluster_similarity(all_data[, 1], pred_resp_bin2,
                              similarity = "jaccard", method = "independence")
