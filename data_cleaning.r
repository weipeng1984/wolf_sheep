# this script is to clean the data 

# library the package
library(tidyverse)
library(dplyr)
library(tibble)

setwd("/Users/weipeng/Dropbox/Programming/R/project_r/Project5/implicit_intention_perception")

filenames = list.files(path = getwd(), pattern = 'Chasing_+.*csv')

# creating prolific id

prolific_id = substr(filenames, 9, 32)

# detecting ip loss prolific id
no_ip = data.frame(prolific_id = character(), stringsAsFactors = FALSE)

for (i in 1:length(prolific_id)) {
  df = read.csv(paste0('Chasing_', prolific_id[i], '.csv'), 
                    header = TRUE, sep = ',', stringsAsFactors = FALSE)

  if (dim(df)[2] < 24) {
    no_ip[i, 1] = prolific_id[i]
    df = add_column(df, ip = NA, .after = "internal_node_id")
  }
  no_ip = na.omit(no_ip)
  write.table(no_ip, file = paste0('no_ip', '.csv'), sep = ",", col.names = TRUE,
              row.names = FALSE, qmethod = "double")
  write.table(df, file = paste0('Chasing_', prolific_id[i], '.csv'), sep = ",", col.names = TRUE,
              row.names = FALSE, qmethod = "double")
}

# deleting irrelevant rows and columns

for (i in 1:length(prolific_id)) {

  df = read.csv(paste0('Chasing_', prolific_id[i], '.csv'), 
                    header = TRUE, sep = ',', stringsAsFactors = FALSE)
  df = df[-c(1:6), ]
  df = df[, -c(1, 5:6, 8:10, 12:16)]

  write.table(df, file = paste0('Chasing_', prolific_id[i], '.csv'), sep = ",", col.names = TRUE,
              row.names = FALSE, qmethod = "double")
}







