# Here, we check if exist homogeneous answers for the first question.

# library the package
library(tidyverse)
library(dplyr)

setwd("/Users/weipeng/Dropbox/Programming/R/project_r/Project5/implicit_intention_perception")

filenames = list.files(path = getwd(), pattern = 'Chasing_+.*csv')

# creating prolific id

prolific_id = substr(filenames, 9, 32)

# delete the beginning rows and irrelevant column
for (i in 1:length(prolific_id)) {
  df = read.csv(paste0('Chasing_', prolific_id[i], '.csv'), 
                    header = TRUE, sep = ',', stringsAsFactors = FALSE)
  df = df[-c(1:6), ]
  df = df[, -c(1, 5:6, 8:10, 12:16)]
  write.table(df, file = paste0('Chasing_', prolific_id[i], '.csv'), sep = ",", col.names = TRUE,
              row.names = FALSE, qmethod = "double")
}


##########################################################################################

# count the homogeneous answer for the first question - "wolf-sheep chasing behavior"

ans_homo = data.frame(prolific_id = character(), max_homo = double(), stringsAsFactors = FALSE)

for (i in 1:length(prolific_id)) {
  
  df = read.csv(paste0('Chasing_', prolific_id[i], '.csv'), header = TRUE,
                sep = ",", stringsAsFactors = FALSE)
  num_f = 0
  num_j = 0
  for (k in 1:length(df$trial_type)) {
    if (df[k, 1] == 'f' && df[k, 2] == 'video-keyboard-response') {
      num_f = num_f + 1
    } else if (df[k, 1] == 'j' && df[k, 2] == 'video-keyboard-response') {
       num_j = num_j + 1
    }
  max_homo = max(num_f, num_j)
  }
  ans_homo[i, 1] = prolific_id[i]
  ans_homo[i, 2] = max_homo
}

write.table(ans_homo,
            file = paste0('ans_homo_chasing', '.csv'), sep = ",", col.names = TRUE,
            row.names = FALSE, qmethod = "double")














