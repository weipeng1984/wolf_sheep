# this script is to calculate the accuracy of "wolf-sheep" chasing question

# library the package
library(tidyverse)
library(dplyr)

setwd("/Users/weipeng/Dropbox/Programming/R/project_r/Project5/implicit_intention_perception/")

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


#################################################################################################

# to calculate the accuracy of first question 'wolf-sheep'

acc_total = data.frame(prolific_id = character(), acc_total = double(), stringsAsFactors = FALSE)

for (i in 1:length(prolific_id)) {

  df = read.csv(paste0('Chasing_', prolific_id[i], '.csv'), header = TRUE,
                sep = ",", stringsAsFactors = FALSE)
  h = 0
  for (k in 1:length(df$trial_type)) {
    if (df[k, 1] == 'f' && df[k, 2] == 'video-keyboard-response' && df[k, 6] == 'f') {
      h = h + 1
    } else if (df[k, 1] == 'j' && df[k, 2] == 'video-keyboard-response' && df[k, 6] == 'j') {
       h = h + 1
    }
  }
  acc_total[i, 1] = prolific_id[i]
  acc_total[i, 2] = scales::percent(h / 96)
}

write.table(acc_total,
            file = paste0('acc_chasing', '.csv'), sep = ",", col.names = TRUE,
            row.names = FALSE, qmethod = "double")







