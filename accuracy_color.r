# this script is to calculate the accuracy of "green-purple" attention check question

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

acc_color = data.frame(prolific_id = character(), acc_color = double(), stringsAsFactors = FALSE)

for (i in 1:length(prolific_id)) {
  
  df = read.csv(paste0('Chasing_', prolific_id[i], '.csv'), header = TRUE,
                sep = ",", stringsAsFactors = FALSE)
  df = df[!(df$trial_type == "video-keyboard-response"), ]
  df = df[(df$wolf == "n"), ]
  h = 0
  unique_id = sort(unique(df$unique_id))
  for (j in unique_id) {

    df_select = df[df$unique_id == j, ]
    if (df_select[1, 1] == df_select[1, 11] && df_select[2, 1] == df_select[2, 12]) {
      h = h + 1
    }
  }
  acc_color[i, 1] = prolific_id[i]
  acc_color[i, 2] = scales::percent(h / 48)
}

write.table(acc_color,
            file = paste0('acc_color', '.csv'), sep = ",", col.names = TRUE,
            row.names = FALSE, qmethod = "double")




