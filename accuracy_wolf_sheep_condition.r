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

acc_wolf_sheep_60 = data.frame(prolific_id = character(), acc_wolf_sheep_60 = double(), stringsAsFactors = FALSE)

for (i in 1:length(prolific_id)) {
  
  df = read.csv(paste0('Chasing_', prolific_id[i], '.csv'), header = TRUE,
                sep = ",", stringsAsFactors = FALSE)
  df = df[!(df$trial_type == "video-keyboard-response" | df$wolf == "n"), ]
  df = df[df$angle == 60, ]
  h = 0
  unique_id = sort(unique(df$unique_id))
  for (j in unique_id) {
    
    df_select = df[df$unique_id == j, ]
    if (df_select[1, 1] == df_select[1, 9] && df_select[2, 1] == df_select[2, 10]) {
      h = h + 1
    }
  }
  acc_wolf_sheep_60[i, 1] = prolific_id[i]
  acc_wolf_sheep_60[i, 2] = scales::percent(h / 16)
}

write.table(acc_wolf_sheep_60,
            file = paste0('acc_wolf_sheep_60', '.csv'), sep = ",", col.names = TRUE,
            row.names = FALSE, qmethod = "double")

