# To calculate the index of Signal Detection Theory

library(tidyverse)
library(dplyr)
library(psycho)


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

# to calculate the indicators of Signal Detection Theory (SDT), sensitivity and false alarm rate 
# separately for three chasing subtlety 5, 30, and 60 degree

sdt = data.frame(prolific_id = character(),
                 n_hit = double(), n_fa = double(), n_miss = double(), n_cr = double(),
                 stringsAsFactors = FALSE)

for (i in 1:length(prolific_id)) {
  
  df = read.csv(paste0('Chasing_', prolific_id[i], '.csv'), header = TRUE,
                sep = ",", stringsAsFactors = FALSE)
  df = df[(df$trial_type == "video-keyboard-response"), ]
  df = df[(df$angle == 60), ]
  # counter the number of hit 
  h = 0
  for (k in 1:length(df$trial_type)) {
    if (df[k, 1] == 'f' && df[k, 6] == 'f') {
      h = h + 1
    }
  }
  # counter the number of false alarm
  f = 0
  for (k in 1:length(df$trial_type)) {
    if (df[k, 1] == 'f' && df[k, 6] == 'j') {
      f = f + 1
    }
  }
  # counter the number of miss
  m = 0
  for (k in 1:length(df$trial_type)) {
    if (df[k, 1] == 'j' && df[k, 6] == 'f') {
      m = m + 1
    }
  }
  # counter the number of correct rejection
  c = 0
  for (k in 1:length(df$trial_type)) {
    if (df[k, 1] == 'j' && df[k, 6] == 'j') {
      c = c + 1
    }
  }

  sdt[i, 1] = prolific_id[i]
  sdt[i, 2] = h
  sdt[i, 3] = f
  sdt[i, 4] = m
  sdt[i, 5] = c
}

write.table(sdt,
            file = paste0('sdt_chasing_60', '.csv'), sep = ",", col.names = TRUE,
            row.names = FALSE, qmethod = "double")
#######################################################################
# calculate the index

# calculate different index


df = data.frame( prolific_id = c(sdt$prolific_id),
                 n_hit = c(sdt$n_hit),
                 n_fa = c(sdt$n_fa), 
                 n_miss = c(sdt$n_miss),
                 n_cr = c(sdt$n_cr)
                 )
indices = psycho::dprime(df$n_hit, df$n_fa, df$n_miss, df$n_cr)
df = cbind(df, indices)

write.table(df, file = "sdt_stats_60.csv", sep = ",", col.names = TRUE,
            row.names = FALSE, qmethod = "double")

