# This is to match the prolific id in chasing task and prolific id in survey

# library the package
library(tidyverse)
library(dplyr)

filenames = list.files(path = getwd(), pattern = 'Chasing_+.*csv')

# creating prolific id

prolific_id_chasing = substr(filenames, 9, 32)


df_survey = read.csv(file.choose(), header = TRUE, sep = ',', stringsAsFactors = FALSE)

# find the common prolific id 

id_chasing_survey = intersect(prolific_id_chasing, df_survey$prolific_id)

# find the id which doesn't overlap

id_no_match = c(setdiff(prolific_id_chasing, df_survey$prolific_id), 
                setdiff(df_survey$prolific_id, prolific_id_chasing))

write.table(id_chasing_survey, file = paste0('id_chasing_survey', '.csv'), sep = ",", col.names = TRUE,
            row.names = FALSE, qmethod = "double")

write.table(id_no_match, file = paste0('id_no_match', '.csv'), sep = ",", col.names = TRUE,
            row.names = FALSE, qmethod = "double")

