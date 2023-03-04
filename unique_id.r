# This is to get the unique element (prolific id)

df = read.csv(file.choose(), header = TRUE, sep = ',', stringsAsFactors = FALSE)

df = df %>% distinct(df$prolific_id, .keep_all = TRUE)


write.table(df, file = paste0('exclu_id', '.csv'), sep = ",", col.names = TRUE,
            row.names = FALSE, qmethod = "double")

