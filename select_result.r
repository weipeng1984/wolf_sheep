# selection based on exclusion result

# read stats data

df = read.csv(file.choose(), header = TRUE, sep = ',', stringsAsFactors = FALSE)

# read the exclusion prolific id

df_exclusion = read.csv(file.choose(), header = TRUE, sep = ',', stringsAsFactors = FALSE)

attach(df)
attach(df_exclusion)

df = subset(df, (!df$prolific_id %in% df_exclusion$prolific_id))

write.table(df, file = paste0('acc_wolf_sheep_60_exclu', '.csv'), sep = ",", col.names = TRUE,
            row.names = FALSE, qmethod = "double")
