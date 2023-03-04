# This is to check duplicate id in files

df = read.csv(file.choose(), header = TRUE, sep = ',', stringsAsFactors = FALSE)

df$prolific_id[duplicated(df$prolific_id)]

