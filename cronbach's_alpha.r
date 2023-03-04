# for calculating Cronbach's alpha value

library(psych)

df = read.csv(file.choose() ,header = T, sep = ",", stringsAsFactors = FALSE)


alpha(fw)

fw = df[, c(8, 11, 14, 17, 20)]
de = df[, c(9, 12, 15, 18, 21)]
du = df[, c(10, 13, 16, 19, 22)]

