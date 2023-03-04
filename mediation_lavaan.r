

library(lavaan)
library(semPlot)

df = read.csv(file.choose(), header = TRUE, sep = ',', stringsAsFactors = FALSE)

attach(df)
# one mediator

model = "# a path
         n_fa_rate ~ a * de

         # b path
         dprime ~ b * n_fa_rate

         # c prime path 
         dprime ~ cp * de

         # indirect and total effects
         ab := a * b
         total := cp + ab"

set.seed(1234)
fit = sem(model, data = df, se = "bootstrap")

summary(fit, fit.measures = TRUE, standardize = TRUE, rsquare = TRUE)


