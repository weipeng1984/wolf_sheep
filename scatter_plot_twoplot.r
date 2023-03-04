# this is for scatter plot

df = read.csv(file.choose(), header = TRUE, sep = ',', stringsAsFactors = FALSE)

library("ggplot2")
library("ggpubr")
library("gridGraphics")

A = ggscatter(df, x = "rpbs", y = "dprime", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson",
              xlab = "Revised Paranormal Belief Scale", ylab = "Perceptual sensitivity")

B = ggscatter(df, x = "aq_short", y = "dprime", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson",
              xlab = "Autism-Spectrum Quotient", ylab = "Perceptual sensitivity")


theme_set(theme_bw() + theme(legend.position = "top"))

figure = ggarrange(A, B, labels = c("A", "B"),
                   ncol = 2, nrow = 1)
figure


#################################################################################################


A = ggscatter(df, x = "rpbs", y = "n_fa_rate", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson",
              xlab = "Revised Paranormal Belief Scale", ylab = "False alarm rate")

B = ggscatter(df, x = "aq_short", y = "n_fa_rate", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson",
              xlab = "Autism-Spectrum Quotient", ylab = "False alarm rate")


theme_set(theme_bw() + theme(legend.position = "top"))

figure = ggarrange(A, B, labels = c("A", "B"),
                   ncol = 2, nrow = 1)
figure


#################################################################################################

A = ggscatter(df, x = "rpbs", y = "acc_wolf_sheep", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson",
              xlab = "Revised Paranormal Belief Scale", ylab = "Identification accuracy")

B = ggscatter(df, x = "aq_short", y = "acc_wolf_sheep", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson",
              xlab = "Autism-Spectrum Quotient", ylab = "Identification accuracy")


theme_set(theme_bw() + theme(legend.position = "top"))

figure = ggarrange(A, B, labels = c("A", "B"),
                   ncol = 2, nrow = 1)
figure




