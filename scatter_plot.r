# this is for scatter plot

df = read.csv(file.choose(), header = TRUE, sep = ',', stringsAsFactors = FALSE)

library("ggplot2")
library("ggpubr")
library("gridGraphics")

# to calculate the false alarm rate

df$n_fa_rate = df$n_fa / (df$n_fa + df$n_cr)

# to calculate the z-score of false alarm rate
df$n_fa_rate = scale(df$n_fa_rate)[, 1]


A = ggscatter(df, x = "fw", y = "acc_wolf_sheep", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson",
              xlab = "Belief in free will", ylab = "Identification accuracy")

B = ggscatter(df, x = "de", y = "acc_wolf_sheep", 
              add = "reg.line", conf.int = TRUE, 
              cor.coef = TRUE, cor.method = "pearson",
              xlab = "Belief in determinism", ylab = "Identification accuracy")

C = ggscatter(df, x = "du", y = "acc_wolf_sheep",
              add = "reg.line", conf.int = TRUE,
              cor.coef = TRUE, cor.method = "pearson",
              xlab = "Belief in dualism", ylab = "Identification accuracy")



theme_set(theme_bw() + theme(legend.position = "top"))

figure = ggarrange(A, B, C, labels = c("A", "B", "C"),
                   ncol = 2, nrow = 2)
figure


#################################################################################################

# single scatter plot

ggscatter(df, x = "total_rpbf", y = "dprime", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "The Revised Paranormal Belief Scale", ylab = "Sensitivity")




