# To process the survey data

library(tidyverse)

setwd("/Users/weipeng/Dropbox/Programming/R/project_r/Project5/implicit_intention_perception/survey")

# read the data

df_survey = read.csv(file.choose(), header = TRUE, sep = ',', stringsAsFactors = FALSE)

# select the relevant column

df_survey = subset(df_survey, select = c(1, 6:80))

# Change the column names of survey data

names(df_survey)[1] = 'response_id'
names(df_survey)[2] = 'ip'
names(df_survey)[3] = 'prolific_id'
names(df_survey)[4] = 'education'
names(df_survey)[5] = 'gender'
names(df_survey)[6] = 'age'
names(df_survey)[7] = 'country'
names(df_survey)[8] = 'f01'
names(df_survey)[9] = 'f02'
names(df_survey)[10] = 'f03'
names(df_survey)[11] = 'f04'
names(df_survey)[12] = 'f05'
names(df_survey)[13] = 'f06'
names(df_survey)[14] = 'f07'
names(df_survey)[15] = 'f08'
names(df_survey)[16] = 'f09'
names(df_survey)[17] = 'f10'
names(df_survey)[18] = 'f11'
names(df_survey)[19] = 'f12'
names(df_survey)[20] = 'f13'
names(df_survey)[21] = 'f14'
names(df_survey)[22] = 'f15'
names(df_survey)[23] = 'p01'
names(df_survey)[24] = 'p02'
names(df_survey)[25] = 'p03'
names(df_survey)[26] = 'p04'
names(df_survey)[27] = 'p05'
names(df_survey)[28] = 'p06'
names(df_survey)[29] = 'p07'
names(df_survey)[30] = 'p08'
names(df_survey)[31] = 'p09'
names(df_survey)[32] = 'p10'
names(df_survey)[33] = 'p11'
names(df_survey)[34] = 'p12'
names(df_survey)[35] = 'p13'
names(df_survey)[36] = 'p14'
names(df_survey)[37] = 'p15'
names(df_survey)[38] = 'p16'
names(df_survey)[39] = 'p17'
names(df_survey)[40] = 'p18'
names(df_survey)[41] = 'p19'
names(df_survey)[42] = 'p20'
names(df_survey)[43] = 'p21'
names(df_survey)[44] = 'p22'
names(df_survey)[45] = 'p23'
names(df_survey)[46] = 'p24'
names(df_survey)[47] = 'p25'
names(df_survey)[48] = 'p26'
names(df_survey)[49] = 'aq1'
names(df_survey)[50] = 'aq2'
names(df_survey)[51] = 'aq3'
names(df_survey)[52] = 'aq4'
names(df_survey)[53] = 'aq5'
names(df_survey)[54] = 'aq6'
names(df_survey)[55] = 'aq7'
names(df_survey)[56] = 'aq8'
names(df_survey)[57] = 'aq9'
names(df_survey)[58] = 'aq10'
names(df_survey)[59] = 'aq11'
names(df_survey)[60] = 'aq12'
names(df_survey)[61] = 'aq13'
names(df_survey)[62] = 'aq14'
names(df_survey)[63] = 'aq15'
names(df_survey)[64] = 'aq16'
names(df_survey)[65] = 'aq17'
names(df_survey)[66] = 'aq18'
names(df_survey)[67] = 'aq19'
names(df_survey)[68] = 'aq20'
names(df_survey)[69] = 'aq21'
names(df_survey)[70] = 'aq22'
names(df_survey)[71] = 'aq23'
names(df_survey)[72] = 'aq24'
names(df_survey)[73] = 'aq25'
names(df_survey)[74] = 'aq26'
names(df_survey)[75] = 'aq27'
names(df_survey)[76] = 'aq28'

# check duplicate item

df_survey$prolific_id[duplicated(df_survey$prolific_id)]

# calculate the free will beliefs (don't reverse the Determinism subscale)
# dimension of the data-frame

# extract Free Will Inventory

for (i in 1:dim(df_survey)[1]) {
  for (j in 1:dim(df_survey)[2]) {
    if (df_survey[i, j] == 'Strongly disagree') {
      df_survey[i, j][df_survey[i, j] == 'Strongly disagree'] = 1
    } else if (df_survey[i, j] == 'Disagree') {
      df_survey[i, j][df_survey[i, j] == 'Disagree'] = 2
    } else if (df_survey[i, j] == 'Somewhat disagree') {
      df_survey[i, j][df_survey[i, j] == 'Somewhat disagree'] = 3
    } else if (df_survey[i, j] == 'Neither agree nor disagree') {
      df_survey[i, j][df_survey[i, j] == 'Neither agree nor disagree'] = 4
    } else if (df_survey[i, j] == 'Somewhat agree') {
      df_survey[i, j][df_survey[i, j] == 'Somewhat agree'] = 5
    } else if (df_survey[i, j] == 'Agree') {
      df_survey[i, j][df_survey[i, j] == 'Agree'] = 6
    } else if (df_survey[i, j] == 'Strongly agree') {
      df_survey[i, j][df_survey[i, j] == 'Strongly agree'] = 7
    }
  }
}

freewill = c(8, 11, 14, 17, 20)
determinism = c(9, 12, 15, 18, 21)
dualism = c(10, 13, 16, 19, 22)


# calculating the free will beliefs

df_survey$fw = rowSums(sapply(df_survey[, freewill], as.numeric))

# calculating the Determinism

df_survey$de = rowSums(sapply(df_survey[, determinism], as.numeric))

# calculating the dualism subscale

df_survey$du = rowSums(sapply(df_survey[, dualism], as.numeric))


# starting to transform the Revised Paranormal Belief Scale 

rpbs = c(23:44, 46:48)

for (i in 1:dim(df_survey)[1]) {
  for (j in rpbs) {
    if (df_survey[i, j] == 'Strongly Disagree') {
      df_survey[i, j][df_survey[i, j] == 'Strongly Disagree'] = 1
    } else if (df_survey[i, j] == 'Moderately Disagree') {
      df_survey[i, j][df_survey[i, j] == 'Moderately Disagree'] = 2
    } else if (df_survey[i, j] == 'Slightly Disagree') {
      df_survey[i, j][df_survey[i, j] == 'Slightly Disagree'] = 3
    } else if (df_survey[i, j] == 'Uncertain') {
      df_survey[i, j][df_survey[i, j] == 'Uncertain'] = 4
    } else if (df_survey[i, j] == 'Slightly Agree') {
      df_survey[i, j][df_survey[i, j] == 'Slightly Agree'] = 5
    } else if (df_survey[i, j] == 'Moderately Agree') {
      df_survey[i, j][df_survey[i, j] == 'Moderately Agree'] = 6
    } else if (df_survey[i, j] == 'Strongly Agree') {
      df_survey[i, j][df_survey[i, j] == 'Strongly Agree'] = 7
    }
  }
}

# reversed item-23 in the Revised Paranormal Belief Scale

rpbs_r = 45

for (i in 1:dim(df_survey)[1]) {
  for (j in rpbs_r) {
    if (df_survey[i, j] == 'Strongly Disagree') {
      df_survey[i, j][df_survey[i, j] == 'Strongly Disagree'] = 7
    } else if (df_survey[i, j] == 'Moderately Disagree') {
      df_survey[i, j][df_survey[i, j] == 'Moderately Disagree'] = 6
    } else if (df_survey[i, j] == 'Slightly Disagree') {
      df_survey[i, j][df_survey[i, j] == 'Slightly Disagree'] = 5
    } else if (df_survey[i, j] == 'Uncertain') {
      df_survey[i, j][df_survey[i, j] == 'Uncertain'] = 4
    } else if (df_survey[i, j] == 'Slightly Agree') {
      df_survey[i, j][df_survey[i, j] == 'Slightly Agree'] = 3
    } else if (df_survey[i, j] == 'Moderately Agree') {
      df_survey[i, j][df_survey[i, j] == 'Moderately Agree'] = 2
    } else if (df_survey[i, j] == 'Strongly Agree') {
      df_survey[i, j][df_survey[i, j] == 'Strongly Agree'] = 1
    }
  }
}

trb = c(23, 30, 37, 44)
psi = c(24, 31, 38, 45)
witchcraft = c(25, 32, 39, 46)
superstition = c(26, 33, 40)
spiritualism = c(27, 34, 41, 47)
elf = c(28, 35, 42)
precognition = c(29, 36, 43, 48)

# calculating the Traditional Religious Belief
df_survey$trb = rowSums(sapply(df_survey[, trb], as.numeric))

# calculating the Psi
df_survey$psi = rowSums(sapply(df_survey[, psi], as.numeric))

# calculating the Witchcraft
df_survey$witchcraft = rowSums(sapply(df_survey[, witchcraft], as.numeric))

# calculating the Superstition
df_survey$superstition = rowSums(sapply(df_survey[, superstition], as.numeric))

# calculating the Spiritualism
df_survey$spiritualism = rowSums(sapply(df_survey[, spiritualism], as.numeric))

# calculating the Extraordinary Life Forms
df_survey$elf = rowSums(sapply(df_survey[, elf], as.numeric))

# calculating the Precognition
df_survey$precognition = rowSums(sapply(df_survey[, precognition], as.numeric))

# calculating the total score

df_survey$rpbs = rowSums(sapply(df_survey[, 23:48], as.numeric))

# starting to transform the AQ-short (non-reverse items)

aq_short = c(which(colnames(df_survey)=="aq2"), which(colnames(df_survey)=="aq4"), 
             which(colnames(df_survey)=="aq5"), 
             which(colnames(df_survey)=="aq7"), which(colnames(df_survey)=="aq10"), 
             which(colnames(df_survey)=="aq13"), which(colnames(df_survey)=="aq14"), 
             which(colnames(df_survey)=="aq15"), which(colnames(df_survey)=="aq16"), 
             which(colnames(df_survey)=="aq22"), which(colnames(df_survey)=="aq23"), 
             which(colnames(df_survey)=="aq25"), which(colnames(df_survey)=="aq26"))

aq_short_reverse = c(which(colnames(df_survey)=="aq1"), which(colnames(df_survey)=="aq3"),
                     which(colnames(df_survey)=="aq6"), which(colnames(df_survey)=="aq8"),
                     which(colnames(df_survey)=="aq9"), which(colnames(df_survey)=="aq11"),
                     which(colnames(df_survey)=="aq12"), which(colnames(df_survey)=="aq17"),
                     which(colnames(df_survey)=="aq18"), which(colnames(df_survey)=="aq19"),
                     which(colnames(df_survey)=="aq20"), which(colnames(df_survey)=="aq21"),
                     which(colnames(df_survey)=="aq24"), which(colnames(df_survey)=="aq27"),
                     which(colnames(df_survey)=="aq28"))

for (i in 1:dim(df_survey)[1]) {
  for (j in aq_short) {
    if (df_survey[i, j] == 'definitely disagree') {
      df_survey[i, j][df_survey[i, j] == 'definitely disagree'] = 1
    } else if (df_survey[i, j] == 'slightly disagree') {
      df_survey[i, j][df_survey[i, j] == 'slightly disagree'] = 2
    } else if (df_survey[i, j] == 'slightly agree') {
      df_survey[i, j][df_survey[i, j] == 'slightly agree'] = 3
    } else if (df_survey[i, j] == 'definitely agree') {
      df_survey[i, j][df_survey[i, j] == 'definitely agree'] = 4
    }
  }
}

# now to calculate the reversed items

for (i in 1:dim(df_survey)[1]) {
  for (j in aq_short_reverse) {
    if (df_survey[i, j] == 'definitely disagree') {
      df_survey[i, j][df_survey[i, j] == 'definitely disagree'] = 4
    } else if (df_survey[i, j] == 'slightly disagree') {
      df_survey[i, j][df_survey[i, j] == 'slightly disagree'] = 3
    } else if (df_survey[i, j] == 'slightly agree') {
      df_survey[i, j][df_survey[i, j] == 'slightly agree'] = 2
    } else if (df_survey[i, j] == 'definitely agree') {
      df_survey[i, j][df_survey[i, j] == 'definitely agree'] = 1
    }
  }
}

social_skills = c(49, 57, 58, 60, 63, 72, 75, 74)
routine = c(50, 65, 67, 74)
switching = c(52, 56, 66, 69)
imagination = c(51, 54, 59, 62, 68, 71, 73, 76)
factor_numbers_patterns = c(53, 55, 61, 64, 70)

# calculating the Social skills
df_survey$social_skills = rowSums(sapply(df_survey[, social_skills], as.numeric))

# calculating the Routine
df_survey$routine = rowSums(sapply(df_survey[, routine], as.numeric))

# calculating the Switching
df_survey$switching = rowSums(sapply(df_survey[, switching], as.numeric))

# calculating the Imagination
df_survey$imagination = rowSums(sapply(df_survey[, imagination], as.numeric))

# calculating the Factor numbers and patterns
df_survey$factor_numbers_patterns = rowSums(sapply(df_survey[, factor_numbers_patterns], as.numeric))

# calculating the AQ-short

df_survey$aq_short = rowSums(sapply(df_survey[, 49:76], as.numeric))


write.table(df_survey, file = "survey.csv", sep = ",", col.names = TRUE,
            row.names = FALSE, qmethod = "double")









