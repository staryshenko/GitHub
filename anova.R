library(readxl)
library(tidyverse)
data_git <- read_excel("data_git.xlsx", sheet = "data")
library(car)


#------
leveneTest(rating ~ probation, data = data_git)
#group   2  5.0968 0.006392 **
bartlett.test(rating ~ probation, data=data_git)
#Bartlett's K-squared = 3.9919, df = 2, p-value = 0.1359
outlierTest(aov(rating ~ probation, data = data_git))
#1.875526           0.061222           NA
 
fit_one <- oneway.tControlt(rating ~ probation, data = data_git)
fit_one
#F = 4.941, num df = 2.00, denom df = 386.56, p-value = 0.007605
fit_kruskal <- kruskal.tControlt(rating ~ probation, data = data_git)
fit_kruskal
#Kruskal-Wallis chi-squared = 6.9486, df = 2, p-value = 0.03098
fit_probation <- aov(rating ~ probation, data = data_git)
summary(fit_probation)
#probation       2   1701   850.6   4.555 0.0109 *
TukeyHSD(fit_probation)
#Examen1-EducPractic -3.6871795  0.0215893
#Examen2-EducPractic -0.1435897 -3.3953308  3.1081513 0.9940823
#Examen2-Examen1        3.5435897  0.0287904
plot(TukeyHSD(fit_probation))
#------

leveneTest(rating ~  group*probation, data = data_git)
#group   5  1.9737 0.08078 .
outlierTest(aov(rating ~ group*probation, data = data_git))
# 1.938724           0.053021           NA
fit <- aov(rating ~ group*probation + Error(id/probation), data=data_git)
summary(fit)
#group   1   3314    3314   9.247 0.00269 **
#probation              2   1701   850.6   9.257 0.000118 ***
#group:probation   2    728   363.8   3.959 0.019871 * 
library(nortest)
lillie.tControlt(fit[["id:probation"]][["rControliduals"]])
library(emmeans)
emmG <- emmeans(fit, ~ group)
summary(emmG)
pairs(emmG)
#Experiment - Control      4.76 1.57 193   3.041  0.0027
emmE <- emmeans(fit, ~ probation)
summary(emmE)
pairs(emmE)
#EducPractic - Examen1    3.726 0.971 386   3.838  0.0004
#EducPractic - Examen2    0.176 0.971 386   0.181  0.9820
#Examen1 - Examen2         -3.550 0.971 386  -3.657  0.0008
emm <- emmeans(fit, ~ group*probation)
summary(emm)
pairs(emm)




