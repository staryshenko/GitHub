library(readxl)
library(tidyverse)
data_git <- read_excel("data_git.xlsx", sheet = "data")


var.test(rating~group,data=data_git)
#F = 0.96858, num df = 287, denom df = 296, p-value = 0.7859 поправка Уелча непотрібна
t.test(rating~group,data=data_git, var.equal = TRUE)
#t = 4.2478, df = 583, p-value = 2.512e-05
wilcox.test(rating~group,data=data_git, paired = FALSE)
#W = 52061, p-value = 4.877e-06

#------
var.test(rating~group,data=data_git[data_git$probation=='Examen1',])
#F = 1.0318, num df = 95, denom df = 98, p-value = 0.8772 поправка Уелча непотрібна
t.test(rating~group,data=data_git[data_git$probation=='Examen1',], var.equal = TRUE)
#t = 1.543, df = 193, p-value = 0.1245
wilcox.test(rating~group,data=data_git[data_git$probation=='Examen1',], paired = FALSE)
#W = 5444.5, p-value = 0.07764

var.test(rating~group,data=data_git[data_git$probation=='EducPractic',])
#F = 0.95292, num df = 95, denom df = 98, p-value = 0.8141 поправка Уелча непотрібна
t.test(rating~group,data=data_git[data_git$probation=='EducPractic',], var.equal = TRUE)
#t = 4.1004, df = 193, p-value = 6.073e-05
wilcox.test(rating~group,data=data_git[data_git$probation=='EducPractic',], paired = FALSE)
#W = 6297.5, p-value = 7.217e-05

var.test(rating~group,data=data_git[data_git$probation=='Examen2',])
#F = 0.86724, num df = 95, denom df = 98, p-value = 0.4864 поправка Уелча непотрібна
t.test(rating~group,data=data_git[data_git$probation=='Examen2',], var.equal = TRUE)
#t = 1.7721, df = 193, p-value = 0.07795
wilcox.test(rating~group,data=data_git[data_git$probation=='Examen2',], paired = FALSE)
#W = 5569, p-value = 0.03729
