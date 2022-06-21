library(readxl)
library(tidyverse)
data_git <- read_excel("data_git.xlsx", sheet = "data")


#----------------
library(sm)
library(nortest)

hist(data_git$rating, freq = FALSE, breaks=20,col="lightblue", 
     main="Вся вибірка", xlab = "Бали", ylab = "Щільність")
lines(density(data_git$rating), lwd = 2, col="blue")
lillie.test(data_git$rating)
sm.density(data_git$rating, model = "Normal")

#--
hist(data_git$rating[data_git$group=='Experiment'], freq = FALSE, breaks=20,col="lightblue", 
     main="Experiment", xlab = "Бали", ylab = "Щільність")
lines(density(data_git$rating[data_git$group=='Experiment']), lwd = 2, col="blue")
lillie.test(data_git$rating[data_git$group=='Experiment'])
sm.density(data_git$rating[data_git$group=='Experiment'], model = "Normal")

hist(data_git$rating[data_git$group=='Control'], freq = FALSE, breaks=20,col="lightblue", 
     main="Experiment", xlab = "Бали", ylab = "Щільність")
lines(density(data_git$rating[data_git$group=='Control']), lwd = 2, col="blue")
lillie.test(data_git$rating[data_git$group=='Control'])
sm.density(data_git$rating[data_git$group=='Control'], model = "Normal")
#--
hist(data_git$rating[data_git$probation=='Examen1'], freq = FALSE, breaks=20,col="lightblue", 
     main="Examen1", xlab = "Бали", ylab = "Щільність")
lines(density(data_git$rating[data_git$probation=='Examen1']), lwd = 2, col="blue")
lillie.test(data_git$rating[data_git$probation=='Examen1'])
sm.density(data_git$rating[data_git$probation=='Examen1'], model = "Normal")

hist(data_git$rating[data_git$probation=='EducPractic'], freq = FALSE, breaks=20,col="lightblue", 
     main="EducPractic", xlab = "Бали", ylab = "Щільність")
lines(density(data_git$rating[data_git$probation=='EducPractic']), lwd = 2, col="blue")
lillie.test(data_git$rating[data_git$probation=='EducPractic'])
sm.density(data_git$rating[data_git$probation=='EducPractic'], model = "Normal")

hist(data_git$rating[data_git$probation=='Examen2'], freq = FALSE, breaks=20,col="lightblue", 
     main="Examen2", xlab = "Бали", ylab = "Щільність")
lines(density(data_git$rating[data_git$probation=='Examen2']), lwd = 2, col="blue")
lillie.test(data_git$rating[data_git$probation=='Examen2'])
sm.density(data_git$rating[data_git$probation=='Examen2'], model = "Normal")

#--
hist(data_git$rating[data_git$probation=='Examen1' & data_git$group=='Experiment'], freq = FALSE, breaks=20,col="lightblue", 
     main="Examen1", xlab = "Бали", ylab = "Щільність")
lines(density(data_git$rating[data_git$probation=='Examen1' & data_git$group=='Experiment']), lwd = 2, col="blue")
lillie.test(data_git$rating[data_git$probation=='Examen1' & data_git$group=='Experiment'])
sm.density(data_git$rating[data_git$probation=='Examen1' & data_git$group=='Experiment'], model = "Normal")

hist(data_git$rating[data_git$probation=='EducPractic' & data_git$group=='Experiment'], freq = FALSE, breaks=20,col="lightblue", 
     main="EducPractic", xlab = "Бали", ylab = "Щільність")
lines(density(data_git$rating[data_git$probation=='EducPractic' & data_git$group=='Experiment']), lwd = 2, col="blue")
lillie.test(data_git$rating[data_git$probation=='EducPractic' & data_git$group=='Experiment'])
sm.density(data_git$rating[data_git$probation=='EducPractic' & data_git$group=='Experiment'], model = "Normal")

hist(data_git$rating[data_git$probation=='Examen2' & data_git$group=='Experiment'], freq = FALSE, breaks=20,col="lightblue", 
     main="Examen2", xlab = "Бали", ylab = "Щільність")
lines(density(data_git$rating[data_git$probation=='Examen2' & data_git$group=='Experiment']), lwd = 2, col="blue")
lillie.test(data_git$rating[data_git$probation=='Examen2' & data_git$group=='Experiment'])
sm.density(data_git$rating[data_git$probation=='Examen2' & data_git$group=='Experiment'], model = "Normal")

hist(data_git$rating[data_git$probation=='Examen1' & data_git$group=='Control'], freq = FALSE, breaks=20,col="lightblue", 
     main="Examen1", xlab = "Бали", ylab = "Щільність")
lines(density(data_git$rating[data_git$probation=='Examen1' & data_git$group=='Control']), lwd = 2, col="blue")
lillie.test(data_git$rating[data_git$probation=='Examen1' & data_git$group=='Control'])
sm.density(data_git$rating[data_git$probation=='Examen1' & data_git$group=='Control'], model = "Normal")

hist(data_git$rating[data_git$probation=='EducPractic' & data_git$group=='Control'], freq = FALSE, breaks=20,col="lightblue", 
     main="EducPractic", xlab = "Бали", ylab = "Щільність")
lines(density(data_git$rating[data_git$probation=='EducPractic' & data_git$group=='Control']), lwd = 2, col="blue")
lillie.test(data_git$rating[data_git$probation=='EducPractic' & data_git$group=='Control'])
sm.density(data_git$rating[data_git$probation=='EducPractic' & data_git$group=='Control'], model = "Normal")

hist(data_git$rating[data_git$probation=='Examen2' & data_git$group=='Control'], freq = FALSE, breaks=20,col="lightblue", 
     main="Examen2", xlab = "Бали", ylab = "Щільність")
lines(density(data_git$rating[data_git$probation=='Examen2' & data_git$group=='Control']), lwd = 2, col="blue")
lillie.test(data_git$rating[data_git$probation=='Examen2' & data_git$group=='Control'])
sm.density(data_git$rating[data_git$probation=='Examen2' & data_git$group=='Control'], model = "Normal")























