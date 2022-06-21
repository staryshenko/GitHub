library(readxl)
library(tidyverse)
data_git <- read_excel("data_git.xlsx", sheet = "data")
library(GGally)
select(data_git,"group", "probation", "rating") %>% ggpairs
#----------------

select(data_git,"group", "probation", "rating") %>% 
  ggplot + aes(x = group, y = rating, colour = group)+ geom_boxplot() +
  geom_jitter(width = 0.2) + labs(x = "Група", y = "Середній бал",colour = "Група")

select(data_git,"group", "probation", "rating") %>% 
  ggplot + aes(x = probation, y = rating, colour = probation)+ geom_boxplot() +
  geom_jitter(width = 0.2) + labs(x = "Вид", y = "Середній бал",colour = "Вид")

select(data_git,"group", "probation", "rating") %>% 
  ggplot + aes(x = group, y = rating, colour = probation)+ geom_boxplot() +
  geom_jitter(width = 0.2) + labs(x = "Група", y = "Середній бал",colour = "Вид")
select(data_git,"group", "probation", "rating") %>% 
  ggplot + aes(x = probation, y = rating, colour = group)+ geom_boxplot() +
  geom_jitter(width = 0.2) + labs(x = "Вид", y = "Середній бал",colour = "Група")



select(data_git,"group", "probation", "rating") %>% 
  ggplot( aes(x = group, y = rating)) +
  geom_boxplot(aes(fill = probation))+labs(x = "Група", y = "Середній бал",fill = "Вид")

library(ggpubr)
ggboxplot(data_git, x = "probation", y = "rating", color = "group", palette = c("#00AFBB", "#E7B800"))
ggboxplot(data_git, x = "group", y = "rating", color = "probation", palette = c("#00AFBB", "#E7B800", "#e7009a"))



interaction.plot(data_git$group, data_git$probation, data_git$rating)
interaction.plot(data_git$group, data_git$probation, data_git$rating, type="b", col=c("red","blue")) 

library(gplots)
plotmeans(rating ~ interaction(group, probation, sep=" "), data=data_git,
          connect=list(c(1,3,5),c(2,4,6)),
          col=c("red", "darkgreen"),
          main = "Title", xlab = "xlab")

#----------------

