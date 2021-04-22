#day_4_quiz
#Xichavo Mathebula
#22 April 2021


library(tidyverse)
library(plotly)
library(ggplot2)

package:datasets

#load data
Orange <- datasets::Orange
ToothGrowth <- datasets::ToothGrowth
warpbreaks <- datasets::warpbreaks


Orange %>% 
  group_by(Tree)

Orange %>% 
  group_by(Tree) %>% 
  summarise(Tree_var = var(Orange))

Orange %>% 
  group_by(Tree) %>% 
  summarise(norm_Orange = as.numeric(shapiro.test(dat)[2]))
Orange %>% 
 

ggplot(data = Orange, aes(x = age, fill = circumference)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = circumference), colour = NA, alpha = 0.4) +
  labs(x = "value")
shapiro.test(Orange$age)

ToothGrowth %>% 
  summarise(dose_var = var(ToothGrowth))


ggplot(data = Orange, aes(x = dose, fill = supp)) +
  geom_histogram(position = "dodge", binwidth = 1, alpha = 0.8) +
  geom_density(aes(y = 1*..count.., fill = dose), colour = NA, alpha = 0.4) +
  labs(x = "value")
shapiro.test(ToothGrowth$dose)

warpbreaks %>% 
  summarise(breaks_var = var(warpbreaks))

shapiro.test(warpbreaks$breaks)

library(tidyverse)
library(ggpubr)
library(corrplot)

Orange %>% 
  group_by(Tree)
#Hypothesis- the circumference of the tree is dependent on the age of the tree

#Question 2

library(tidyverse)
library(readr)
SACTN_data <- read_csv("data/SACTN_data.csv")
View(SACTN_data)

SACTN_data %>% 
group_by(site)


library(readr)
SACTN_data <- read_csv("data/SACTN_data.csv")
View(SACTN_data)
  gather(key = "variable", value = "value", -src, -index, -date)
  