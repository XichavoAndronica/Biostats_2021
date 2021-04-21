#Day_3
#A.J Smit
#21 april 2021
#correlation



library(tidyverse)
library(ggpubr)
library(corrplot)

# Load data
ecklonia <- read_csv("data/ecklonia.csv")

ecklonia_sub <- ecklonia %>% 
  select(-species, - site, - ID)

cor.test(x = ecklonia$stipe_length, ecklonia$frond_length,
         use = "everything", method = "pearson")
ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson

# Create ordinal data
ecklonia$length <- as.numeric(cut((ecklonia$stipe_length+ecklonia$frond_length), breaks = 3))

# Run test on any variable
cor.test(ecklonia$length, ecklonia$digits)

ecklonia_norm <- ecklonia_sub %>% 
  gather(key = "variable") %>% 
  group_by(variable) %>% 
  summarise(variable_norm = as.numeric(shapiro.test(value)[2]))
ecklonia_norm

cor.test(ecklonia$primary_blade_length, ecklonia$primary_blade_width, method = "kendall")

# Calculate Pearson r beforehand for plotting
r_print <- paste0("r = ", 
                  round(cor(x = ecklonia$stipe_length, ecklonia$frond_length),2))

# Then create a single panel showing one correlation
ggplot(data = ecklonia, aes(x = stipe_length, y = frond_length)) +
  geom_smooth(method = "lm", colour = "grey90", se = F) +
  geom_point(colour = "mediumorchid4") +
  geom_label(x = 300, y = 240, label = r_print) +
  labs(x = "Stipe length (cm)", y = "Frond length (cm)") +
  theme_pubclean()

corrplot(ecklonia_pearson, method = "circle")

#Exercise

library(ggplot2)
library(dplyr)
library(ggplot2)
library(reshape)
library(ggpubr)
library(corrplot)
library(reshape2)

ecklonia_pearson <- cor(ecklonia_sub)
ecklonia_pearson

#melt the data
melted <- melt(ecklonia_pearson)

ggplot(data = melted, mapping=aes(x = X1, y = X2, fill =value)) +
  geom_tile()

