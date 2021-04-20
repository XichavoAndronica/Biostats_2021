#DAY_2_R
library(tidyverse)
ecklonia <- read_csv("data/ecklonia.csv") %>% 
  gather(key = "variable", value = "value", -species, -site, -ID)

ggplot(data = ecklonia, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip()
ecklonia_sub <- ecklonia %>% 
  filter(variable == "stipe_mass")

# then create a new figure
ggplot(data = ecklonia_sub, aes(x = variable, y = value, fill = site)) +
  geom_boxplot() +
  coord_flip() +
  labs(y = "stipe mass (kg)", x = "") +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank())

ecklonia_sub %>% 
  group_by(site) %>% 
  summarise(stipe_mass_var = two_assum(value)[1],
            stipe_mass_norm = two_assum(value)[2])

# traditional output
t.test(value ~ site, data = ecklonia_sub, var.equal = TRUE, alternative = "greater")

# dataframe output
compare_means(value ~ site, data = ecklonia_sub, method = "t.test", var.equal = TRUE, alternative = "greater")


# First grab the data
chicks <- as_tibble(ChickWeight)

# Then subset out only the sample sets to be compared
chicks_sub <- chicks %>% 
  filter(Diet %in% c(1, 2), Time == 21)

chicks.aov1 <- aov(weight ~ Diet, data = filter(chicks, Time == 21))
summary(chicks.aov1)
