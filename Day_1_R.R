#day_1_R

#Calculate sample size
library(tidyverse)
chicks <- as_tibble(ChickWeight)
chicks
nrow(chicks)
?ChickWeight
unique(chiks$Chick)

#Calculate the mean final mass of the chiks at 20
library(tidyverse)
chicks %>% 
filter(Time == "20")
summarise(mean)



# library(e1071)
kurtosis(faithful$eruptions)
library(tidyverse)
chicks %>% 
  filter(Time == "20")
group_by(Diet) %>% 
chicks %>% chicks
  

chicks %>% 
  summarise(min_wt = min(weight),
            qrt1_wt = quantile(weight, p = 0.25),
            med_wt = median(weight),
            qrt3_wt = quantile(weight, p = 0.75),
            max_wt = max(weight))
dat1 <- c(NA, 12, 76, 34, 23)

# Without telling R to ommit missing data
mean(dat1)