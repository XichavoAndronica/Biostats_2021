#Xichavo_Mathebula
#20 April 2021
#Author A.J Smit
#Assignment 1


#section 1

#built-in dataset BOD
library(tidyverse)
data("BOD")
#C is the correct answer

#section 2

#load the dplyr package & the murder dataset
library(tidyverse)
library(dplyr)
library(dslabs)
data(murders)

glimpse(state.region)
head(murders, 5)
tail(murders,8)
group_by(murders,state)
select(murders, region)

select(murders, state = 1, population = 4)

#the murder data is well detailed and descriptive, and its not a lot of data, thus easy to work with and to play around it as one can easily select a specific set to look at.

#removing Florida
murders[murders$state != "Florida",]

#creating a new data frame
no_south <- murders[murders$region != "South",]
#there are 34 states left in this category.

#calculate the population size
murders %>% 
  filter(region == "South") %>% 
summarise(south_pop = sum(population))

murders %>% 
  filter(region == "West") %>% 
summarise(west_pop = sum(population))

#creating a new data frame
Northeast <- murders %>% 
  filter(region == "Northeast") %>% 
  summarise(northeast_pop = sum(population))

#creating plots

library(tidyverse)
data("murders") 
ggplot(data = murders, aes(x = state, y = population)) +
  geom_point() +
  geom_line(aes(group = region))

histogram_1 <- ggplot(data = murders, aes(x = population)) +
  geom_histogram(aes(fill = population), position = "dodge", binwidth = 100) +
  labs(x = "total population", y = "region")
histogram_1

#compare population size
#The south region has a higher population number than the west region.

#create new data frame

library(dplyr)
Total <- murders %>% 
filter(total > 20) %>% 
filter(total < 100)  

create_object <- murders
slice(murders, c(10:24, 26))

murders_tibble <- as_tibble(murders)

tibble_region <- as_tibble(murders) %>% 
  group_by(region)

#section 3

library(tidyverse)
library(dplyr)
library(dslabs)
data("heights")

data("heights")

glimpse(heights)
head(heights, 6)
head(heights, 7)
tail(heights, 8)
tail(heights, 5)
group_by(heights,sex)
select(heights, sex)
#

#average standard deviation of males and females
X.heights <- heights %>% 
  heights %>% 
  filter(sex == "Female") %>% 
  summarise(min_fem = min(heights))
            median_fem = median(heights)
            max_fem = max(heights)
            ave_fem = mean(heights)
            sd_mal = sd(heights)

#section 4

#vector
X <- c(1,6,21,19,NA,73,NA)
Y <- c(NA,NA,3,NA,13,24,NA)
summary(X)
summary(Y)
#for X vector there is 2 missing variables, and there is 4 missing variables for Y vector

#missing values
mean(X, na.rm = TRUE)
mean(Y, na.rm = TRUE)

Seasonal_data <- data.frame(year=c(2015,2016,2017,2018))
winter=c(41,39,47,40)
spring=c(41,46,57,45)
summer=c(75,52,85,66)
Autumn=c(57,66,52,56)

ggplot() +
  geom_line(data = Seasonal_data, aes(x = year, y = summer), colour = "red") +
  labs(x = "Year", y = "Temperature (F)") +
  ggtitle("Average winter temperatures over a period of 4 years (2015 to 2018)")

ggplot(data = Seasonal_data, aes(x = year, y = winter)) +
  geom_bar(stat = "identity", color = "black", fill = "red") +
  labs(x = "Year", y = "Temperature (F)") +
  ggtitle("Average winter temperatures over a period of  4 years (2015 to 2018)")

cats_data<-tibble(cats=c("A","B","C"),
                    position=c("1-2-3","3-1-2","2-3-1"),
                    minutes=c(3,3,3),
                    seconds=c(12,44,15))

cats-data
cats_data %>% 
separate(col = position, into = c("first_place", "second_place", "third_place"))

cats_data %>%
unite(minutes, seconds, col = "total_time")

#section 6
library(tidyverse)
Orange <- datasets::Orange

gather(Orange, Tree, age, circumference) 
#collects a set of column names and place them into a single column

Orange %>% 
  spread(key = Tree, value = 1)
#spread data in which two or more variables are within the same column
Orange %>% 
separate(col = Tree, into = c("1", "2", "3","4", "5"))
#separate a single column that contains two or more different observations into different columns
Orange %>% 
  joining,by = c("Tree", "age")
#joins two or more data frames together
Orange %>% 
arrange(col = 3, row = 15)
#Arrange observations in rows based on variables in columns
Orange %>% 
  select(Tree, age, circumference)
#Select columns from the data set that need to be used 
Orange %>% 
group_by(age, circumference)
#group data of the same kind together
Orange %>% 
mutate(col = Tree)
#Create new columns
