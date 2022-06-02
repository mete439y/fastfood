

suppressPackageStartupMessages(library(tidyverse))

suppressPackageStartupMessages(library(openintro))

fastfood <- openintro::fastfood

Q1 <- fastfood %>%
  group_by(item, restaurant)%>%
  summarise(sum = sum(calories))%>%
  filter(restaurant == "Burger King" | restaurant == "Chick Fil-A")%>%
  arrange(desc(sum))%>%
  head(1)%>%
  select(item)
Q1

Q2 <- fastfood%>%
  group_by(restaurant)%>%
  summarise(mean = round(mean(sugar), 2), .groups = 'drop')%>%
  filter(restaurant == "Subway")%>%
  select(mean)%>%
  as.data.frame()


Q3 <- fastfood%>%
  group_by(restaurant)%>%
  summarise(mean = round(mean(calories), 2))%>%
  filter(restaurant == "Taco Bell")%>%
  select(mean)%>%
  as.data.frame()



Q4 <- fastfood%>%
  mutate(fatXsugar = total_fat*sugar)%>%
  select(restaurant,item, fatXsugar)%>%
  arrange(desc(fatXsugar))%>%
  head(3)



Q5 <- fastfood%>%
  group_by(restaurant)%>%
  summarise(mean = round(mean(sat_fat),2))%>%
  filter( mean > 10)%>%
  summarise(total = n())


