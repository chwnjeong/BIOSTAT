library(here)
library(tidyverse)
library(ggpubr)

# Import datasets
mbti <- read_csv(here("countries.csv"))
# MBTI processing: Change column header: dash to underbar
mbti <- mbti |> 
  rename_with(~ str_replace_all(., "-", "_"))

happy <- read_csv(here("world-happiness-report-2021.csv"))
# Happy processing 1: change space to underbar
# Happy processing 2: Order the country alphabetically
happy <- happy |> 
  rename_with(~ str_replace_all(., " ", "_")) |> 
  arrange(Country_name)

# Check there are no duplicate ->> no duplicates
length(mbti |> pull(Country) |> unique()) == nrow(mbti)
length(happy |> pull(Country_name) |> unique()) == nrow(happy)

# Check there are no missing values ->> no missing values
any(duplicated(mbti))
any(duplicated(happy))

# Check whether the countries are named differently between datasets
# ->> Congo is named differently, 
# Congo, Congo (Kinshasa), Congo (Barzzaville) are different country
# so I will exclude Congo
setdiff(mbti$Country, happy$Country_name)
setdiff(happy$Country_name, mbti$Country)

# Make mbti into pivot
mbti_wide <- mbti |> 
  pivot_longer(cols = "ESTJ_A":"INFJ_A",
               names_to = "Category",
               values_to = "Value")

# Divide category specifically 
mbti_wide <- mbti_wide |> 
  mutate(E_flag = if_else(substr(Category, 1, 1)=="E", 1, 0),
         S_flag = if_else(substr(Category, 2, 2)=="S", 1, 0),
         T_flag = if_else(substr(Category, 3, 3)=="T", 1, 0),
         J_flag = if_else(substr(Category, 4, 4)=="J", 1, 0),
         A_flag = if_else(substr(Category, 6, 6)=="A", 1, 0))

mbti_summary <- mbti_wide |> 
  group_by(Country) |> 
  summarise(
    E_sum = sum(Value[E_flag == 1]),
    S_sum = sum(Value[S_flag == 1]),
    T_sum = sum(Value[T_flag == 1]),
    J_sum = sum(Value[J_flag == 1]),
    A_sum = sum(Value[A_flag == 1])
  )
  
# Merge datasets
mbti_happy <- inner_join(mbti_summary, happy, c("Country" = "Country_name"))%>%
  select(Country, E_sum, S_sum, T_sum, J_sum, A_sum, Ladder_score)

# Find correlation
ggplot(mbti_happy, aes(x = E_sum, y = Ladder_score)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(
    title = "Correlation between Happiness Index and E Proportion",
    x = "E Proportion",
    y = "Happiness Index"
  ) +
  theme_minimal() +
  stat_cor(method = "pearson") 

ggplot(mbti_happy, aes(x = S_sum, y = Ladder_score)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(
    title = "Correlation between Happiness Index and S Proportion",
    x = "S Proportion",
    y = "Happiness Index"
  ) +
  theme_minimal() +
  stat_cor(method = "pearson") 

ggplot(mbti_happy, aes(x = T_sum, y = Ladder_score)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(
    title = "Correlation between Happiness Index and T Proportion",
    x = "T Proportion",
    y = "Happiness Index"
  ) +
  theme_minimal() +
  stat_cor(method = "pearson") 

ggplot(mbti_happy, aes(x = J_sum, y = Ladder_score)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(
    title = "Correlation between Happiness Index and J Proportion",
    x = "J Proportion",
    y = "Happiness Index"
  ) +
  theme_minimal() +
  stat_cor(method = "pearson") 

ggplot(mbti_happy, aes(x = A_sum, y = Ladder_score)) +
  geom_point() +
  geom_smooth(method = "lm", color = "blue", se = FALSE) +
  labs(
    title = "Correlation between Happiness Index and A Proportion",
    x = "A Proportion",
    y = "Happiness Index"
  ) +
  theme_minimal() +
  stat_cor(method = "pearson") 
