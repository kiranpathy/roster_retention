#Roster Retention Project Script

library(plyr)
library(dplyr)
library(devtools)
library(DT)
library(ggplot2)
library(ggrepel)
library(ggthemes)
library(gridExtra)
library(janitor)
library(plotly)
library(stringr)
library(tidyr)
library(tidyselect)
library(tidyverse)
library(data.table)
library(reactable)
library(lubridate)
library(ggpubr)
library(paletteer)
library(knitr)
library(gt)
library(tinytex)

hitters_2024 <- read_csv("2024_hitters.csv")

hitters_2024 <- hitters_2024 %>%
  mutate(pos_group = case_when(
    primary_pos %in% c("1B", "2B", "3B", "SS") ~ "IF",
    primary_pos %in% c("LF", "CF", "RF") ~ "OF",
    .default = primary_pos
  ))

#Look at some data
no_mlb_2025 <- hitters_2024 %>%
  filter(`2025_status`== 0)
no_mlb_2025

mlb_2025 <- hitters_2024 %>%
  filter(`2025_status`== 1)
mlb_2025

mlb_2025 %>%
  filter(wrc_plus < 100)

no_mlb_2025 %>%
  summarise(median_age = median(age),
            median_wrc_plus = median(wrc_plus),
            median_drs = median(total_drs),
            median_pa = median(pa))

mlb_2025 %>%
  summarise(median_age = median(age),
            median_wrc_plus = median(wrc_plus),
            median_drs = median(total_drs),
            median_pa = median(pa))

table(no_mlb_2025$pos_group)
table(mlb_2025$pos_group)

table(no_mlb_2025$`2025_contract_status`)
table(mlb_2025$`2025_contract_status`)

#Models

model_offense <- glm(family = binomial, `2025_status` ~ wrc_plus, data = hitters_2024)
summary(model_offense)

model_usage <- glm(family = binomial, `2025_status` ~ pa, data = hitters_2024)
summary(model_usage)

model_performance <- glm(family = binomial, `2025_status` ~ wrc_plus + total_drs + pa, data = hitters_2024)
summary(model_performance)

model_demographic <- glm(family = binomial, `2025_status` ~ age + `2025_contract_status`, data = hitters_2024)
summary(model_demographic)

model_offense2 <- glm(family = binomial, `2025_status` ~ wrc_plus * pa, data = hitters_2024)
summary(model_offense2)

#Graph

graph_wrc <- hitters_2024 %>%
  ggplot(aes(x = wrc_plus, y = `2025_status`)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial")) +
  labs(
    x = "wRC+ (2024)",
    y = "Probability of MLB Opening Day Roster Intention in 2025",
    title = "Offensive Performance Predicting Roster Retention"
  )
graph_wrc

graph_pa <- hitters_2024 %>%
  ggplot(aes(x = pa, y = `2025_status`)) +
  geom_point() +
  geom_smooth(method = "glm", method.args = list(family = "binomial"), color = "red") +
  labs(
    x = "PA (2024)",
    y = "Probability of MLB Opening Day Roster Intention in 2025",
    title = "Usage Predicting Roster Retention"
  )
graph_pa




