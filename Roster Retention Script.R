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

#Models

model1 <- glm(family = binomial, `2025_status` ~ age + `2025_contract_status`, data = hitters_2024)
summary(model1)

model2 <- glm(family = binomial, `2025_status` ~ age + `2025_contract_status` + wrc_plus, data = hitters_2024)
summary(model2)

model3 <- glm(family = binomial, `2025_status` ~ age + `2025_contract_status` + wrc_plus + total_drs, data = hitters_2024)
summary(model3)

model4 <- glm(family = binomial, `2025_status` ~ age + `2025_contract_status` + wrc_plus + total_drs + pa + primary_pos, data = hitters_2024)
summary(model4)