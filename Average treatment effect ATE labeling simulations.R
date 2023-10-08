## Packages

knitr::opts_chunk$set(echo = FALSE)

library(gridExtra)
library(plot3D)
library(randtoolbox)
library(knitr)
library(tidyverse)
library(stargazer)
library(sandwich)
library(lmtest)
library(multiwayvcov)
library(ggdag)
library(broom)
library(patchwork)
library(readr)
library(magrittr)
library(ggplot2)
library(read_csv)
library(dplyr)
library(tidyr)
library(openxlsx)
library(tidydr)
library(ggplot)


## Data calorie

Choice_data <- read_csv("C:\\Users\\const\\OneDrive\\Desktop\\PhD\\R\\CsvAllFields.csv")
Choice_design <- read_csv("C:\\Users\\const\\OneDrive\\Desktop\\PhD\\R\\Flv3_CBC_Design.csv")
Choice_design1 <- read_csv("C:\\Users\\const\\OneDrive\\Desktop\\PhD\\R\\Flv4_CBC1_Design.csv")
Choice_design3 <- read_csv("C:\\Users\\const\\OneDrive\\Desktop\\PhD\\R\\Flv4_CBC3_Design.csv")
Choice_design2 <- read_csv("C:\\Users\\const\\OneDrive\\Desktop\\PhD\\R\\Flv4_CBC2_Design.csv")
Choice_design4 <- read_csv("C:\\Users\\const\\OneDrive\\Desktop\\PhD\\R\\Flv4_CBC4_Design.csv")

## No Labels

transform(Choice_design,"ID" = paste0(Version, "-", Task,"_", Concept))

Choice_data_long <- pivot_longer(Choice_data, cols = starts_with("CBC_"), names_to = "col_name",
                                 values_to = "calorie_answer") %>% 
  mutate(ID = paste(sys_RespNum, 
                    gsub("CBC_Random", "", col_name),
                    calorie_answer,
                    sep = "_"))

Choice_data_calorie <- left_join(Choice_data_long, 
                                 select(Choice_design, c(ID, Calorie, Sugar, Satfat)), 
                                 by = "ID") %>%
  replace_na(list(Calorie= 0))

Choice_data_calorie %>% select(Calorie, ID, calorie_answer, col_name, sys_RespNum, Sugar, Satfat) %>% View

## Coarse calorie labels

Choice_data_long1 <- pivot_longer(Choice_data, cols = starts_with("CBC1_" ), names_to = "col_name",
                                  values_to = "calorie_answer1") %>% 
  mutate(ID = paste(sys_CBCVersion_CBC1, 
                    gsub("CBC1_Random", "", col_name),
                    calorie_answer1,
                    sep = "_"))

Choice_data_calorie1 <- left_join(Choice_data_long1, 
                                  select(Choice_design1, c(ID, Calorie)), 
                                  by = "ID") %>% 
  replace_na(list(Calorie = 0))

Choice_data_calorie1 %>% select(Calorie, ID, calorie_answer1, col_name, sys_CBCVersion_CBC1) %>% View

## Detailed calorie labels

Choice_data_long3 <- pivot_longer(Choice_data, cols = starts_with("CBC3_" ), names_to = "col_name",
                                  values_to = "calorie_answer3") %>% 
  mutate(ID = paste(sys_CBCVersion_CBC3,
                    gsub("CBC3_Random", "", col_name),
                    calorie_answer3,
                    sep = "_"))

Choice_data_calorie3 <- left_join(Choice_data_long3, 
                                  select(Choice_design3, c(ID, Calorie)), 
                                  by = "ID") %>% 
  replace_na(list(Calorie = 0))

Choice_data_calorie3 %>% select(Calorie, ID, calorie_answer3, col_name, sys_CBCVersion_CBC3) %>% View

## Sugar labels

Choice_data_long2 <- pivot_longer(Choice_data, cols = starts_with("CBC2_" ), names_to = "col_name",
                                  values_to = "sugar_answer2") %>% 
  mutate(ID = paste(sys_CBCVersion_CBC2,
                    gsub("CBC2_Random", "", col_name),
                    sugar_answer2,
                    sep = "_"))

Choice_data_sugar <- left_join(Choice_data_long2, 
                                  select(Choice_design2, c(ID, Sugar)), 
                                  by = "ID") %>% 
  replace_na(list(Calorie = 0))

Choice_data_sugar %>% select(Sugar, ID, sugar_answer2, col_name, sys_CBCVersion_CBC2) %>% View

## Saturated fat labels

Choice_data_long4 <- pivot_longer(Choice_data, cols = starts_with("CBC4_" ), names_to = "col_name",
                                  values_to = "satfat_answer4") %>% 
  mutate(ID = paste(sys_CBCVersion_CBC4,
                    gsub("CBC4_Random", "", col_name),
                    satfat_answer4,
                    sep = "_"))

Choice_data_fat <- left_join(Choice_data_long4, 
                                  select(Choice_design4, c(ID, Satfat)), 
                                  by = "ID") %>% 
  replace_na(list(Calorie = 0))

Choice_data_fat %>% select(Satfat, ID, satfat_answer4, col_name, sys_CBCVersion_CBC4) %>% View


## Choice_data_nutrients full

Calorie1 <- Choice_data_calorie1$Calorie
Calorie3 <- Choice_data_calorie3$Calorie
Sugar1 <- Choice_data_sugar$Sugar
Satfat1 <- Choice_data_fat$Satfat

Choice_data_calorie$Calorie1 <- Calorie1
Choice_data_calorie$Calorie3 <- Calorie3
Choice_data_calorie$Sugar1 <- Sugar1
Choice_data_calorie$Satfat1 <- Satfat1

Choice_data_calorie$Caloriefull <- rowSums(Choice_data_calorie[, c("Calorie1", "Calorie3")])

Choice_data_calorie <- mutate(Choice_data_calorie, "Programcal" = ifelse(is.na(CBC1_Random1), "Coarse", "Detailed")) ##1 coarse y 2 detailed
Choice_data_calorie <- mutate(Choice_data_calorie, "Programsug" = ifelse(is.na(CBC2_Random1), 1, 2))
Choice_data_calorie <- mutate(Choice_data_calorie, "Programfat" = ifelse(is.na(CBC4_Random1), 1, 2))
 
Choice_data_calorie %>% select(col_name, ID, Calorie, Sugar, Satfat, Calorie1, Calorie3, 
                               Caloriefull, Sugar1, Satfat1, Programcal, Programsug, Programfat) %>% View

Choice_data_nutrients <- Choice_data_calorie
order_col <- colnames(Choice_data_nutrients)
print(order_col)
Choice_data_nutrients[,241:251] <- replace(Choice_data_nutrients[,241:251], is.na(Choice_data_nutrients[,241:251]), 0)

Choice_data_nutrients %>% select(col_name, ID, Calorie, Sugar, Satfat, Calorie1, Calorie3, 
                               Caloriefull, Sugar1, Satfat1, Programcal, Programsug, Programfat) %>% View

write.xlsx(Choice_data_calorie, 'C:/Users/const/OneDrive/Desktop/PhD/R/Choice_data_nutrients.xlsx',colNames = TRUE)

## Check balance

Choice_data_nutrients %>%
  count(Programcal) %>% 
  mutate(prop = n / sum(n))

Choice_data_nutrients %>% 
  group_by(Programcal) %>% 
  summarize(avg_sex = mean(S1),
            avg_age = mean(S2),
            pro_ethnicity = mean(S3),
            avg_education = mean(S4),
            avg_income = mean(S5))


plot_diff_education <- ggplot(Choice_data_nutrients, aes(x = Program, y = S4, color = Program)) +
  stat_summary(geom = "pointrange", fun.data = "mean_se", fun.args = list(mult = 1.96)) +
  guides(color = FALSE) +
  labs(x = NULL, y = "Education")

plot_hist_education <- ggplot(Choice_data_nutrients, aes(x = S4, fill = Program)) +
  geom_histogram(binwidth = 1, color = "white") +
  guides(fill = FALSE) +
  labs(x = "Education", y = "Count") +
  facet_wrap(vars(Program), ncol = 1)

plot_diff_education + plot_hist_education

## ATE

Choice_data_nutrients %>% 
  group_by(Programcal) %>% 
  summarize(avg_post = mean(Calorie))

Choice_data_nutrients %>% 
  group_by(Programcal) %>% 
  summarize(avg_post = mean(Caloriefull))

model_rct <- lm(Caloriefull ~ Programcal, data = Choice_data_nutrients)
tidy(model_rct)

avg_sugar <- mean(Choice_data_nutrients$Sugar)
avg_sugar1 <- mean(Choice_data_nutrients$Sugar1)
avg_sugar-avg_sugar1

avg_satfat <- mean(Choice_data_nutrients$Satfat)
avg_satfat1 <- mean(Choice_data_nutrients$Satfat1)
avg_satfat-avg_satfat1

ggplot(Choice_data_nutrients, aes(x = Programcal, y = Caloriefull, color = Programcal)) +
  stat_summary(geom = "pointrange", fun.data = "mean_se", fun.args = list(mult = 1.96)) +
  guides(color = FALSE) +
  labs(x = NULL, y = "Calorie label")

