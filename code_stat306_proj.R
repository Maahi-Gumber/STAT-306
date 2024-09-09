"""
title: STAT 306 (S2024 T2): Final Project (Group B2)
author:
- Jordan Bourak (48510689)
- Jeffrey Xiong (24893570)
- Chirag Kharade (84784602)
- Maahi Gumber (76901438)
"""
# required packages
library(tidyverse)
library(ggplot2)
library(gridExtra)
library(xtable)
library(leaps)
library(olsrr)
library(broom)
library(lubridate)

## Raw Data Exploration & Manipulation

athlete_results <- read_csv("data/Olympic_Athlete_Event_Results.csv")

# converting date column appropriately 
bio <- read_csv("data/Olympic_Athlete_Bio.csv") |> 
  mutate(born = as.Date(born, "%d %B %Y"))

olympics_info <- read_csv("data/Olympics_Games.csv") |> 
  mutate(start_date = map(start_date, \(x) word(x, 1, 2))) |> 
  unite(date, start_date, year, sep=" ") |> 
  mutate(date = ifelse(edition_id == 61,           # as 2020 Olympic was postponed due to Covid
                       "23 July 2021",
                       date)) |> 
  mutate(date = as.Date(date, "%d %B %Y")) |> 
  rename(country_held = country_noc)

medal_tally <- read_csv("data/Olympic_Games_Medal_Tally.csv")

# filtering out dataset with Canadian medals
canada_medals <- olympics_info |> 
  select(date, edition_id) |> 
  left_join(filter(medal_tally, country_noc == "CAN")) |> 
  filter(!is.na(total)) |> 
  rename(medals = total) |> 
  select(date, medals)

# filtering out summer olympic games
summer_athlete_results <- athlete_results |> 
  left_join(y = select(bio, -country_noc, -country), by = "athlete_id") |> 
  left_join(y = select(olympics_info, edition_id, date), by = "edition_id") |> 
  rename(country = country_noc) |> 
  filter(str_detect(edition, regex("summer", ignore_case = TRUE))) |> 
  mutate(age = as.numeric(difftime(date, born, units = "days")) / 365.25)

##
canada_events <- summer_athlete_results |> 
  filter(country == "CAN") |> 
  pull(result_id) |> 
  unique()

# filtering to include only individual sport entries for canada_events
all_ind_entries <- summer_athlete_results |> 
  filter(result_id %in% canada_events,
         !isTeamSport) |> 
  mutate(isCanada = country == "CAN") |> 
  distinct(date, isCanada, result_id, athlete_id) |> 
  group_by(date) |> 
  summarize(ind_entries = sum(isCanada),
            other_ind_entries = n() - ind_entries)

# filtering to include only team sport entries for canada_events
all_team_entries <- summer_athlete_results |> 
  filter(result_id %in% canada_events,
         isTeamSport) |> 
  mutate(isCanada = country == "CAN") |> 
  distinct(date, isCanada, result_id, pos) |> 
  group_by(date) |> 
  summarize(team_entries = sum(isCanada),
            other_team_entries = n() - team_entries)

# dataset with Canadian entries and others
entries <- merge(all_ind_entries, all_team_entries, by = "date") |>
  mutate(entries = ind_entries + team_entries,
         other_entries = other_ind_entries + other_team_entries)

# finding female proportions and avg age
canada_athlete_info <- summer_athlete_results |> 
  filter(country == "CAN") |>
  group_by(date) |>
  summarize(avg_age = mean(age, na.rm = TRUE),
            prop_female = sum(sex == "Female", na.rm = TRUE) / n())

# 
canada <- merge(canada_medals, entries, by = "date") |> 
  merge(canada_athlete_info, by = "date") |> 
  mutate(date = decimal_date(date)) |> 
  select(medals, date, entries, other_entries, avg_age, prop_female)

## checking null values
missing_age <- summer_athlete_results |> filter(country == "CAN") |> 
  distinct(edition, athlete_id, age) |> group_by(edition) |> 
  summarize(missing_age = sum(is.na(age)), total = n()) |> 
  mutate(prop_missing = missing_age / total) |> 
  arrange(-prop_missing)
head(missing_age)

missing_sex <- summer_athlete_results |> filter(country == "CAN", is.na(sex))
head(missing_sex)

summary(canada)

## Preliminary Data Exploration

p_date <- ggplot(canada) +
  geom_line(aes(x = date, y = medals)) + 
  geom_point(aes(x = date, y = medals)) +
  labs(x = "Time", y = "")

p_entries <- ggplot(canada) +
  geom_point(aes(x = entries, y = medals), shape = 1) + 
  labs(x = "Canadian Entries", y = "Medals Won")

p_other_entries <- ggplot(canada) +
  geom_point(aes(x = other_entries, y = medals), shape = 1) + 
  labs(x = "Other Country Entries", y = "")

p_age <- ggplot(canada) +
  geom_point(aes(x = avg_age, y = medals), shape = 1) + 
  labs(x = "Avg Age (years)", y = "")

p_prop_female <- ggplot(canada) +
  geom_point(aes(x = prop_female, y = medals), shape = 1) + 
  labs(x = "Female Participation", y = "")

grid.arrange(p_date,
             p_entries, p_other_entries,
             p_age, p_prop_female,
             nrow = 3, layout_matrix = rbind(c(1,1), c(2, 3), c(4, 5)))

# Compute correlation matrix
cor_matrix <- cor(canada)

# Create xtable object
xtable_cor <- xtable(cor_matrix, digits = 3, caption = "Pairwise sample correlations between variables")

# Print LaTeX table with custom positioning and booktabs
print(xtable_cor, 
      floating = TRUE,      # Include table floating environment
      table.placement = "H", # Hold position in LaTeX
      include.rownames = TRUE, 
      booktabs = TRUE)  

## Regression Analysis

model1 <- lm(medals ~ date + entries + other_entries + avg_age, data = canada)
summary(model1)
glance(model1) |>
  select(r.squared, adj.r.squared, sigma)

model1_summary <- summary(model1)$coef

# Create an xtable object for the coefficients
xtable_coef <- xtable(model1_summary, digits = 3,
                      caption = "Regression coefficients for Model 1, $R^2 = 0.516$, $adjR^2 = 0.424$, $\\hat{\\sigma} = 7.055$")

# Print the table with custom LaTeX options
print(xtable_coef,
      floating = TRUE,               # Include table in a floating environment
      table.placement = "H",          # Ensure the table stays in the desired position in LaTeX
      booktabs = TRUE,                # Use booktabs for clean LaTeX formatting
      include.rownames = TRUE,        # Include row names (if required)
      caption.placement = "top") 

p_res1 <- data.frame(i = 1:nrow(canada), e = ls.diag(model1)$std.res) |> 
  ggplot() +
  geom_point(aes(x = i, y = e), shape = 1) +  
  labs(x = "i", y = "Standardized residual")
p_res2 <- data.frame(e = model1$residuals, yhat = model1$fitted.values) |> 
  ggplot() + 
  geom_point(aes(x = yhat, y = e), shape = 1) + 
  labs(x = "Fitted Value", y = "Residual")
grid.arrange(p_res1, p_res2, ncol = 2)

canada_no1984 <- canada |> 
  filter(floor(date) != 1984)
model2 <- lm(medals ~ date*entries + other_entries + avg_age, data = canada_no1984)
model2_s <- summary(model2)
model2_s
model2_diag <- ls.diag(model2)
glance(model2) |>
  select(r.squared, adj.r.squared, sigma)
print(model2_diag$std.res)


model2_summary <- summary(model2)$coef

# Create an xtable object for the coefficients
xtable_coef_model2 <- xtable(model2_summary, digits = 3,
                             caption = "Regression coefficients for Model 2, $R^2 = 0.635$, $adjR^2 = 0.539$, $\\hat{\\sigma} = 4.653$")

# Print the table with custom LaTeX options
print(xtable_coef_model2,
      floating = TRUE,               # Include table in a floating environment
      table.placement = "H",          # Ensure the table stays in the desired position in LaTeX
      booktabs = TRUE,                # Use booktabs for clean LaTeX formatting
      include.rownames = TRUE,        # Include row names (if required)
      caption.placement = "top")      # Place caption at the top of the table

sort(ls.diag(lm(medals ~ date + avg_age + entries + other_entries, data = canada_no1984))$std.res)

# as we see correlation in residual graph, we use lag to see if the pattern persists through different time stamps
e_lag <- data.frame(e = model2$residuals) |> 
  mutate(e_lag1 = lag(e),
         e_lag2 = lag(e, 2)) 
p_e_lag1 <-  e_lag |>
  select(e, e_lag1) |> 
  na.omit() |> 
  ggplot() +
  geom_point(aes(x = e_lag1, y = e), shape = 1) + 
  labs(x = bquote(e[i-1]), y = bquote(e[i]))
p_e_lag2 <-  e_lag |>
  select(e, e_lag2) |> 
  na.omit() |> 
  ggplot() +
  geom_point(aes(x = e_lag2, y = e), shape = 1) + 
  labs(x = bquote(e[i-2]), y = bquote(e[i]))

grid.arrange(p_e_lag1, p_e_lag2, ncol = 2)

### Regression Subsets
subsets <- regsubsets(medals ~ ., data = canada_no1984, method = "exhaustive")
ssubsets <- summary(subsets)
summary_table <- data.frame(ssubsets$rsq,
                            ssubsets$adjr2,
                            ssubsets$cp,
                            1:5 + 1) |> 
  cbind(ifelse(ssubsets$which, "*", "")) 
names(summary_table) <- c("R2", "adjR2", "Cp", "p", "Intercept", "date", "entries", "other_entries", "avg_age", "prop_female")

xtable_summary <- xtable(summary_table, digits = 3,
                         caption = "Summary of models chosen by regsubsets")

# Print the xtable with LaTeX options
print(xtable_summary,
      floating = TRUE,                # Include the table in a floating environment
      table.placement = "H",           # Ensure the table stays in the desired position
      booktabs = TRUE,                 # Use booktabs for LaTeX formatting
      include.rownames = TRUE,         # Include row names if necessary
      caption.placement = "top",       # Place the caption at the top
      scalebox = 0.9)                  # Scale down the table size in LaTeX

model_subset <- lm(medals ~ date + entries + avg_age + prop_female, data = canada_no1984)
summary(model_subset)

model_subset_summary <- summary(model_subset)$coef

# Create an xtable object for the coefficients
xtable_coef_subset <- xtable(model_subset_summary, digits = 3,
                             caption = "Regression coefficients for suggested variable subset model, $R^2 = 0.658$, $adjR^2 = 0.590$, $\\hat{\\sigma} = 4.390$")

# Print the xtable with LaTeX options
print(xtable_coef_subset,
      floating = TRUE,               # Keep the table in a floating environment
      table.placement = "H",          # Ensure the table stays in the desired position
      booktabs = TRUE,                # Use booktabs for a clean LaTeX look
      include.rownames = TRUE,        # Include row names
      caption.placement = "top")      # Place caption at the top of the table

#comparison
model_comparison <- data.frame(
  `$R^2$` = c(summary(model2)$r.squared,
              summary(model_subset)$r.squared),
  `$adjR^2$` = c(summary(model2)$adj.r.squared,
                 summary(model_subset)$adj.r.squared),
  `$\\hat{\\sigma}$` = c(summary(model2)$sigma,
                         summary(model_subset)$sigma),
  `$df$` = c(summary(model2)$df[2],
             summary(model_subset)$df[2]),
  check.names = FALSE)
rownames(model_comparison) <- c("Interpretable Model (Model 2)", "Best Variable Subset")

xtable_model_comparison <- xtable(model_comparison, digits = 3,
                                  caption = "Model Comparison")

# Print the xtable with LaTeX options
print(xtable_model_comparison,
      floating = TRUE,               # Include table in a floating environment
      table.placement = "H",          # Keep the table at the desired position
      booktabs = TRUE,                # Use booktabs for cleaner table formatting
      include.rownames = TRUE,        # Include row names in the table
      sanitize.text.function = identity, # Prevent escaping of LaTeX commands in table content
      caption.placement = "top")      # Place caption at the top of the table

