setwd("~/Desktop/BaseballProject")
spin21 <- read.csv("active-spin21.csv")
spin22 <- read.csv("active-spin22.csv")
spin23 <- read.csv("active-spin23.csv")
spin24 <- read.csv("active-spin24.csv")
spin25 <- read.csv("active-spin25.csv")

# Pitch movement files
move21 <- read.csv("pitch_movement21.csv")
move22 <- read.csv("pitch_movement22.csv")
move23 <- read.csv("pitch_movement23.csv")
move24 <- read.csv("pitch_movement24.csv")
move25 <- read.csv("pitch_movement25.csv")

library(dplyr)
pitcher_lists <- list(
  unique(spin21[["last_name..first_name"]]),
  unique(spin22[["last_name..first_name"]]),
  unique(spin23[["last_name..first_name"]]),
  unique(spin24[["last_name..first_name"]]),
  unique(spin25[["last_name..first_name"]]),
  unique(move21[["last_name..first_name"]]),
  unique(move22[["last_name..first_name"]]),
  unique(move23[["last_name..first_name"]]),
  unique(move24[["last_name..first_name"]]),
  unique(move25[["last_name..first_name"]])
)
common_pitchers <- Reduce(intersect, pitcher_lists)
length(common_pitchers)

#s21 <- spin21 %>% select(`last_name..first_name`, spin21 = active_spin_fourseam)
#s22 <- spin22 %>% select(`last_name..first_name`, spin22 = active_spin_fourseam)
#s23 <- spin23 %>% select(`last_name..first_name`, spin23 = active_spin_fourseam)
#s24 <- spin24 %>% select(`last_name..first_name`, spin24 = active_spin_fourseam)
#s25 <- spin25 %>% select(`last_name..first_name`, spin25 = active_spin_fourseam)

# Step 2: Join them together by name
spin_all <- bind_rows(spin21, spin22, spin23, spin24, spin25)
spin_filtered <- spin_all %>%
  filter(`last_name..first_name` %in% common_pitchers)

avg_spin_5yr <- spin_filtered %>%
  group_by(`last_name..first_name`) %>%
  summarise(avg_5yr_spin_rate = mean(active_spin_fourseam, na.rm = TRUE))

library(tidyr)
#repeat with velocity and movement
move_all <- bind_rows(move21, move22, move23, move24, move25)
#filter by common names 
move_filtered <- move_all %>%
  filter(`last_name..first_name` %in% common_pitchers)

avg_move_5yr <- move_filtered %>%
  group_by(`last_name..first_name`) %>%
  summarise(
    avg_5yr_velocity = mean(avg_speed, na.rm = TRUE),
    avg_5yr_horizontal_movement = mean(pitcher_break_x, na.rm = TRUE),
    avg_5yr_vertical_movement = mean(pitcher_break_z, na.rm = TRUE)
  )

combined_all_5yr <- avg_spin_5yr %>%
  inner_join(avg_move_5yr, by = "last_name..first_name")

# HL classification 
classified <- combined_all_5yr %>%
  mutate(
    spin_class = ifelse(avg_5yr_spin_rate >= median(avg_5yr_spin_rate, na.rm = TRUE), "H", "L"),
    velo_class = ifelse(avg_5yr_velocity >= median(avg_5yr_velocity, na.rm = TRUE), "H", "L"),
    move_class = ifelse((avg_5yr_horizontal_movement + avg_5yr_vertical_movement) >= 
                          median(avg_5yr_horizontal_movement + avg_5yr_vertical_movement, na.rm = TRUE), "H", "L")
  )

classified <- classified %>%
  mutate(profile = paste0(spin_class, velo_class, move_class))


#IMPORT WAR DATA
war21 <- read.csv("WAR2021.csv") %>% select(NAME, WAR)
war22 <- read.csv("WAR2022.csv") %>% select(NAME, WAR)
war23 <- read.csv("WAR2023.csv") %>% select(NAME, WAR)
war24 <- read.csv("WAR2024.csv") %>% select(NAME, WAR)
war25 <- read.csv("WAR2025.csv") %>% select(NAME, WAR)

war_all <- bind_rows(war21, war22, war23, war24, war25)
avg_war <- war_all %>%
  group_by(NAME) %>%
  summarise(avg_WAR = mean(WAR, na.rm = TRUE))

avg_war <- avg_war %>%
  mutate(NAME = gsub("\\*", "", NAME))

classified <- classified %>%
  separate(`last_name..first_name`, into = c("last", "first"), sep = ", ") %>%
  mutate(NAME = paste(first, last))

classified_war <- classified %>%
  left_join(avg_war, by = "NAME")


profile_summary <- classified_war %>%
  group_by(profile) %>%
  summarise(
    avg_WAR = mean(avg_WAR, na.rm = TRUE),
    count = n()
  )

#Regression line
lm_model <- lm(avg_WAR ~ avg_5yr_spin_rate + avg_5yr_velocity +
                 avg_5yr_horizontal_movement + avg_5yr_vertical_movement,
               data = classified_war)

# See the summary results
summary(lm_model)

library(ggplot2)



# Step 1: Convert data to long format
long_data <- classified_war %>%
  select(avg_WAR,
         `Spin Rate` = avg_5yr_spin_rate,
         `Velocity` = avg_5yr_velocity,
         `Horizontal Movement` = avg_5yr_horizontal_movement,
         `Vertical Movement` = avg_5yr_vertical_movement) %>%
  pivot_longer(
    cols = -avg_WAR,
    names_to = "Variable",
    values_to = "Value"
  )

# Step 2: Plot all variables in a faceted grid
ggplot(long_data, aes(x = Value, y = avg_WAR)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", color = "firebrick", se = TRUE) +
  facet_wrap(~ Variable, scales = "free_x") +
  labs(
    title = "Relationships Between Pitch Metrics and WAR",
    x = "Pitch Metric Value",
    y = "Average WAR"
  ) +
  theme_minimal()

ggplot(classified_war, aes(x = avg_5yr_velocity, y = avg_WAR, color = profile)) +
  geom_point(size = 3, alpha = 0.7) +
  labs(title = "Velocity vs WAR by Pitcher Profile",
       x = "Average 5-Year Velocity (MPH)",
       y = "Average WAR") +
  theme_minimal()

dist_data <- classified_war %>%
  select(
    `Spin Rate` = avg_5yr_spin_rate,
    `Velocity` = avg_5yr_velocity,
    `Horizontal Movement` = avg_5yr_horizontal_movement,
    `Vertical Movement` = avg_5yr_vertical_movement
  ) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value")

# Step 2: Plot all distributions
ggplot(dist_data, aes(x = Value)) +
  geom_histogram(fill = "steelblue", color = "white", bins = 15, alpha = 0.8) +
  facet_wrap(~ Variable, scales = "free_x") +
  theme_minimal() +
  labs(title = "Distributions of Pitching Variables",
       x = "Value",
       y = "Count")
