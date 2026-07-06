library(dplyr)
library(ggplot2)
library(tidyr)

data_dir <- file.path("data", "raw")
figures_dir <- file.path("outputs", "figures")
tables_dir <- file.path("outputs", "tables")

dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(tables_dir, recursive = TRUE, showWarnings = FALSE)

read_raw <- function(filename) {
  read.csv(file.path(data_dir, filename))
}

season_labels <- 2021:2025
short_years <- substr(season_labels, 3, 4)

spin_data <- lapply(short_years, function(year) {
  read_raw(paste0("active-spin", year, ".csv"))
})

movement_data <- lapply(short_years, function(year) {
  read_raw(paste0("pitch_movement", year, ".csv"))
})

war_data <- lapply(season_labels, function(year) {
  read_raw(paste0("WAR", year, ".csv")) %>%
    select(NAME, WAR)
})

pitcher_lists <- c(
  lapply(spin_data, function(df) unique(df[["last_name..first_name"]])),
  lapply(movement_data, function(df) unique(df[["last_name..first_name"]]))
)

common_pitchers <- Reduce(intersect, pitcher_lists)

avg_active_spin <- bind_rows(spin_data) %>%
  filter(`last_name..first_name` %in% common_pitchers) %>%
  group_by(`last_name..first_name`) %>%
  summarise(
    avg_5yr_active_spin = mean(active_spin_fourseam, na.rm = TRUE),
    .groups = "drop"
  )

avg_movement <- bind_rows(movement_data) %>%
  filter(`last_name..first_name` %in% common_pitchers) %>%
  group_by(`last_name..first_name`) %>%
  summarise(
    avg_5yr_velocity = mean(avg_speed, na.rm = TRUE),
    avg_5yr_horizontal_movement = mean(pitcher_break_x, na.rm = TRUE),
    avg_5yr_vertical_movement = mean(pitcher_break_z, na.rm = TRUE),
    .groups = "drop"
  )

avg_war <- bind_rows(war_data) %>%
  group_by(NAME) %>%
  summarise(avg_WAR = mean(WAR, na.rm = TRUE), .groups = "drop") %>%
  mutate(NAME = gsub("\\*", "", NAME))

classified_war <- avg_active_spin %>%
  inner_join(avg_movement, by = "last_name..first_name") %>%
  mutate(
    spin_class = ifelse(
      avg_5yr_active_spin >= median(avg_5yr_active_spin, na.rm = TRUE),
      "H",
      "L"
    ),
    velo_class = ifelse(
      avg_5yr_velocity >= median(avg_5yr_velocity, na.rm = TRUE),
      "H",
      "L"
    ),
    move_class = ifelse(
      (avg_5yr_horizontal_movement + avg_5yr_vertical_movement) >=
        median(
          avg_5yr_horizontal_movement + avg_5yr_vertical_movement,
          na.rm = TRUE
        ),
      "H",
      "L"
    ),
    profile = paste0(spin_class, velo_class, move_class)
  ) %>%
  separate(`last_name..first_name`, into = c("last", "first"), sep = ", ") %>%
  mutate(NAME = paste(first, last)) %>%
  left_join(avg_war, by = "NAME")

model_data <- classified_war %>%
  filter(
    is.finite(avg_WAR),
    is.finite(avg_5yr_active_spin),
    is.finite(avg_5yr_velocity),
    is.finite(avg_5yr_horizontal_movement),
    is.finite(avg_5yr_vertical_movement)
  )

profile_summary <- model_data %>%
  group_by(profile) %>%
  summarise(
    avg_WAR = mean(avg_WAR, na.rm = TRUE),
    count = n(),
    .groups = "drop"
  ) %>%
  arrange(desc(avg_WAR))

lm_model <- lm(
  avg_WAR ~ avg_5yr_active_spin + avg_5yr_velocity +
    avg_5yr_horizontal_movement + avg_5yr_vertical_movement,
  data = model_data
)

model_summary <- summary(lm_model)

correlation_summary <- tibble(
  metric = c(
    "Active spin %",
    "Velocity",
    "Horizontal movement",
    "Vertical movement"
  ),
  variable = c(
    "avg_5yr_active_spin",
    "avg_5yr_velocity",
    "avg_5yr_horizontal_movement",
    "avg_5yr_vertical_movement"
  )
) %>%
  rowwise() %>%
  mutate(
    r = cor(model_data[[variable]], model_data$avg_WAR, use = "complete.obs"),
    p_value = cor.test(model_data[[variable]], model_data$avg_WAR)$p.value
  ) %>%
  ungroup() %>%
  select(metric, r, p_value)

write.csv(model_data, file.path(tables_dir, "pitcher_model_data.csv"), row.names = FALSE)
write.csv(profile_summary, file.path(tables_dir, "profile_summary.csv"), row.names = FALSE)
write.csv(correlation_summary, file.path(tables_dir, "correlation_summary.csv"), row.names = FALSE)

model_coefficients <- as.data.frame(model_summary$coefficients)
model_coefficients$term <- row.names(model_coefficients)
row.names(model_coefficients) <- NULL
write.csv(
  model_coefficients,
  file.path(tables_dir, "regression_coefficients.csv"),
  row.names = FALSE
)

long_data <- model_data %>%
  select(
    avg_WAR,
    `Active Spin %` = avg_5yr_active_spin,
    Velocity = avg_5yr_velocity,
    `Horizontal Movement` = avg_5yr_horizontal_movement,
    `Vertical Movement` = avg_5yr_vertical_movement
  ) %>%
  pivot_longer(
    cols = -avg_WAR,
    names_to = "Metric",
    values_to = "Value"
  )

metric_relationship_plot <- ggplot(long_data, aes(x = Value, y = avg_WAR)) +
  geom_point(alpha = 0.65, color = "#2E6F9E") +
  geom_smooth(method = "lm", color = "#B23A48", se = TRUE) +
  facet_wrap(~ Metric, scales = "free_x") +
  labs(
    title = "Pitch Metrics vs. Average WAR",
    x = "Five-year average metric value",
    y = "Average WAR"
  ) +
  theme_minimal(base_size = 11)

ggsave(
  file.path(figures_dir, "pitch-metrics-vs-war.pdf"),
  metric_relationship_plot,
  width = 9,
  height = 6
)

velocity_profile_plot <- ggplot(
  model_data,
  aes(x = avg_5yr_velocity, y = avg_WAR, color = profile)
) +
  geom_point(size = 3, alpha = 0.75) +
  labs(
    title = "Velocity vs. WAR by Pitcher Profile",
    x = "Average five-year velocity (mph)",
    y = "Average WAR",
    color = "Profile"
  ) +
  theme_minimal(base_size = 11)

ggsave(
  file.path(figures_dir, "velocity-vs-war-by-profile.pdf"),
  velocity_profile_plot,
  width = 8,
  height = 5
)

distribution_data <- model_data %>%
  select(
    `Active Spin %` = avg_5yr_active_spin,
    Velocity = avg_5yr_velocity,
    `Horizontal Movement` = avg_5yr_horizontal_movement,
    `Vertical Movement` = avg_5yr_vertical_movement
  ) %>%
  pivot_longer(cols = everything(), names_to = "Metric", values_to = "Value")

distribution_plot <- ggplot(distribution_data, aes(x = Value)) +
  geom_histogram(fill = "#2E6F9E", color = "white", bins = 15, alpha = 0.85) +
  facet_wrap(~ Metric, scales = "free_x") +
  labs(
    title = "Distributions of Pitching Metrics",
    x = "Five-year average metric value",
    y = "Pitcher count"
  ) +
  theme_minimal(base_size = 11)

ggsave(
  file.path(figures_dir, "pitch-metric-distributions.pdf"),
  distribution_plot,
  width = 9,
  height = 6
)

print(profile_summary)
print(correlation_summary)
print(model_summary)
