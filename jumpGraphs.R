rm(list = ls())  # Clear environment

library(ggplot2)
library(dplyr)
library(readr)
library(stringr)

# --- Load & prepare data ---
df <- read_csv("~/Code/jumps.csv")

# Convert Date
df$Date <- as.Date(df$Date, format = "%m/%d/%Y")
df$TryNum <- as.numeric(df$Try)

# Convert Mark to meters
convert_mark_to_meters <- function(mark_str) {
  matches <- regmatches(mark_str, regexec("^(\\d+)'(\\d+\\.?\\d*)", mark_str))[[1]]
  if (length(matches) != 3) return(NA)
  feet <- as.numeric(matches[2])
  inches <- as.numeric(matches[3])
  meters <- (feet * 12 + inches) * 0.0254
  return(meters)
}
df$Distance_m <- sapply(df$Mark, convert_mark_to_meters)
df$Distance_ft <- df$Distance_m * 3.28084

# Offset attempts for same date
df <- df %>%
  group_by(Date) %>%
  arrange(Date, Name) %>%
  mutate(Offset = TryNum * 0.6) %>%
  ungroup() %>%
  mutate(PlotDate = Date + Offset)

# Create output folder
output_dir <- "plots"
if (!dir.exists(output_dir)) dir.create(output_dir)

# Loop through each athlete + event combo
athlete_event_combos <- df %>%
  distinct(Name, Event) %>%
  filter(!is.na(Name) & !is.na(Event))

for (i in seq_len(nrow(athlete_event_combos))) {
  name_i <- athlete_event_combos$Name[i]
  event_i <- athlete_event_combos$Event[i]
  
  df_i <- df %>%
    filter(Name == name_i, Event == event_i)
  
  if (nrow(df_i) < 2) next  # Skip if not enough data
  
  p <- ggplot(df_i, aes(x = PlotDate, y = Distance_ft)) +
    geom_point(color = "#6a0dad", size = 3, alpha = 1) +  # Solid purple points
    geom_smooth(method = "lm", se = FALSE, size = 1, color = "#6a0dad", alpha = 0.3) +  # Linear trendline in translucent purple
    geom_smooth(method = "loess", se = FALSE, size = 1, color = "#FFD700", alpha = 0.4) +  # LOESS trendline in translucent gold
    scale_y_continuous(
      name = "Distance (ft)",
      sec.axis = sec_axis(~ . / 3.28084, name = "Distance (m)")
    ) +
    labs(
      title = paste("Jump Trend:", name_i, "-", event_i),
      x = "Date"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      plot.title = element_text(size = 14, face = "bold")
    )
  
  safe_filename <- paste0(str_replace_all(name_i, "[^a-zA-Z0-9]", "_"),
                          "_",
                          str_replace_all(event_i, "[^a-zA-Z0-9]", "_"),
                          ".png")
  ggsave(filename = file.path(output_dir, safe_filename), plot = p, width = 8, height = 5, dpi = 300)
}
