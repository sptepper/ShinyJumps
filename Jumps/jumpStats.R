library(dplyr)
library(readr)
library(lubridate)
library(stringr)
library(broom)

# Load data
df <- read_csv("./jumps.csv")

# Group and count entries
summary_table <- df %>%
  group_by(Name, Event) %>%
  summarize(Entries = n(), .groups = "drop") %>%
  arrange(desc(Entries))

# Print the table
print(summary_table)

# Optional: View in a viewer if you're using RStudio
if (interactive()) View(summary_table)


# Get all athlete names
all_athletes <- df %>%
  distinct(Name)

# Count scratches per athlete
scratch_counts <- df %>%
  filter(Mark == "X") %>%
  group_by(Name) %>%
  summarize(Scratch_Count = n(), .groups = "drop")

# Join and fill in 0s for athletes with no scratches
full_summary <- all_athletes %>%
  left_join(scratch_counts, by = "Name") %>%
  mutate(Scratch_Count = ifelse(is.na(Scratch_Count), 0, Scratch_Count)) %>%
  arrange(desc(Scratch_Count))

# Print or view
print(full_summary)

if (interactive()) View(full_summary)



# Convert date
df <- df %>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"))

# Convert marks to meters
convert_mark_to_meters <- function(mark_str) {
  if (mark_str == "X") return(NA)
  matches <- str_match(mark_str, "^(\\d+)'(\\d+\\.?\\d*)")
  if (is.na(matches[1, 1])) return(NA)
  feet <- as.numeric(matches[1, 2])
  inches <- as.numeric(matches[1, 3])
  meters <- (feet * 12 + inches) * 0.0254
  return(meters)
}

df <- df %>%
  mutate(Distance_m = sapply(Mark, convert_mark_to_meters)) %>%
  filter(!is.na(Distance_m))

# Fit linear models per athlete/event/division and extract slope
slopes <- df %>%
  group_by(Name, Division, Event) %>%
  filter(n() >= 2) %>%
  summarise(
    model = list(lm(Distance_m ~ as.numeric(Date))),
    .groups = "drop"
  ) %>%
  mutate(
    tidied = lapply(model, tidy),
    slope_m_per_day = sapply(tidied, function(t) t$estimate[t$term == "as.numeric(Date)"]),
    slope_ft_per_week = slope_m_per_day * 7 * 3.28084
  )

# Convert slope to ft/inch format
slopes <- slopes %>%
  mutate(
    slope_feet = floor(slope_ft_per_week),
    slope_inches = round((slope_ft_per_week - slope_feet) * 12, 2),
    Trend = paste0(slope_feet, "'", slope_inches, "\" per week")
  )

# Keep only positive trends
positive_slopes <- slopes %>%
  filter(slope_ft_per_week > 0)

# Top 5 per Division & Event (include ties)
top5_trends <- positive_slopes %>%
  group_by(Division, Event) %>%
  arrange(desc(slope_ft_per_week)) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 5 | slope_ft_per_week == nth(slope_ft_per_week, 5)) %>%
  ungroup() %>%
  select(Division, Event, Name, Trend, slope_ft_per_week) %>%
  arrange(Division, Event, desc(slope_ft_per_week), Name)

# Display
print(top5_trends)

if (interactive()) View(top5_trends)