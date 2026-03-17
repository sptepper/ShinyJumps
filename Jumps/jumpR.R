# Script for local exploration of data

setwd("~/Code/ShinyJumps/")
jumps <- read.csv("./jumps.csv")

#DATA TYPE CONVERSIONS
jumps$Date <- as.Date(jumps$Date, format = "%m/%d/%Y")
jumps$TryNum <- as.numeric(jumps$Try)

#Date + Try offset
jumps <- jumps %>% 
  group_by(Date) %>% 
  arrange(Date, Name) %>% 
  mutate(Offset = TryNum * 0.6) %>% 
  ungroup() %>% 
  mutate(PlotDate = Date + Offset)

#Clean Mark
# What to do about X and PASS

#Mark to Fractional Meters
convert_mark_to_meters <- function(mark_str) {
  # Remove quotes if they exist and split on apostrophe
  clean <- gsub('"', '', mark_str)
  parts <- strsplit(clean, "'")[[1]]
  feet <- as.numeric(parts[1])
  inches <- as.numeric(parts[2])
  total_inches <- feet * 12 + inches
  meters <- total_inches * 0.0254
  return(meters)
}

jumps$Distance <- sapply(jumps$Mark, convert_mark_to_meters)

# Filter into Groups

long_jump <- jumps %>%
  filter(Event=="Long")

boys_long <- long_jump %>% 
  filter(Division=="Boy")

girls_long <- long_jump %>% 
  filter(Division=="Girl")

triple_jump <- jumps %>% 
  filter(Event=="Triple")

boys_triple <- triple_jump %>% 
  filter(Division=="Boy")

girls_triple <- triple_jump %>% 
  filter(Division=="Girl")

# Plot all jumps
ggplot(long_jump, aes(x = PlotDate, y = Distance, color = Division)) +
  geom_point(size = 2, shape = 13) + 
  labs(title = "Wenatchee Long Jumps",
       x = "Date",
       y = "Distance (m)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

ggplot(triple_jump, aes(x = PlotDate, y = Distance, color = Division)) +
  geom_point(size = 2, shape = 13) + 
  labs(title = "Wenatchee Triple Jumps",
       x = "Date",
       y = "Distance (m)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

## ggplot(data = long_jump) + geom_point(mapping = aes(x = PlotDate, y = Distance, color = Division))

pb <- ggplot(boys_long, aes(x = PlotDate, y = Distance, color = Name)) +
  geom_point(size = 2, shape = 13) + 
  geom_smooth(method = "lm", se=F) +
  labs(title = "Wenatchee Long Jumps (Boys)",
       x = "Date",
       y = "Distance (m)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

pg <- ggplot(girls_long, aes(x = PlotDate, y = Distance, color = Name)) +
  geom_point(size = 2, shape = 13) + 
  geom_smooth(method = "lm", se=F) +
  labs(title = "Wenatchee Long Jumps (Girls)",
       x = "Date"
  ) +
  scale_y_continuous(
    name = "Distance (m)",
    sec.axis = sec_axis(~ . * 3.28084, name = "Distance (ft)")
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

pbt <- ggplot(boys_triple, aes(x = PlotDate, y = Distance, color = Name)) +
  geom_point(size = 2, shape = 13) + 
  geom_smooth(method = "lm", se=F) +
  labs(title = "Wenatchee Triple Jumps (Boys)",
       x = "Date"
  ) +
       scale_y_continuous(
         name = "Distance (m)",
         sec.axis = sec_axis(~ . * 3.28084, name = "Distance (ft)")
       ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

pgt <- ggplot(girls_triple, aes(x = PlotDate, y = Distance, color = Name)) +
  geom_point(size = 2, shape = 13) + 
  geom_smooth(method = "lm", se=F) +
  labs(title = "Wenatchee Triple Jumps (Girls)",
       x = "Date",
       y = "Distance (m)"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

interactive_plot <- ggplotly(pg, tooltip = "hmm")
interactive_plot

long_jump_parker <- long_jump %>% 
  filter(Name=="Parker Averi")

ggplot(data = long_jump_parker, aes(x = PlotDate, y = Distance, color = Meet)) + geom_point() + 
  geom_smooth(method = "lm") +
  labs(title = "Parker Averi Long Jumps",
       x = "Date",
       y = "Distance (m)"
  ) +
  theme_minimal()