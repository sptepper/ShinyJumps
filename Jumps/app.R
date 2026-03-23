library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(readr)
library(shinyjs)

# --- Load & prepare data ---
df <- read_csv("jumps.csv")

# Convert Date
df$Date <- as.Date(df$Date, format = "%m/%d/%Y")
df$TryNum <- as.numeric(df$Try)

# Convert Mark from feet to meters
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

# Determine Level: Varsity or JV
df <- df %>%
  mutate(Level = ifelse(grepl("Varsity", Division, ignore.case = TRUE) |
                          grepl("Final", Event, ignore.case = TRUE),
                        "Varsity", "JV"))

# UI
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Jump Attempts"),
  fluidRow(
    column(
      width = 3,
      wellPanel(
        actionButton("toggleFilters", "Show/Hide Filters", class = "btn btn-sm btn-secondary"),
        hidden(div(id = "filterPanel", style = "margin-top: 10px;",
                   selectInput("division", "Filter by Division:", choices = c("All", unique(df$Division)), selected = "All", width = "100%"),
                   selectInput("event", "Filter by Event:", 
                               choices = c("Long", "Triple"), 
                               selected = "Long", 
                               width = "100%"),
                   selectInput(
                     "athlete",
                     "Filter by Athlete:",
                     choices = c("All", sort(unique(df$Name))),
                     selected = "All",
                     width = "100%"
                   ),
                   selectInput("trendType", "Trendline Type:", choices = c("Linear", "LOESS"), selected = "Linear", width = "100%")
        ))
      )
    ),
    column(
      width = 9,
      fluidRow(
        column(width = 12,
               div(style = "text-align: center; margin-bottom: 10px;",
                   actionButton("btnHideTop5", "Hide Top 5", class = "btn btn-sm btn-outline-primary", style = "margin-right: 5px;"),
                   actionButton("btnHideBelowTop5", "Hide Below Top 5", class = "btn btn-sm btn-outline-primary")
               )
        )
      ),
      plotlyOutput("jumpPlot", height = "650px")
    )
  ),
  tags$style(HTML(".plotly-legend {
      position: relative !important;
      display: flex !important;
      flex-wrap: wrap !important;
      justify-content: center !important;
    }
    .plot-container .legend {
      margin-top: 20px;
    }
    .btn-sm {
      padding: 4px 8px;
      font-size: 0.8em;
    }
    .btn-active {
      background-color: #0056b3 !important;
      color: white !important;
    }
  "))
)

# Server
server <- function(input, output, session) {
  observeEvent(input$toggleFilters, {
    toggle("filterPanel", anim = TRUE)
  })
  
  # Top 5 toggles
  top5_state <- reactiveValues(hideTop5 = FALSE, hideBelowTop5 = FALSE)
  
  observeEvent(input$btnHideTop5, {
    top5_state$hideTop5 <- !top5_state$hideTop5
    toggleClass("btnHideTop5", "btn-active", condition = top5_state$hideTop5)
  })
  
  observeEvent(input$btnHideBelowTop5, {
    top5_state$hideBelowTop5 <- !top5_state$hideBelowTop5
    toggleClass("btnHideBelowTop5", "btn-active", condition = top5_state$hideBelowTop5)
  })
  
  observe({
    req(input$event)
    
    filtered_names <- df %>%
      filter(grepl(input$event, Event, ignore.case = TRUE)) %>%
      distinct(Name) %>%
      pull(Name) %>%
      sort()
    
    updateSelectInput(
      session,
      "athlete",
      choices = c("All", filtered_names),
      selected = "All"
    )
  })
  
  observeEvent(input$athlete, {
    if (input$athlete != "All") {
      updateSelectInput(session, "division", selected = "All")
    }
  })
  
  filtered_data <- reactive({
    df_filtered <- df
    
    if (input$division != "All") {
      df_filtered <- df_filtered[df_filtered$Division == input$division, ]
    }
    
    # Required event selection (no All)
    df_filtered <- df_filtered[grepl(input$event, df_filtered$Event, ignore.case = TRUE), ]
    
    if (input$athlete != "All") {
      df_filtered <- df_filtered[df_filtered$Name == input$athlete, ]
    }
    
    top5_names <- df_filtered %>%
      group_by(Name) %>%
      summarize(best = max(Distance_m, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(best)) %>%
      slice_head(n = 5) %>%
      pull(Name)
    
    if (top5_state$hideTop5) {
      df_filtered <- df_filtered[!df_filtered$Name %in% top5_names, ]
    }
    if (top5_state$hideBelowTop5) {
      df_filtered <- df_filtered[df_filtered$Name %in% top5_names, ]
    }
    
    df_filtered
  })
  
  output$jumpPlot <- renderPlotly({
    df_plot <- filtered_data()
    y_col <- df_plot$Distance_ft
    y_label <- "Distance (ft)"
    
    event_title <- if (grepl("Triple", input$event, ignore.case = TRUE)) {
      "Triple Jumps"
    } else {
      "Long Jumps"
    }
    
    df_plot$tooltip_text <- paste(
      "Athlete:", df_plot$Name,
      "<br>Date:", format(df_plot$Date, "%b %d, %Y"),
      "<br>Division:", df_plot$Division,
      "<br>Event:", df_plot$Event,
      "<br>Mark:", df_plot$Mark,
      "<br>Distance:", round(y_col, 2), "ft"
    )
    
    # Compute bottom position for X marks
    y_min <- min(y_col, na.rm = TRUE)
    
    # Compute bottom and top for annotations
    y_min <- min(y_col, na.rm = TRUE)
    y_max <- max(y_col, na.rm = TRUE)
    
    # Build meet boundaries
    meet_bounds <- df_plot %>%
      group_by(Meet) %>%
      summarize(
        start = min(PlotDate),
        end = max(PlotDate),
        mid = mean(c(min(PlotDate), max(PlotDate))),
        .groups = "drop"
      )
    
    p <- ggplot(df_plot, aes(x = PlotDate, y = y_col, color = Name, text = tooltip_text)) +
      geom_point(size = 3, alpha = 0.85) +
      # Vertical dashed lines for meet boundaries
      geom_vline(data = meet_bounds, aes(xintercept = start), linetype = "dashed", color = "gray50", alpha = 0.6, inherit.aes = FALSE) +
      geom_vline(data = meet_bounds, aes(xintercept = end), linetype = "dashed", color = "gray50", alpha = 0.6, inherit.aes = FALSE) +
      # Meet labels at top
      geom_text(
        data = meet_bounds,
        aes(x = mid, y = y_max + 0.5, label = Meet),
        inherit.aes = FALSE,
        size = 3,
        color = "black"
      ) +
      # Add X marks for scratches (do not affect trendlines)
      geom_text(
        data = df_plot %>% filter(Mark %in% c("X", "PASS")),
        aes(x = PlotDate, y = y_min - 0.5, label = ifelse(Mark == "PASS", "P", "X"), color = Name),
        inherit.aes = FALSE,
        size = 3.5,
        alpha = 0.9
      ) +
      labs(
        title = event_title,
        x = "Date of Jump",
        y = y_label,
        color = "Athlete"
      ) +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "bottom",
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 9)
      )
    
    if (input$trendType == "Linear") {
      p <- p + geom_smooth(aes(group = Name), method = "lm", se = FALSE, size = 0.4, linetype = "solid")
    } else {
      p <- p + geom_smooth(aes(group = Name), method = "loess", se = FALSE, size = 0.4, linetype = "solid")
    }
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.3))
  })
}

shinyApp(ui, server)
