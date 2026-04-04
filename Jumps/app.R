library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(readr)
library(shinyjs)

# --- Load & prepare data ---
df2025 <- read_csv("jumps.csv")
df2026 <- read_csv("jumps2026.csv")
df_high <- read_csv("highjumps2026.csv")

prepare_df <- function(df, year_label) {
  df$Date <- as.Date(df$Date, format = "%m/%d/%Y")
  df$TryNum <- as.numeric(df$Try)
  df$Year <- year_label
  
  # Only for Long/Triple jumps
  if ("Mark" %in% colnames(df)) {
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
  }
  
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
  
  return(df)
}

df2025 <- prepare_df(df2025, "2025")
df2026 <- prepare_df(df2026, "2026")

# Prepare High Jump separately
df_high$Date <- as.Date(df_high$Date, format = "%m/%d/%Y")
df_high$TryNum <- as.numeric(df_high$Try)
df_high$Year <- "2026"

convert_height_to_ft <- function(h) {
  matches <- regmatches(h, regexec("^(\\d+)'(\\d+\\.?\\d*)", h))[[1]]
  if (length(matches) != 3) return(as.numeric(h)) # fallback if numeric
  feet <- as.numeric(matches[2])
  inches <- as.numeric(matches[3])
  return(feet + inches/12)
}

format_height_ft_in <- function(x) {
  feet <- floor(x)
  inches <- round((x - feet) * 12)
  
  # Handle rounding edge case (e.g. 5.999 -> 6'0)
  feet <- ifelse(inches == 12, feet + 1, feet)
  inches <- ifelse(inches == 12, 0, inches)
  
  paste0(feet, "'", inches)
}

df_high$Height_ft <- sapply(df_high$Height, convert_height_to_ft)

df_high <- df_high %>%
  group_by(Meet, Date, Name) %>%
  arrange(Height_ft, TryNum) %>%
  ungroup()

# Create a meet-wide attempt index
df_high <- df_high %>%
  group_by(Meet, Date) %>%
  arrange(Height_ft, TryNum) %>%
  mutate(
    attempt_index = row_number(),
    total_attempts = n()
  ) %>%
  ungroup()

df_high <- df_high %>%
  mutate(
    PlotDate = Date + (attempt_index / total_attempts) * 2  # <-- controls width
  )

# Combine all
df_all <- bind_rows(df2025, df2026, df_high)

# --- UI ---
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Jump Attempts"),
  
  # --- Top Control Bar ---
  fluidRow(
    column(
      width = 12,
      div(
        style = "display: flex; flex-wrap: wrap; align-items: center; gap: 10px; margin-bottom: 15px;",
        
        # Filters
        selectInput("year",  " ",
                    choices = c("2025","2026"),
                    selected = "2026", width = "100px"),
        
        checkboxInput("showPrevYear", "Show Previous Year", value = FALSE),
        
        selectInput("event", " " ,
                    choices = c("Long","Triple","High"),
                    selected = "Long", width = "120px"),
        
        
        selectInput("division", "Division:",
                    choices = c("All", unique(df_all$Division)),
                    selected = "All", width = "160px"),
        
        selectInput("athlete", "Athlete:",
                    choices = c("All Athletes", sort(unique(df_all$Name))),
                    selected = "All", width = "180px"),
        
        selectInput(
          "trendType",
          "Trend:",
          choices = c("None", "Linear", "LOESS"),
          selected = "None",
          width = "120px"
        ),
        
        sliderInput(
          "rankRange",
          "PB Rank:",
          min = 1,
          max = 10,   # placeholder, will update dynamically
          value = c(1, 10),
          step = 1,
          width = "250px"
        )
      )
    )
  ),
  
  # --- Plot ---
  fluidRow(
    column(
      width = 12,
      plotlyOutput("jumpPlot", height = "650px")
    )
  ),
  
  tags$style(HTML("
    .plotly-legend {
      position: relative !important;
      display: flex !important;
      flex-wrap: wrap !important;
      justify-content: center !important;
    }
    .plot-container .legend {margin-top: 20px;}
    .btn-sm {padding: 4px 8px; font-size: 0.8em;}
    .btn-active {background-color: #0056b3 !important; color: white !important;}
    
    /* Make labels tighter */
    .form-group {margin-bottom: 5px;}
  "))
)

# --- Server ---
server <- function(input, output, session) {
  
  observeEvent(input$btnHideTop5, {
    top5_state$hideTop5 <- !top5_state$hideTop5
    if(top5_state$hideTop5) top5_state$hideBelowTop5 <- FALSE
    toggleClass("btnHideTop5","btn-active",condition=top5_state$hideTop5)
  })
  
  observeEvent(input$btnHideBelowTop5, {
    top5_state$hideBelowTop5 <- !top5_state$hideBelowTop5
    if(top5_state$hideBelowTop5) top5_state$hideTop5 <- FALSE
    toggleClass("btnHideBelowTop5","btn-active",condition=top5_state$hideBelowTop5)
  })
  
  # Update athlete list based on year/event/division
  observe({
    req(input$year, input$event, input$division)
    df_filtered <- df_all %>% filter(Year==input$year)
    if(input$division!="All") df_filtered <- df_filtered %>% filter(Division==input$division)
    df_filtered <- df_filtered %>% filter(grepl(input$event, Event, ignore.case=TRUE))
    athlete_choices <- sort(unique(df_filtered$Name))
    updateSelectInput(session,"athlete", choices=c("All",athlete_choices), selected="All")
  })
  
  # Rank Range Slider
  observe({
    ranks <- ranked_athletes()
    n <- nrow(ranks)
    
    updateSliderInput(
      session,
      "rankRange",
      min = 1,
      max = max(1, n),
      value = c(1, max(1, n))
    )
  })
  
  filtered_data <- reactive({
    df_filtered <- df_all %>%
      filter(Year == input$year)
    

    
    if (input$division != "All") {
      df_filtered <- df_filtered %>% filter(Division == input$division)
    }
    

    
    df_filtered <- df_filtered %>%
      filter(grepl(input$event, Event, ignore.case = TRUE))

    
    # Get ranked athletes
    ranks <- ranked_athletes()
    
    selected_names <- ranks %>%
      filter(rank >= input$rankRange[1],
             rank <= input$rankRange[2]) %>%
      pull(Name)
    
    df_filtered <- df_filtered %>%
      filter(Name %in% selected_names)
    
    if (input$athlete != "All") {
      df_filtered <- df_filtered %>% filter(Name == input$athlete)
    }
    
    df_filtered
  })
  
  all_athletes <- sort(unique(df_all$Name))
  
  # Generate stable color palette
  palette_colors <- RColorBrewer::brewer.pal(8, "Set2")
  palette_colors <- colorRampPalette(palette_colors)(length(all_athletes))
  
  
  athlete_colors <- setNames(palette_colors, all_athletes)
  
  ranked_athletes <- reactive({
    df_filtered <- df_all %>%
      filter(Year == input$year)
    
    if (input$division != "All") {
      df_filtered <- df_filtered %>% filter(Division == input$division)
    }
    
    df_filtered <- df_filtered %>%
      filter(grepl(input$event, Event, ignore.case = TRUE))
    
    # Choose correct metric
    df_summary <- df_filtered %>%
      group_by(Name) %>%
      summarize(
        best = max(
          ifelse(grepl("High", Event, ignore.case = TRUE),
                 Height_ft,
                 Distance_ft),
          na.rm = TRUE
        ),
        .groups = "drop"
      ) %>%
      arrange(desc(best)) %>%
      mutate(rank = row_number())
    
    df_summary
  })
  
  output$jumpPlot <- renderPlotly({
    df_plot <- filtered_data()
    
    current_year <- input$year
    prev_year <- as.character(as.numeric(current_year) - 1)
    
    # Prev year filtering
    if (input$division != "All") {
      df_prev <- df_prev %>% filter(Division == input$division)
    }
    
    df_prev <- df_all %>%
      filter(Year == prev_year)
    df_prev <- df_prev %>%
      filter(grepl(input$event, Event, ignore.case = TRUE))
    
    visible_names <- unique(df_plot$Name)
    
    df_prev <- df_prev %>%
      filter(Name %in% visible_names)
    
    is_high <- grepl("High", input$event, ignore.case=TRUE)
    
    if (is_high) {
      y_col <- df_plot$Height_ft
      y_label <- "Height (ft)"
      event_title <- "High Jump Attempts"
    } else {
      y_col <- df_plot$Distance_ft
      y_label <- "Distance (ft)"
      event_title <- ifelse(grepl("Triple", input$event, ignore.case=TRUE),"Triple Jumps","Long Jumps")
    }
    
    df_plot$y_val <- y_col
    
    df_plot$tooltip_text <- if (is_high) {
      paste(
        "Athlete:", df_plot$Name,
        "<br>Date:", format(df_plot$Date,"%b %d, %Y"),
        "<br>Meet:", df_plot$Meet,
        "<br>Height:", df_plot$Height,
        "<br>Result:", df_plot$Result,
        "<br>Attempt:", df_plot$TryNum
      )
    } else {
      paste(
        "Athlete:", df_plot$Name,
        "<br>Date:", format(df_plot$Date,"%b %d, %Y"),
        "<br>Division:", df_plot$Division,
        "<br>Event:", df_plot$Event,
        "<br>Attempt:", df_plot$TryNum,
        "<br>Mark:", df_plot$Mark,
        "<br>Distance:", round(y_col,2),"ft"
      )
    }
    
    y_min <- min(df_plot$y_val, na.rm=TRUE)
    y_max <- max(df_plot$y_val, na.rm=TRUE)
    
    meet_bounds <- df_plot %>% group_by(Meet) %>%
      summarize(start=min(PlotDate), end=max(PlotDate),
                mid=mean(c(min(PlotDate),max(PlotDate))), .groups="drop")
    
    best_per_meet <- df_plot %>%
      group_by(Name, Meet) %>%
      filter(!is.na(y_val)) %>%
      mutate(best_val = max(y_val, na.rm = TRUE)) %>%
      summarize(
        best_val = first(best_val),
        best_date = max(PlotDate[y_val == best_val]),
        .groups = "drop"
      )
    
    season_best <- df_plot %>%
      group_by(Name) %>%
      filter(!is.na(y_val)) %>%
      mutate(best_val = max(y_val, na.rm = TRUE)) %>%
      summarize(
        best_val = first(best_val),
        best_date = max(PlotDate[y_val == best_val]),
        .groups = "drop"
      )
    
    ## Previous Year
    # Get current meet layout
    meet_map <- df_plot %>%
      group_by(Meet) %>%
      summarize(
        start = min(PlotDate),
        end = max(PlotDate),
        mid = mean(c(start, end)),
        .groups = "drop"
      )
    
    prev_meets <- df_prev %>%
      distinct(Meet, Date) %>%
      arrange(Date) %>%
      mutate(meet_index = row_number())
    
    current_meets <- meet_map %>%
      arrange(start) %>%
      mutate(meet_index = row_number())
    
    df_prev <- df_prev %>%
      left_join(prev_meets, by = "Meet") %>%
      left_join(current_meets, by = "meet_index", suffix = c("", "_curr"))
    
    df_prev <- df_prev %>%
      group_by(Meet) %>%
      arrange(Height_ft, TryNum) %>%
      mutate(
        attempt_index = row_number(),
        total_attempts = n()
      ) %>%
      ungroup() %>%
      mutate(
        PlotDate = start + (attempt_index / total_attempts) * (end - start)
      )
    
    p <- ggplot(df_plot, aes(x=PlotDate, y=y_val, color=Name, text=tooltip_text))
    
    if (is_high) {
      # Successes
      p <- p + geom_point(
        data = df_plot %>% filter(Result=="O"),
        size = 3, alpha = 0.9
      )
      # Misses
      p <- p + geom_text(
        data = df_plot %>% filter(Result=="X"),
        aes(label="X"),
        size = 4, fontface = "bold"
      )
      
      # clear percentage per height
      clear_pct <- df_plot %>%
        filter(!is.na(y_val)) %>%
        group_by(Name, Height_ft) %>%
        summarize(
          attempts = n(),
          clears = sum(Result == "O", na.rm = TRUE),
          pct = clears / attempts,
          .groups = "drop"
        )
      
      athlete_order <- df_plot %>%
        distinct(Name) %>%
        arrange(Name) %>%
        mutate(offset_index = row_number())
      
      clear_pct <- clear_pct %>%
        left_join(athlete_order, by = "Name")
      
      x_min <- min(df_plot$PlotDate, na.rm = TRUE)
      
      clear_pct <- clear_pct %>%
        mutate(
          label = paste0(round(pct * 100), "%"),
          x_pos = x_min - 0.5 + (offset_index - 1) * 0.3
        )
      
      p <- p +
        geom_text(
          data = clear_pct,
          aes(x = x_pos, y = Height_ft, label = label, color = Name),
          size = 3,
          hjust = 1,
          inherit.aes = FALSE
        )
      
      p <- p +
        scale_color_manual(values = athlete_colors)
      
      # Max cleared height line
      max_heights <- df_plot %>% filter(Result=="O") %>% group_by(Name) %>%
        summarize(max_height=max(y_val,na.rm=TRUE),
                  last_date=max(PlotDate), .groups="drop")
      p <- p + geom_segment(
        data=max_heights,
        aes(x=last_date, xend=max(df_plot$PlotDate)+1, y=max_height, yend=max_height, color=Name),
        linetype="solid", size=1, alpha=0.35, inherit.aes=FALSE
      )
      
      y_min <- floor(min(df_plot$y_val, na.rm = TRUE) * 6) / 6
      y_max <- ceiling(max(df_plot$y_val, na.rm = TRUE) * 6) / 6
      
      # 2-inch increments = 1/6 ft
      minor_breaks <- seq(y_min, y_max, by = 1/6)
      
      # Whole feet
      major_breaks <- seq(floor(y_min), ceiling(y_max), by = 1)
      
      p <- p +
        scale_y_continuous(
          breaks = minor_breaks,
          labels = format_height_ft_in,
        )
      
      theme(
        panel.grid.major.y = element_line(color = "gray60", size = 0.6),
        panel.grid.minor.y = element_line(color = "gray80", size = 0.3)
      )
    } else {
      # Long/Triple jumps
      p <- p +
        geom_point(size=3, alpha=0.85) +
        geom_text(data=df_plot %>% filter(Mark %in% c("X","PASS")),
                  aes(x=PlotDate, y=y_min-0.5, label=ifelse(Mark=="PASS","P","X"), color = Name),
                  , size = 4.5, alpha = 0.9, inherit.aes = FALSE) +
        labs(title = event_title, x = "Date of Jump", y = y_label, color = "Athlete") +
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              legend.position = "bottom",
              legend.title = element_text(size = 10),
              legend.text = element_text(size = 9))
      
      # Meet Best
      p <- p +
        geom_point(
          data = best_per_meet,
          aes(x = best_date, y = best_val, color = Name, group = Name),
          size = 4,
          shape = 21,
          stroke = 0.45,
          fill = NA,
          color = "#363636",
          inherit.aes = FALSE
        )
      
      # Season Best
      p <- p +
        geom_point(
          data = season_best,
          aes(x = best_date, y = best_val, color = Name, group = Name),
          size = 4,
          shape = 21,
          stroke = 0.7,
          color = "black",
          fill = NA,
          inherit.aes = FALSE
        )
    }
    
    
    # Trendlines
    if (!is_high && input$trendType != "None") {
      if (input$trendType == "Linear") {
        p <- p + geom_smooth(
          aes(group = Name),
          method = "lm",
          se = FALSE,
          size = 0.4,
          alpha = 0.25
        )
      } else if (input$trendType == "LOESS") {
        p <- p + geom_smooth(
          aes(group = Name),
          method = "loess",
          se = FALSE,
          size = 0.4,
          alpha = 0.25
        )
      }
    }
    
    # Previous Year
    if (input$showPrevYear && nrow(df_prev) > 0) {
      
      df_prev$y_val <- if (is_high) df_prev$Height_ft else df_prev$Distance_ft
      
      # Points
      p <- p +
        geom_point(
          data = df_prev,
          aes(x = PlotDate, y = y_val, color = Name),
          alpha = 0.15,
          size = 2,
          inherit.aes = FALSE
        )
      
      # High jump X marks
      if (is_high) {
        p <- p +
          geom_text(
            data = df_prev %>% filter(Result == "X"),
            aes(x = PlotDate, y = y_val, label = "X", color = Name),
            alpha = 0.15,
            size = 3,
            inherit.aes = FALSE
          )
      }
      
      # previous trendlines
      if (!is_high && input$trendType != "None") {
        p <- p +
          geom_smooth(
            data = df_prev,
            aes(x = PlotDate, y = y_val, group = Name, color = Name),
            method = ifelse(input$trendType == "Linear", "lm", "loess"),
            se = FALSE,
            alpha = 0.1,
            size = 0.2,
            linetype="dashed",
            inherit.aes = FALSE
          )
      }
    }
    
    
    ggplotly(p, tooltip = "text") %>%
      layout(legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.3))
    
    # Meet dividers + labels
    p <- p +
      geom_vline(data=meet_bounds, aes(xintercept=start),
                 linetype="dashed", color="gray50", alpha=0.6, inherit.aes=FALSE) +
      geom_vline(data=meet_bounds, aes(xintercept=end),
                 linetype="dashed", color="gray50", alpha=0.6, inherit.aes=FALSE) +
      geom_text(data=meet_bounds, aes(x=mid, y=y_max + 0.3, label=Meet),
                inherit.aes=FALSE, size=3) +
      labs(title=event_title, x="Date", y=y_label, color="Athlete") +
      theme_minimal() +
      theme(axis.text.x=element_text(angle=45,hjust=1),
            legend.position="bottom")
    
    ggplotly(p, tooltip="text") %>%
      layout(legend=list(orientation="h", x=0.5, xanchor="center", y=-0.3))
  })
}

# --- Run App ---
shinyApp(ui, server)