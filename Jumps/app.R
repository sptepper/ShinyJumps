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

df_high$Height_ft <- sapply(df_high$Height, convert_height_to_ft)

df_high <- df_high %>%
  group_by(Date) %>%
  arrange(Date, Name) %>%
  mutate(Offset = TryNum * 0.6) %>%
  ungroup() %>%
  mutate(PlotDate = Date + Offset)

# Combine all
df_all <- bind_rows(df2025, df2026, df_high)

# --- UI ---
ui <- fluidPage(
  useShinyjs(),
  titlePanel("Jump Attempts"),
  fluidRow(
    column(
      width = 3,
      wellPanel(
        actionButton("toggleFilters", "Show/Hide Filters", class = "btn btn-sm btn-secondary"),
        hidden(div(id = "filterPanel", style = "margin-top: 10px;",
                   selectInput("event", "Event:", choices = c("Long","Triple","High"), selected = "Long", width = "100%"),
                   selectInput("year", "Year:", choices = c("2025","2026"), selected = "2026", width = "100%"),
                   selectInput("division", "Division:", choices = c("All", unique(df_all$Division)), selected = "All", width = "100%"),
                   selectInput("athlete", "Athlete:", choices = c("All", sort(unique(df_all$Name))), selected = "All", width = "100%"),
                   selectInput("trendType", "Trendline Type:", choices = c("Linear","LOESS"), selected = "Linear", width = "100%")
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
  tags$style(HTML(".plotly-legend {position: relative !important; display: flex !important; flex-wrap: wrap !important; justify-content: center !important;}
                   .plot-container .legend {margin-top: 20px;}
                   .btn-sm {padding: 4px 8px; font-size: 0.8em;}
                   .btn-active {background-color: #0056b3 !important; color: white !important;}"))
)

# --- Server ---
server <- function(input, output, session) {
  
  observeEvent(input$toggleFilters, { toggle("filterPanel", anim = TRUE) })
  
  top5_state <- reactiveValues(hideTop5 = FALSE, hideBelowTop5 = FALSE)
  
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
  
  top5_names_reactive <- reactive({
    df_filtered <- df_all %>% filter(Year==input$year)
    if(input$division!="All") df_filtered <- df_filtered %>% filter(Division==input$division)
    df_filtered <- df_filtered %>% filter(grepl(input$event, Event, ignore.case=TRUE))
    df_filtered %>% group_by(Name) %>% summarize(best=max(ifelse(grepl("High", Event, ignore.case=TRUE), Height_ft, Distance_ft),na.rm=TRUE),.groups="drop") %>%
      arrange(desc(best)) %>% slice_head(n=5) %>% pull(Name)
  })
  
  filtered_data <- reactive({
    df_filtered <- df_all %>% filter(Year==input$year)
    if(input$division!="All") df_filtered <- df_filtered %>% filter(Division==input$division)
    df_filtered <- df_filtered %>% filter(grepl(input$event, Event, ignore.case=TRUE))
    if(input$athlete!="All") df_filtered <- df_filtered %>% filter(Name==input$athlete)
    
    top5_names <- top5_names_reactive()
    if(top5_state$hideTop5) df_filtered <- df_filtered %>% filter(!Name %in% top5_names)
    if(top5_state$hideBelowTop5) df_filtered <- df_filtered %>% filter(Name %in% top5_names)
    
    df_filtered
  })
  
  output$jumpPlot <- renderPlotly({
    df_plot <- filtered_data()
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
      # Max cleared height line
      max_heights <- df_plot %>% filter(Result=="O") %>% group_by(Name) %>%
        summarize(max_height=max(y_val,na.rm=TRUE),
                  last_date=max(PlotDate), .groups="drop")
      p <- p + geom_segment(
        data=max_heights,
        aes(x=last_date, xend=max(df_plot$PlotDate)+1, y=max_height, yend=max_height, color=Name),
        linetype="solid", size=1, inherit.aes=FALSE
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
    }
    
    # Trendlines
    if (input$trendType == "Linear") {
      p <- p + geom_smooth(aes(group = Name), method = "lm", se = FALSE, size = 0.4)
    } else {
      p <- p + geom_smooth(aes(group = Name), method = "loess", se = FALSE, size = 0.4)
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