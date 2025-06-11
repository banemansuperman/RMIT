library(shiny)
library(tidyverse)
library(lubridate)
library(plotly)
library(ggplot2)
library(ggrepel)
library(DT)

# Load and clean data
cci_data <- read.csv("OECD_T.csv", skip = 2)
colnames(cci_data)[1] <- "Date"
cci_data$Date <- as.Date(sub(" .*", "", cci_data$Date))
cci_data <- cci_data %>% mutate(across(-Date, ~ as.numeric(as.character(.))))
# Clean missing values
cci_clean <- cci_data %>%
  select(where(~ sum(is.na(.)) / nrow(cci_data) < 0.05))
# Pivot to long format
cci_long <- cci_clean %>%
  pivot_longer(cols = -Date, names_to = "Country", values_to = "CCI")
# UI
ui <- fluidPage(
  titlePanel("From Crisis to Confidence: How Consumer Confidence Shapes Global Economies (2008–2024)"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Why Consumer Confidence Matters"),
      p("Consumer confidence drives spending, saving, and borrowing. When confidence collapses as in 2008 it triggers recessions, austerity, and even political upheavals like Brexit."),
      
      p(strong("Key Economic Events:"), 
        br(), "- 2008: Lehman Brothers Collapse",
        br(), "- 2020: Pandemic Start"),
      
      selectInput("selected_countries", "Select Country/Countries:", 
                  choices = sort(unique(cci_long$Country)),
                  selected = c("Australia", "United.States", "Germany"), multiple = TRUE),
      
      dateRangeInput("date_range", "Select Date Range:",
                     start = min(cci_long$Date), end = max(cci_long$Date),
                     min = min(cci_long$Date), max = max(cci_long$Date)),
      
      sliderInput("cci_range", "Select CCI Range:",
                  min = floor(min(cci_long$CCI, na.rm = TRUE)),
                  max = ceiling(max(cci_long$CCI, na.rm = TRUE)),
                  value = c(95, 105), step = 1)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Consumer Confidence Trends",
                 plotlyOutput("cciPlot")
        ),
        tabPanel("Recovery Comparison",
                 plotlyOutput("recoveryPlot")
        ),
        tabPanel("Latest CCI Values",
                 DTOutput("dataTable")
        ),
        tabPanel("References",
                 HTML("<ul>
                       <li><a href='https://www.oecd.org/en/data/indicators/consumer-confidence-index-cci.html'  target='_blank'>OECD</a>: Consumer Confidence Index Data</li>
                       <li><a href='https://www.economicsobservatory.com/why-did-the-global-financial-crisis-of-2007-09-happen'  target='_blank'>Turner, J. (2023)</a>: Why did the global financial crisis happen?</li>
                       </ul>")
        )
      )
    )
  )
)

# Server logic
server <- function(input, output) {
  filtered_data <- reactive({
    req(input$selected_countries)
    cci_long %>%
      filter(
        Country %in% input$selected_countries,
        Date >= input$date_range[1],
        Date <= input$date_range[2],
        CCI >= input$cci_range[1],
        CCI <= input$cci_range[2]
      )
  })
  
  # Plot 1 CCI Trend
  output$cciPlot <- renderPlotly({
    p <- ggplot(filtered_data(), aes(x = Date, y = CCI, color = Country)) +
      geom_line(size = 1) +
      theme_minimal() +
    theme(
      plot.title = element_text(face = "bold", size = 16),
      plot.subtitle = element_text(size = 12, color = "gray40"),
      axis.title = element_text(size = 12),
      axis.text = element_text(size = 10),
      panel.grid.minor.y = element_line(color = "gray95"),
      panel.grid.major.y = element_line(color = "gray90"),
      legend.position = "bottom",
      legend.title = element_blank(),
      legend.spacing = unit(0.1, "cm"),
      legend.key.size = unit(0.3, "cm")
    ) +
      labs(title = "Consumer Confidence Index (CCI)", subtitle = "Long-term average = 100",
           x = "Year", y = "CCI", color = "Country") +
      theme(legend.position = "bottom")+
      geom_hline(yintercept = 100, linetype = "dotted", color = "gray50")
    ggplotly(p)
  })

  # Plot 2 Recovery comparison (2008 vs 2024)
  output$recoveryPlot <- renderPlotly({
    all_dates <- sort(unique(cci_long$Date))
    baseline_date <- min(all_dates)  
    post_crisis_date <- max(all_dates)  
    selected_countries <- req(input$selected_countries)
    baseline <- cci_long %>%
      filter(Date == baseline_date, Country %in% selected_countries) %>%
      rename(CCI_baseline = CCI)
    post_crisis <- cci_long %>%
      filter(Date == post_crisis_date, Country %in% selected_countries) %>%
      rename(CCI_post = CCI)
    
    # Join on Country and remove missing values
    recovery_data <- inner_join(baseline, post_crisis, by = "Country") %>%
      filter(!is.na(CCI_baseline), !is.na(CCI_post)) %>%
      mutate(Recovery_Rate = (CCI_post - CCI_baseline) / CCI_baseline * 100) %>%
      arrange(desc(Recovery_Rate))
    
    recovery_data <- recovery_data %>%
      mutate(Recovery_Rate = round(Recovery_Rate, 1))
    print(paste("Baseline Date:", baseline_date))
    print(paste("Post-Crisis Date:", post_crisis_date))
    print(paste("Number of matched countries:", nrow(recovery_data)))
    if (nrow(recovery_data) > 0) {
      plot_ly(recovery_data, x = ~Country, y = ~Recovery_Rate, type = "bar", color = ~sign(Recovery_Rate)) %>%
        layout(title = paste("Percentage Change in CCI (", format(baseline_date, "%Y"), " vs ", format(post_crisis_date, "%Y"), ")", sep = ""),
               xaxis = list(title = "Country"),
               yaxis = list(title = "Change (%)"),
               showlegend = FALSE)
    } else {
      plot_ly() %>%
        add_markers(x = 0, y = 0, text = "No matching country data found for selected dates.")
    }
  })
  
  output$dataTable <- renderDT({
    cci_long %>%
      group_by(Country) %>%
      filter(Date == max(Date)) %>%
      ungroup() %>%
      select(Country, Date, CCI)
  }, options = list(pageLength = 10))
}

shinyApp(ui = ui, server = server)
