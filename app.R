# Load packages
library(shiny)
library(ggplot2)
library(dplyr)
library(usmap)
library(tidyr)

# Read in dataset
kansas_population_data <- read.csv("/Users/chenxi/KansasPopulation/KansasPopulation/KansasPopulation.csv")
# Remove rows where County is "Kansas"
kansas_population_data <- kansas_population_data[kansas_population_data$County != "Kansas", ]
# Change to wide format
kansas_population_data_wide <- pivot_wider(kansas_population_data, names_from = Year, values_from = Population)

# calculate year difference
kansas_population_data_wide$"11-10" <- kansas_population_data_wide$"2011" - kansas_population_data_wide$"2010"
kansas_population_data_wide$"12-10" <- kansas_population_data_wide$"2012" - kansas_population_data_wide$"2010"
kansas_population_data_wide$"13-10" <- kansas_population_data_wide$"2013" - kansas_population_data_wide$"2010"
kansas_population_data_wide$"14-10" <- kansas_population_data_wide$"2014" - kansas_population_data_wide$"2010"
kansas_population_data_wide$"15-10" <- kansas_population_data_wide$"2015" - kansas_population_data_wide$"2010"
kansas_population_data_wide$"16-10" <- kansas_population_data_wide$"2016" - kansas_population_data_wide$"2010"
kansas_population_data_wide$"17-10" <- kansas_population_data_wide$"2017" - kansas_population_data_wide$"2010"
kansas_population_data_wide$"18-10" <- kansas_population_data_wide$"2018" - kansas_population_data_wide$"2010"
kansas_population_data_wide$"12-11" <- kansas_population_data_wide$"2012" - kansas_population_data_wide$"2011"
kansas_population_data_wide$"13-11" <- kansas_population_data_wide$"2013" - kansas_population_data_wide$"2011"
kansas_population_data_wide$"14-11" <- kansas_population_data_wide$"2014" - kansas_population_data_wide$"2011"
kansas_population_data_wide$"15-11" <- kansas_population_data_wide$"2015" - kansas_population_data_wide$"2011"
kansas_population_data_wide$"16-11" <- kansas_population_data_wide$"2016" - kansas_population_data_wide$"2011"
kansas_population_data_wide$"17-11" <- kansas_population_data_wide$"2017" - kansas_population_data_wide$"2011"
kansas_population_data_wide$"18-11" <- kansas_population_data_wide$"2018" - kansas_population_data_wide$"2011"
kansas_population_data_wide$"13-12" <- kansas_population_data_wide$"2013" - kansas_population_data_wide$"2012"
kansas_population_data_wide$"14-12" <- kansas_population_data_wide$"2014" - kansas_population_data_wide$"2012"
kansas_population_data_wide$"15-12" <- kansas_population_data_wide$"2015" - kansas_population_data_wide$"2012"
kansas_population_data_wide$"16-12" <- kansas_population_data_wide$"2016" - kansas_population_data_wide$"2012"
kansas_population_data_wide$"17-12" <- kansas_population_data_wide$"2017" - kansas_population_data_wide$"2012"
kansas_population_data_wide$"18-12" <- kansas_population_data_wide$"2018" - kansas_population_data_wide$"2012"
kansas_population_data_wide$"14-13" <- kansas_population_data_wide$"2014" - kansas_population_data_wide$"2013"
kansas_population_data_wide$"15-13" <- kansas_population_data_wide$"2015" - kansas_population_data_wide$"2013"
kansas_population_data_wide$"16-13" <- kansas_population_data_wide$"2016" - kansas_population_data_wide$"2013"
kansas_population_data_wide$"17-13" <- kansas_population_data_wide$"2017" - kansas_population_data_wide$"2013"
kansas_population_data_wide$"18-13" <- kansas_population_data_wide$"2018" - kansas_population_data_wide$"2013"
kansas_population_data_wide$"15-14" <- kansas_population_data_wide$"2015" - kansas_population_data_wide$"2014"
kansas_population_data_wide$"16-14" <- kansas_population_data_wide$"2016" - kansas_population_data_wide$"2014"
kansas_population_data_wide$"17-14" <- kansas_population_data_wide$"2017" - kansas_population_data_wide$"2014"
kansas_population_data_wide$"18-14" <- kansas_population_data_wide$"2018" - kansas_population_data_wide$"2014"
kansas_population_data_wide$"16-15" <- kansas_population_data_wide$"2016" - kansas_population_data_wide$"2015"
kansas_population_data_wide$"17-15" <- kansas_population_data_wide$"2017" - kansas_population_data_wide$"2015"
kansas_population_data_wide$"18-15" <- kansas_population_data_wide$"2018" - kansas_population_data_wide$"2015"
kansas_population_data_wide$"17-16" <- kansas_population_data_wide$"2017" - kansas_population_data_wide$"2016"
kansas_population_data_wide$"18-16" <- kansas_population_data_wide$"2018" - kansas_population_data_wide$"2016"
kansas_population_data_wide$"18-17" <- kansas_population_data_wide$"2018" - kansas_population_data_wide$"2017"

# Create a categorical variable based on conditions
kansas_population_data_wide$"11-10" <- cut(kansas_population_data_wide$"11-10", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"12-10" <- cut(kansas_population_data_wide$"12-10", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"13-10" <- cut(kansas_population_data_wide$"13-10", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"14-10" <- cut(kansas_population_data_wide$"14-10", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"15-10" <- cut(kansas_population_data_wide$"15-10", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"16-10" <- cut(kansas_population_data_wide$"16-10", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"17-10" <- cut(kansas_population_data_wide$"17-10", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"18-10" <- cut(kansas_population_data_wide$"18-10", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"12-11" <- cut(kansas_population_data_wide$"12-11", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"13-11" <- cut(kansas_population_data_wide$"13-11", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"14-11" <- cut(kansas_population_data_wide$"14-11", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"15-11" <- cut(kansas_population_data_wide$"15-11", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"16-11" <- cut(kansas_population_data_wide$"16-11", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"17-11" <- cut(kansas_population_data_wide$"17-11", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"18-11" <- cut(kansas_population_data_wide$"18-11", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"13-12" <- cut(kansas_population_data_wide$"13-12", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"14-12" <- cut(kansas_population_data_wide$"14-12", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"15-12" <- cut(kansas_population_data_wide$"15-12", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"16-12" <- cut(kansas_population_data_wide$"16-12", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"17-12" <- cut(kansas_population_data_wide$"17-12", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"18-12" <- cut(kansas_population_data_wide$"18-12", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"14-13" <- cut(kansas_population_data_wide$"14-13", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"15-13" <- cut(kansas_population_data_wide$"15-13", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"16-13" <- cut(kansas_population_data_wide$"16-13", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"17-13" <- cut(kansas_population_data_wide$"17-13", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"18-13" <- cut(kansas_population_data_wide$"18-13", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"15-14" <- cut(kansas_population_data_wide$"15-14", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"16-14" <- cut(kansas_population_data_wide$"16-14", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"17-14" <- cut(kansas_population_data_wide$"17-14", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"18-14" <- cut(kansas_population_data_wide$"18-14", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"16-15" <- cut(kansas_population_data_wide$"16-15", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"17-15" <- cut(kansas_population_data_wide$"17-15", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"18-15" <- cut(kansas_population_data_wide$"18-15", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"17-16" <- cut(kansas_population_data_wide$"17-16", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"18-16" <- cut(kansas_population_data_wide$"18-16", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))
kansas_population_data_wide$"18-17" <- cut(kansas_population_data_wide$"18-17", 
                                           breaks = c(-Inf, 0, Inf),
                                           labels = c("Decrease", "Increase"))


# Define the UI
ui <- fluidPage(
  titlePanel("Kansas Population Viewer"),
  tabsetPanel(
    tabPanel("Charts", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("chart_type", "Select Chart Type", choices = c("Bar Chart", "Line Chart", "Pie Chart")),
                 conditionalPanel(
                   condition = "input.chart_type == 'Bar Chart'",
                   selectInput("year", "Select Year", choices = c("ALL", unique(kansas_population_data$Year))),
                   sliderInput("top_n", "Select Top N Counties", min = 3, max = 10, value = 5),
                   actionButton("update", "Update Plot")
                 ),
                 conditionalPanel(
                   condition = "input.chart_type == 'Line Chart'",
                   selectInput("county_line_chart", "Select County", 
                               choices = unique(kansas_population_data$County)),
                   checkboxInput("add_mean_line", "Show Average Population", value = FALSE),
                   actionButton("update_line_chart", "Update Plot")
                 ),
                 conditionalPanel(
                   condition = "input.chart_type == 'Pie Chart'",
                   selectInput("pie_year", "Select Year", choices = unique(kansas_population_data$Year)),
                   actionButton("update_pie_chart", "Update Plot")
                 )
               ),
               mainPanel(
                 plotOutput("population_plot")
               )
             )),
    

    tabPanel("Heatmaps", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("heatmap_type", "Select Heatmap Type", choices = c("Population Heatmap", "Population Trend Heatmap"), selected = "Population Heatmap"),
                 conditionalPanel(
                   condition = "input.heatmap_type == 'Population Heatmap'",
                   selectInput("heatmap_year", "Select Year", choices = as.character(2010:2018), selected = "2010"),
                 ),
                 conditionalPanel(
                   condition = "input.heatmap_type == 'Population Trend Heatmap'",
                   selectInput("heatmap_year1", "Select Year from", choices = as.character(2010:2018), selected = "2010"),
                   selectInput("heatmap_year2", "to", choices = as.character(2011:2018), selected = "2011")
                 )
               ),
               mainPanel(
                 uiOutput("heatmap_output")
               )
             ))
  )
)

# Define the server
server <- function(input, output, session) {
  filtered_data <- reactive({
    if (input$year == "ALL") {
      kansas_population_data_all <- kansas_population_data
      kansas_population_data_all$Year <- as.factor(kansas_population_data_all$Year)
      sorted_data <- kansas_population_data_all[order(kansas_population_data_all$Year, -kansas_population_data_all$Population), ]
      top_n_data <- sorted_data[ave(1:nrow(sorted_data), sorted_data$Year, FUN = seq_along) <= input$top_n, ]
      return(top_n_data)
    } else {
      filter_data <- kansas_population_data[kansas_population_data$Year == input$year, ]
      top_n_data <- head(filter_data[order(-filter_data$Population), ], input$top_n)
      return(top_n_data)
    }
  })
  
  output$population_plot <- renderPlot({
    if (input$chart_type == "Bar Chart") {
      if (input$year == "ALL") {
        ggplot(filtered_data(), aes(x = reorder(County, -Population), y = Population, fill = Year)) +
          geom_bar(stat = "identity", position = "dodge") +
          labs(title = paste("Top", input$top_n, "Counties by Population (All Years)"),
               x = "County",
               y = "Population") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                plot.title = element_text(face = "bold", size = 16),
                axis.title = element_text(face = "bold", size = 14),
                axis.text = element_text(size = 12)) +
          scale_y_continuous(labels = scales::comma_format()) +
          scale_fill_brewer(palette = "Set3")
      } else {
        ggplot(filtered_data(), aes(x = reorder(County, -Population), y = Population)) +
          geom_bar(stat = "identity", position = "dodge", fill = "skyblue") +
          labs(title = paste("Top", input$top_n, "Counties by Population in", input$year),
               x = "County",
               y = "Population") +
          theme(axis.text.x = element_text(angle = 45, hjust = 1),
                plot.title = element_text(face = "bold", size = 16),
                axis.title = element_text(face = "bold", size = 14),
                axis.text = element_text(size = 12)) +
          scale_y_continuous(labels = scales::comma_format()) +
          coord_cartesian(ylim = c(0, max(kansas_population_data$Population)))
      }
    } else if (input$chart_type == "Line Chart") {
      selected_data <- kansas_population_data %>% filter(County == input$county_line_chart)
      mean_data <- kansas_population_data %>%
        group_by(Year) %>%
        summarise(mean_population = mean(Population))
      
      max_y_value <- ifelse(input$add_mean_line, max(c(selected_data$Population, mean_data$mean_population)), max(selected_data$Population))
      min_y_value <- ifelse(input$add_mean_line, min(c(selected_data$Population, mean_data$mean_population)), min(selected_data$Population))
      
      ggplot(selected_data, aes(x = Year, y = Population, group = 1)) +
        geom_line(color = "steelblue", size = 1.5) +
        geom_point(color = "steelblue", size = 3) +
        labs(title = paste("Population trend for", input$county_line_chart),
             x = "Year",
             y = "Population") +
        theme_minimal() +
        theme(plot.title = element_text(face = "bold", size = 16),
              axis.title = element_text(face = "bold", size = 14),
              axis.text = element_text(size = 12),
              legend.position = "none") +
        geom_line(data = mean_data, aes(x = Year, y = mean_population),
                  linetype = "dashed", color = "red", 
                  alpha = ifelse(input$add_mean_line, 1, 0)) +
        guides(fill=FALSE) +  
        ylim(min_y_value, max_y_value)  
    } else {
      if (input$pie_year != "") {
        pie_filtered_data <- filter(kansas_population_data, Year == input$pie_year) %>%
          arrange(desc(Population))
        pie_data <- data.frame(
          County = c(head(pie_filtered_data$County, 5), "Others"),
          Population = c(head(pie_filtered_data$Population, 5), sum(tail(pie_filtered_data$Population, -5)))
        )
        pie_chart <- ggplot(pie_data, aes(x = "", y = Population, fill = County)) +
          geom_bar(stat = "identity", width = 1, color = "white") +
          coord_polar("y") +
          scale_fill_brewer(palette = "Set3") +
          theme_void() +
          theme(plot.title = element_text(face = "bold", size = 16),
                legend.position = "bottom") +
          labs(title = paste("Population Proportion by County in", input$pie_year)) +
          guides(fill = guide_legend(reverse = TRUE))  
        
        print(pie_chart)
      }
    }
  })
  
  output$heatmap_output <- renderUI({
    if (input$heatmap_type == "Population Heatmap") {
      plotOutput("heatmap1")
    } else {
      plotOutput("heatmap2")
    }
  })
  
  output$heatmap1 <- renderPlot({
    plot_usmap(
      regions = "counties",
      include = "KS",
      data = kansas_population_data_wide,
      values = input$heatmap_year,
      color = "black"
    ) +
      scale_fill_continuous(low = "white", high = "red", labels = scales::label_number()) +
      labs(
        title = paste("Population Heatmap -", input$heatmap_year),
        subtitle = "The population heatmap of the counties in Kansas."
      ) +
      theme(
        panel.background = element_rect(color = "black", fill = "lightblue"),
        legend.position = "right",
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 14),
        legend.text = element_text(size = 12),  
        legend.title = element_text(size = 14)
      )
  })
  
  observe({
    updateSelectInput(session, "heatmap_year2", choices = as.character((as.numeric(input$heatmap_year1) + 1):2018))
  })
  
  selected_heatmap_year <- reactive({
    paste0(substr(input$heatmap_year2, 3, 4), "-", substr(input$heatmap_year1, 3, 4))
  })
  
  output$heatmap2 <- renderPlot({
    plot_usmap(
      regions = "counties", include = "KS", 
      data = kansas_population_data_wide, values = selected_heatmap_year(),
      color = "black"
    ) + 
      scale_fill_manual(
        values = c("Decrease" = "forestgreen", "Increase" = "red"),
        labels = c("Decrease", "Increase")
      ) +
      labs(
        title = "Population Trend Heatmap",
        subtitle = paste("The population trend of the counties between", input$heatmap_year1, "and", input$heatmap_year2, "in Kansas.")
      ) + 
      theme(
        panel.background = element_rect(color = "black", fill = "lightblue"), 
        legend.position = "right",
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 14),
        legend.text = element_text(size = 12),  
        legend.title = element_text(size = 14)
      )
  })
}

# Run the app
shinyApp(ui = ui, server = server)
