#on line 518 add created by Estelle Ou
library(tidyverse)
library(lubridate)
library(shiny)
library(zoo)
library(gridExtra)
library(wesanderson)
library(viridis)
library(grid)
library(directlabels)
library(RColorBrewer)
library(shinythemes)
library(ggthemes)


#loading country classifications

source("country_classification.R")
source("estelle_theme.R")

#loading in functions for manupulating data
source("functions.R")


#reactive output ---------------------------------------------------------

server <- function(input, output) {
  
  #load updated data
  #reading in latest cleaned covid data separately to speed up the process
  cntry_cleaned_mobility_2022_onwards <-
    read_csv(url("https://raw.githubusercontent.com/estelleou/covid_monitoring_app/main/cleaned_data/cntry_cleaned_mobility_2022_onwards.csv?token=GHSAT0AAAAAABSOFOUAUTFAHTOVWB4OMKEAYTD53BQ"))
  
  cntry_cleaned_covid_2022_onwards <- 
    read_csv(url("https://raw.githubusercontent.com/estelleou/covid_monitoring_app/main/cleaned_data/cntry_cleaned_covid_2022_onward.csv"))
  
  increases_in_deaths_and_cases_within_this_week <-  
    read_csv(url("https://raw.githubusercontent.com/estelleou/covid_monitoring_app/main/cleaned_data/increases_in_deaths_and_cases_within_this_week"))
  
  mobility_and_case_speed_data_2022_onwards <-  
    read_csv(url("https://raw.githubusercontent.com/estelleou/covid_monitoring_app/main/cleaned_data/mobility_and_case_speed_2022_onwards.csv"))
  
  region_covid_data <- 
    read_csv(url("https://raw.githubusercontent.com/estelleou/covid_monitoring_app/main/cleaned_data/region_covid_data.csv"))
  
  mobility_and_case_speed_data <- 
    read_csv(url("https://raw.githubusercontent.com/estelleou/covid_monitoring_app/main/cleaned_data/mobility_and_case_speed_data.csv"))

  #hotspots charts ---------------------------------------------------------  
  
  #cache all the graphs to optimize refresh rate --------------------------
  em_case_hotspot <- reactive({
    
    cases_hotspot_visuals( increases_in_deaths_and_cases_within_this_week, em) +
      labs(title = "EM Covid Case Hotspots")
  }) 
  
  dm_case_hotspot <- reactive({
    cases_hotspot_visuals( increases_in_deaths_and_cases_within_this_week, dm) +
      labs(title = "DM Covid Case Hotpots")
  }) 
  
  eu_case_hotspot <-reactive({
    cases_hotspot_visuals( increases_in_deaths_and_cases_within_this_week, eu) +
      labs(title = "EU Covid Case Hotspots")
  }) 
  
  asia_case_hotspot <-reactive({
    cases_hotspot_visuals( increases_in_deaths_and_cases_within_this_week, asia)+
      labs(title = "Asia Covid Case Hotspots")
  }) 
  
  latam_case_hotspot <-reactive({
    cases_hotspot_visuals( increases_in_deaths_and_cases_within_this_week, latam) +
      labs(title = "Latam Covid Case Hotspots")
  }) 
  
  global_case_hotspot <-reactive({
    cases_hotspot_visuals( increases_in_deaths_and_cases_within_this_week, "global") +
      labs(title = "Top 20 Global Covid Case Hotspots")
  }) 
  
  em_death_hotspot <-reactive({
    death_hotspot_visuals( increases_in_deaths_and_cases_within_this_week, em) +
      labs(title = "EM Covid Death Hotspots")
  }) 
  
  dm_death_hotspot <-reactive({
    death_hotspot_visuals( increases_in_deaths_and_cases_within_this_week, dm) +
      labs(title = "DM Covid Death Hotpots")
  }) 
  
  eu_death_hotspot <-reactive({
    death_hotspot_visuals( increases_in_deaths_and_cases_within_this_week, eu) +
      labs(title = "EU Covid Death Hotspots")
  }) 
  
  asia_death_hotspot <-reactive({
    death_hotspot_visuals( increases_in_deaths_and_cases_within_this_week, asia)+
      labs(title = "Asia Covid Death Hotspots")
  }) 
  
  latam_death_hotspot <-reactive({
    death_hotspot_visuals( increases_in_deaths_and_cases_within_this_week, latam) +
      labs(title = "Latam Covid Death Hotspots")
  }) 
  
  global_death_hotspot <-reactive({
    
    death_hotspot_visuals( increases_in_deaths_and_cases_within_this_week, "global") +
      labs(title = "Top 20 Global Covid Death Hotspots")
    
  })
  
  
  output$hotspots <- renderPlot({
    
    
    if (input$type =="deaths") {
      
      grid.arrange(em_death_hotspot(), dm_death_hotspot(), eu_death_hotspot(), asia_death_hotspot(), 
                   latam_death_hotspot(), global_death_hotspot(),
                   nrow = 2)
      
    } else { 
      
      
      grid.arrange(em_case_hotspot(), dm_case_hotspot(), eu_case_hotspot(), asia_case_hotspot(), 
                   latam_case_hotspot(), global_case_hotspot(),
                   nrow = 2)
      
    }
    
  })
  
  
  #regional charts ----------------------------------------------------------
  last_region <- reactive({
    region_covid_data %>%
      filter(!is.na(continent)) %>%
      filter(continent != "Oceania") %>%
      filter(date == today()-1)
  })
  
  regional_covid_cases <- reactive({

  
    region_covid_data %>%
      select(date,continent, new_cases_avg_per_pop) %>%
      mutate(continent = (factor(continent, levels = c("North America", "Europe", "South America", "Asia","Africa", "Oceania")))) %>%
      filter(continent != "Oceania") %>%
      ggplot() +
      geom_line(aes(x = date, y = new_cases_avg_per_pop, color = continent), size = 1.2) +
      geom_dl(data = last_region(),
              aes(x = date, y = new_cases_avg_per_pop, color = continent, label = continent),
              method = list('last.bumpup', cex = 1.2, hjust = 0, 
                            vjust = 1, fontface = "bold"))+
      labs(x = "", y = "",
           title= "Daily New Covid Cases per million (7-day avg.)",
           subtitle = paste0("As of ", today()-1)) +
      scale_x_date(lim = c(as.Date("2020-02-15"),
                           as.Date(max(last_region()$date))),
                   date_label = "%b %y",
                   date_breaks = "3 months" ) +
      estelle_theme()+
      scale_color_manual(values = c( "#0099cc", "#778088", "#832e31", 
                                     "#cc9900", "#006633"))+
      theme(plot.margin = margin(0.1, 3, 0.1, 0.1, "cm"),
            plot.title = element_text(size = 15, vjust = -1, color = "black"),
            plot.subtitle = element_text(size = 15),
            legend.position = "none")+
      coord_cartesian(clip = "off")
    
  })
  
  regional_covid_deaths <- reactive({
    
    region_covid_data %>%
      select(date,continent, new_deaths_avg_per_pop) %>%
      mutate(continent = (factor(continent, levels = c("North America", "Europe", "South America", "Asia","Africa", "Oceania")))) %>%
      filter(continent != "Oceania") %>%
      ggplot() +
      geom_line(aes(x = date, y = new_deaths_avg_per_pop, color = continent), size = 1.2) +
      geom_dl(data = last_region(),
              aes(x = date, y = new_deaths_avg_per_pop, color = continent, label = continent),
              method = list('last.bumpup', cex = 1.2, hjust = 0, 
                            vjust = 1, fontface = "bold"))+
      labs(x = "", y = "",
           title= "Daily New Covid Deaths per million (7-day avg.)",
           subtitle = paste0("As of ", today()-1)) +
      scale_x_date(lim = c(as.Date("2020-02-15"),
                           as.Date(max(last_region()$date))),
                   date_label = "%b %y",
                   date_breaks = "3 months" ) +
      estelle_theme()+
      scale_color_manual(values = c( "#0099cc", "#778088", "#832e31", 
                                     "#cc9900", "#006633"))+
      theme(plot.margin = margin(0.1, 3, 0.1, 0.1, "cm"),
            plot.title = element_text(size = 15, vjust = -1, color = "black"),
            plot.subtitle = element_text(size = 15),
            legend.position = "none")+
      coord_cartesian(clip = "off")
    
  })
  
  output$regional_ts <- renderPlot({
    
    grid.arrange(regional_covid_cases(), regional_covid_deaths(),
                 nrow = 1)
    
  })
  
  
  #country covid case and death ranking bars by region ------------------------
  
  output$covid_rankings <- renderPlot({
    

    
    if (input$region =="em") {
      
      grid.arrange(top_5_country_ranked_by_cases(cntry_cleaned_covid_2022_onwards, em), top_5_country_ranked_by_deaths(cntry_cleaned_covid_2022_onwards, em),
                   nrow = 1)
      
    } else if (input$region == "dm") {
      
      grid.arrange(top_5_country_ranked_by_cases(cntry_cleaned_covid_2022_onwards, dm), top_5_country_ranked_by_deaths(cntry_cleaned_covid_2022_onwards, dm),
                   nrow = 1)
      
    } else if (input$region == "latam") {
      
      grid.arrange(top_5_country_ranked_by_cases(cntry_cleaned_covid_2022_onwards, latam), top_5_country_ranked_by_deaths(cntry_cleaned_covid_2022_onwards, latam),
                   nrow = 1)
    } else if (input$region == "asia") {
      
      grid.arrange(top_5_country_ranked_by_cases(cntry_cleaned_covid_2022_onwards, asia), top_5_country_ranked_by_deaths(cntry_cleaned_covid_2022_onwards, asia),
                   nrow = 1)
      
    } else {
      
      grid.arrange(top_5_country_ranked_by_cases(cntry_cleaned_covid_2022_onwards, eu), top_5_country_ranked_by_deaths(cntry_cleaned_covid_2022_onwards, eu),
                   nrow = 1)
      
    }
    
  })
  
  
  #mobility average charts
  

  
  output$mobility_throughout_the_years <-  renderPlot({
    
    #reference values for y-axis on mobility charts
    y_axis_min_max_reference_data_full <- 
      cntry_cleaned_mobility %>%
      filter(country == input$country) %>% 
      filter(!is.na(roll_index)) %>% 
      mutate(roll_index = 100+roll_index) %>% 
      select(roll_index)
    
    y_axis_min_max_reference_data <- 
      cntry_cleaned_mobility_2022_onwards %>%
      filter(country == input$country) %>% 
      filter(!is.na(roll_index)) %>% 
      mutate(roll_index = 100+roll_index) %>% 
      select(roll_index)
  
    
    year_1 <- mobility_case_chart(mobility_and_case_speed_data %>%
                                    filter(country == input$country) %>% 
                                    filter(date < "2021-01-01"), input$country) +
      labs(caption = "@ou_estelle") +
      scale_y_continuous(lim = c(min(y_axis_min_max_reference_data_full$roll_index), 
                                 ifelse(max(y_axis_min_max_reference_data_full$roll_index)>100,
                                        max(y_axis_min_max_reference_data_full$roll_index), 101)))
    
    year_2 <- mobility_case_chart(mobility_and_case_speed_data%>%
                                    filter(country == input$country) %>% 
                                    filter(date >= "2021-01-01") %>% 
                                    filter(date < "2022-01-01"), input$country) +
      scale_y_continuous(lim = c(min(y_axis_min_max_reference_data_full$roll_index), 
                                 ifelse(max(y_axis_min_max_reference_data_full$roll_index)>100,
                                        max(y_axis_min_max_reference_data_full$roll_index), 101)))
    
    year_3<- mobility_case_chart(mobility_and_case_speed_data_2022_onwards %>%
                                   filter(country == input$country) %>% 
                                   filter(date >= "2022-01-01") %>% 
                                   filter(date < "2023-01-01"), input$country) +
      labs(
        title = "Red refers to when average daily covid cases are rising", 
        subtitle = "Grey refers to when average daily covid cases are falling") +
      scale_y_continuous(lim = c(min(y_axis_min_max_reference_data_full$roll_index), 
                                 ifelse(max(y_axis_min_max_reference_data$roll_index)>100,
                                        max(y_axis_min_max_reference_data$roll_index), 101)))+
      theme(plot.margin = margin(0.2, 0.5, 0.2, 0.5, "cm"), 
            plot.title = element_text(size = 13, vjust = -1, color = "#993333", 
                                      face = "bold",
                                      margin = margin(0,0,0.3,0, "cm")),
            plot.subtitle = element_text(size = 13, vjust = -1, color = "#778088",
                                         face = "bold",
                                         margin = margin(0,0,0.2,0, "cm")))
    
    
    weekday <-weekday_mobility(cntry_cleaned_mobility_2022_onwards %>%
                                 filter(country == input$country))
    
    weekend <-weekend_mobility(cntry_cleaned_mobility_2022_onwards %>%
                                 filter(country == input$country))
    
    
    grid.arrange(year_3, year_2, year_1, weekday, weekend, 
                 layout_matrix = rbind(c(1,  4, 5),
                                       c(2,  4, 5),
                                       c(3,  NA, NA)),
                 top = textGrob(paste0( "Average Mobility* (% of Jan-Feb 2020 levels) as of ", format(max(cntry_cleaned_mobility_2022_onwards$date),
                                                                                                      "%B-%d")),
                                gp = gpar(fontface = 4, fontsize =15),
                                x = 0.2),
                 bottom = textGrob(
                   "*Average Mobility in Recreational, Retail, Transit and Office Spaces with (+) residential activity calculated as a drag on overall mobility \nSource: JHU and Google Mobility Data",
                   gp = gpar(fontface = 3, fontsize = 11), 
                   x = 0.7
                 ))
    
  })
  
  
}

#generate dropdown selection for average mobility charts    
cntry_cleaned_mobility <- read_csv("cleaned_data/cntry_cleaned_mobility.csv")
country_list <- as.list(unique(cntry_cleaned_mobility$country))

#start of UI file 
ui <-  shinyUI(fluidPage(

  
  theme = shinythemes::shinytheme("flatly"),
  
  # App title ----
  h1(id = "big heading", "Covid Macro Monitor", style =  "font-size: 100%"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input:  ----
      tags$style(type='text/css', ".selectize-input { font-size: 100%; line-height: 75%;} 
                 .selectize-dropdown { font-size: 100%; line-height: 75%; }
                 .label {font-size: 100%; line-height: 75%"),
      selectInput(inputId = "type",
                  label = "Covid Metric",
                  list(`Cases` = "cases", `Death` = "deaths")
      ),
      width = 2
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Hotspots and Regional----
      h1("Covid Hotspots For the Week In Red: ", style ="font-size: 100%;" ),
      plotOutput(outputId = "hotspots", width = "100%", height = "850px"),
      hr(),
      h1("Regional Covid Case and Death Levels", style ="font-size: 100%;" ),
      plotOutput(outputId = "regional_ts", width = "100%", height = "650px"),
      width = 10,
    )
  ),
  
  #Country Level Cases and Death Charts ---------------------------------
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: options----
      selectInput(inputId = "region",
                  label = "Pick A Country Grouping",
                  list(`Emerging Markets` = "em", `Developed Markets` = "dm",
                       `Latin America` = "latam", `European Union` = "eu",
                       `Asia` = "asia")
      ),
      width = 2,
      height = 1
      
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Country level by region----
      hr(),
      h1("Top 5 Countries with the Highest Cases/Deaths Per Capita within Country Grouping", style ="font-size: 100%;" ),
      plotOutput(outputId = "covid_rankings", width = "100%", height = "550px"),
      width = 10,
    )
  ),
  
  
  # # #REgional Average mobility charts ------------------------------------
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      selectInput(inputId = "country",
                  label = "Pick A Country",
                  country_list
      ),
      width = 2,
      
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Country level by region----
      hr(),
      h1("Mobility Indicators", style ="font-size: 100%;" ),
      plotOutput(outputId = "mobility_throughout_the_years", width = "100%",height = "800px"),
      width = 10,
    )
  )
  
))

# Create Shiny app ----
shinyApp(ui, server)


