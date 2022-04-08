#on line 518 add created by Estelle Ou
wd <- "D:/Estelle/Rscripts/covid_monitoring_app"

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

source("D:/Estelle/Rscripts/estelle_theme.R")

##!!!!!!!!!!! put this on a cron when done !!!!#######
source("D:/Estelle/Rscripts/covid_monitoring_app/data_cleaning.R")

#load in cleaned data that's updated once a day on a cron-----------------------

load("cleaned_data/cntry_cleaned_covid.rda")
load("cleaned_data/cntry_cleaned_mobility.rda")
load("cleaned_data/increases_in_deaths_and_cases_within_this_week.rda")
load("cleaned_data/region_covid_data.rda")

#loading country classifications

source("D:/Estelle/Rscripts/covid_monitoring/Rscripts/country_classification.R")

#loading in functions for manupulating data
source("functions.R")

#reactive output ---------------------------------------------------------

server <- function(input, output) {
  
#hotspots charts ---------------------------------------------------------  
  
  #cache all the graphs to optimize refresh rate --------------------------
  em_case_hotspot <- reactive({
    
    cases_hotspot_visuals(em) +
      labs(title = "EM Covid Case Hotspots")
    }) 
  
  dm_case_hotspot <- reactive({
    cases_hotspot_visuals(dm) +
      labs(title = "DM Covid Case Hotpots")
    }) 
  
  eu_case_hotspot <-reactive({
    cases_hotspot_visuals(eu) +
      labs(title = "EU Covid Case Hotspots")
    }) 
  
  asia_case_hotspot <-reactive({
    cases_hotspot_visuals(asia)+
      labs(title = "Asia Covid Case Hotspots")
    }) 
  
  latam_case_hotspot <-reactive({
    cases_hotspot_visuals(latam) +
      labs(title = "Latam Covid Case Hotspots")
    }) 
  
  global_case_hotspot <-reactive({
    cases_hotspot_visuals("global") +
      labs(title = "Top 20 Global Covid Case Hotspots")
    }) 
  
  em_death_hotspot <-reactive({
    death_hotspot_visuals(em) +
      labs(title = "EM Covid Death Hotspots")
    }) 
  
  dm_death_hotspot <-reactive({
    death_hotspot_visuals(dm) +
      labs(title = "DM Covid Death Hotpots")
    }) 
  
  eu_death_hotspot <-reactive({
    death_hotspot_visuals(eu) +
      labs(title = "EU Covid Death Hotspots")
    }) 
  
  asia_death_hotspot <-reactive({
    death_hotspot_visuals(asia)+
      labs(title = "Asia Covid Death Hotspots")
    }) 
  
  latam_death_hotspot <-reactive({
    death_hotspot_visuals(latam) +
      labs(title = "Latam Covid Death Hotspots")
    }) 
  
  global_death_hotspot <-reactive({
    
    death_hotspot_visuals("global") +
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
      filter(!is.na(continent)) %>%
      filter(date > "2020-01-31") %>%
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
            plot.title = element_text(size = 20, vjust = -1, color = "black"),
            plot.subtitle = element_text(size = 20),
            legend.position = "none")+
      coord_cartesian(clip = "off")

      })

  regional_covid_deaths <- reactive({

    region_covid_data %>%
      filter(!is.na(continent)) %>%
      filter(date > "2020-01-31") %>%
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
            plot.title = element_text(size = 20, vjust = -1, color = "black"),
            plot.subtitle = element_text(size = 20),
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

      grid.arrange(top_5_country_ranked_by_cases(em), top_5_country_ranked_by_deaths(em),
                   nrow = 1)

    } else if (input$region == "dm") {

      grid.arrange(top_5_country_ranked_by_cases(dm), top_5_country_ranked_by_deaths(dm),
                   nrow = 1)

    } else if (input$region == "latam") {

      grid.arrange(top_5_country_ranked_by_cases(latam), top_5_country_ranked_by_deaths(latam),
                   nrow = 1)
    } else if (input$region == "asia") {

      grid.arrange(top_5_country_ranked_by_cases(asia), top_5_country_ranked_by_deaths(asia),
                   nrow = 1)

    } else {

      grid.arrange(top_5_country_ranked_by_cases(eu), top_5_country_ranked_by_deaths(eu),
                   nrow = 1)

    }

  })
  
}
      

ui <- source("ui.R")
# Create Shiny app ----
shinyApp(ui = ui, server = server)
