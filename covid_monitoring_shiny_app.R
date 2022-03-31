#on line 518 add created by Estelle Ou
wd <- "D:/Estelle/Rscripts/covid_monitoring"

library(tidyverse)
library(lubridate)
library(shiny)
library(zoo)
library(gridExtra)
library(wesanderson)
library(viridis)
library(grid)
library(RColorBrewer)

source("D:/Estelle/Rscripts/estelle_theme.R")

#raw data ---------------------------------------------------------------------

#cases and vaccines
covid_data <- 
  read_csv(url("https://raw.githubusercontent.com/owid/covid-19-data/master/public/data/owid-covid-data.csv"))

#mobility data
urlfile="https://raw.githubusercontent.com/ActiveConclusion/COVID19_mobility/master/google_reports/mobility_report_countries.csv"
mobility<-read_csv(url(urlfile))

#data cleaning ---------------------------------------------------------------

#Mobility data cleaning 
cntry_cleaned_mobility <-
  #creating country-level mobility data
  mobility %>%
  #taking the country total numbers and getting rid of county/city-level data
  filter(region == "Total") %>%
  #this is your index calculation
  mutate(index = (`retail and recreation` + `transit stations` + workplaces - residential)/4) %>%
  group_by(country) %>%
  mutate(roll_index = rollmean(index, k=7, align = "right", fill = NA)) %>% 
  ungroup()

#COVID data cleaning 
cntry_cleaned_covid <-
  covid_data %>% 
  select(date, location, total_cases, new_cases, total_deaths, new_deaths, icu_patients, new_cases_per_million, new_deaths_per_million,
         hosp_patients, weekly_hosp_admissions, weekly_icu_admissions, total_tests,
         new_tests, total_vaccinations, people_vaccinated, new_vaccinations, people_fully_vaccinated,
         stringency_index, population) %>% 
  rename(country = location) %>% 
  #smoothing out by creating 7-day rolling average
  group_by(country) %>% 
  fill(new_cases, .direction = "down") %>%
  fill(new_deaths, .direction = "down") %>%
  fill(new_vaccinations, .direction = "down") %>%
  fill(people_fully_vaccinated, .direction = "down") %>% 
  mutate(new_cases_avg = rollmean(new_cases, k= 7, align = "right", fill = NA),
         new_deaths_avg =rollmean(new_deaths, k = 7, align = "right", fill = NA),
         weekly_hosp_admin_avg = rollmean(weekly_hosp_admissions, k = 7, align = "right", fill = NA),
         weekly_icu_admin_avg = rollmean(weekly_icu_admissions, k = 7, align = "right", fill = NA),
         new_vac_avg = rollmean(new_vaccinations , k = 7, align = "right", fill = NA)) %>% 
  select(-new_cases, -new_deaths, -weekly_hosp_admissions, -weekly_icu_admissions, -new_vaccinations) %>%
  mutate( total_cases_per_pop = (total_cases/population),
          total_vax_per_pop = (total_vaccinations/population)*100,
          fully_vaxed_per_pop = (people_fully_vaccinated/population)*100,
          new_vax_avg_per_pop = ((new_vac_avg/population)*100),
          population = population/1000000,
          new_deaths_avg_per_pop = (new_deaths_avg/population),
          new_cases_avg_per_pop = (new_cases_avg/population)) %>% 
  mutate(new_cases_avg_per_pop = ifelse(new_cases_avg_per_pop <0, 0, new_cases_avg_per_pop),
         new_deaths_avg_per_pop = ifelse(new_deaths_avg_per_pop <0, 0, new_deaths_avg_per_pop)) %>% 
  ungroup()



#loading country classifications

source(paste0(wd, "/Rscripts/country_classification.R"))


#functions for manupulating data -------------------

hotspot <- function(region, ranking_type) {
  
  increases_in_deaths_and_cases_within_this_week <- 
    cntry_cleaned_covid %>% 
    group_by(country) %>% 
    mutate(change_7day_avg_new_cases_per_pop = new_cases_avg_per_pop-lag(new_cases_avg_per_pop),
           change_7day_avg_new_deaths_per_pop = new_deaths_avg_per_pop-lag(new_deaths_avg_per_pop),
           avg_chg_weeklycases = rollmean(change_7day_avg_new_cases_per_pop, k = 14, align = "right", fill = NA),
           avg_chg_weeklydeaths = rollmean(change_7day_avg_new_deaths_per_pop, k = 14, align = "right", fill = NA),) %>% 
    filter(date == last(date)) %>%  
    ungroup() 
  
  if (ranking_type=="deaths") {
    
    hotspot <- 
      increases_in_deaths_and_cases_within_this_week %>%
      arrange(desc(avg_chg_weeklydeaths)) 
    
    if (region == "global") {
      data <- 
        hotspot %>% 
        slice_head(n = 20)
      
    } else {
      
      data <- 
        hotspot %>% 
        filter(country %in% region)
      
    }
    
  } else { 
    
    hotspot <- 
      increases_in_deaths_and_cases_within_this_week %>%
      arrange(desc(avg_chg_weeklycases)) 
    
    if (region == "global") {
      data <- 
        hotspot %>% 
        slice_head(n = 20)
      
    } else {
      
      data <- 
        hotspot %>% 
        filter(country %in% region)
      
    }
    
  }
  
  return(data)
}


#visual ranking of hotspots
cases_hotspot_visuals <- function(region) {
  
  ranked_data <- 
    hotspot(region, "cases") %>% 
    select(country, avg_chg_weeklycases) 
  
  ranked_list <- 
    unique(ranked_data$country)
  
  hot <- 
    ranked_data %>% 
    mutate(country = fct_rev(factor(country, levels = ranked_list))) %>% 
    filter(country != "NA") %>% 
    mutate(avg_chg_weeklycases = ifelse(avg_chg_weeklycases >0, avg_chg_weeklycases, 0))
  
  cool <- 
    ranked_data %>% 
    mutate(country = fct_rev(factor(country, levels = ranked_list))) %>% 
    filter(country != "NA") %>% 
    mutate(avg_chg_weeklycases = ifelse(avg_chg_weeklycases <0, avg_chg_weeklycases, 0))
  
  
  ggplot() +
    geom_bar(data = hot, aes(y= country, x = avg_chg_weeklycases, fill = avg_chg_weeklycases>0),
             stat = "identity", fill = "red") +
    geom_bar(data = cool, aes(y= country, x = avg_chg_weeklycases, fill = avg_chg_weeklycases<=0),
             stat = "identity", fill = "grey") +
    geom_vline(xintercept = 0)+
    labs(x= "", y= "", 
         subtitle = paste0("Change in Weekly Average New Cases Per Million (14-day avg.) \nAs of ", today()-1)) +
    theme_bw()+
    theme(
      plot.margin = margin(0, 1, 0.5, 1, unit = "cm"),
      plot.subtitle= element_text(size = 16),
      plot.title = element_text(size = 16),
      axis.text = element_text(face = "bold", size = 16),
      axis.text.y = element_text(face = "bold", size = 16,
                                 color = as.vector((rev(ifelse(ranked_data$avg_chg_weeklycases >= 0, "red", "black"))))))
}

#visual ranking of hotspots
death_hotspot_visuals <- function(region) {
  
  ranked_data <- 
    hotspot(region, "deaths") %>% 
    select(country, avg_chg_weeklydeaths) 
  
  ranked_list <- 
    unique(ranked_data$country)
  
  hot <- 
    ranked_data %>% 
    mutate(country = fct_rev(factor(country, levels = ranked_list))) %>% 
    filter(country != "NA") %>% 
    mutate(avg_chg_weeklydeaths = ifelse(avg_chg_weeklydeaths >0, avg_chg_weeklydeaths, 0))
  
  cool <- 
    ranked_data %>% 
    mutate(country = fct_rev(factor(country, levels = ranked_list))) %>% 
    filter(country != "NA") %>% 
    mutate(avg_chg_weeklydeaths = ifelse(avg_chg_weeklydeaths <0, avg_chg_weeklydeaths, 0))
  
  
  ggplot() +
    geom_bar(data = hot, aes(y= country, x = avg_chg_weeklydeaths, fill = avg_chg_weeklydeaths>0),
             stat = "identity", fill = "red") +
    geom_bar(data = cool, aes(y= country, x = avg_chg_weeklydeaths, fill = avg_chg_weeklydeaths<=0),
             stat = "identity", fill = "grey") +
    geom_vline(xintercept = 0)+
    labs(x= "", y= "", 
         subtitle = paste0("Change in Weekly Average New Deaths Per Million (14-day avg.) \nAs of ", today()-1)) +
    theme_bw()+
    theme(
      plot.margin = margin(0, 1, 0.5, 1, unit = "cm"),
      plot.subtitle= element_text(size = 16),
      plot.title = element_text(size = 16),
      axis.text = element_text(face = "bold", size = 16),
      axis.text.y = element_text(face = "bold", size = 16,
                                 color = as.vector((rev(ifelse(ranked_data$avg_chg_weeklydeaths >= 0, "red", "black"))))))
}

#cache all the graphs to optimize refresh rate ---------------------------------
em_case_hotspot <-
  cases_hotspot_visuals(em) +
  labs(title = "EM Covid Case Hotspots")

dm_case_hotspot <-
  cases_hotspot_visuals(dm) +
  labs(title = "DM Covid Case Hotpots")

eu_case_hotspot <-
  cases_hotspot_visuals(eu) +
  labs(title = "EU Covid Case Hotspots")

asia_case_hotspot <-
  cases_hotspot_visuals(asia)+
  labs(title = "Asia Covid Case Hotspots")

latam_case_hotspot <-
  cases_hotspot_visuals(latam) +
  labs(title = "Latam Covid Case Hotspots")

global_case_hotspot <-
  cases_hotspot_visuals("global") +
  labs(title = "Top 20 Global Covid Case Hotspots")

em_death_hotspot <-
  death_hotspot_visuals(em) +
  labs(title = "EM Covid Death Hotspots")

dm_death_hotspot <-
  death_hotspot_visuals(dm) +
  labs(title = "DM Covid Death Hotpots")

eu_death_hotspot <-
  death_hotspot_visuals(eu) +
  labs(title = "EU Covid Death Hotspots")

asia_death_hotspot <-
  death_hotspot_visuals(asia)+
  labs(title = "Asia Covid Death Hotspots")

latam_death_hotspot <-
  death_hotspot_visuals(latam) +
  labs(title = "Latam Covid Death Hotspots")

global_death_hotspot <-
  death_hotspot_visuals("global") +
  labs(title = "Top 20 Global Covid Death Hotspots")


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Covid Hotspots For the Week"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      selectInput(inputId = "type",
                  label = "Covid Situation",
                  list(`Cases` = "cases", `Death` = "deaths")
                 ),
      width = 1
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot", width = "1600px", height = "850px"),
      
      width = 11
    )
  )
)


#reactive output ---------------------------------------------------------

server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    if (input$type =="deaths") {

      grid.arrange(em_death_hotspot, dm_death_hotspot, eu_death_hotspot, asia_death_hotspot, 
                   latam_death_hotspot, global_death_hotspot,
                   nrow = 2)
     
      
    } else { 
      
      
      grid.arrange(em_case_hotspot, dm_case_hotspot, eu_case_hotspot, asia_case_hotspot, 
                   latam_case_hotspot, global_case_hotspot,
                   nrow = 2)
      
      }
    
  })
  
}


# Create Shiny app ----
shinyApp(ui = ui, server = server)

