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


# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Covid Hotspots For the Week"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      selectInput(inputId = "region",
                  label = "Region",
                  # choices = c( "em" = `em`, "dm" = `dm`, "asia" = `asia`, 
                  #              "Latam" = `latam`, "EU" = `eu`)
                  list(`EM` = "em", `DM` = "dm", `Asia` = "asia", 
                       `Latam` = "latam", `EU` = "eu"))
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      plotOutput(outputId = "distPlot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {
  
  # Histogram of the Old Faithful Geyser Data ----
  # with requested number of bins
  # This expression that generates a histogram is wrapped in a call
  # to renderPlot to indicate that:
  #
  # 1. It is "reactive" and therefore should be automatically
  #    re-executed when inputs (input$bins) change
  # 2. Its output type is a plot
  output$distPlot <- renderPlot({
    
    hotspot <- 
      cntry_cleaned_covid %>% 
      group_by(country) %>% 
      mutate(change_7day_avg_new_cases_per_pop = new_cases_avg_per_pop-lag(new_cases_avg_per_pop),
             avg_chg_weeklycases = rollmean(change_7day_avg_new_cases_per_pop, k = 14, align = "right", fill = NA)) %>% 
      filter(date == last(date)) %>%  
      ungroup() %>% 
      arrange(desc(avg_chg_weeklycases)) %>% 
      select(country, avg_chg_weeklycases) 
    
    if (input$region == "em") {
      
      ranked_data <- 
        hotspot %>% 
        filter(country %in% em) 
      
    } else if (input$region == "dm") {
      
      ranked_data <- 
        hotspot %>% 
        filter(country %in% dm) 
      
    }  else if (input$region == "eu") { 
      
      ranked_data <- 
        hotspot %>% 
        filter(country %in% eu) 
      
    } else if (input$region == "latam") {
      
      ranked_data <- 
        hotspot %>% 
        filter(country %in% latam) 
      
    } else if (input$region == "asia") {
      
      ranked_data <- 
        hotspot %>% 
        filter(country %in% asia) 
      
    } else {
      
      ranked_data <-
        hotspot %>%
        slice_head(n = 20)
    }
    
    
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
        plot.title = element_text(size = 18),
        axis.text = element_text(face = "bold", size = 18),
        axis.text.y = element_text(face = "bold", size = 18,
                                   color = as.vector((rev(ifelse(ranked_data$avg_chg_weeklycases >= 0, "red", "black"))))))
    
  })
  
}

# Create Shiny app ----
shinyApp(ui = ui, server = server)
