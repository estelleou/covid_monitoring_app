#covid monitoring canonical script 
wd <- "D:/Estelle/Rscripts/covid_monitoring"
library(tidyverse)
library(lubridate)
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

mobility_data <- 
  cntry_cleaned_mobility %>% 
  select(date, country, roll_index) %>% 
  pivot_wider(names_from = country, values_from = roll_index)
write_csv(mobility_data, paste0(wd,"/output_data/mobility_data_full.csv"))


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

#regional covid cases data cleaning, charting, and output --------------------


region_cases <-
  covid_data %>% 
  select(date, location, continent,new_cases) %>% 
  rename(country = location) %>%
  #smoothing out by creating 7-day rolling average
  group_by(country) %>% 
  fill(new_cases, .direction = "down") %>%
  ungroup() %>% 
  select(-country) %>% 
  filter(date >= "2020-02-24") %>% 
  unique() %>% 
  group_by(date, continent) %>%
  mutate(new_cases = sum(new_cases, na.rm = T)) %>% 
  unique() %>% 
  arrange(date) %>% 
  ungroup() %>% 
  group_by(continent) %>% 
  mutate( new_cases_avg = rollmean(new_cases, k= 7, align = "right", fill = NA)) 

region_pop <-
  covid_data %>% 
  select(date, continent,population) %>% 
  group_by(date, continent) %>% 
  mutate(population = sum(population, na.rm = T), 
         population = population/1000000) %>% 
  unique() %>% 
  ungroup() %>% 
  filter(date == today()-1)%>% 
  select(-date)

region_covid_data <-
  region_cases %>% 
  left_join(region_pop, by = "continent") %>% 
  mutate(new_cases_avg_per_pop = (new_cases_avg/population)) %>% 
  mutate(new_cases_avg_per_pop = ifelse(is.na(new_cases_avg_per_pop),lag(new_cases_avg_per_pop), new_cases_avg_per_pop)) 

region_output <-
region_covid_data %>% 
  filter(!is.na(continent)) %>% 
  filter(date > "2020-01-31") %>%
  select(date,continent, new_cases_avg_per_pop) %>% 
  pivot_wider(names_from = continent, values_from = new_cases_avg_per_pop)

write_csv(region_output,  paste0(wd,"/output_data/regional_cases_data_per_capita.csv"))

last_region <- 
  region_covid_data %>% 
  filter(!is.na(continent)) %>% 
  filter(continent != "Oceania") %>% 
  filter(date == today()-1)

region_covid_data %>% 
  filter(!is.na(continent)) %>% 
  filter(date > "2020-01-31") %>%
  select(date,continent, new_cases_avg_per_pop) %>% 
  mutate(continent = (factor(continent, levels = c("North America", "Europe", "South America", "Asia","Africa", "Oceania")))) %>% 
  filter(continent != "Oceania") %>%
  ggplot() +
  geom_line(aes(x = date, y = new_cases_avg_per_pop, color = continent), size = 1.5) +
  geom_text(data= last_region,
            aes(x = date, y = new_cases_avg_per_pop,label=continent, color = continent),
            position=position_nudge(10), hjust=0, show.legend=TRUE,
            size = 5) +
  labs(x = "", y = "", 
       title= "Daily New Covid Cases per million (7-day avg.)",
       subtitle = paste0("As of ", today()-1)) +
  scale_x_date(lim = c(as.Date("2020-02-15"),
                       as.Date(max(last_region$date))+130),
               date_label = "%b %y",
               date_breaks = "3 months" ) +
  estelle_theme()+
  scale_color_brewer(palette = "Set2")+
  # scale_color_manual(values = wes_palette("Cavalcanti1", n = 5))
  theme(plot.margin = margin(0.1, 0.2, 0.1, 0.1, "cm"), 
        plot.title = element_text(size = 20, vjust = -1, color = "black"),
        plot.subtitle = element_text(size = 20),
        legend.position = "none")
ggsave("D:/Estelle/Rscripts/covid_monitoring/output/delta_progress.png", w = 8.3, h = 5.2)


#loading country classifications

source(paste0(wd, "/Rscripts/country_classification.R"))

#FUNCTIONS ---------------------------------------------------------

#country where cases and deaths are rising the fastest

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
        plot.title = element_text(size = 18),
        axis.text = element_text(face = "bold", size = 18),
        axis.text.y = element_text(face = "bold", size = 18,
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
      plot.title = element_text(size = 18),
      axis.text = element_text(face = "bold", size = 18),
      axis.text.y = element_text(face = "bold", size = 18,
                                 color = as.vector((rev(ifelse(ranked_data$avg_chg_weeklydeaths >= 0, "red", "black"))))))
}

#heatmap function

mobility_heatmap <- function(list_of_countries) {
  
  ranked_data <- 
    cntry_cleaned_mobility %>% 
    filter(country %in% list_of_countries) %>% 
    filter(date == last(date)) %>% 
    group_by(roll_index,country) %>%
    arrange(roll_index) %>%
    ungroup()
  
  ranked_list <- 
    unique(ranked_data$country)
  
  
  cntry_cleaned_mobility %>% 
    filter(country %in% ranked_list) %>% 
    mutate(country = fct_rev(factor(country, levels = ranked_list))) %>% 
    filter(country != "NA") %>% 
    ggplot()+
    geom_tile(aes(x = date, y = country,fill=roll_index)) +
    scale_fill_viridis(name="% of Jan-Feb 2020 levels",option="C") +
    scale_x_date(lim = c(as.Date("2020-03-15"),
                         as.Date(max(cntry_cleaned_mobility$date))),
                 date_label = "%b %y",
                 date_breaks = "4 months" )+
    theme_classic() +
    theme(plot.title = element_text(size = 25, face ="bold"),
          plot.subtitle= element_text(size = 20),
          axis.text = element_text(face = "bold", size = 18),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 18))
}

mobility_ts_chart <- function(list_of_countries) {
  
  ranked_data <- 
    cntry_cleaned_mobility %>% 
    filter(country %in% list_of_countries) %>% 
    filter(date == last(date)) %>% 
    group_by(roll_index,country) %>%
    arrange(roll_index) %>%
    ungroup()
  
  ranked_list <- 
    unique(ranked_data$country)
  
  
  cntry_cleaned_mobility %>%
    filter(country %in% ranked_list) %>%
    ggplot()+
    geom_line(aes(x = date, y = roll_index, color = country), size= 2) +
    geom_text(data= ranked_data,
              aes(x = date, y = roll_index,label=country, color = country),
              position=position_nudge(10), hjust=0, show.legend=TRUE,
              size = 4) +
    scale_color_manual(values = wes_palette("Cavalcanti1", n = 5)) +
    scale_x_date(lim = c(as.Date("2020-02-15"),
                         as.Date(max(cntry_cleaned_mobility$date))+30),
                 date_label = "%b %Y",
                 date_breaks = "3 months" ) +
    geom_hline(yintercept = 0) +
    theme_classic() +
    theme(legend.position = "none", 
          axis.text = element_text(face = "bold")) +
    labs(title = "Average Mobility in Recreational, Retail, Transit and Office Spaces", 
         subtitle = paste0("% of Jan-Feb 2020 levels (As of ", max(cntry_cleaned_mobility$date),")"),
         caption = "Source: Google Mobility Data",
         x = "", 
         y = "")+
    estelle_theme() +
    theme(plot.margin = margin(0.5, 1.5, 0.5, 0.5, "cm"), 
          plot.title = element_text(size = 15, vjust = -1, color = "black"),
          plot.subtitle = element_text(size = 15), 
          axis.text = element_text(size = 15))
}

mobility_composition_charts <- function(country_name) {
  
  
  composition_bars <- 
    cntry_cleaned_mobility %>% 
    filter(country == country_name) %>% 
    #dampening each composition effect so that it visually adds up to the averaged 
    #mobility index number
    #mobility index number
    mutate(`retail and recreation` = rollmean(`retail and recreation`, k=7, align = "right", fill = NA)/4,
           `transit stations` = rollmean(`transit stations`, k=7, align = "right", fill = NA)/4,
           workplaces = rollmean(workplaces, k=7, align = "right", fill = NA)/4,
           residential = (rollmean(residential, k=7, align = "right", fill = NA)/4)) %>% 
    select(-region, -roll_index, -parks, -`grocery and pharmacy`, -index, -country) %>% 
    pivot_longer(-date)
  
  
  ggplot() +
    geom_bar(data = composition_bars, aes(x = date, y = value, fill = name ), position = "stack", stat = "identity") +
    geom_vline(xintercept = as.Date(max(cntry_cleaned_mobility$date)), linetype = "dashed")+
    geom_line(data = cntry_cleaned_mobility %>%
                filter(country == country_name),
              aes(x = date, y = roll_index, color =
                    "Average Mobility Index (% of Jan-Feb 2020 levels) \n(+) residential activity is calculated as a drag on overall mobility"), size = 1.5)+
    geom_hline(yintercept = 0) +
    scale_fill_manual(values = wes_palette("Darjeeling1", n = 5)) +
    scale_color_manual(values = c("black"))+
    scale_x_date(lim = c(as.Date("2020-03-15"),
                         as.Date(max(cntry_cleaned_mobility$date))),
                 date_label = "%b %y",
                 date_breaks = "3 months" )+
    labs(x = "", y = "", 
         title = paste0(country_name , ": Mobility as Percent of Jan-Feb 2020 levels (as of ", max(cntry_cleaned_mobility$date), ")"),
         caption = "Source: Google Mobility Data")+
    theme_classic() +
    theme(plot.title = element_text(size = 18, face ="bold"),
          legend.position = "top",
          plot.subtitle= element_text(size = 12),
          axis.text = element_text(face = "bold", size = 12),
          legend.title = element_blank(),
          legend.text = element_text(size = 12), 
          plot.margin = margin(0.2, 0.7, 0.2, 0.2, "cm"))+
    guides(fill=guide_legend(nrow = 2))
  
}


covid_cases_ranking_bar <- function(list_of_countries , ranking_variable_name) {
  
  ranked_data <- 
    cntry_cleaned_covid %>% 
    filter(country %in% list_of_countries) %>% 
    filter(date == last(date)) %>% 
    group_by(new_cases_avg_per_pop,country) %>%
    arrange(new_cases_avg_per_pop) %>%
    ungroup()
  
  ranked_list <- 
    unique(ranked_data$country)
  
  
  cntry_cleaned_covid %>% 
    filter(country %in% list_of_countries) %>% 
    filter(date == last(date)) %>% 
    mutate(country = (factor(country, levels = ranked_list))) %>% 
    filter(country != "NA") %>% 
    ggplot()+
    geom_bar(aes( y = country,x= new_cases_avg_per_pop), stat = "identity", fill = "black") +
    theme_classic() +
    theme(plot.title = element_text(size = 25, face ="bold"),
          plot.subtitle= element_text(size = 20),
          axis.text = element_text(face = "bold", size = 18),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 18))
}

covid_cases_heatmap <- function(list_of_countries , ranking_variable_name) {
  
  ranked_data <- 
    cntry_cleaned_covid %>% 
    filter(country %in% list_of_countries) %>% 
    filter(date == last(date)) %>% 
    group_by(new_cases_per_million,country) %>%
    arrange(new_cases_per_million) %>%
    ungroup()
  
  ranked_list <- 
    unique(ranked_data$country)
  
  
  cntry_cleaned_covid %>% 
    filter(country %in% list_of_countries) %>% 
    mutate(country = (factor(country, levels = ranked_list))) %>% 
    filter(country != "NA") %>% 
    ggplot()+
    geom_tile(aes(x = date, y = country,fill= new_cases_per_million)) +
    scale_fill_viridis(name=ranking_variable_name,option="C") +
    scale_x_date(lim = c(as.Date("2020-03-15"),
                         as.Date(max(cntry_cleaned_covid$date))),
                 date_label = "%b %y",
                 date_breaks = "4 months" )+
    theme_classic() +
    theme(plot.title = element_text(size = 25, face ="bold"),
          plot.subtitle= element_text(size = 20),
          axis.text = element_text(face = "bold", size = 18),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 18))
}



covid_deaths_ranking_bar <- function(list_of_countries , ranking_variable_name) {
  
  ranked_data <- 
    cntry_cleaned_covid %>% 
    filter(country %in% list_of_countries) %>% 
    filter(date == last(date)) %>% 
    group_by(new_deaths_avg_per_pop,country) %>%
    arrange(new_deaths_avg_per_pop) %>%
    ungroup()
  
  ranked_list <- 
    unique(ranked_data$country)
  
  
  cntry_cleaned_covid %>% 
    filter(country %in% list_of_countries) %>% 
    filter(date == last(date)) %>% 
    mutate(country = (factor(country, levels = ranked_list))) %>% 
    filter(country != "NA") %>% 
    ggplot()+
    geom_bar(aes( y = country,x= new_deaths_avg_per_pop), stat = "identity", fill = "red") +
    theme_classic() +
    theme(plot.title = element_text(size = 25, face ="bold"),
          plot.subtitle= element_text(size = 20),
          axis.text = element_text(face = "bold", size = 18),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 18))
}

covid_deaths_heatmap <- function(list_of_countries , ranking_variable_name) {
  
  ranked_data <- 
    cntry_cleaned_covid %>% 
    filter(country %in% list_of_countries) %>% 
    filter(date == last(date)) %>% 
    group_by(new_deaths_per_million,country) %>%
    arrange(new_deaths_per_million) %>%
    ungroup()
  
  ranked_list <- 
    unique(ranked_data$country)
  
  
  cntry_cleaned_covid %>% 
    filter(country %in% list_of_countries) %>% 
    mutate(country = (factor(country, levels = ranked_list))) %>% 
    filter(country != "NA") %>% 
    ggplot()+
    geom_tile(aes(x = date, y = country,fill= new_deaths_per_million)) +
    scale_fill_viridis(name=ranking_variable_name, option = "C") +
    scale_x_date(lim = c(as.Date("2020-03-15"),
                         as.Date(max(cntry_cleaned_covid$date))),
                 date_label = "%b %y",
                 date_breaks = "4 months" )+
    theme_classic() +
    theme(plot.title = element_text(size = 25, face ="bold"),
          plot.subtitle= element_text(size = 20),
          axis.text = element_text(face = "bold", size = 18),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 18))
}

vaccintaion_ranking <- function(list_of_countries) {
  
  ranked_data <- 
    cntry_cleaned_covid %>% 
    filter(country %in% list_of_countries) %>% 
    filter(date == last(date)) %>% 
    group_by(fully_vaxed_per_pop,country) %>%
    arrange(fully_vaxed_per_pop) %>%
    ungroup()
  
  ranked_list <- 
    unique(ranked_data$country)
  
  
  cntry_cleaned_covid %>% 
    filter(country %in% list_of_countries) %>% 
    filter(date == last(date)) %>% 
    mutate(country = (factor(country, levels = ranked_list))) %>% 
    filter(country != "NA") %>% 
    ggplot()+
    geom_bar(aes(x = fully_vaxed_per_pop, y = country), stat = "identity",  fill = "#336600") +
    geom_vline(xintercept = 75, linetype = "dashed")+
    scale_x_continuous(lim = c(0, 100, 25))+
    theme_classic() +
    theme(plot.title = element_text(size = 25, face ="bold"),
          plot.subtitle= element_text(size = 20),
          axis.text = element_text(face = "bold", size = 18),
          legend.title = element_text(size = 18),
          legend.text = element_text(size = 18))
}

#vaccination rate chart
vax_rate <- function(cntry) {
  
  cntry_vax_number <- 
    cntry_cleaned_covid %>% 
    filter(country == cntry) %>% 
    select(date,country, fully_vaxed_per_pop) %>% 
    fill(fully_vaxed_per_pop , .direction = "down") %>% 
    filter(date == last(date)) 
  
  cntry_cleaned_covid %>% 
    select(date, country, new_vax_avg_per_pop) %>% 
    filter(country == cntry) %>% 
    mutate(new_vax_avg_per_pop = ifelse(date <= as.Date("2021-02-01"), 0, new_vax_avg_per_pop)) %>%
    fill(new_vax_avg_per_pop, .direction = "down") %>%
    filter(country == cntry) %>% 
    ggplot()+
    geom_line(aes(x = date, y = new_vax_avg_per_pop), color = "dark green", size = 1)+
    scale_x_date(lim = c(as.Date("2020-03-15"),
                         as.Date(max(cntry_cleaned_covid$date))),
                 date_label = "%b %y",
                 date_breaks = "4 months" ) +
    labs(title = paste0("Daily Vaccination Rate (7-day avg.) \n(% of pop.)", " (", 
                        round(cntry_vax_number$fully_vaxed_per_pop, 1), 
                        "% Fully Vaccinated)"),
         x = "", y = "") +
    theme_classic() +
    theme(plot.title = element_text(size = 12 ),
          plot.subtitle= element_text(size = 12),
          axis.text = element_text(face = "bold", size = 12),
          plot.margin = margin(5,5,5,5))
}

new_cases <- function(cntry){
  cntry_cleaned_covid %>% 
    select(date, country, new_cases_avg) %>% 
    filter(country == cntry) %>% 
    ggplot() + 
    geom_line(aes(x= date, y = new_cases_avg), color = "black", size = 1) +
    scale_x_date(lim = c(as.Date("2020-03-15"),
                         as.Date(max(cntry_cleaned_covid$date))),
                 date_label = "%b %y",
                 date_breaks = "4 months" ) +
    labs(subtitle = "New Cases (7-day avg.)",
         x = "", y = "") +
    theme_classic() +
    theme(axis.text = element_text(face = "bold", size = 12),
          plot.subtitle = element_text(size = 12))
}

new_deaths <- function(cntry){
  cntry_cleaned_covid %>% 
    select(date, country, new_deaths_avg) %>% 
    filter(country == cntry) %>% 
    ggplot() + 
    geom_line(aes(x= date, y = new_deaths_avg), color = "red", size = 1) +
    scale_x_date(lim = c(as.Date("2020-03-15"),
                         as.Date(max(cntry_cleaned_covid$date))),
                 date_label = "%b %y",
                 date_breaks = "4 months" ) +
    labs(subtitle = "New Deaths (7-day avg.)",
         x = "", y = "") +
    theme_classic() +
    theme(axis.text = element_text(face = "bold", size = 12),
          plot.subtitle = element_text(size = 12), 
          plot.margin = margin(0.2, 0.7, 0.2, 0.2, "cm"))
}


# OUTPUT ---------------------------------------------------------------------
#covid situation in hotspot countries
em_death_hotspots <- 
  hotspot(em, "deaths") %>% 
  filter(avg_chg_weeklydeaths >0) %>% 
  slice_head(n = 5) %>% 
  select(country)

dm_death_hotspots <- 
  hotspot(dm, "deaths") %>% 
  filter(avg_chg_weeklydeaths >0) %>% 
  slice_head(n = 5) %>% 
  select(country)

pdf(paste0(wd, "/output/em_covid_situation_pulls.pdf"), h = 8, w = 10.5)

for (i in 1:5) {
grid.arrange( mobility_composition_charts(em_death_hotspots$country[i]), 
              vax_rate(em_death_hotspots$country[i]), new_cases(em_death_hotspots$country[i]),
              new_deaths(em_death_hotspots$country[i]),
              layout_matrix = rbind(c(1, 1, 1),
                                    c(1, 1, 1),
                                    c(2, 3, 4)),
              bottom = textGrob(
                "Source: JHU",
                gp = gpar(fontface = 3, fontsize = 12),
                hjust = 1,
                x = 1))
}

dev.off()

#dm hotspot situation pull ---------------------------------------------

pdf(paste0(wd, "/output/dm_covid_situation_pulls.pdf"), h = 8, w = 10.5)

for (i in 1:length(dm_death_hotspots)) {
  
  grid.arrange( mobility_composition_charts(dm_death_hotspots$country[i]),
                vax_rate(dm_death_hotspots$country[i]), new_cases(dm_death_hotspots$country[i]),
                new_deaths(dm_death_hotspots$country[i]),
                layout_matrix = rbind(c(1, 1, 1),
                                      c(1, 1, 1),
                                      c(2, 3, 4)),
                bottom = textGrob(
                  "Source: JHU",
                  gp = gpar(fontface = 3, fontsize = 12),
                  hjust = 1,
                  x = 1))
  
}

dev.off()


#Hotpots------------------------------------------------------------------

em_hotspot <-
  cases_hotspot_visuals(em) +
  labs(title = "EM Covid Case Hotspots")

dm_hotspot <-
  cases_hotspot_visuals(dm) +
  labs(title = "DM Covid Case Hotpots")

eu_hotspot <-
  cases_hotspot_visuals(eu) +
  labs(title = "EU Covid Case Hotspots")

asia_hotspot <-
  cases_hotspot_visuals(asia)+
  labs(title = "Asia Covid Case Hotspots")

latam_hotspot <-
  cases_hotspot_visuals(latam) +
  labs(title = "Latam Covid Case Hotspots")

global_hotspot <-
  cases_hotspot_visuals("global") +
  labs(title = "Top 20 Global Covid Case Hotspots")

# G20 -----------------------------------------------------------------------
g20_mobility <-
  mobility_heatmap(g_20_exEU) +
  labs(title = "G20 Mobility",
       subtitle = paste0("From Least Mobile (Top) to Most on ",format(max(cntry_cleaned_mobility$date), "%m-%d")),
       x = "",
       y = "")

g20_covid_cases <-
  covid_cases_ranking_bar(g_20_exEU, "Cases per million") +
  labs(title = "G20 Covid Cases per million",
       subtitle = paste0("(7-day avg.) as of ",format(max(cntry_cleaned_covid$date), "%b-%d")),
       x = "",
       y = "")

g20_covid_deaths <-
  covid_deaths_ranking_bar(g_20_exEU, "Deaths per million") +
  labs(title = "G20 Covid Deaths per million",
       subtitle = paste0("(7-day avg.) as of ",format(max(cntry_cleaned_covid$date), "%b-%d")),
       x = "",
       y = "")

g20_vaccination <-
  vaccintaion_ranking(g_20_exEU) +
  labs(title = "G20 Vaccination",
       subtitle = paste0("(% of population) as of ", format(max(cntry_cleaned_covid$date), "%b-%d")),
       x = "",
       y = "")

#EM ----------------------------------------------------------------------
em_mobility <-
  mobility_heatmap(em) +
  labs(title = "EM Mobility",
       subtitle = paste0("From Least Mobile (Top) to Most on ",format(max(cntry_cleaned_mobility$date), "%m-%d")),
       x = "",
       y = "")


em_covid_cases <-
  covid_cases_ranking_bar(em, "Cases per million") +
  labs(title = "EM Covid Cases per million",
       subtitle = paste0("(7-day avg.) as of ",format(max(cntry_cleaned_covid$date), "%b-%d")),
       x = "",
       y = "")

em_covid_deaths <-
  covid_deaths_ranking_bar(em, "Deaths per million") +
  labs(title = "EM Covid Deaths per million",
       subtitle = paste0("(7-day avg.) as of ",format(max(cntry_cleaned_covid$date), "%b-%d")),
       x = "",
       y = "")

em_vaccination <-
  vaccintaion_ranking(em) +
  labs(title = "EM Vaccination",
       subtitle = paste0("(% of population) as of ", format(max(cntry_cleaned_covid$date), "%b-%d")),
       x = "",
       y = "")

#major advanced ----------------------------------------------------------------------

dm_mobility <-
  mobility_heatmap(dm) +
  labs(title = "DM Mobility",
       subtitle = paste0("From Least Mobile (Top) to Most on ",format(max(cntry_cleaned_mobility$date), "%m-%d")),
       x = "",
       y = "")


dm_covid_cases <-
  covid_cases_ranking_bar(dm, "Cases per million") +
  labs(title = "DM Covid Cases per million",
       subtitle = paste0("(7-day avg.) as of ",format(max(cntry_cleaned_covid$date), "%b-%d")),
       x = "",
       y = "")

dm_covid_deaths <-
  covid_deaths_ranking_bar(dm, "Deaths per million") +
  labs(title = "DM Covid Deaths per million",
       subtitle = paste0("(7-day avg.) as of ",format(max(cntry_cleaned_covid$date), "%b-%d")),
       x = "",
       y = "")

dm_vaccination <-
  vaccintaion_ranking(dm) +
  labs(title ="DM Vaccination",
       subtitle = paste0("(% of population) as of ", format(max(cntry_cleaned_covid$date), "%b-%d")),
       x = "",
       y = "")

#OUTPUT -------------------------------------------------------------------


pdf(paste0(wd, "/output/covid_rankings.pdf"), w = 30, h = 15)

grid.arrange(em_hotspot, dm_hotspot, eu_hotspot, asia_hotspot, latam_hotspot, global_hotspot,
             nrow = 2,
             top = textGrob("Covid Case Hotspots This Week In Red",
                            gp = gpar(fontface = 4, fontsize = 30,
                            hjust = 4.3)),
             bottom = textGrob(
               "Source: JHU",
               gp = gpar(fontface = 3, fontsize = 15),
               hjust = 1,
               x = 1))

grid.arrange(g20_mobility, g20_covid_cases, g20_covid_deaths, g20_vaccination,
             nrow = 1,
             top = textGrob("G20 COUNTRIES",
                            gp = gpar(fontface = 4, fontsize = 30),
                            hjust = 4.3),
             bottom = textGrob(
               "Source: JHU and Google Mobility Data",
               gp = gpar(fontface = 3, fontsize = 15),
               hjust = 1,
               x = 1))

grid.arrange(em_mobility, em_covid_cases, em_covid_deaths, em_vaccination,
             nrow = 1,
             top = textGrob("EM COUNTRIES",
                            gp = gpar(fontface = 4, fontsize = 30),
                            hjust = 4.3),
             bottom = textGrob(
               "Source: JHU and Google Mobility Data",
               gp = gpar(fontface = 3, fontsize = 15),
               hjust = 1,
               x = 1))


grid.arrange(dm_mobility, dm_covid_cases, dm_covid_deaths, dm_vaccination,
             nrow = 1,
             top = textGrob("DM COUNTRIES",
                            gp = gpar(fontface = 4, fontsize = 30),
                            hjust = 4.3),
             bottom = textGrob(
               "Source: JHU and Google Mobility Data",
               gp = gpar(fontface = 3, fontsize = 15),
               hjust = 1,
               x = 1))

dev.off()
