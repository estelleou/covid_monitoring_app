#estelle_theme charts -----------------------------

estelle_theme <- function(){
  
  font <- "Helvetica"
  theme(
    
    plot.title = element_text(family=font,
                              size=28,
                              face="bold",
                              color="#336699"),
    
    plot.subtitle = element_text(family=font,
                                 size=22,
                                 margin=margin(9,0,9,0)),
    plot.caption = element_text(family=font, 
                                size = 18),
    
    legend.text.align = 0,
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(family=font,
                               size=18,
                               color="#222222"),
    
    axis.text = element_text(family=font,
                             size=18,
                             color="#222222"),
    axis.text.x = element_text(margin=margin(5, b = 10)),
    axis.ticks = element_blank(),
    axis.line = element_line(),
    
    panel.background = element_blank(),
    
    strip.background = element_rect(fill="white"),
    strip.text = element_text(size  = 22,  hjust = 0)
  )
  
}


#functions for manupulating data -------------------

hotspot <- function(region, ranking_type) {
  
  if (ranking_type=="deaths") {
    
    hotspot <- 
      increases_in_deaths_and_cases_within_this_week %>%
      filter(date == last(date)) %>%  
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
      filter(date == last(date)) %>%  
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
      plot.subtitle= element_text(size = 14),
      plot.title = element_text(size = 14),
      axis.text = element_text(face = "bold", size = 14),
      axis.text.y = element_text(face = "bold", size = 14,
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
      plot.subtitle= element_text(size = 14),
      plot.title = element_text(size = 14),
      axis.text = element_text(face = "bold", size = 14),
      axis.text.y = element_text(face = "bold", size = 14,
                                 color = as.vector((rev(ifelse(ranked_data$avg_chg_weeklydeaths >= 0, "red", "black"))))))
}


#country cases and death time-series charts by region -------------------------


top_5_country_ranked_by_cases <- function(region){
  ranked_data <-
    cntry_cleaned_covid %>%
    filter(country %in% region) %>%
    filter(date == last(date)) %>%
    group_by(new_cases_avg_per_pop,country) %>%
    arrange(new_cases_avg_per_pop) %>%
    ungroup() %>% 
    slice_tail(n=5)

  ranked_list <-
    unique(ranked_data$country)
  
  cntry_cleaned_covid %>% 
    filter(country %in% ranked_list) %>% 
    filter(country != "NA") %>% 
    ggplot()+ 
    geom_line(aes(x = date, y = new_cases_avg_per_pop, color = country), size = 1.2) +
    geom_dl(data = ranked_data,
            aes(x = date, y = new_cases_avg_per_pop, color = country, label = country),
            method = list('last.bumpup', cex = 1.2, hjust = 0, 
                          vjust = 1, fontface = "bold"))+
    labs(x = "", y = "", 
         title= "Daily New Covid Cases per million (7-day avg.)",
         subtitle = paste0("As of ", today()-1)) +
    scale_x_date(lim = c(today()-365,
                       as.Date(max(ranked_data$date))),
               date_label = "%b %y" ) +
    estelle_theme()+
    scale_color_manual(values = c( "#0099cc", "#778088", "#832e31", 
                                   "#cc9900", "#006633"))+
    theme(plot.margin = margin(0.1, 3, 0.1, 0.1, "cm"), 
          plot.title = element_text(size = 20, vjust = -1, color = "black"),
          plot.subtitle = element_text(size = 20),
          legend.position = "none") +
    coord_cartesian(clip = "off")
  
}


top_5_country_ranked_by_deaths <- function(region){
  ranked_data <-
    cntry_cleaned_covid %>%
    select(date, country, new_deaths_avg_per_pop) %>% 
    filter(country %in% region) %>%
    filter(date == last(date)) %>%
    group_by(new_deaths_avg_per_pop,country) %>%
    arrange(new_deaths_avg_per_pop) %>%
    ungroup() %>% 
    slice_tail(n=5)
  
  ranked_list <-
    unique(ranked_data$country)
  
  cntry_cleaned_covid %>% 
    filter(country %in% ranked_list) %>% 
    filter(country != "NA") %>% 
    ggplot()+ 
    geom_line(aes(x = date, y = new_deaths_avg_per_pop, color = country), size = 1.2) +
    geom_dl(data = ranked_data,
            aes(x = date, y = new_deaths_avg_per_pop, color = country, label = country),
            method = list('last.bumpup', cex = 1.2, hjust = 0, 
                          vjust = 1, fontface = "bold"))+
    labs(x = "", y = "", 
         title= "Daily New Covid Deaths per million (7-day avg.)",
         subtitle = paste0("As of ", today()-1)) +
    scale_x_date(lim = c(today()-365,
                         as.Date(max(ranked_data$date))),
                 date_label = "%b %y") +
    estelle_theme()+
    scale_color_manual(values = c( "#0099cc", "#778088", "#832e31", 
                                   "#cc9900", "#006633"))+
    theme(plot.margin = margin(0.1, 3, 0, 0, "cm"), 
          plot.title = element_text(size = 20, vjust = -1, color = "black"),
          plot.subtitle = element_text(size = 20),
          legend.position = "none") +
    coord_cartesian(clip = "off")
  
}


#average mobility charts with dynamic colors for when cases are increasing 

 mobility_case_chart<- function(data, country_name) {
  
     
  mobility_and_case_speed_data <- 
     data %>% 
    mutate(x_axis_date = update(date, year = 1)) %>% 
    mutate(roll_index = 100+roll_index) %>% 
    left_join(increases_in_deaths_and_cases_within_this_week)
    
  
  acceleration <- 
    mobility_and_case_speed_data %>% 
    select(date, x_axis_date, roll_index, avg_chg_weeklycases) %>% 
    mutate(roll_index_1 = ifelse(avg_chg_weeklycases>=0, roll_index, 0))
  
  deceleration <- 
    mobility_and_case_speed_data %>% 
    select(date, x_axis_date, roll_index, avg_chg_weeklycases) %>% 
    mutate(roll_index_2 = ifelse(avg_chg_weeklycases<=0, roll_index, 0))
    
  #reference values for y-axis
  y_axis_min_max_reference_data <- 
    cntry_cleaned_mobility %>% 
    filter(country == country_name) %>% 
    filter(!is.na(roll_index)) %>% 
    mutate(roll_index = 100+roll_index) %>% 
    select(roll_index)
  
  
    ggplot()+
    geom_line(data = acceleration, aes(x = x_axis_date, y = roll_index_1), 
              color = "#ff3300", size = 1.5) +
    geom_line(data = deceleration, aes(x = x_axis_date, y = roll_index_2),
              color = "#778088", size = 1.5) +
    geom_line(data = deceleration, aes(x = x_axis_date, y = roll_index),
                color = "#778088", size = 1.5, alpha = 0.4) +
    scale_x_date(lim = c(as.Date("0000-12-31"), as.Date("0001-12-31")),
                 date_label = "%b", 
                 date_breaks = "2 months") +
    geom_hline(yintercept = 100, linetype = 2) +
    theme_classic() +
    labs(x = paste0(year(mobility_and_case_speed_data$date)), 
         y = "")+
    scale_y_continuous(lim = c(min(y_axis_min_max_reference_data$roll_index), 
                               ifelse(max(y_axis_min_max_reference_data$roll_index)>100,
                                      max(y_axis_min_max_reference_data$roll_index), 101)))+
    estelle_theme() +
    theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), 
          plot.title = element_text(size = 16, vjust = -1),
          plot.subtitle = element_text(size = 16, vjust = -1),
          axis.text = element_text(size = 16), 
          axis.title = element_text(size = 16))
 }
 
 
 #weekly change in mobility trends
 
 working_days = c("Monday", "Tuesday", "Wednesday", 
              "Thurday", "Friday")
 
   
  weekday_mobility <- function(data) {
     
   data %>% 
      filter(date > today() - 90) %>% 
   select(country, date, residential, `grocery and pharmacy`, 
          `transit stations`, `workplaces`, `retail and recreation`) %>% 
   mutate(weekdays = weekdays(date)) %>% 
   #filtering out weekdays
   filter(weekdays %in% working_days) %>% 
   mutate(week = week(date)) %>% 
   group_by(week) %>% 
   summarise(workplaces = mean(workplaces, na.rm = T), 
             `transit stations` = mean(`transit stations`, na.rm = T)) %>% 
   ungroup() %>% 
   #calculate weekly % change in mobility compared with Jan 2020
   mutate(workplace_weekly_chg = workplaces - lag(workplaces), 
          transit_weekly_chg = `transit stations` - lag(`transit stations`)) %>% 
   mutate(date = as.Date(paste(week,year(today()), 'Mon'), '%U %Y %a')) %>% 
   select(date, workplace_weekly_chg, transit_weekly_chg) %>% 
   pivot_longer(-date) %>% 
   filter(!is.na(value)) %>% 
   filter(!is.na(date)) %>% 
   mutate(date = as.character(date)) %>% 
   mutate(name = ifelse(name == "workplace_weekly_chg" , "Places of Work", 
                        "Transit Stations")) %>% 
   ggplot() +
   geom_bar(aes(x= date, y = value, fill = name), stat = "identity", 
            position = "dodge") +
   geom_hline(yintercept = 0)+
   labs(x = " ", y = " ", 
        title = paste0(unique(data$country), ": Change in Mobility on Weekdays"),
        subtitle = "Weekly % Change in Mobility vs. Jan-Feb 2020 levels")+
   scale_x_discrete(labels = function(x) strftime(x, "%b %d")) +
   scale_fill_manual(values  = c("#cc9900", "#003333"))+
   estelle_theme() +
   theme(plot.margin = margin(0.5, 0.2, 0.5, 0.2, "cm"), 
         plot.title = element_text(size = 16, vjust = -1, color = "#993333", 
                                   face = "bold",
                                   margin = margin(0,0,0.3,0, "cm")),
         legend.text = element_text(size = 16),
         plot.subtitle = element_text(size = 15, vjust = -1, color = "#778088",
                                      face = "bold",
                                      margin = margin(0,0,0.5,0, "cm")),
         axis.text = element_text(size = 16),
         axis.text.x = element_text(angle = 45, hjust = 1),
         axis.title = element_text(size = 16), 
         legend.position = "top") +
   guides(fill = guide_legend(nrow = 1))
   
  }
  

             
  weekend_mobility <- function(data) {
    
  data %>%   
  filter(date > today() - 90) %>% 
  select(country, date, residential, `grocery and pharmacy`, 
         `transit stations`, `workplaces`, `retail and recreation`) %>% 
    mutate(weekdays = weekdays(date)) %>% 
    #filtering out weekdays
    filter(weekdays %in% working_days) %>% 
    mutate(week = week(date)) %>% 
    group_by(week) %>% 
    summarise(residential = mean(residential, na.rm = T), 
              `retail and recreation` = mean(`retail and recreation`, na.rm = T)) %>% 
    ungroup() %>% 
    #calculate weekly % change in mobility compared with Jan 2020
    mutate(resident_weekly_chg = residential - lag(residential), 
           retail_weekly_chg = `retail and recreation` - lag(`retail and recreation`)) %>% 
    mutate(date = as.Date(paste(week,year(today()), 'Mon'), '%U %Y %a')) %>% 
    select(date, resident_weekly_chg, retail_weekly_chg) %>% 
    pivot_longer(-date) %>% 
    filter(!is.na(value)) %>% 
    filter(!is.na(date)) %>% 
    mutate(date = as.character(date)) %>% 
    mutate(name = ifelse(name == "resident_weekly_chg" , "Places of Residence", 
                         "Retail and Recreation")) %>% 
    ggplot() +
    geom_bar(aes(x= date, y = value, fill = name), stat = "identity", 
             position = "dodge") +
    geom_hline(yintercept = 0)+
    labs(x = " ", y = " ", 
         title = paste0(unique(data$country),  ": Change in Mobility on Weekends"),
         subtitle = "Weekly % Change in Mobility vs. Jan-Feb 2020 levels")+
    scale_x_discrete(labels = function(x) strftime(x, "%b %d")) +
    scale_fill_manual(values  = c("#006633", "#0099cc"))+
    estelle_theme() +
    theme(plot.margin = margin(0.5, 0.2, 0.5, 0.2, "cm"), 
          plot.title = element_text(size = 16, vjust = -1, color = "#993333", 
                                    face = "bold",
                                    margin = margin(0,0,0.3,0, "cm")),
          legend.text = element_text(size = 16),
          plot.subtitle = element_text(size = 16, vjust = -1, color = "#778088",
                                       face = "bold",
                                       margin = margin(0,0,0.5,0, "cm")),
          axis.text = element_text(size = 16),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_text(size = 16), 
          legend.position = "top") +
    guides(fill = guide_legend(nrow = 1))
}

  
  #state-level mobility data for choropleth map --------------------------
  
  state_cleaned_mobility <- function(country_name) {
    #creating country-level mobility data
    mobility %>%
    #taking the country total numbers and getting rid of county/city-level data
    filter(country== country_name) %>%
    select(-country) %>% 
    filter(region != "Total") %>%
    mutate(index = (`retail and recreation` + `transit stations` +workplaces - residential)/4) %>%
    # mutate(index = (`retail and recreation` - residential)/2) %>%
    group_by(region) %>% 
    mutate(roll_index = rollmean(index, k=7, align = "right", fill = NA)) %>% 
    ungroup() 
  }
  
  #state-level case data for choropleth map --------------------------
  
  state_cleaned_case <- function(country_name) {
    
    #creating state-level mobility data
      state_covid_data %>% 
      rename(country = state) %>% 
      select(-fips) %>% 
      group_by(country) %>%  
      group_by(country) %>% 
      fill(cases, .direction = "down") %>%
      fill(deaths, .direction = "down") %>%
      mutate(new_cases = cases - lag(cases,1),
             new_cases = ifelse(new_cases <=0, 0, new_cases),
             #to get rid of reporting anomalies
             new_cases = ifelse(new_cases > lag(new_cases,1)+100, lag(new_cases, 1), new_cases),
             new_deaths = deaths - lag(deaths,1),
             new_deaths = ifelse(new_deaths <=0, 0, new_deaths),
             new_deaths = ifelse(new_deaths > lag(new_deaths,1)+100, lag(new_deaths, 1), new_deaths),
             new_cases_avg = rollmean(new_cases, k= 7, align = "right", fill = NA) ,
             new_deaths_avg = rollmean(new_deaths, k = 7, align = "right", fill = NA),
             new_cases_avg_per_mill = new_cases_avg/population , 
             new_deaths_avg_per_mill = new_deaths_avg/population,
             weekly_chg_new_cases = new_cases_avg_per_mill -lag(new_cases_avg_per_mill,1),
             weekly_chg_new_deaths = new_deaths_avg_per_mill -lag(new_deaths_avg_per_mill,1),
             row_avg_weekly_new_cases = rollmean(weekly_chg_new_cases, k = 14, align = "right", fill = NA),
             row_avg_weekly_new_deaths = rollmean(weekly_chg_new_deaths, k = 14, align = "right", fill = NA),) %>% 
      ungroup() %>% 
      filter(country != "United States" & country != "X17"& country != "Puerto Rico" & country != "Northern Mariana Islands" &
               country != "Virgin Islands" & country != "Guam") %>% 
      rename(region = country)
  }
  
  