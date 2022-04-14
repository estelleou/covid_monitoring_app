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
          plot.title = element_text(size = 20, vjust = -1),
          plot.subtitle = element_text(size = 20, vjust = -1),
          axis.text = element_text(size = 20), 
          axis.title = element_text(size = 20))
 }
 
 
 #weekly change in mobility trends
 
 working_days = c("Monday", "Tuesday", "Wednesday", 
              "Thurday", "Friday")
 
 
 cntry_cleaned_mobility %>% 
   filter(country == "Germany") %>% 
   # filter(date <= "2022-01-01") %>% 
   filter(date > "2022-01-01") %>%
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
   ggplot() +
   geom_bar(aes(x= date, y = value, fill = name), stat = "identity", 
            position = "dodge") +
   geom_hline(yintercept = 0)+
   labs(x = " ", y = " ")+
   scale_x_discrete(labels = function(x) strftime(x, "%b %d")) +
   scale_fill_manual(values  = c("#cc9900", "#003333"))+
   estelle_theme() +
   theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), 
         plot.title = element_text(size = 10, vjust = -1),
         legend.text = element_text(size = 10),
         plot.subtitle = element_text(size = 10, vjust = -1),
         axis.text = element_text(size = 10),
         axis.text.x = element_text(angle = 45, hjust = 1),
         axis.title = element_text(size = 10), 
         legend.position = "top") +
   guides(fill = guide_legend(nrow = 1))
   
             
             

