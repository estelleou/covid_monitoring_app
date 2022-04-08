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
    theme(plot.margin = margin(0.1, 3, 0.1, 0, "cm"), 
          plot.title = element_text(size = 20, vjust = -1, color = "black"),
          plot.subtitle = element_text(size = 20),
          legend.position = "none") +
    coord_cartesian(clip = "off")
  
}


#average mobility charts with dynamic colors for when cases are increasing 

#cleaning for label position --------------
ranked_data <- 
  cntry_cleaned_mobility %>% 
  filter(country == "Germany") %>% 
  filter(date == last(date)) %>% 
  group_by(roll_index,country) %>%
  arrange(roll_index) %>%
  ungroup()

ranked_list <- 
  unique(ranked_data$country)
# -----------------------------------------



 mobility_case_chart<- function(data, country_name) {
  
     
  mobility_and_case_speed_data <- 
     data %>% 
    mutate(x_axis_date = update(date, year = 1)) %>% 
    mutate(roll_index = 100+roll_index) %>% 
    left_join(increases_in_deaths_and_cases_within_this_week)
    
  
  acceleration <- 
    mobility_and_case_speed_data %>% 
    select(date, x_axis_date, roll_index, avg_chg_weeklycases) %>% 
    mutate(roll_index = ifelse(avg_chg_weeklycases>=0, roll_index, 0))
  
  deceleration <- 
    mobility_and_case_speed_data %>% 
    select(date, x_axis_date, roll_index, avg_chg_weeklycases) %>% 
    mutate(roll_index = ifelse(avg_chg_weeklycases<=0, roll_index, 0))
    
  #reference values for y-axis
  y_axis_min_max_reference_data <- 
    cntry_cleaned_mobility %>% 
    filter(country == country_name) %>% 
    filter(!is.na(roll_index)) %>% 
    mutate(roll_index = 100+roll_index) %>% 
    select(roll_index)
  
  
    ggplot()+
    geom_line(data = acceleration, aes(x = x_axis_date, y = roll_index), 
              color = "#cc9900", size = 1.5) +
    geom_line(data = deceleration, aes(x = x_axis_date, y = roll_index),
              color = "#778088", size = 1.5) +
    scale_x_date(lim = c(as.Date("0000-12-31"), as.Date("0001-12-31")),
                 date_label = "%b", 
                 date_breaks = "2 months") +
    geom_hline(yintercept = 100, linetype = 2)+
    theme_classic() +
    labs(x = "", 
         y = "") +
    scale_y_continuous(lim = c(min(y_axis_min_max_reference_data$roll_index), 
                               max(y_axis_min_max_reference_data$roll_index)))+
    estelle_theme() +
    theme(plot.margin = margin(0.2, 0.5, 0.5, 0.5, "cm"), 
          plot.title = element_text(size = 15, vjust = -1, color = "black"),
          plot.subtitle = element_text(size = 15), 
          axis.text = element_text(size = 15))
 }
 
 
mobility_throughout_the_years <-  function(country_name) {
  

year_1 <- mobility_case_chart(cntry_cleaned_mobility %>%
                            filter(country == country_name) %>% 
                            filter(date < "2021-01-01"), country_name)

year_2 <- mobility_case_chart(cntry_cleaned_mobility %>%
                              filter(country == country_name) %>% 
                              filter(date >= "2021-01-01") %>% 
                              filter(date < "2022-01-01"), country_name )


year_3<- mobility_case_chart(cntry_cleaned_mobility %>%
                              filter(country == country_name) %>% 
                              filter(date >= "2022-01-01") %>% 
                              filter(date < "2023-01-01"), country_name ) 
  

grid.arrange(year_3, year_2, year_1, nrow = 3)

}
