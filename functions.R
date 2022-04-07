#functions for manupulating data -------------------

hotspot <- function(region, ranking_type) {
  
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
