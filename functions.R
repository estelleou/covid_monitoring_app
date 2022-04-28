#functions for manupulating data -------------------

hotspot <- function(data_set, region, ranking_type) {
  
  if (ranking_type=="deaths") {
    
    hotspot <- 
      data_set %>%
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
      data_set %>%
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
cases_hotspot_visuals <- function(data_set, region) {
  
  ranked_data <- 
    hotspot(data_set, region, "cases") %>% 
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
      plot.subtitle= element_text(size = 10),
      plot.title = element_text(size = 12),
      axis.text = element_text(face = "bold", size = 12),
      axis.text.y = element_text(face = "bold", size = 12,
                                 color = as.vector((rev(ifelse(ranked_data$avg_chg_weeklycases >= 0, "red", "black"))))))
}

#visual ranking of hotspots
death_hotspot_visuals <- function(data_set, region) {
  
  ranked_data <- 
    hotspot(data_set, region, "deaths") %>% 
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
      plot.subtitle= element_text(size = 10),
      plot.title = element_text(size = 12),
      axis.text = element_text(face = "bold", size = 12),
      axis.text.y = element_text(face = "bold", size = 12,
                                 color = as.vector((rev(ifelse(ranked_data$avg_chg_weeklydeaths >= 0, "red", "black"))))))
}


#country cases and death time-series charts by region -------------------------


top_5_country_ranked_by_cases <- function(data, region){
  ranked_data <-
    data %>%
    filter(country %in% region) %>%
    filter(date == last(date)) %>%
    group_by(new_cases_avg_per_pop,country) %>%
    arrange(new_cases_avg_per_pop) %>%
    ungroup() %>% 
    slice_tail(n=5)

  ranked_list <-
    unique(ranked_data$country)
  
  data%>% 
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
          plot.title = element_text(size = 15, vjust = -1, color = "black"),
          plot.subtitle = element_text(size = 15),
          legend.position = "none") +
    coord_cartesian(clip = "off")
  
}


top_5_country_ranked_by_deaths <- function(data, region){
  
  ranked_data <-
    data %>%
    select(date, country, new_deaths_avg_per_pop) %>% 
    filter(country %in% region) %>%
    filter(date == last(date)) %>%
    group_by(new_deaths_avg_per_pop,country) %>%
    arrange(new_deaths_avg_per_pop) %>%
    ungroup() %>% 
    slice_tail(n=5)
  
  ranked_list <-
    unique(ranked_data$country)
  
  data%>% 
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
          plot.title = element_text(size = 15, vjust = -1, color = "black"),
          plot.subtitle = element_text(size = 15),
          legend.position = "none") +
    coord_cartesian(clip = "off")
  
}


#average mobility charts with dynamic colors for when cases are increasing 

 mobility_case_chart<- function(data, country_name) {
  
  
  acceleration <- 
    data %>% 
    select(date, x_axis_date, roll_index, avg_chg_weeklycases) %>% 
    mutate(roll_index_1 = ifelse(avg_chg_weeklycases>=0, roll_index, 0))
  
  deceleration <- 
    data %>% 
    select(date, x_axis_date, roll_index, avg_chg_weeklycases) %>% 
    mutate(roll_index_2 = ifelse(avg_chg_weeklycases<=0, roll_index, 0))
  
  
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
      labs(x = paste0(year(data$date)), 
           y = "") +
      estelle_theme() +
      theme(plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"), 
            plot.title = element_text(size = 13, vjust = -1),
            plot.subtitle = element_text(size = 13, vjust = -1),
            axis.text = element_text(size = 13), 
            axis.title = element_text(size = 13))
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
         plot.title = element_text(size = 13, vjust = -1, color = "#993333", 
                                   face = "bold",
                                   margin = margin(0,0,0.3,0, "cm")),
         legend.text = element_text(size = 13),
         plot.subtitle = element_text(size = 12, vjust = -1, color = "#778088",
                                      face = "bold",
                                      margin = margin(0,0,0.5,0, "cm")),
         axis.text = element_text(size = 13),
         axis.text.x = element_text(angle = 45, hjust = 1),
         axis.title = element_text(size = 13), 
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
          plot.title = element_text(size = 13, vjust = -1, color = "#993333", 
                                    face = "bold",
                                    margin = margin(0,0,0.3,0, "cm")),
          legend.text = element_text(size = 13),
          plot.subtitle = element_text(size = 13, vjust = -1, color = "#778088",
                                       face = "bold",
                                       margin = margin(0,0,0.5,0, "cm")),
          axis.text = element_text(size = 13),
          axis.text.x = element_text(angle = 45, hjust = 1),
          axis.title = element_text(size = 13), 
          legend.position = "top") +
    guides(fill = guide_legend(nrow = 1))
}

  

  