#Estelle's graph theme

library(tidyverse)
library(lubridate)

  
estelle_theme <- function(){
  
  font <- "Helvetica"
  
 theme(
  
    plot.title = element_text(family=font,
                                       size=14,
                                       face="bold",
                                       color="#336699"),

    plot.subtitle = element_text(family=font,
                                          size=12,
                                          margin=margin(9,0,9,0)),
    plot.caption = element_text(family=font, 
                                size = 12),
    
    legend.text.align = 0,
    legend.background = element_blank(),
    legend.title = element_blank(),
    legend.key = element_blank(),
    legend.text = element_text(family=font,
                                        size=13,
                                        color="#222222"),
    
    axis.text = element_text(family=font,
                                      size=13,
                                      color="#222222"),
    axis.text.x = element_text(margin=margin(5, b = 10)),
    # axis.ticks = element_blank(),
    axis.line = element_line(),

    panel.background = element_blank(),

    strip.background = element_rect(fill="white"),
    strip.text = element_text(size  = 13,  hjust = 0)
  )
}

colors <- 
  c("#FFCC33", "#0066CC")
  


