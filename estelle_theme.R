

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