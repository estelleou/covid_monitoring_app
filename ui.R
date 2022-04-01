# Define UI for app that draws a histogram ----
shinyUI(fluidPage(
  
  theme = shinytheme("flatly"),
  
  # App title ----
  titlePanel("Covid Macro Monitor"),
  
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
      
      # Output: Hotspots and Regional----
      h1("Covid Hotspots For the Week"),
      plotOutput(outputId = "hotspots", width = "1600px", height = "850px"),
      br(),
      h1("Regional Covid and Death Levels"),
      plotOutput(outputId = "regional_ts", width = "1600px", height = "550px"),
      width = 11,
     )
    
   
    ),
  
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      selectInput(inputId = "region",
                  label = "Regional",
                  list(`Emerging Markets` = "em", `Developed Markets` = "dm",
                       `Latin America` = "latam", `European Union` = "eu",
                       `Asia` = "asia", `Global` = 'global')
      ),
      width = 1,
      height = 1
      
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Country level by region----
      h1("Country Covid Time-Series Data"),
     
      
      width = 11,
    )
  )
  
  
  
  )
)
