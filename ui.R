# Define UI for app that draws a histogram ----
shinyUI(fluidPage(
  
  theme = shinytheme("flatly"),
  
  # App title ----
  titlePanel("Covid Macro Monitor"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input:  ----
      tags$style(type='text/css', ".selectize-input { font-size: 18px; line-height: 18px;} 
                 .selectize-dropdown { font-size: 18px; line-height: 18px; }
                 .label {font-size: 19px; line-height: 19px"),
      selectInput(inputId = "type",
                  label = "Covid Metric",
                  list(`Cases` = "cases", `Death` = "deaths")
      ),
      width = 1
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Hotspots and Regional----
      h1("Covid Hotspots For the Week In Red: "),
      plotOutput(outputId = "hotspots", width = "1700px", height = "850px"),
      hr(),
      h1("Regional Covid Case and Death Levels"),
      plotOutput(outputId = "regional_ts", width = "1700px", height = "650px"),
      width = 11,
     )
    ),
  
  #Country Level Cases and Death Charts ---------------------------------
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ----
      selectInput(inputId = "region",
                  label = "Pick A Country Grouping",
                  list(`Emerging Markets` = "em", `Developed Markets` = "dm",
                       `Latin America` = "latam", `European Union` = "eu",
                       `Asia` = "asia")
      ),
      width = 2,
      height = 1
      
    ),
    # Main panel for displaying outputs ----
   mainPanel(
      
      # Output: Country level by region----
      hr(),
      h1("Top 5 Countries with the Highest Cases/Deaths Per Capita within Country Grouping"),
      plotOutput(outputId = "covid_rankings", width = "1600px", height = "550px"),
      width = 10,
    )
  ),
  
  
  # # #REgional Average mobility charts ------------------------------------
  sidebarLayout(

    # Sidebar panel for inputs ----
    sidebarPanel(

      # Input: Slider for the number of bins ----
      selectInput(inputId = "country",
                  label = "Pick A Country",
                  country_list
      ),
      width = 2,

    ),
    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Country level by region----
      hr(),
      h1("Mobility Indicators"),
      plotOutput(outputId = "mobility_throughout_the_years", width = "1500px", height = "800px"),
      hr(),
      h1("United States: Change in Average Mobility* and Covid Cases Within States"),
      plotOutput(outputId = "mobility_within_state", width = "1500px", height = "800px"),
      width = 10,
        )
    )
  
  
)
)
