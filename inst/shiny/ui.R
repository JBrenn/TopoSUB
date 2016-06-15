ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(width=2, 
                 fluid = T, position = "left",
                 selectInput("variable", label = h4("Variable"), choices = c("SWC_liq_50", "Evaptranspiration_mm_", "snow_water_equivalent_mm_", "Ptotal_mm_")),
                 selectInput("period", label = h4("Period"), 
                             choices = c( "baseline","diff_abs_per1","diff_abs_per2","diff_abs_per3","diff_perc_per1","diff_perc_per2",
                                          "diff_perc_per3","per1","per2","per3")), 
                 selectInput("map", label = h4("Map"),
                             choices = c("MAM","JJA","DJF","SON","VEG","YEAR")),
                 sliderInput("opacity", label = "Map opacity", min = 0, max = 1, value = 0.7),
                 actionButton("do", "Apply Changes")
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Mapping", leafletOutput("mymap", height = 800)),
        tabPanel("Statistics", 
                 plotOutput("plot1", width = 800, height = 800),
                 selectInput("attribute", label = h4("Attribute"),
                             choices = c("dem","slp","svf","asp"))) 
      )
      
    )
  ))
