server <- function(input, output, session) {
  
  observeEvent(input$do, {
    
    #   # load variable names
    #   variable <- reactive({
    #     variable <- dir("OUTperiods", recursive = F)
    #     return(variable)
    #   })
    # 
    #   # reactive variable input
    #   observe({
    #     updateSelectInput(session, "variable",
    #                       choices = variable)
    #     })
    
    # landuse map
    
    output$mymap <- renderLeaflet({
      
      if (input$map=="YEAR") map_name <- "period" else map_name <- input$map
      
      # read maps
      # map
      fun <- raster(x = file.path("OUTperiods",input$variable,input$period,paste(map_name,"_fun.tif",sep="")))
      
      #colors
      pal_fun <- colorNumeric(c('#ef8a62','#f7f7f7','#67a9cf'), values(fun), na.color = "transparent")
      #pal_fun <- colorNumeric(c('#a6611a','#dfc27d','#f5f5f5','#80cdc1','#018571'), values(fun), na.color = "transparent")
      
      if (input$period=="baseline" | input$period=="per1" | input$period=="per2" | input$period=="per3" ) {
        
        # mam
        sd <- raster(x = file.path("OUTperiods",input$variable,input$period,paste(map_name,"_sd.tif",sep="")))
        
        pal_sd <- colorNumeric(c('#ef8a62','#f7f7f7','#67a9cf'),values(sd), na.color = "transparent")
        #pal_sd <- colorNumeric(c('#a6611a','#dfc27d','#f5f5f5','#80cdc1','#018571'),values(sd), na.color = "transparent")
        
        leaflet() %>%
          addTiles(group = "Tiles", options = providerTileOptions(opacity = 0.40)) %>%
          addProviderTiles("Esri.WorldShadedRelief", group = "Relief", options = providerTileOptions(opacity = 0.5)) %>%
          addProviderTiles("Esri.WorldImagery", group = "Image", options = providerTileOptions(opacity = 0.5)) %>%
          #map fun
          addRasterImage(fun, colors = pal_fun, opacity = input$opacity, group = "fun") %>%
          #map sd
          addRasterImage(sd, colors = pal_sd, opacity = input$opacity, group = "sd") %>%
          # Legends
          addLegend(pal = pal_fun, values = values(fun), title = paste(input$variable,"fun")) %>%
          addLegend(pal = pal_sd,  values = values(sd), title = paste(input$variable,"sd")) %>%
          addLayersControl(baseGroups = c("Tiles","Terrain"),
                           overlayGroups = c("fun","sd"), options = layersControlOptions(collapsed = FALSE))
      } else {
        leaflet() %>%
          addTiles(group = "Tiles", options = providerTileOptions(opacity = 0.40)) %>%
          addProviderTiles("Esri.WorldShadedRelief", group = "Relief", options = providerTileOptions(opacity = 0.5)) %>%
          addProviderTiles("Esri.WorldImagery", group = "Image", options = providerTileOptions(opacity = 0.5)) %>%
          #map fun
          addRasterImage(fun, colors = pal_fun, opacity = input$opacity, group = "fun") %>%
          # Legends
          addLegend(pal = pal_fun, values = values(fun), title = paste(input$variable,"fun")) %>%
          addLayersControl(baseGroups = c("Tiles","Terrain"),
                           overlayGroups = c("fun"), options = layersControlOptions(collapsed = FALSE))
      }
      
    })
    
    # read listpoints and process
    listpoints <- read.csv("listpoints.txt")
    listpoints$soil <- ifelse(listpoints$soil==9 | listpoints$soil==14 | listpoints$soil==15, 1, listpoints$soil)
    
    listpoints$soil <- ifelse(listpoints$soil==6 | listpoints$soil==7 | listpoints$soil==8 | listpoints$soil==10 | 
                                listpoints$soil==11 | listpoints$soil==13, 2, listpoints$soil)
    
    listpoints$soil <- ifelse(listpoints$soil==4 | listpoints$soil==5 | listpoints$soil==12, 3, listpoints$soil)
    
    listpoints$soil_fac <- factor(listpoints$soil, labels = c("Braunerde","Ranker","Other"))
    landcover_chr <- c("Urban","Forest","Grassland(dry)/Pastures","Meadows","Rocks","Bare soil","Larch Meadows",
                       "Agriculture","Glacier/Snow","Lake/River/Bog")[sort(unique(listpoints$landcover))]
    listpoints$landcover_fac <- factor(listpoints$landcover, labels = landcover_chr)
    
    output$plot1 <- renderPlot({
      # read csv monthly average
      month_avg <- fread(file.path("OUTcsv",input$variable,"average_month.csv"), header = T)
      setkey(month_avg, Index)
      # gather table, join month averages and listpoints and filter
      month_avg <-  
        month_avg  %>% 
        gather("id", "Value", 2:dim(month_avg)[2], factor_key = TRUE)  %>%
        mutate(id = as.integer(id))  %>% 
        left_join(listpoints, "id")  %>% 
        filter(landcover == 2 | landcover == 3 | landcover == 4 | landcover == 6 | landcover == 7)  %>% 
        separate(Index, c("m", "y")) %>% 
        mutate(season = ifelse(m=="Dez" | m=="Jan" | m=="Feb", 1, 4))  %>% 
        mutate(season = ifelse(m=="Mär" | m=="Apr" | m=="Mai", 2, season))  %>% 
        mutate(season = ifelse(m=="Jun" | m=="Jul" | m=="Aug", 3, season))  %>% 
        mutate(season = factor(season, labels = c("DJF","MAM","JJA","SON")))
      
      if(input$map != "YEAR" & input$map != "VEG") {
        month_avg <-
          month_avg  %>% 
          filter(season==input$map)
      }     
      
      # filter time frame or calculate differences
      
      if(input$period == "baseline") timeframe <- c(1980,2009)
      if(input$period == "per1") timeframe <- c(2020,2049)
      if(input$period == "per2") timeframe <- c(2045,2074)
      if(input$period == "per3") timeframe <- c(2070,2099)
      
      month_avg <-
        month_avg  %>% 
        filter(y >= timeframe[1] & y <= timeframe[2])
      
      # ggplot
      input$attribute
      
      if (input$map != "YEAR" & input$map != "VEG" ) {
        gg <- ggplot(month_avg, aes_string(y = "Value", x =  input$attribute)) +
          geom_jitter(alpha = 0.01, color = "black") + 
          geom_smooth() + 
          facet_grid(landcover_fac ~ soil_fac)
      } else {
        gg <- ggplot(month_avg, aes_string(y = "Value", x = input$attribute, color = "season", group = "season")) +
          geom_jitter(alpha = 0.01) + 
          geom_smooth() + 
          facet_grid(landcover_fac ~ soil_fac)
      }      
      
      gg
      
    })
    
  })
}