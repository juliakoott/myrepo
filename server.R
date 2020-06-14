#serwer
library(shiny)
library(ggplot2)
library(coronavirus)

shinyServer(function(input, output) {

    #MAPA
    output$tekst1 <- renderText({
      tekst1 = paste(as.character(input$chosenDate),
                     as.character(class(input$chosenDate))
      )
    }
    
    )
    
    output$covMap <- renderLeaflet({
      initializeWorldMap()
    })
    
    
    renderDynamicMapElements = reactive({
      dynamicMapElements(input$chosenDate)
    })
    
    observe(
      {
        dynamicElements = renderDynamicMapElements()
        
        HTML_labels2map = dynamicElements$HTML_labels
        data2map_norm_radius = dynamicElements$data2map
        
        # 
        coordCols = c('latitude','longitude')
        mapDesignOpts = list(color =  '#2db300',
                             weight = 1.2)
        
        # '#00b3b3'; # '#5e94ae' ciemno lazurowy 
        
        
        styleCSS = list(
          "color" = '#2fb054',
          "box-shadow" = "3px 3px 5px 5px rgba(0,0,0,0.2)"
        )
        # "color" = '#2f94b0', "color" = '#5187a2', "color" = '#3988ae',
        
        labelOptionsList = labelOptions(textsize = "15px",style = styleCSS)
        
        
        # Dodanie kol do mapy
        leafletProxy(mapId = "covMap", data = data2map_norm_radius[,coordCols]) %>%
          clearMarkers() %>%
          addCircleMarkers(radius = data2map_norm_radius$confirmedRadius, 
                           weight = mapDesignOpts$weight, 
                           color = mapDesignOpts$color, 
                           label = HTML_labels2map, 
                           labelOptions = labelOptionsList)
        
        
      }
      
    )
#URL
    url <- a("World Health Organization", href="https://www.who.int/emergencies/diseases/novel-coronavirus-2019?gclid=CjwKCAjwlZf3BRABEiwA8Q0qqyA0AT6PqeRz4nxMeCfOv0GU-CD7HOlMNSaVakfqywOzM6bzIrmEohoCuLoQAvD_BwE")
    url2 <- a("Aktualne informacje rządowe", href="https://www.gov.pl/web/koronawirus")
    url3 <- a("Główny Inspektorat Sanitarny", href="https://gis.gov.pl/")
    
    output$tab <- renderUI({
      tagList("WHO link:", url)})
    output$tab2 <- renderUI({
      tagList("GOV link:", url2)})
    output$tab3 <- renderUI({
      tagList("GIS link:", url3)})
 #obrazek   
    output$IM1 <- renderImage({
      pfad <- "https://gis.gov.pl/wp-content/uploads/2018/04/mycie-r%C4%85k-kwadrat.png"
      list(src="rece.png",
           contentType = 'image/png',
           width = 400,
           height = 400,
           alt = "gis")
    }, deleteFile = F)
  #analiza  
    output$plot1<-renderPlot({
      ggplot(data = covid_PL, aes(x = date, y = confirmed)) +
        geom_point() +
        stat_smooth(method = "lm", col = "dodgerblue3") +
        theme(panel.background = element_rect(fill = "white"),
              axis.line.x=element_line(),
              axis.line.y=element_line()) +
        ggtitle("Linear Model Fitted to Data")
      
      #predykcja = data.frame(predict(model, covid_PL))
    })
 #ANALIZA 2   
    output$ploty<-renderPlot({
    if(input$death==TRUE)
      return(plot(x_deaths,y_deaths,xlab="Czas", ylab="l. ofiar"))
      else if(input$confirmed==TRUE)
        return(plot(x_infected,y_infected,xlab="Czas", ylab="l. zakażonych"))
      else if(input$recovered==TRUE)
        return(plot(x_recovered,y_recovered,xlab="Czas", ylab="l. wyleczonych"))
      else if(input$testy==TRUE)
        return(list(barplot(data_tests[,2], space=0, col=red),
               par(new=TRUE),
               barplot(data_tests[,3], space=0, col=blue),
               title("COVID19 | Polska | Testy-Zakazeni (sumaryczny)"),
               legend("topleft",c("Testy","Zarażeni"), cex=0.6, fill=c(red,blue)),
               grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")))
      })

    
    output$plotyDzien<-renderPlot({
      if(input$deathD==TRUE)
        return(plot(x_deaths_daily,y_deaths_daily,xlab="Czas", ylab="l. ofiar na dzień"))
      else if(input$confirmedD==TRUE)
        return(plot(x_infected_daily,y_infected_daily,xlab="Czas", ylab="l. zakażonych na dzień"))
      else if(input$recoveredD==TRUE)
        return(plot(x_recovered_daily,y_recovered_daily,xlab="Czas", ylab="l. wyleczonych na dzień"))
      else if(input$testyD==TRUE)
        return(list(barplot(data_tests_daily[,2], space=0, col=red),
               par(new=TRUE),
               barplot(data_tests_daily[,3], space=0, col=blue),
               title("COVID19 | Polska | Testy-Zakazeni (dzienny)"),
               legend("topleft",c("Testy","Zarażeni"), cex=0.6, fill=c(red,blue)),
               grid(nx = NULL, ny = NULL, col = "lightgray", lty = "dotted")))
    })
  ##  output$plot2<-renderPlot({
    #  ggplot(dat(),aes(x=date,y=num))+geom_point(colour='red')},height = 400,width = 600)
    
})

    
    
  


