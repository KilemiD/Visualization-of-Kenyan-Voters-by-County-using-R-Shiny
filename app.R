
library(shiny)
library(shinydashboard)
library(leaflet)
library(rgdal)
library(raster)
library(dplyr)
library(RColorBrewer)
library(plotly)
library(htmltools)
library(DT)
library(RColorBrewer)
library(readr)
library(reshape2)
library("tidyverse")
library(ggfittext)

#voters data
voter2=read_csv("voter2.csv")
tribe=read.csv("tribe.csv")
#loading county shapefile
county_shp2<-readOGR("Kenya_Counties_(080719).shp")
#merging the two files
county_shp2@data = data.frame(county_shp2@data, 
                              voter2[match(county_shp2@data$NAME_1, 
                                         voter2$County),])
# Define UI 
ui <- dashboardPage(
  skin = "purple",
  dashboardHeader(
    title = "2022 ELECTION DASHBOARD.", titleWidth = 500,
    tags$li(actionLink("LinkedIn", 
                       label = "", 
                       icon = icon("twitter"),
                       onclick = "window.open('https://twitter.com/dankilemi')"),
            class = "dropdown"),
    tags$li(actionLink("Facebook", 
                       label = "", 
                       icon = icon("facebook"),
                       onclick = "window.open('https://web.facebook.com/kilemi.kilemi.79/')"),
            class = "dropdown")
  ),#disable = TRUE
    dashboardSidebar(
      disable = TRUE
      
    ),
  
  dashboardBody(
    fluidRow(
      tabBox(
        title = NULL, width = 12,
        # The id lets us use input$tabset1 on the server to find the current tab
        id = "tabset1", height = "250px",
        tabPanel("Kenya's Voter Register",
                 fluidRow(
                   
                   valueBoxOutput(
                     "voter1"
                     
                   ),#end box
                   
                   valueBoxOutput(
                     "voter2"
                     
                   ), #end box
                   
                   valueBoxOutput(
                     "voter3"
                     
                   )
                   
                 ),
                 fluidRow(
                   column(width = 3,
                          selectInput(
                            inputId = "stats2",
                            label = "Select Indicator",
                            choices =c(
                              "Registered Voters"="Total",
                              "Gema Registered Voters"="Gema",
                              "Kalenjin"="Kalenjin",
                              "Luhya"="Luhya",
                              "Luo"="Luo",
                              "Kamba"="Luo",
                              "Kisii"="Kisii",
                              "Coast"="Coast",
                              "Somali"="Somali",
                              "Other"="Other"
                              
                            ),selected = "Total"
                            
                          ) 
                          
                          
                   ) #end column
                   
                   
                 ),
                 #end row
                 fluidRow(
                   column(
                     width = 6,
                     #box(
                     # title = "MAP",
                     # status = "primary",solidHeader = TRUE,
                     # width = NULL,height = 600,collapsible = TRUE,
                     leafletOutput("maps2",height = 500)
                     #)
                   ),#end column
                   
                   column(width = 3,
                          plotlyOutput("top2",height = 500)
                   ),
                   
                   column(width = 3,
                          plotlyOutput("bottom2",height = 500)
                   )
                   
                   
                   
                 )
                 
      )
      
    )
    )
  )
  )

    


# Define server logic required to draw a lineplot
server <- function(input, output,session) {
                                                     
  output$voter1<-renderValueBox({
    valueBox(
      format(sum(voter2[,input$stats2]),big.mark=",",scientific=FALSE), "REGISTERED VOTERS", icon = icon("info"),
      color = "purple"
      #red, yellow, aqua, blue, light-blue, green, navy, teal, olive, lime, 
      #orange, fuchsia, purple, maroon, black.
    )
  })
    output$voter2<-renderValueBox({
      valueBox(tribe%>%
                 filter(Tribe == input$stats2) %>%
                 summarise("Mean" = paste0(format(round(mean(Average_Turnout),digits=0),big.mark=",",
                                           scientific=FALSE),"%")),
               "AVERAGE TURNOUT",icon=icon('info'),color="purple"
      )
    })
    
    output$voter3<-renderValueBox({
      valueBox(tribe%>%
                 filter(Tribe == input$stats2) %>%
                 summarise("Mean" = format(round(mean(Projected_Voters),digits=0),big.mark=",",
                                           scientific=FALSE)),
               "PROJECTED VOTERS ",icon=icon('info'),color="purple"
      )
    })
    
    

  
  #rendering the basemap for voters
  output$maps2<-renderLeaflet(
    leaflet(county_shp2) %>%
      setView(lng=37.9083,lat=0.1769,zoom = 6) %>%
      addPolygons(
        color = ~pal4(Total),
        smoothFactor = 0.5,
        weight = 2, opacity = 1.0,
        fillOpacity = 1.0,
        highlightOptions = highlightOptions(
          weight = 1,
          color = "blue",
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = paste(
          "<strong>County:</strong>",county_shp2$NAME_1,
          "<br>",
          "<strong>Registered Voters:</strong>",county_shp2$Total
          
        ) %>% lapply(htmltools::HTML),
        labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                  padding = "3px 8px"), 
                                     textsize = "13px", direction = "auto"),
        
        popup = ~paste(
          "<strong>County:</strong>",NAME_1,
          "<br>",
          "<strong>Registered Voters:</strong>",Total
          
        )
        
      ) %>%
      addLegend(title = "Registered Voters",
                pal = pal4, values = county_shp2$Total, opacity = 1)
    
  )
  
  
  
  #color functions
  #population legend
  #palpop<-colorBin("YlOrBr", county_shp2$Popultn)
  
  #male population
  #pal2<-colorBin("YlOrBr", county_shp2$Ml_Pplt)
  
  #female population
  #pal3<-colorBin("YlOrBr", county_shp2$Fml_Ppl)
  
  #registered voters
  risk.bins <-c(0,150000,300000,500000,750000, 1000000, 1500000,2600000)
  risk.pal <- colorBin( "plasma", county_shp2$Total,
                        bins=risk.bins, na.color = "#aaff56")
  
  
  pal4<-colorBin("YlOrBr",county_shp2$Total)
  
  #ethnic voters by county gema
  pal5<-colorBin("YlOrBr", county_shp2$Gema)
  
  #ethnic voters by county kalenjin
  pal6<-colorBin("YlOrBr", county_shp2$Kalenjin)
  
  #ethnic voters by county luhya
  pal7<-colorBin("YlOrBr", county_shp2$Luhya)
  
  #ethnic voters by county luo
  pal8<-colorBin("YlOrBr", county_shp2$Luo)
  
  #ethnic voters by county kamba
  pal9<-colorBin("YlOrBr", county_shp2$Kamba)
  
  #ethnic voters by county kisii
  pal10<-colorBin("YlOrBr", county_shp2$Kisii)
  
  #ethnic voters by county coast
  pal11<-colorBin("YlOrBr", county_shp2$Coast)
  
  #ethnic voters by county somali
  pal12<-colorBin("YlOrBr", county_shp2$Somali)
  
  #ethnic voters by county other
  pal13<-colorBin("YlOrBr", county_shp2$Other)
  
  
  observe({
    proxy<-leafletProxy("maps2") %>% clearControls()
    if ("Total" %in% input$stats2){
      proxy %>%
        addPolygons(
          data = county_shp2,
          color =  ~pal4(Total),
          smoothFactor = 0.5,
          weight = 2,
          opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "brown",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>County:</strong>",county_shp2$NAME_1,
            "<br>",
            "<strong>Registered Voters:</strong>",county_shp2$Total
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          popup = ~paste(
            "<strong>County:</strong>",NAME_1,
            "<br>",
            "<strong>Registered Voters:</strong>",Total
            
          )
          
        ) %>%
        addLegend(title = "Registered Voters",
                  pal = pal4, values = county_shp2$TOtal, opacity = 1)
    }
    
    
    else  if ("Gema" %in% input$stats2){
      proxy %>%
        addPolygons(
          data = county_shp2,
          color =  ~pal5(Gema),
          smoothFactor = 0.5,
          weight = 2,
          opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "brown",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>County:</strong>",county_shp2$NAME_1,
            "<br>",
            "<strong>GEMA Registered Voters:</strong>",county_shp2$Gema
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          popup = ~paste(
            "<strong>County:</strong>",NAME_1,
            "<br>",
            "<strong>GEMA Registered Voters:</strong>",Gema
            
          )
          
        ) %>%
        addLegend(title = "GEMA Registered Voters",
                  pal = pal5, values = county_shp2$Gema, opacity = 1)
    }
    
    
    else if ("Kalenjin" %in% input$stats2){
      proxy %>%
        addPolygons(
          data = county_shp2,
          color =  ~pal6(Kalenjin),
          smoothFactor = 0.5,
          weight = 2,
          opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "brown",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>County:</strong>",county_shp2$NAME_1,
            "<br>",
            "<strong>Kalenjin:</strong>",county_shp2$Kalenjin
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          popup = ~paste(
            "<strong>County:</strong>",NAME_1,
            "<br>",
            "<strong>Kalenjin:</strong>",Kalenjin
            
          )
          
        ) %>%
        addLegend(title = "Kalenjin",
                  pal = pal6, values = county_shp2$Kalenjin, opacity = 1)
    }
    
    
    
    else if ("Luhya" %in% input$stats2){
      proxy %>%
        addPolygons(
          data = county_shp2,
          color =  ~pal7(Luhya),
          smoothFactor = 0.5,
          weight = 2,
          opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "brown",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>County:</strong>",county_shp2$NAME_1,
            "<br>",
            "<strong>Luhya:</strong>",county_shp2$Luhya
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          popup = ~paste(
            "<strong>County:</strong>",NAME_1,
            "<br>",
            "<strong>Luhya:</strong>",Luhya
            
          )
          
        ) %>%
        addLegend(title = "Luhya",
                  pal = pal7, values = county_shp2$Luhya, opacity = 1)
    }
    
    else if ("5" %in% input$stats2){
      proxy %>%
        addPolygons(
          data = county_shp2,
          color =  ~pal8(Luo),
          smoothFactor = 0.5,
          weight = 2,
          opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "brown",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>County:</strong>",county_shp2$NAME_1,
            "<br>",
            "<strong>Luo:</strong>",county_shp2$Luo
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          popup = ~paste(
            "<strong>County:</strong>",NAME_1,
            "<br>",
            "<strong>Luo:</strong>",Luo
            
          )
          
        ) %>%
        addLegend(title = "Luo",
                  pal = pal8, values = county_shp2$Luo, opacity = 1)
    }
    
    else if ("6" %in% input$stats2){
      proxy %>%
        addPolygons(
          data = county_shp2,
          color =  ~pal9(Kamba),
          smoothFactor = 0.5,
          weight = 2,
          opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "brown",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>County:</strong>",county_shp2$NAME_1,
            "<br>",
            "<strong>Kamba:</strong>",county_shp2$Kamba
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          popup = ~paste(
            "<strong>County:</strong>",NAME_1,
            "<br>",
            "<strong>Kamba:</strong>",Kamba
            
          )
          
        ) %>%
        addLegend(title = "Kamba",
                  pal = pal9, values = county_shp2$Kamba, opacity = 1)
    }
    
    else if ("7" %in% input$stats2){
      proxy %>%
        addPolygons(
          data = county_shp2,
          color =  ~pal10(Kisii),
          smoothFactor = 0.5,
          weight = 2,
          opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "brown",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>County:</strong>",county_shp2$NAME_1,
            "<br>",
            "<strong>Kisii:</strong>",county_shp2$Kisii
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          popup = ~paste(
            "<strong>County:</strong>",NAME_1,
            "<br>",
            "<strong>Kisii:</strong>",Kisii
            
          )
          
        ) %>%
        addLegend(title = "Kisii",
                  pal = pal10, values = county_shp2$Kisii, opacity = 1)
    }
    
    else if ("8" %in% input$stats2){
      proxy %>%
        addPolygons(
          data = county_shp2,
          color =  ~pal11(Coast),
          smoothFactor = 0.5,
          weight = 2,
          opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "brown",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>County:</strong>",county_shp2$NAME_1,
            "<br>",
            "<strong>Coast:</strong>",county_shp2$Coast
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          popup = ~paste(
            "<strong>County:</strong>",NAME_1,
            "<br>",
            "<strong>Coast:</strong>",Coast
            
          )
          
        ) %>%
        addLegend(title = "Coast",
                  pal = pal11, values = county_shp2$Coast, opacity = 1)
    }
    
    else if ("9" %in% input$stats2){
      proxy %>%
        addPolygons(
          data = county_shp2,
          color =  ~pal12(Somali),
          smoothFactor = 0.5,
          weight = 2,
          opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "brown",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>County:</strong>",county_shp2$NAME_1,
            "<br>",
            "<strong>Somali:</strong>",county_shp2$Somali
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          popup = ~paste(
            "<strong>County:</strong>",NAME_1,
            "<br>",
            "<strong>Somali:</strong>",Somali
            
          )
          
        ) %>%
        addLegend(title = "Somali",
                  pal = pal12, values = county_shp2$Somali, opacity = 1)
    }
    
    else if ("10" %in% input$stats2){
      proxy %>%
        addPolygons(
          data = county_shp2,
          color =  ~pal13(Other),
          smoothFactor = 0.5,
          weight = 2,
          opacity = 1.0,
          fillOpacity = 1.0,
          highlightOptions = highlightOptions(
            weight = 1,
            color = "brown",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = paste(
            "<strong>County:</strong>",county_shp2$NAME_1,
            "<br>",
            "<strong>Other:</strong>",county_shp2$Other
            
          ) %>% lapply(htmltools::HTML),
          labelOptions = labelOptions( style = list("font-weight" = "normal", 
                                                    padding = "3px 8px"), 
                                       textsize = "13px", direction = "auto"),
          popup = ~paste(
            "<strong>County:</strong>",NAME_1,
            "<br>",
            "<strong>Other:</strong>",Other
            
          )
          
        ) %>%
        addLegend(title = "Other",
                  pal = pal13, values = county_shp2$Other, opacity = 1)
    }
    
  })
  
  output$top2<-renderPlotly({
    if("Total" %in% input$stats2){
      voter2 %>% dplyr::select(County,Total) %>%
        arrange(desc(Total)) %>%
        top_n(5) %>% ggplot(aes(x=reorder(County,Total),y=Total))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Total,y =Total-150000),
                  position = position_dodge(width = 0.8))+
        labs(
          title = "Top 5 counties",
          y="Voters",
          x="County"
        )+
        coord_flip()
      
    }  
    
    else if("Gema" %in% input$stats2){
      voter2 %>% dplyr::select(County,Gema) %>%
        arrange(desc(Gema)) %>%
        top_n(5) %>% ggplot(aes(x=reorder(County,Gema),y=Gema))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Gema))+
        labs(
          title = "Top 5 counties",
          y="Gema",
          x="County"
        )+
        coord_flip()
      
    }  
    
    else if("Kalenjin" %in% input$stats2){
      voter2 %>% dplyr::select(County,Kalenjin) %>%
        arrange(desc(Kalenjin)) %>%
        top_n(5) %>% ggplot(aes(x=reorder(County,Kalenjin),y=Kalenjin))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Kalenjin))+
        labs(
          title = "Top 5 counties",
          y="Kalenjin",
          x="County"
        )+
        coord_flip()
    }  
    
    else if("Luhya" %in% input$stats2){
      voter2 %>% dplyr::select(County,Luhya) %>%
        arrange(desc(Luhya)) %>%
        top_n(5) %>% ggplot(aes(x=reorder(County,Luhya),y=Luhya))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Luhya))+
        labs(
          title = "Top 5 counties",
          y="Luhya",
          x="County"
        )+
        coord_flip()
    } 
    
    else if("Luo" %in% input$stats2){
      voter2 %>% dplyr::select(County,Luo) %>%
        arrange(desc(Luo)) %>%
        top_n(5) %>% ggplot(aes(x=reorder(County,Luo),y=Luo))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Luo))+
        labs(
          title = "Top 5 counties",
          y="Luo",
          x="County"
        )+
        coord_flip()
    } 
    
    else if("Kamba" %in% input$stats2){
      voter2 %>% dplyr::select(County,Kamba) %>%
        arrange(desc(Kamba)) %>%
        top_n(5) %>% ggplot(aes(x=reorder(County,Kamba),y=Kamba))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Kamba))+
        labs(
          title = "Top 5 counties",
          y="Kamba",
          x="County"
        )+
        coord_flip()
    }
    
    else if("Kisii" %in% input$stats2){
      voter2 %>% dplyr::select(County,Kisii) %>%
        arrange(desc(Kisii)) %>%
        top_n(5) %>% ggplot(aes(x=reorder(County,Kisii),y=Kisii))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Kisii))+
        labs(
          title = "Top 5 counties",
          y="Kisii",
          x="County"
        )+
        coord_flip()
    }
    
    else if("Coast" %in% input$stats2){
      voter2 %>% dplyr::select(County,Coast) %>%
        arrange(desc(Coast)) %>%
        top_n(5) %>% ggplot(aes(x=reorder(County,Coast),y=Coast))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Coast))+
        labs(
          title = "Top 5 counties",
          y="Coast",
          x="County"
        )+
        coord_flip()
    }
    
    else if("Somali" %in% input$stats2){
      voter2 %>% dplyr::select(County,Somali) %>%
        arrange(desc(Somali)) %>%
        top_n(5) %>% ggplot(aes(x=reorder(County,Somali),y=Somali))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Somali))+
        labs(
          title = "Top 5 counties",
          y="Somali",
          x="County"
        )+
        coord_flip()
    }
    
    else if("Other" %in% input$stats2){
      voter2 %>% dplyr::select(County,Other) %>%
        arrange(desc(Other)) %>%
        top_n(5) %>% ggplot(aes(x=reorder(County,Other),y=Other))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Other))+
        labs(
          title = "Top 5 counties",
          y="Other",
          x="County"
        )+
        coord_flip()
    }
  })
  
  output$bottom2<-renderPlotly({
    if("Total" %in% input$stats2){
      voter2 %>% dplyr::select(County,Total) %>%
        arrange(desc(Total)) %>%
        top_n(-5) %>% ggplot(aes(x=reorder(County,Total),y=Total))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Total))+
        labs(
          title = "Bottom 5 counties",
          y="Number of Registered Voters",
          x="County"
        )+
        coord_flip()
      
    }
    
    else if("Gema" %in% input$stats2){
      voter2 %>% dplyr::select(County,Gema) %>%
        arrange(desc(Gema)) %>%
        top_n(-5) %>% ggplot(aes(x=reorder(County,Gema),y=Gema))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Gema))+
        labs(
          title = "Bottom 5 counties",
          y="Gema Voters",
          x="County"
        )+
        coord_flip()
      
    }
    
    else if("Kalenjin" %in% input$stats2){
      voter2 %>% dplyr::select(County,Kalenjin) %>%
        arrange(desc(Kalenjin)) %>%
        top_n(-5) %>% ggplot(aes(x=reorder(County,Kalenjin),y=Kalenjin))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Kalenjin))+
        labs(
          title = "Bottom 5 counties",
          y="Kalenjin",
          x="County"
        )+
        coord_flip()
      
    }  
    
    else if("Luhya" %in% input$stats2){
      voter2 %>% dplyr::select(County,Luhya) %>%
        arrange(desc(Luhya)) %>%
        top_n(-5) %>% ggplot(aes(x=reorder(County,Luhya),y=Luhya))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Luhya))+
        labs(
          title = "Bottom 5 counties",
          y="Luhya",
          x="County"
        )+
        coord_flip()
      
    }  
    
    else if("Luo" %in% input$stats2){
      voter2 %>% dplyr::select(County,Luo) %>%
        arrange(desc(Luo)) %>%
        top_n(-5) %>% ggplot(aes(x=reorder(County,Luo),y=Luo))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Luo))+
        labs(
          title = "Bottom 5 counties",
          y="Luo",
          x="County"
        )+
        coord_flip()
    } 
    
    else if("Kamba" %in% input$stats2){
      voter2 %>% dplyr::select(County,Kamba) %>%
        arrange(desc(Kamba)) %>%
        top_n(-5) %>% ggplot(aes(x=reorder(County,Kamba),y=Kamba))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Kamba))+
        labs(
          title = "Bottom 5 counties",
          y="Kamba",
          x="County"
        )+
        coord_flip()
    } 
    
    else if("Kisii" %in% input$stats2){
      voter2 %>% dplyr::select(County,Kisii) %>%
        arrange(desc(Kisii)) %>%
        top_n(-5) %>% ggplot(aes(x=reorder(County,Kisii),y=Kisii))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Kisii))+
        labs(
          title = "Bottom 5 counties",
          y="Kisii",
          x="County"
        )+
        coord_flip()
    } 
    
    else if("Coast" %in% input$stats2){
      voter2 %>% dplyr::select(County,Coast) %>%
        arrange(desc(Coast)) %>%
        top_n(-5) %>% ggplot(aes(x=reorder(County,Coast),y=Coast))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Coast))+
        labs(
          title = "Bottom 5 counties",
          y="Coast",
          x="County"
        )+
        coord_flip()
    } 
    
    else if("Somali" %in% input$stats2){
      voter2 %>% dplyr::select(County,Somali) %>%
        arrange(desc(Somali)) %>%
        top_n(-5) %>% ggplot(aes(x=reorder(County,Somali),y=Somali))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Somali))+
        labs(
          title = "Bottom 5 counties",
          y="Somali",
          x="County"
        )+
        coord_flip()
    } 
    
    else if("Other" %in% input$stats2){
      voter2 %>% dplyr::select(County,Other) %>%
        arrange(desc(Other)) %>%
        top_n(-5) %>% ggplot(aes(x=reorder(County,Other),y=Other))+
        geom_col(fill="#FF6666")+
        geom_text(aes(label=Other))+
        labs(
          title = "Bottom 5 counties",
          y="Other",
          x="County"
        )+
        coord_flip()
    } 
  })

  
}

# Run the application 
shinyApp(ui = ui, server = server)

