library(shiny)
library(shinydashboard)
library(graphics)
library(mapplots)
library(data.table)
library(geo)
library(shapefiles)
library(lattice)
library(latticeExtra)
library(ggplot2)
library(reshape)
library(reshape2)
library(plyr)
library(memisc)
library(xlsx)

shinyUI<- fluidPage(
  tabsetPanel(
    tabPanel("Metadata", 
             fluidRow(
               column(8, tags$h1("Methodology and caveats of Landings data by ICES rectangle for all UK registered vessels")
               ),
               column(4,
                      tags$img( height = 220, width = 277, src = "/srv/shiny-server/for git/MMO_logo2.jpg")
               )
             ),
             fluidRow(
{column(10,
        box(width = 10, title = "MMO Interactive Mapping App"
            
        )
)}
             ),
fluidRow(
  column(10,
         box( width = 10, title = "Abstract and Lineage", 
{
  div(style = "overflow-y:scroll; max-height: 600px", 
      textOutput("text1",),   
      tags$a(href="http://www.fao.org/fishery/topic/1617/en", "http://www.fao.org/fishery/topic/1617/en"),
      tags$p(),
      textOutput("text2")
      
  )
}
         )
  )
)
    ),
tabPanel("Maps",
         fluidRow(
           column(8, tags$h1("Landings data by ICES rectangle Map by selection"),
                  
                  uiOutput("selectMapSpecies"),                      
                  
                  actionButton(inputId = "mapGo", label = "Update")
           ),
           column(4,
                  tags$img( height = 220, width = 277, src = "/srv/shiny-server/for git/MMO_logo2.jpg")
           )
         ),
         fluidRow(
           box( width = 12, title = "Estimated Quantity Landed by ICES Rectangle"     
                , plotOutput("Map_q", width = 1600, height= 400 ))
         ),
         fluidRow(
           box( width = 12, title = "Estimated Value Landed by ICES Rectangle"     
                , plotOutput("Map_v", width = 1600, height= 400 ))
         ),
         fluidRow( 
           column(12,
                  downloadButton("downloadData", "Click to download .txt map data"),
                  tags$br(),
                  tags$hr()
                  
           )
         )
)

  )
)
