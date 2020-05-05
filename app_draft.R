require(dplyr)
require(tidyr)
require(stringr)
require(rvest)
require(shiny)
require(leaflet)
require(shinythemes)
require(ggplot2)
library(shinyWidgets)
require(shinydashboard)
source("https://raw.githubusercontent.com/willoutcault/608_final/master/scrapingtools.R")
coords <- read.csv('https://raw.githubusercontent.com/willoutcault/608_final/master/us_cities.csv', TRUE, ",")



ui <- fluidPage(
    
    tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
                
    includeCSS("https://raw.githubusercontent.com/willoutcault/608_final/master/styles.css"),   
     
    headerPanel("Career Search"),
    
    fluidRow(column(5, align="center",                  
        textInput("job", label= NULL, value="", placeholder="Job Title, Skill, Industry"),
        textInput("location", label= NULL,value="", placeholder="City, State (i.e. 'Albany, NY')"),
        actionButton("go", "Search"),
        id = "search_bar", 
        class = "search_bar_panel")),
                
    leafletOutput("map"),

    absolutePanel(bottom = 10, left = 10, right = 10,
                  fluidRow(
                      column(4,
                             h4("Skills"),
                             plotOutput("plot1"),
                      ),
                      column(4,
                             h4("Wage"),
                             plotOutput("plot2"),
                      ),
                      column(4,
                             h4("Location"),
                             plotOutput("plot3"),
                      )
                  )
    )
)





server <- function(input,output){
    
    
    #skills and details
    
    jobskills <- eventReactive(input$go, {
        skillscrape(input$job, input$location)
    })
    
    
    jobdetails <- eventReactive(input$go,{
        detailscrape(input$job, input$location)
    })
    
    
    
    
    ## Plot1
    output$plot2<- renderPlot({

            ggplot(count_wages(jobdetails()), aes(x=salary))+
                geom_density(color="darkblue", fill="lightblue")+
                geom_vline(aes(xintercept=mean(salary)),color="blue", linetype="dashed", size=1)+
                xlab("Salary (thousands)")+
                theme(axis.title.y=element_blank(),
                      axis.text.y=element_blank(),
                      axis.ticks.y=element_blank())

        })
    ## Plot2
    output$plot3<- renderPlot({

            ggplot(count_location(jobdetails())[1:10,], aes(x=reorder(Location, n),y=n, label="count")) +
                geom_bar(stat='identity', width=.5, fill = "lightblue", color = "darkblue")  +
                scale_fill_manual(name="Cities Hiring") + 
                coord_flip()+
                xlab("City")+
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank())

        })
    ## Map
        output$map <- renderLeaflet({
            coordsdf <- count_location(jobdetails())
            coordsdf$lat <- get_lat(coords,coordsdf)
            coordsdf$lng <- get_lng(coords,coordsdf)
            leaflet(coordsdf) %>%
                addTiles() %>%
                addCircleMarkers(lat = ~lat, lng = ~lng)
        #output$map <- renderLeaflet({
            #coordsdf <- count_location(jobdetails())
            #coordsdf$lat <- get_lat(coords,coordsdf)
            #coordsdf$lng <- get_lng(coords,coordsdf)
            #leaflet(coordsdf) %>%
            #    addProviderTiles("CartoDB.Positron") %>%
            #    fitBounds(~min(lng), ~min(lat), ~max(lng), ~max(lat))
            #    addCircles(lng = ~lng,
            #               lat = ~lat,
            #               popup = paste("<b>Job Count: </b>",coordsdf$n,"<br>","<b>City: </b>",coordsdf$Location),
            #               weight = 1,
            #               radius = ~ sqrt(n) * 1000)
            
        })
    #Plot3
    output$plot1<- renderPlot({

            ggplot(count_skills(jobskills())[1:10,], aes(x=reorder(skills, n),y=n, label="count")) +
                geom_bar(stat='identity', width=.5, fill = "lightblue", color = "darkblue") +
                scale_fill_manual(name="Data Analyst Skills") + 
                coord_flip()+
                theme(axis.title.x=element_blank(),
                      axis.text.x=element_blank(),
                      axis.ticks.x=element_blank())+
                xlab("Skill")

        })
    
    closeAllConnections()
}


shinyApp(ui = ui, server = server)
