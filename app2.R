require(dplyr)
require(curl)
require(tidyr)
require(stringr)
require(rvest)
require(shiny)
require(leaflet)
require(ggplot2)
require(shinydashboard)
require(DT)
require(scales)
require(shinycssloaders)
require(doSNOW)
require(foreach)
coords <- read.csv('https://raw.githubusercontent.com/willoutcault/608_final/master/us_cities.csv', TRUE, ",")
#source("https://raw.githubusercontent.com/willoutcault/608_final/master/scrapingtools2.R")

header <- dashboardHeader(
  title = "Job Board"
)

body <- dashboardBody(
  includeCSS("https://raw.githubusercontent.com/willoutcault/608_final/master/styles.css"),
  fluidRow(
    column(width = 9,
           box(width = NULL, solidHeader = TRUE,
               leafletOutput("map", height = 471) %>% withSpinner(color="#0dc5c1")
           ),
           box(width = NULL,
               DT::dataTableOutput("jobPositionsTable")
           )
    ),
    column(width = 3,
           box(width = NULL, status = "warning",
               textInput("job", label= NULL, value="", placeholder="Job Title, Skill, Industry"),
               
               p(
                 class = "text-muted",
                 paste("Ex. Data Scientist"
                 )
               ),
               textInput("location", label= NULL, value="", placeholder="City, State"),
               
               p(
                 class = "text-muted",
                 paste("Ex. Albany, NY"
                 )
               ),
               actionButton("go", "Search")
           ),
           box(width = NULL, status = "warning",
               plotOutput("plot1",  height = "50%"),
               plotOutput("plot2",  height = "50%"),
               plotOutput("plot3",  height = "50%"),
               p(class = "text-muted"
               )
           )
    )
  )
)

ui <- dashboardPage(
  header,
  dashboardSidebar(disable = TRUE),
  body
)


server <- function(input,output){
  
  
  #skills and details
  
  
  jobdetails <- eventReactive(input$go,{
    detailscrape(input$job, input$location)
  })
  
  
  output$plot1<- renderPlot({
    
    ggplot(count_skills(jobdetails())[1:10,], aes(x=reorder(skills, n),y=(n/sum(n)), label="count")) +
      geom_bar(stat='identity', width=.5, fill = "lightblue", color = "darkblue") +
      scale_fill_manual(name="Data Analyst Skills") + 
      coord_flip()+
      labs(title="Top 10 Skills", 
           subtitle="Based Off % Demand") +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            text = element_text(size=13)) +
      scale_y_continuous(labels = scales::percent) +
      scale_x_discrete(position = "left")
    
    
  }, height = 198, width = 425)
  
  output$plot2<- renderPlot({
    
    ggplot(count_wages(jobdetails()), aes(x=salary))+
      geom_density(color="darkblue", fill="lightblue")+
      geom_vline(aes(xintercept=mean(salary)),color="blue", linetype="dashed", size=1)+
      labs(title="Salary Distribution") +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks.y = element_blank()) +
      scale_x_continuous(labels = scales::dollar_format(prefix="$"))
    
    
  }, height = 198, width = 425)
  
  output$plot3<- renderPlot({
    
    ggplot(count_location(jobdetails())[1:10,], aes(x=reorder(Location, n),y=(n/sum(n)), label="count")) +
      geom_bar(stat='identity', width=.5, fill = "lightblue", color = "darkblue")  +
      scale_fill_manual(name="Cities Hiring") + 
      coord_flip()+
      labs(title="Top 10 Cities", 
           subtitle="Based Off Total Open Positions") +
      theme(axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            text = element_text(size=13)) +
      scale_y_continuous(labels = scales::percent) +
      scale_x_discrete(position = "left")
    
  }, height = 198, width =425)
  
  output$map <- renderLeaflet({
    df2 <- count_skills_by_loc(jobdetails())
    coordsdf <- leaflet_points(jobdetails(),df2, coords)
    pal <- colorFactor(
      palette = c('gray', 'orange', 'blue', 'purple'),
      domain = coordsdf$type
    )
    leaflet(coordsdf) %>%
      addTiles() %>%
      fitBounds(~min(lng), ~min(lat), ~max(lng), ~max(lat)) %>% 
      addProviderTiles(providers$Esri.WorldTopoMap) %>% 
      addCircles(lat = ~lat,
                 lng = ~lng,
                 weight = 2,
                 popup = paste("<b>Job Count: </b>",coordsdf$Job_Count,"<br>",
                               "<b>City: </b>",coordsdf$Location,"<br>", 
                               "<b>Salary: </b>",coordsdf$Salary, "<br>",
                               "<b>Skills: </b>", coordsdf$Skills),
                 radius = ~ sqrt(Job_Count)*1250,
                 color = ~pal(type))
    
  })
  
  output$jobPositionsTable <- renderDataTable({
    positions <- select(jobdetails(), Title, Company, Location, Salary, urls)
    positions <- cbind("(index)" = 1:nrow(positions), positions)
    positions$Salary <- paste0("$", formatC(as.numeric(positions$Salary), format="f", digits=2, big.mark=","))
    positions$Salary[positions$Salary=="$ NA"] <- 'N/A'
    datatable(positions, options = list(paging = FALSE, scrollY = "225px"), rownames = FALSE)
  })
  
  
  
  
  
  closeAllConnections()
}

shinyApp(ui = ui, server = server)
