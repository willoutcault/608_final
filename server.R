server <- function(input,output){
    
    
    #skills and details
    
    jobskills <- eventReactive(input$go, {
        skillscrape(input$job, input$location)
    })
    
    
    jobdetails <- eventReactive(input$go,{
        detailscrape(input$job, input$location)
    })
    
    
    
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
    
    output$plot2<- renderPlot({
        
        ggplot(count_wages(jobdetails()), aes(x=salary))+
            geom_density(color="darkblue", fill="lightblue")+
            geom_vline(aes(xintercept=mean(salary)),color="blue", linetype="dashed", size=1)+
            xlab("Salary (thousands)")+
            theme(axis.title.y=element_blank(),
                  axis.text.y=element_blank(),
                  axis.ticks.y=element_blank())
        
    })
    
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
    
    output$map <- renderLeaflet({
        coordsdf <- count_location(jobdetails())
        coordsdf$lat <- get_lat(coords,coordsdf)
        coordsdf$lng <- get_lng(coords,coordsdf)
        leaflet(coordsdf) %>%
            addTiles() %>%
            addCircles(lat = ~lat,
                       lng = ~lng,
                       weight = 1,
                       popup = paste("<b>Job Count: </b>",coordsdf$n,"<br>","<b>City: </b>",coordsdf$Location),
                       radius = ~ sqrt(n)*1000)
        
    })
    
    output$jobPositionsTable <- renderDataTable({
        positions <- select(jobdetails(), Title, Location, Salary, urls)
        positions <- cbind("(index)" = 1:nrow(positions), positions)
        positions$Salary <- paste0("$", formatC(as.numeric(positions$Salary), format="f", digits=2, big.mark=","))
        positions$Salary[positions$Salary=="$ NA"] <- 'N/A'
        datatable(positions, options = list(paging = FALSE, scrollY = "225px"), rownames = FALSE)
    })
    
    
    
    
    
    closeAllConnections()
}

