header <- dashboardHeader(
    title = "Job Board"
)

body <- dashboardBody(
    fluidRow(
        column(width = 9,
               box(width = NULL, solidHeader = TRUE,
                   leafletOutput("map", height = 500)
               ),
               box(width = NULL,
                   DT::dataTableOutput("jobPositionsTable"),
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
                   actionButton("go", "Search"),
               ),
               box(width = NULL, status = "warning",
                   plotOutput("plot1"),
                   plotOutput("plot2"),
                   plotOutput("plot3"),
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