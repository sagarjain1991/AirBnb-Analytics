shinyUI(
  navbarPage(
    title = "Airbnb Analytics",
    id = "nav",
    theme = shinytheme("flatly"),
    tabPanel("Map",
             div(class="outer", tags$head( includeCSS("style.css"), includeScript("gomap.js")),

          leafletOutput("map",width="100%", height="100%"),
          absolutePanel(id = "controls", class = "panel panel-default", fixed = FALSE,
              draggable = TRUE, top = 150, left =35 , right ="auto" , bottom = "auto",
              width = 280, height = "auto",
                      h2(img(src = "airbnb.png", height =40), "Find Accomodation:"),
                      h6( "filter:"),
                      checkboxGroupInput("room", h4("Room type:"), choices = room, selected = room),
                      sliderInput("price", h4("Price"), min = 1, max = 511, value = c(1, 511)),
                      sliderInput("review",h4("Num of Reviews"), min = 0, max = 257, value = c(0, 257)),
                      sliderInput("rating",h4("Scores Rating"), min = 0, max = 100, value = c(0, 100))
             )
            )),
    
        tabPanel("Listings",
                 titlePanel("Airbnb Listings"),
                 br('Analytics of listings based on the neighborhood.'),
                 br('Using this we can observe the trend in the room type in various neighborhood of the city.'),
                 fluidRow(
				   column(1,
						  br(),
						  br(),
						  checkboxGroupInput("neighbour", h4("Select Neighborhoods:"), choices = neighbour, selected = neighbour)
				   ),
                   column(3,
                          br(),
                          br(),
                          br('filter'),
                          selectInput("listformat", h4("Showing as:"), choices = c("Count","Percentage")),
                          sliderInput("listrating", h4("Scores Rating"), min = 0, max = 100, value = c(0, 100)),
                          br(),
                          br(),
                          htmlOutput("activelist", width=500,height=250),
                          br()
  
                 ),
                 column(3, 
                        br(),
                        br(),
                        br('filter'),
                        selectInput("listformatprice", h4("Showing as:"), choices = c("Count","Percentage")),
                        sliderInput("listratingprice", h4("Price"), min = 1, max = 511, value = c(1, 511)),
                        br(),
                        br(),
                        htmlOutput("activelistprice", width=500,height=250),
                        br()
                        
                 ),
                 column(3, 
                          br(),
                          br(),
                          br('filter'),
                          selectInput("listformatreview", h4("Showing as:"), choices = c("Count","Percentage")),
                          sliderInput("listratingreview", h4("Num of Reviews"), min = 0, max = 257, value = c(0, 257)),
                          br(),
                          br(),
                          htmlOutput("activelistreview", width=500,height=250),
                          br()
                 )
                ),
                 titlePanel("Host List"),
                 fluidRow(
                   column(3,
                          br(),
                          sliderInput("hostn",h4("Top n Super hosts"), min = 0, 
                                      max = 200, value = 20),
                          dateRangeInput('hostt',
                                         h4("Host Since:"),
                                         start = "2008-06-26", end = "2019-02-10",
                                         min = "2008-6-26", max = "2019-02-10",
                                         separator = " to ", format = "yy/mm/dd",
                                         startview = 'week',  weekstart = 1)
                          
                   ),
                   column(9,
                          DT::dataTableOutput("ziptable")
                   )
                 )
                 ),
  
    
          tabPanel("Trend",    
                     titlePanel("Trend"),
                   fluidRow(
                     column(2,
                     br('The Popularity trend of Airbnb bookings are plotted using the review count per day as our metrics.'),
                     br('We can zoom in the trend using the slider provided on the graph to see the magnified trend in particular period of time, 
                        to observe seasonality, peaks during weekends and so on.'),
                     br('Also in the box above we can specify the Moving Average Window for the chart')
                     ),
                     column(10,
                     dygraphOutput("dygraph")
                     )
                     ),
                   titlePanel("Trend Analysis"),
                   fluidRow(
                     br(),
                     br(),
                     column(3,selectInput("tm", h4("Showing data by: "), choices = choice, selected = "Year"),
                              dateRangeInput('tr', h4("Select Date Range:"), start = "2008-10-06", end = "2019-02-10", min = "2008-10-06", max = "2019-02-10",
                              separator = " to ", format = "yy/mm/dd", startview = 'week',  weekstart = 1)
                            ),
                     column(7,htmlOutput("geoChart"))
                   )
                   ),
  
        tabPanel("Text Analytics",
              sidebarLayout(
                sidebarPanel(
                  br(),
                  br(),
                  selectInput("boroughSelection", "Select a Neighbourhood",choices = boroughs, selected = "Bronx"),
                  hr(),
                  conditionalPanel(condition = "input.mytabs == 'panel1'",sliderInput("freqNo","Select Minimum Frequecy:",min = 100,  max = 5000, value = 1000)),
                  
                  conditionalPanel(
                    condition = "input.mytabs == 'panel2'",
                    checkboxInput("checkbox", "Filter by Top Percentage", FALSE),
                    uiOutput("conditionalInput"),
                    hr(),
                    uiOutput("color1"),
                    uiOutput("color2")
                  ),
                  
                  conditionalPanel(
                    condition = "input.mytabs == 'panel3'",
                    radioButtons("numTopic","Topic#",choices = c('listing', 'host','recommendation','experience','area','review','comment','transport','complaint','place'),selected = NULL,inline = FALSE),
                    hr(),
                    colourpicker::colourInput("bcol", "Bar colour", "#34a7e2", showColour = c("background")),
                    colourpicker::colourInput("lcol", "Line colour", "black", showColour = "background")
                  )
                ),
                
                mainPanel(
                  tabsetPanel(id = "mytabs",
                              tabPanel(title = "WordCloud",plotOutput("plot"), value = "panel1"),
                              tabPanel(title = "High Frequency Words",plotOutput("freqplot"), value = "panel2"),
                              tabPanel(title = "Topic Modelling",plotlyOutput("dtm"), value = "panel3")
                              
                  )
                )
              )
    )
    
    
    
            
    
))
          
