server <- function(input, output) {
## review
    reviewdata <- reactive({
                              df <- review %>%  filter(date>=input$tr[1], date<=input$tr[2]) %>% group_by_(input$tm) %>% summarise(.,count=n())
                          })
    
    reviewdate<-reactive({
                              df <- review %>% group_by(date) %>% summarise(.,count=n())
                              xt <- xts(x = df$count, order.by = df$date)
                        })
    
    rwd<-reactive({review_txt})

    output$dygraph <- renderDygraph({
      dygraph(reviewdate(),ylab = "Num. of Reviews / Day") %>%
        dyOptions(drawGrid = input$showgrid, stackedGraph = TRUE, colors = "grey")%>%
        dySeries("V1", label = "Num. of Reviews")%>%
        dyRangeSelector()%>%
        dyShading(from = "2008-1-1", to = "2020-1-1", color = "black") %>%
        dyRoller(rollPeriod = 1)
    })
    

wordcloud_rep <- repeatable(wordcloud)
output$wordcloud<- renderPlot({
                                wordcloud_rep(words = rwd()$word, freq = rwd()$X1, scale=c(5,1),
                                              min.freq = input$rfreq, 
                                              max.words=input$rmax,
                                              rot.per=0.2,
                                              random.order=F, 
                                              colors=brewer.pal(8, "Dark2"))
                              })

output$geoChart <- renderGvis({gvisLineChart(reviewdata(),options=list(legend="none",hAxis="{title:'Time'}",
                               vAxis="{title:'Number of Reviews'}",
                               series="[{color:'blue', targetAxisIndex: 0}]"))
                              })


## map
mapdata <- reactive({
  print(input$room)
    df <- map %>%
          filter(
             room_type %in% input$room,
             price >=input$price[1],
             price<=input$price[2],
             number_of_reviews >=input$review[1],
             number_of_reviews <=input$review[2],
             review_scores_rating>=input$rating[1],
             review_scores_rating<=input$rating[2])
  })

  
output$map <- renderLeaflet({
  map<-leaflet() %>%  setView(lng = -98, lat = 38, zoom = 5) %>% addProviderTiles("CartoDB.Positron") %>%
    addTiles(urlTemplate = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
             attribution = 'Maps by <a href="http://www.mapbox.com/">Mapbox</a>')
})


observe({
  leafletProxy("map", data = mapdata()) %>%
    clearMarkerClusters() %>%
    addMarkers(popup = ~paste(sep = "<br/>","Room Type:",room_type,
                              "Neighbourhood:",neighbourhood_group_cleansed,
                              "Price:",price,
                              "Minimum Nights:", minimum_nights,
                              "Num. of Reviews:",number_of_reviews,
                              "Review Rating:",review_scores_rating,
                              "Accommodates:",accommodates,
                              "Beds:",beds,
                              "Description: ",description), 
               clusterOptions = markerClusterOptions())
  
})
  
  
  
  
## listing
listdata <- reactive({
  df <- map %>%
    filter(neighbourhood_group_cleansed %in% input$neighbour,
	       review_scores_rating>=input$listrating[1],
           review_scores_rating<=input$listrating[2])%>%
    select(neighbourhood_group_cleansed,room_type)
})

listdata2<-reactive({
  df<- map %>%
    filter(neighbourhood_group_cleansed %in% input$neighbour,
	       review_scores_rating>=input$listrating[1],
           review_scores_rating<=input$listrating[2])%>%
    group_by(neighbourhood_group_cleansed,room_type) %>%
    tally  %>%
    group_by(neighbourhood_group_cleansed) %>%
    mutate(pct=(100*n)/sum(n))%>%
    select(neighbour=neighbourhood_group_cleansed,room_type,pct)
})

output$activelist<-renderGvis({
  if(input$listformat=="Count"){
    df<-table(listdata()$neighbourhood_group_cleansed,listdata()$room_type)
    df<-as.data.frame.matrix(df)
    df$neighbour<-rownames(df)}
  if(input$listformat=="Percentage") {
    df <- as.data.frame(with(listdata2(), tapply(pct, list(neighbour, room_type) , I)))
    df$neighbour<-rownames(df)
  }
  gvisColumnChart(df,"neighbour",c("Entire home/apt","Private room","Shared room"),
               options=list(colors= "['#58508d', '#bc5090','#ff6361']",
                            legend="bottom",
                            bar="{groupWidth:'90%'}",gvis.editor="Chart Type?",
                            width=500,height=250))
  
})
  




##active list price
listdataprice <- reactive({
  df <- map %>%
    filter(neighbourhood_group_cleansed %in% input$neighbour,
	       price>=input$listratingprice[1],
           price<=input$listratingprice[2])%>%
    select(neighbourhood_group_cleansed,room_type)
})

listdataprice2<-reactive({
  df<- map %>% filter(neighbourhood_group_cleansed %in% input$neighbour, price>=input$listratingprice[1], price<=input$listratingprice[2])%>%
    group_by(neighbourhood_group_cleansed,room_type) %>%
    tally  %>%
    group_by(neighbourhood_group_cleansed) %>%
    mutate(pct=(100*n)/sum(n))%>%
    select(neighbour=neighbourhood_group_cleansed,room_type,pct)
})

output$activelistprice<-renderGvis({
  if(input$listformatprice=="Count"){
    df<-table(listdataprice()$neighbourhood_group_cleansed,listdataprice()$room_type)
    df<-as.data.frame.matrix(df)
    df$neighbour<-rownames(df)}
  if(input$listformatprice=="Percentage") {
    df <- as.data.frame(with(listdataprice2(), tapply(pct, list(neighbour, room_type) , I)))
    df$neighbour<-rownames(df)
  }
  gvisColumnChart(df,"neighbour",c("Entire home/apt","Private room","Shared room"),
               options=list(colors= "['#58508d', '#bc5090','#ff6361']",
                            legend="bottom",
                            bar="{groupWidth:'90%'}",gvis.editor="Chart Type?",
                            width=500,height=250, isStacked=TRUE))
  
})



##active list review
listdatareview <- reactive({
  df <- map %>%
    filter(neighbourhood_group_cleansed %in% input$neighbour, number_of_reviews>=input$listratingreview[1],
           number_of_reviews<=input$listratingreview[2])%>%
    select(neighbourhood_group_cleansed,room_type)
})

listdatapricereview2<-reactive({
  df<- map %>% filter(neighbourhood_group_cleansed %in% input$neighbour, number_of_reviews>=input$listratingreview[1], price<=input$listratingreview[2])%>%
    group_by(neighbourhood_group_cleansed,room_type) %>%
    tally  %>%
    group_by(neighbourhood_group_cleansed) %>%
    mutate(pct=(100*n)/sum(n))%>%
    select(neighbour=neighbourhood_group_cleansed,room_type,pct)
})

output$activelistreview<-renderGvis({
  if(input$listformatreview=="Count"){
    df<-table(listdatareview()$neighbourhood_group_cleansed,listdatareview()$room_type)
    df<-as.data.frame.matrix(df)
    df$neighbour<-rownames(df)}
  
  if(input$listformatreview=="Percentage") {
    df <- as.data.frame(with(listdatapricereview2(), tapply(pct, list(neighbour, room_type) , I)))
    df$neighbour<-rownames(df)
  }
  gvisBarChart(df,"neighbour",c("Entire home/apt","Private room","Shared room"),
               options=list(colors= "['#58508d', '#bc5090','#ff6361']",
                            legend="bottom",
                            bar="{groupWidth:'90%'}",gvis.editor="Chart Type?",
                            width=500,height=250))
  
})




#coropleth graph
output$coroplethmap <- renderGvis({
                                    gvisGeoChart(map, locationvar='neighbourhood_group_cleansed', colorvar='price',
                                                    options=list(region='NYC', width=500,height=250))
                                  })


  

 
  output$ziptable <- DT::renderDataTable({
    df <- host %>% 
      filter(host_since>=input$hostt[1],
             host_since<=input$hostt[2])%>%
      arrange(.,desc(host_total_listings_count))
    DT::datatable(head(df,input$hostn), class = 'cell-border stripe', escape = FALSE)
  })
  
  
  
  
  
  
  ##word analytics
  
  termCount <- reactive({
    if(input$boroughSelection == "Staten Island") {
      return (term.count.staten)
    } else {
      return (term.count.bronx)
    }
  })
  
  
  # First Panel 
  
  output$plot <- renderPlot({
    popular.terms.borough <- filter(termCount(),n > input$freqNo)
    if(nrow(popular.terms.borough)==0)
    {
      plot(1,1,col="white")
      text(1,1,"No data available for selected frequency range")
    }else {
      wordcloud(popular.terms.borough$Terms,popular.terms.borough$n,colors=brewer.pal(8,"Paired"))
    }
  })
  
  
  #2nd Panel
  
  output$conditionalInput <- renderUI({
    if(input$checkbox){
      sliderInput("topFreqPc","Select Top Frequecy Percentage:",min = 1,  max = 10, value = 5)
    }  else {
      sliderInput("lowfreq","Select Low Frequency term:",min = 1000,  max = 5000, value = 2000)
    }
  })
  
  output$color1 <- renderUI({
    if(input$checkbox)
    {
      colourpicker::colourInput("p2bcol", "Bar colour", "blue", showColour = "background")
    }
  })
  
  
  output$color2 <- renderUI({
    if(input$checkbox)
    {
      colourpicker::colourInput("p2lcol", "Line colour", "black", showColour = "background")
    }
  })
  
  
  output$freqplot <- renderPlot({
    if(input$boroughSelection == "Staten Island"){
      if(input$checkbox)
      {
        term.count.staten %>% 
          filter(cume_dist(n) > (1 - (input$topFreqPc/10000))) %>% 
          ggplot(aes(x=reorder(Terms,n),y=n)) + geom_bar(stat='identity', colour=input$p2lcol, fill=input$p2bcol) + 
          coord_flip() + xlab('Counts') + ylab('')
      } else {
        len <- unique(findFreqTerms(dtm_bronx, lowfreq = input$lowfreq))
        x <- rnorm(length(len))
        plot(x,col="white")
        text(x,len)
      }
    } else {
      if(input$checkbox)
      {
        term.count.bronx %>% 
          filter(cume_dist(n) > (1 - (input$topFreqPc/10000))) %>% 
          ggplot(aes(x=reorder(Terms,n),y=n)) + geom_bar(stat='identity', colour=input$p2lcol, fill=input$p2bcol) + 
          coord_flip() + xlab('Counts') + ylab('')
      } else {
        len <- unique(findFreqTerms(dtm_bronx, lowfreq = input$lowfreq))
        x <- rnorm(length(len))
        plot(x,col="white")
        text(x,len, pos =1)
      }
    }
  })
  
  
  # 3rd Panel 
  
  output$dtm <- renderPlotly({
    if(input$boroughSelection == "Staten Island"){
      sum.terms <- as.data.frame(post.lda.staten$terms) %>% #matrix topic * terms
        mutate(topic=c('listing', 'host','recommendation','experience','area','review','comment','transport','complaint','place')) %>% #add a column
        gather(term,p,-topic) %>% #gather makes wide table longer, key=term, value=p, columns=-topic (exclude the topic column)
        group_by(topic) %>%
        mutate(rnk=dense_rank(-p)) %>% #add a column
        filter(rnk <= 10) %>%
        arrange(topic,desc(p))
    } else
    {
      sum.terms <- as.data.frame(post.lda.bronx$terms) %>% #matrix topic * terms
        mutate(topic=c('listing', 'host','recommendation','experience','area','review','comment','transport','complaint','place')) %>% #add a column
        gather(term,p,-topic) %>% #gather makes wide table longer, key=term, value=p, columns=-topic (exclude the topic column)
        group_by(topic) %>%
        mutate(rnk=dense_rank(-p)) %>% #add a column
        filter(rnk <= 10) %>%
        arrange(topic,desc(p))
    }
    sum.terms %>%
      filter(topic==input$numTopic) %>%
      ggplot(aes(x=reorder(term,p),y=p)) + geom_bar(stat='identity', colour=input$lcol, fill=input$bcol) + coord_flip() +
      xlab('Term')+ylab('Probability')+ggtitle(paste("Topic ",input$numTopic)) + theme(text=element_text(size=20))
  })
  
  
  

  

}
