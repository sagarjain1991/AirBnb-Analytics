library(shiny)
library(shinydashboard)
library(googleVis)
library(datasets)
library(leaflet)
library(dplyr)
library(shinythemes)
library(ggplot2)
library(ggthemes)
library(wordcloud)
library(dygraphs)
library(xts)

library(memoise)
library(tidyr)
library(ggmap)
library(cluster)   
library(tm)
library(topicmodels)
library(slam)
library(SnowballC)
library(plotly)


# load data
load("data/review.RData")
load("data/map.RData")
#load("data/map2.RData")
load("data/review_txt.RData")
load("data/host.RData")

load("data/bronx_dtm_termCount.rda")
load("data/staten_dtm_termCount.rda")
load("data/post.lda.bronx.rda")
load("data/post.lda.staten")


insideairbnb <- tags$html(
  tags$body(
    a(href="http://insideairbnb.com/get-the-data.html"))
)

# variable list
neighbour<-c(
  "Boston" = "Boston",
  "Chicago" = "Chicago",
  "Seattle" = "Seattle",
  "San Francisco" = "San Francisco",
  "Jersey City" = "Jersey City"
)

room<-c(
  "Entire home/apt"= "Entire home/apt",
  "Private room" ="Private room",
  "Shared room" ="Shared room"
)

boroughs <- c("Bronx","Staten Island")


choice=names(review)[-c(1:3)]

pal <- colorFactor(c("#EE3B3B", "#0000EE","#66CD00"), domain = c("Entire home/apt", "Private room","Shared room"))


