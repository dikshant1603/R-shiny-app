library(shinydashboard) 
library(shiny) 
library(dplyr)
library(jsonlite)
library(tidyverse)
library(tidytext)
library(stringr)
library(leaflet) 
show_col_types = FALSE
Hotels<-read.csv("Hotels.csv")
HotelReviews<-read.csv("HotelReviews.csv")
Hotels_tbl<-as_tibble(Hotels)
reviews_tbl<-as_tibble(HotelReviews)
Hotels_tbl=Hotels_tbl %>%
  select(-starts_with("hours"))
erac=Hotels_tbl %>%
  filter(str_detect(name,"Enterprise Rent-A-Car"))
business_reviews<-erac%>%
  left_join(reviews_tbl, by="business_id")
library(lubridate)
business_reviewsDateFormatted<-business_reviews%>%
  mutate(date_formatted = as_date(strptime(date,"%m/%d/%Y %H:%M")),
         month_formatted = month(strptime(date,"%m/%d/%Y %H:%M",)),
         day_formatted = day(strptime(date,"%M/%m/%d %H:%M")),
         year_formatted = year(strptime(date,"%m/%d/%Y %H:%M")),
         hour_formatted = hour(strptime(date,"%m/%d/%Y %H:%M")))

### Create a Dashboard User Interface ------------------------------ 
ui <- dashboardPage( 
  
  dashboardHeader(title = "Car Rental Reviews"), 
  
  ### Create the Sidebar ---------------------------------------- 
  dashboardSidebar( 
    
    selectInput("State", h3("Select State"), 
                choices=business_reviews$state, 
                selected="3" ), 
    
    radioButtons("Stars", 
                 h3("Select Star Rating"), 
                 choices = list("1" = 1, 
                                "2" = 2, 
                                "3" = 3, 
                                "4" = 4, 
                                "5" = 5 
                 ), 
                 selected = 3), 
    
    textInput("Text", h3("Search Review Text"), 
              value = ""), 
    verbatimTextOutput("value") 
  ), 
  
  ### Create the Dashboard Body --------------------------------------- 
  
  dashboardBody( 
    ### Create 3 rows to display the Output --------------------- 
    fluidRow( 
      valueBoxOutput("NumberOfCarRentals"), 
      valueBoxOutput("AverageNumberOfReviews"), 
      valueBoxOutput("WordsPerReview") 
    ), 
    
    fluidRow( 
      
      box( 
        status = "info", solidHeader = TRUE, 
        title = "Reviews Over Time", 
        plotOutput("NumberOfReviews") 
      ), 
      
      box( 
        status = "info", solidHeader = TRUE, 
        title = "Words Used in Reviews", 
        plotOutput("WordCloud") 
      ) 
    ), 
    
    fluidRow( 
      box( 
        status = "info", solidHeader = TRUE, 
        title = "Useful & Funny Reviews", 
        plotOutput("UsefulFunny") 
      ), 
      
      box( 
        status = "info", solidHeader = TRUE, 
        title = "Location", 
        leafletOutput("map") 
      ) 
    ) 
  ) 
) 

### Create the Server Side logic required to draw Charts -------------- 

server <- function(input, output) { 
  ## Render the Value Boxes----------------------------------------------- 
  # Total Number of Locations ---------------------------------------- 
  output$NumberOfCarRentals <- renderValueBox({
    valueBox(
      business_reviews%>%
        filter(state==input$State)%>%
        filter(stars.x==input$Stars)%>%
        filter(text %in% input$Text)%>%
        distinct(address)%>%
        count(),
      "Number of reviewed locations"
    )
  }) 
  
  # Total Number of Reviews -------------------------------------------- 
  output$AverageNumberOfReviews <- renderValueBox({
    valueBox(
      business_reviews%>%
        filter(state==input$State)%>%
        filter(stars.x==input$Stars)%>%
        count(),
      "Total number of reviews"
    )
  }) 
  
  # Median Length of Review ---------------------------------------- 
  output$WordsPerReview <- renderValueBox({
    temp<-business_reviews%>%
      filter(state==input$State)%>%
      filter(stars.x==input$Stars)%>%
      group_by(review_id)%>%
      summarise(length_of_word=sapply(strsplit(text, " "), length))
    
    temp<-temp%>%
      filter(!review_id %in% NA)
      
    valueBox(
      median(temp$length_of_word),
      "Median Review length"

    )
    
  }) 
  
  ## Render the Plots----------------------------------------------- 
  output$NumberOfReviews <- renderPlot({ 
    
    business_reviewsDateFormatted%>%
      filter(state==input$State)%>%
      filter(stars.x==input$Stars)%>%
      select(hour_formatted)%>%
      filter(!hour_formatted %in% NA)%>%
      group_by(hour_formatted)%>%
      summarise(No_of_reviews = n())%>%
      ggplot(aes(x=hour_formatted,y=No_of_reviews))+
      geom_line()+
      labs(y= "Number of Reviews", x = "Hour")
  }) 
  
  output$WordCloud <- renderPlot({ 
    library(wordcloud)
    
    createWordCloud = function(train)
    {
      business_reviewsDateFormatted%>%
        filter(state==input$State)%>%
        filter(stars.x==input$Stars)%>%
        unnest_tokens(word,text)%>%
        filter(!word %in% stop_words$word, !word %in% c("enterprise", "rental" , "car", "cars", NA))%>%
        count(word,sort = TRUE)%>%
        ungroup() %>%
        head(50) %>%
        with(wordcloud(word,n,max.words =50,colors=brewer.pal(8,"Dark2")))
      
    }
  createWordCloud(review) 
}) 
  
  output$UsefulFunny <- renderPlot({ 
    business_reviewsDateFormatted%>%
      filter(state==input$State)%>%
      filter(stars.x==input$Stars)%>%
      filter(!funny %in% NA)%>%
      filter(!useful %in% NA)%>%
      ggplot(aes(x=funny, y=useful))+
      geom_point()+
      geom_smooth(method=lm)
  }) 
  
  output$map <- renderLeaflet({ 
    library(leaflet) 
    pal <- colorFactor(c("purple", "red", "orange", "black", "blue"),
                       domain=unique(business_reviews$stars.x))
    
    map<-leaflet(business_reviews) %>%
      addProviderTiles ("CartoDB.Positron") %>%
      addCircleMarkers (
        color= ~pal(business_reviews$stars.x),
        stroke = FALSE, fillOpacity = 0.5,
        lat=business_reviews$latitude,
        lng=business_reviews$longitude,
        clusterOptions = markerClusterOptions(),
        popup=as.character(business_reviews$address))
    map
  }) 
  
  } 
shinyApp(ui, server) 