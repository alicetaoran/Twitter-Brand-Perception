library(tidytext)
library(tidyverse)
library(dplyr)
library(scales)
library(wordcloud)
library(RColorBrewer)
library(ggplot2)
library(shiny)
get_sentiments("bing")


frq<-read.csv("all.csv")
colnames(frq)<-c("index","word","frequency","product")
frq<-frq[,c(4,2,3)]

server <- function(input, output) {
  
  filtered <- reactive({
    
    frq %>%
      filter(
        product == input$productInput,
        frequency > 100
      )
  })
  
  sentiment <- reactive({
    
    frq %>%
      filter(product == input$productInput)%>%
      inner_join(get_sentiments("bing")) %>%
      spread(sentiment, frequency, fill = 0) %>%
      mutate(sentiment = positive - negative)
  })
  
  output$barplot <- renderPlot({
    ggplot(filtered(), aes(x=reorder(word,frequency), y=frequency, fill=frequency)) +
      geom_col()+coord_flip()+xlab("words")
  })
  
  output$wordcloud <- renderPlot({
    filtered() %>%
      with(wordcloud(word, frequency, max.words = 50, colors = brewer.pal(8, "Dark2")))
  })
  
  output$sentiments <- renderPlot({
    ggplot(sentiment(),aes(word,sentiment,fill=sentiment))+
      geom_col()+theme(axis.text.x = element_blank())
  })
  
  output$table <- renderTable({
    filtered()
  })
  
  
}