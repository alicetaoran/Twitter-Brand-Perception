

ui <- fluidPage(
  titlePanel("Words frequency on twitter"),  
  sidebarLayout(
    sidebarPanel(
      radioButtons("productInput", "Product type",
                   choices = c("Alexa", "Google Home", "Apple Home"),
                   selected = "Alexa")),
    mainPanel(
      tabsetPanel(
        tabPanel("Barplot",plotOutput("barplot")),
        tabPanel("Wordcloud",plotOutput("wordcloud")),
        tabPanel("Sentiments",plotOutput("sentiments")),
        tabPanel("Table",tableOutput("table"))
      )
      
      
    )
  )
)

