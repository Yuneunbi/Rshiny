

library(readxl)
library(wordcloud2)
library(dplyr)
require(httr)
require(stringr)
library(jsonlite)
library(RColorBrewer)
library(rJava)
library(KoNLP)

# load(file='C:/Users/eunbi/Documents/R/wordcloud/new/naver_api_blog.RData')
# 
source('C:/Users/eunbi/Documents/R/wordcloud/naver_api_blog.R')

library(shiny)
ui <- fluidPage(
  # Application title
  titlePanel("Word Cloud"),

  sidebarLayout(

    # Sidebar with a slider and selection inputs
    sidebarPanel(
      textInput("text", h5("검색할 단어 입력: (두글자 이상)")),
     
      actionButton("update", "Update"),

      selectInput("select", h4("Word Cloud의 모양:"), choices = list('circle'='circle',"heart" = 'heart', "cloud" = 'cloud',
                                 "apple" = 'apple',"star" = 'star','triangle'='triangle'))

    ,
      sliderInput("slider1", h3("단어 개수"),
              min = 0, max = 100, value = 50),
    sliderInput("slider2", h3("Word Cloud 크기"),
                min = 0, max = 1, value = 0.5)
    ),

    mainPanel(
      wordcloud2Output("plot")
      
    )
  

))



server<-function(input, output, session) {
  

  terms <- reactive({
    input$update

    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")

        count <- get_words(input$text)
        
        return(count)
        

      })
    })
  })

  # output$test <- renderText({
  #   # terms()
  #   enc2utf8(input$text)
  # 
  # })

#
  output$plot <- renderWordcloud2({
    # v <- getTermMatrix(input$selection)
    v <- terms()
    top <- v %>% arrange(desc(v$Freq)) %>% head(input$slider1)
    # 단어를 빈도별로 정렬시키고 상위 input$slider1개만 저장
    
    wordcloud2(top,size = input$slider2,fontFamily = 'Segoe UI',fontWeight = 'bold',
               color='random-light',backgroundColor = 'dark',shape=input$select)
    # wordcloud_rep(names(v), v, scale=c(4,0.5),
                  # min.freq = input$freq, max.words=input$max,
                  # colors=brewer.pal(8, "Dark2"))
  })
}
#
# Define server logic required to draw a histogram


# Run the application
shinyApp(ui = ui, server = server)

