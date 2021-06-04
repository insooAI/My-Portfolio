options(warn=-1)
library(shiny)
library(car)
library(leaps)
library(tidyverse)
library(lubridate)
library(corrplot)
library(glmnet)
library(ggpubr)
library(rstatix)
library(onewaytests)
library(gganimate)
library(tidymodels)
library(ranger)
library(vip)
library(tidymodels)
library(randomForest)
library(forecast)
library(plotly)
library(ggthemes)

getwd()
setwd('C:/Users/eric3/Documents/shiny')



ui <- fluidPage(
  titlePanel('18년~21년 국내 영화 관객 수'),
  
  sidebarLayout(
    sidebarPanel(
      helpText('전체 영화 관람 수'),
      
      selectInput('id',
                  label = '영화종류 선택',
                  choices = c('전체영화'),
                  selected = '전체영화'),                # widget 추가
      dateRangeInput("dates",
                     "Date range",
                     start = "2018-05-07",
                     end = '2021-05-06' ),
    
    ),
    mainPanel(plotOutput('plot')) # plot 기억하자
    
  )
  
  
)


# Server logic
server <- function(input, output){
  

  output$plot <- renderPlot({
    
    all = read.csv('all.csv')
    all = all[-1]
    
    all %>% filter(Date >= input$dates[1] & Date <= input$dates[2]) %>% 
      select(Date, weekdays, weekdays_kind, covid, careCnt, decideCnt, deathCnt, d_val, a_aud) %>% 
      mutate(year = year(Date),
             month = month(Date),
             day = day(Date)) %>% 
      ggplot(aes(careCnt,a_aud, color = covid))+
      geom_point(show.legend = T, alpha=1)+
      scale_y_log10()+
      theme_bw()+
      labs(x = "치료중인 확진자 수 ", y = "관객수")
      
    

  })
  
  
}

shinyApp(ui, server)

