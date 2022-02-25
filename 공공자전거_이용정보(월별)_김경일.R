

# 1. 1. 공공자전거 이용정보(월별) 
# 가보자고~~~

library(ggplot2)
library(tidyverse)
library(dplyr)




ui <-  pageWithSidebar(
  headerPanel("데이터 테이블 예제"),
  sidebarPanel(),
  mainPanel(
    tabsetPanel(
      tabPanel(
        "2019 연령대별 공공자전거 대여건수",
        plotOutput('table1'),
        '-'
      ),
      tabPanel(
        '2020 연령대별 공공자전거 대여건수',
        plotOutput('table2'),
        '-'
      ),
      tabPanel(
        '2021 연령대별 공공자전거 대여건수',
        plotOutput('table3'),
        '-'
      )
    ))
)



# 데이티 호출
seoul_2019m = read.csv("sum2019.csv")
seoul_2020m = read.csv("sum2020.csv")
seoul_2021m = read.csv("sum2021.csv")

# 전 처 리
seoul_2019m$대여일자 <- as.Date(seoul_2019m$대여일자, "%Y-%m-%d")
seoul_2020m$대여일자 <- as.Date(seoul_2020m$대여일자, "%Y-%m-%d")
seoul_2021m$대여일자 <- as.Date(seoul_2021m$대여일자, "%Y-%m-%d")

str(seoul_2019m)
str(seoul_2020m)
str(seoul_2021m)

# 서버
server <- function(input, output) {
  output$table1 <- renderPlot({
    # 2019 연령대별 공공자전거 대여건수
    bar <- ggplot(data = seoul_2019m) + 
      geom_bar(
        mapping = aes(x = 연령대코드, fill = 연령대코드), 
        show.legend = FALSE,
        width = 1
      ) +  
      theme(aspect.ratio = 1) +
      labs(x = NULL, y = NULL)
      
    bar + coord_flip() +
      
      labs(title = "2019 서울 연령대별 공공자전거 대여건수")
  })
  output$table2 <- renderPlot({# 2020 연령대별 공공자전거 대여건수
    bar <- ggplot(data = seoul_2020m) + 
      geom_bar(
        mapping = aes(x = 연령대코드  , fill = 연령대코드), 
        show.legend = FALSE,
        width = 1
      ) +  
      theme(aspect.ratio = 1) +
      labs(x = NULL, y = NULL)
    
    bar + coord_flip() +
      labs(title = "2020 서울 연령대별 공공자전거 대여건수")
  })
  output$table3 <- renderPlot({
    # 2021 연령대별 공공자전거 대여건수
    bar <- ggplot(data = seoul_2021m) + 
      geom_bar(
        mapping = aes(x = 연령대코드  , fill = 연령대코드), 
        show.legend = FALSE,
        width = 1
      ) +  
      theme(aspect.ratio = 1) +
      labs(x = NULL, y = NULL)
    
    bar + coord_flip() +
      labs(title = "2021 서울 연령대별 공공자전거 대여건수")
  })
}

shinyApp(ui, server)
