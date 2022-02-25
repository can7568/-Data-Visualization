# 라이브러리 구동
library(tidyverse)
library(dplyr)
library(ggthemes)
library(ggplot2)
library(hrbrthemes)
library(viridis)
library(gcookbook)
library(shiny)

# 데이터 호출
사고건수 <- read.csv("by day attacker 사고건수.csv",
                    header = T, stringsAsFactors = F)
사망자수 <- read.csv("by day attacker 사망자수.csv",
                 header = T, stringsAsFactors = F)
부상자수 <- read.csv("by day attacker 부상자수.csv",
                  header = T, stringsAsFactors = F)
# 
# View(사고건수)
# View(사망자수)
# View(부상자수)

# 결측치 확인
sum(is.na(사고건수))
sum(is.na(사망자수))
sum(is.na(부상자수))

# 데이터 전처리
사고건수$'일' <- as.numeric(사고건수$'일')
사고건수$'월' <- as.numeric(사고건수$'월')
사고건수$'화' <- as.numeric(사고건수$'화')
사고건수$'수' <- as.numeric(사고건수$'수')
사고건수$'목' <- as.numeric(사고건수$'목')
사고건수$'금' <- as.numeric(사고건수$'금')
사고건수$'토' <- as.numeric(사고건수$'토')
사고건수$'합계' <- as.numeric(사고건수$'합계')
str(사고건수)

사망자수$'일' <- as.numeric(사망자수$'일')
사망자수$'월' <- as.numeric(사망자수$'월')
사망자수$'화' <- as.numeric(사망자수$'화')
사망자수$'수' <- as.numeric(사망자수$'수')
사망자수$'목' <- as.numeric(사망자수$'목')
사망자수$'금' <- as.numeric(사망자수$'금')
사망자수$'토' <- as.numeric(사망자수$'토')
사망자수$'합계' <- as.numeric(사망자수$'합계')
str(사망자수)

부상자수$'일' <- as.numeric(부상자수$'일')
부상자수$'월' <- as.numeric(부상자수$'월')
부상자수$'화' <- as.numeric(부상자수$'화')
부상자수$'수' <- as.numeric(부상자수$'수')
부상자수$'목' <- as.numeric(부상자수$'목')
부상자수$'금' <- as.numeric(부상자수$'금')
부상자수$'토' <- as.numeric(부상자수$'토')
부상자수$'합계' <- as.numeric(부상자수$'합계')
str(부상자수)
#구 정보
b <- distinct(사고건수,사고건수[2])
b
a<- distinct(사고건수,사고건수[1])
a

ui <- pageWithSidebar(
    headerPanel("요일별 자전거 교통사고",),
    sidebarPanel(
        #컬럼 체크박스 선택
      
        checkboxGroupInput("showvars","요일을 선택하세요.",
                           names(사고건수[c(1:10)]),
                           selected=names(사고건수[c(1:10)])),
 
    ),
  
  mainPanel(
    tabsetPanel(
      tabPanel("사고건수", 
               dataTableOutput("mytable1")),
      tabPanel("사망자수", 
               dataTableOutput("mytable2")),
      tabPanel("부상자수", 
               dataTableOutput("mytable3")),
      tabPanel("사고건수 시각화",
               plotOutput("table4",click="plot_click"),
               tableOutput("data")),
      tabPanel("사망자수 시각화",
               plotOutput("table5",click="plot_click"),
               tableOutput("data2")),
      tabPanel("부상자수 시각화",
               plotOutput("table6",click="plot_click"),
               tableOutput("data3")),
      tabPanel("사고건수 시각화(boxplot)",
               plotOutput("table7",click="plot_click")),
      tabPanel("사망자수 시각화(boxplot)",
               plotOutput("table8",click="plot_click")),
      tabPanel("부상자수 시각화(boxplot)",
               plotOutput("table9",click="plot_click"))
      
    )
  )
)

# server
server <- function (input, output) {
 
  output$mytable1 <- renderDataTable({
    사고건수[, input$showvars, drop = FALSE]
  })
  output$mytable2 <- renderDataTable({
      사망자수[, input$showvars, drop = FALSE]
  })
  output$mytable3 <- renderDataTable({
      부상자수[, input$showvars, drop = FALSE]
  })

  output$table4 <- renderPlot({
      ggplot(data=사고건수,
             mapping = aes(x=합계,y=구))+
          geom_point(mapping = aes(color=합계,
                                   size = 합계,
                                   alpha = 0.3))
    })
  output$table5 <- renderPlot({
      ggplot(data=사망자수,
             mapping = aes(x=합계,y=구))+
          geom_point(mapping = aes(color=합계,
                                   size = 합계,
                                   alpha = 0.3))
  })
  output$table6 <- renderPlot({
      ggplot(data=부상자수,
             mapping = aes(x=합계,y=구))+
          geom_point(mapping = aes(color=합계,
                                   size = 합계,
                                   alpha = 0.3))
  })
  output$table7 <- renderPlot({
      ggplot(data= 사고건수,
                mapping = aes(x=금, y=구))+
            geom_boxplot(fill = "lightyellow")
  })
  output$table8 <- renderPlot({
             ggplot(data= 사고건수,
                         mapping = aes(x=토, y=구))+
                         geom_boxplot(fill = "antiquewhite2")
  })
  output$table9 <- renderPlot({
     ggplot(data= 사고건수,
                  mapping = aes(x=목, y=구))+
          geom_boxplot(fill = "antiquewhite4")
  })

  
  
  
  
  output$data <- renderTable({
      req(input$plot_click)
      nearPoints(사고건수,input$plot_click)
  }) 
  output$data2 <- renderTable({
      req(input$plot_click)
      nearPoints(사망자수,input$plot_click)
  }) 
  output$data3 <- renderTable({
      req(input$plot_click)
      nearPoints(부상자수,input$plot_click)
  }) 
 }

shinyApp(ui, server)

# 시각화
# p <- ggplot(data=사고건수,
#             mapping = aes(x=합계,y=구))
# p + geom_point(mapping = aes(color=합계,
#                              size = 합계,
#                              alpha = 0.3))
# 
# p <- ggplot(data=사망자수,
#             mapping = aes(x=합계,y=구))
# p + geom_point(mapping = aes(color=합계,
#                              size = 합계,
#                              alpha = 0.3))
# p <- ggplot(data=부상자수,
#             mapping = aes(x=합계,y=구))
# p + geom_point(mapping = aes(color=합계,
#                              size = 합계,
#                              alpha = 0.3))
# # 사고건수 상위 요일 3개
# sum(사고건수$일) #1097
# sum(사고건수$월) # 1185
# sum(사고건수$화) # 1209
# sum(사고건수$수) # 1232
# sum(사고건수$목) # 1270
# sum(사고건수$금) # 1301
# sum(사고건수$토) # 1285
# 
# ## 금,토,목 순인 것을 확인할 수 있다.
# 
# p <- ggplot(data= 사고건수,
#             mapping = aes(x=금, y=구))
# p + geom_boxplot(fill = "lightyellow")
# 
# p <- ggplot(data= 사고건수,
#             mapping = aes(x=토, y=구))ㅁ
# p + geom_boxplot(fill = "antiquewhite2")
# 
# p <- ggplot(data= 사고건수,
#             mapping = aes(x=목, y=구))
# p + geom_boxplot(fill = "antiquewhite4")
# 
# # 사고건수 상위 구 3개
# tail(sort(사고건수$합계)) ## 송파구 218건,영등포구 214건,동대문구 210건
