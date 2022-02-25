
# 라이브러리

# install.packages("ggthemes")
# install.packages("hrbrthemes")
# install.packages("viridis")
# install.packages("gcookbook")

library(tidyverse)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(hrbrthemes)
library(viridis)
library(gcookbook)



# ui
ui <-  pageWithSidebar(
    headerPanel("데이터 테이블 예제"),
    sidebarPanel(),
    mainPanel(
        tabsetPanel(
            tabPanel(
                "공공자전거 일별 대여",
                plotOutput('table1'),
                '공공자전거 대여량 꾸준히 증가'
            ),
            tabPanel(
                '자전거도로 증가추이',
                plotOutput('table2'),
                '-자전거도로 길이는 2020년 기준 1259km로 2015년보다 61.6% 증가
      
      -특히, 2019년 대비 33.9% 라는 큰 폭으로 자전거도로가 증설됨'
            ),
            tabPanel(
                '자전거도로 유형별 증가추이',
                plotOutput('table3'),
                '유형별로는 자전거보행자겸용도로가 810.4km로 가장 긺.
      
      -그 다음은 자전거 우선도로 191.5km, 자전거전용도로 182.7km , 자전거전용차로 74.3km 순
      
-자전거 도로는 꾸준한 증가추세이고 특히 자전거보행자겸용도로의 증가세가 가파름.'
            ),
            tabPanel(
                '자전거도로와 자전거사고',
                plotOutput('table4'),
                '자전거도로의 길이와 이용자 증가에 따라 자전거사고 건수 또한 지속적으로 높은 수치'
            )
        ))
)

# 데이터 호출
seoul_bike = read.csv("서울특별시_공공자전거 일별 대여건수_(2016-2021).csv")
seoul_bikeroad = read.csv("서울시 자전거도로 현황 통계.csv", skip = 1)
seoul_accident = read.csv("2010_2020_자전거사고.csv", skip = 1)

seoul_bike
seoul_bikeroad
seoul_accident

str(seoul_bike)
str(seoul_bikeroad)
str(seoul_accident)

# 전 처 리
seoul_bike$대여일자 <- as.Date(seoul_bike$대여일자, "%Y-%m-%d")
seoul_bike$대여건수 <- as.integer(seoul_bike$대여건수)

# 연도별 자전거 도로 총 길이 df 생성 및 num 값으로 대체
seoul_bikeroad_t <- subset(seoul_bikeroad, 자치구 == "합계")
seoul_bikeroad_t

# 데이터타입 변경 : chr -> num
seoul_bikeroad_t$길이 <- c(778.8, 868.7, 888.7, 916.0, 940.6, 1258.8)
seoul_bikeroad_t$길이.1 <- as.numeric(seoul_bikeroad_t$길이.1)
seoul_bikeroad_t$길이.2 <- as.numeric(seoul_bikeroad_t$길이.2)
seoul_bikeroad_t$길이.3 <- as.numeric(seoul_bikeroad_t$길이.3)
seoul_bikeroad_t$길이.4 <- as.numeric(seoul_bikeroad_t$길이.4)

# 중복 컬럼명 변경
seoul_bikeroad_t <- rename(seoul_bikeroad_t, "자전거도로" = "길이")
seoul_bikeroad_t <- rename(seoul_bikeroad_t, "자전거전용도로" = "길이.1")
seoul_bikeroad_t <- rename(seoul_bikeroad_t, "자전거보행자겸용도로" = "길이.2")
seoul_bikeroad_t <- rename(seoul_bikeroad_t, "자전거전용차로" = "길이.3")
seoul_bikeroad_t <- rename(seoul_bikeroad_t, "자전거우선도로" = "길이.4")

# 연도별 자전거 총 사고 df 생성 및 num 값으로 대체
seoul_accident_t <- subset(seoul_accident, 기간 >= 2015 & 지역 == "합계")
seoul_accident_t$발생건수 <- c(2230, 1884, 1656, 1471, 1766, 1802)


# 서버
server <- function(input, output) {
    output$table1 <- renderPlot({
        # 서울 특별시 일별 자전거 대여건수
        ggplot(seoul_bike)+geom_point(aes(x = 대여일자, y = 대여건수, color = "대여건수"), size = 3, alpha = 0.6) + 
            geom_smooth(mapping = aes(x = 대여일자, y = 대여건수)) + 
            labs(title = "서울 특별시 공공자전거 일별 대여 건수") +
            xlab(label = "대여일자") +
            ylab(label = "대여건수") + theme(legend.title = element_blank())
    })
    output$table2 <- renderPlot({
        # 서울 특별시 자전거도로 길이 증가추이
        ggplot(seoul_bikeroad_t, aes(x=기간, y=자전거도로)) + 
            geom_bar(stat = "identity") + 
            labs(title = "서울 특별시 자전거도로 증가 추이") +
            xlab(label = "년도") +
            ylab(label = "자전거도로(km)") +
            coord_flip()
    })
    output$table3 <- renderPlot({
        #자전거도로 유형별 증가추이
        ggplot(seoul_bikeroad_t, aes(x = 기간, y = 자전거도로)) + 
            
            geom_line(seoul_bikeroad_t, mapping = aes(x = 기간, y = 자전거전용도로), color = 'green', size = 1.3) +
            geom_point(seoul_bikeroad_t, mapping = aes(x = 기간, y = 자전거전용도로, color = '자전거전용도로'), size = 3) +
            
            geom_line(seoul_bikeroad_t, mapping = aes(x = 기간, y = 자전거보행자겸용도로), color = 'blue', size = 1.3) +
            geom_point(seoul_bikeroad_t, mapping = aes(x = 기간, y = 자전거보행자겸용도로, color = '자전거보행자겸용도로'), size = 3) +
            
            geom_line(seoul_bikeroad_t, mapping = aes(x = 기간, y = 자전거전용차로), color = 'purple', size = 1.3) +
            geom_point(seoul_bikeroad_t, mapping = aes(x = 기간, y = 자전거전용차로, color = '자전거전용차로'), size = 3) + 
            
            geom_line(seoul_bikeroad_t, mapping = aes(x = 기간, y = 자전거우선도로), color = 'black', size = 1.3) +
            geom_point(seoul_bikeroad_t, mapping = aes(x = 기간, y = 자전거우선도로, color = '자전거우선도로'), size = 3) + 
            
            labs(title = "서울 특별시 자전거도로 유형별 증가 추이") +
            xlab(label = "기간") +
            ylab(label = "km") +
            theme(legend.title = element_blank()) +
            theme(legend.position = "right") + 
            guides(color = guide_legend(nrow = 5, override.aes = list(size = 4))) + 
            scale_color_manual(values = c("blue", "black", "green", "purple"))
    })
    output$table4 <- renderPlot({
        # 서울 특별시 자전거도로, 자전거 사고
        ggplot(seoul_bikeroad_t, mapping = aes(x = 기간, y = 자전거도로, color = "자전거도로")) + 
            geom_line(data = seoul_bikeroad_t, aes(x = 기간, y = 자전거도로), size = 1.3)+
            geom_point(data = seoul_bikeroad_t, aes(x = 기간, y = 자전거도로), size = 4, color = 'red') + 
            
            geom_line(data = seoul_accident_t, aes(x = 기간, y = 발생건수, color = "발생건수"), size = 1.3) +
            geom_point(data = seoul_accident_t, aes(x = 기간, y = 발생건수), size = 4, color = 'blue') + 
            labs(title = "서울 특별시 자전거도로와 자전거 사고") +
            xlab(label = "기간") +
            ylab(label = "건수") + 
            theme(legend.title = element_blank()) + 
            scale_color_manual(values = c("blue","red"))
    })
}

shinyApp(ui, server)