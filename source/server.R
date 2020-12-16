library(leaflet)
library(rgdal)
library(dplyr)
library(readxl)
library(ggplot2)


options(scipen=100000)
event <- NULL

# 서울 커피점 정보 
seoul_cafe <- read.csv('../data/seoul_cafe.csv')

cafe_dong <- seoul_cafe %>% 
  group_by(행정동명, 행정동코드) %>% 
  summarise(count = n())

colnames(cafe_dong)[2] <- "adm_cd2"

# 서울 생활 인구
seoul_pop <- read.csv('../data/seoul_pop.csv')

# 수유1동
cafe_dong[cafe_dong$adm_cd2 == 1130561000, ]$adm_cd2 <- 1130561500
seoul_pop[seoul_pop$adm_cd2 == 1130561000, ]$adm_cd2 <- 1130561500
# 수유2동
cafe_dong[cafe_dong$adm_cd2 == 1130562000, ]$adm_cd2 <- 1130562500
seoul_pop[seoul_pop$adm_cd2 == 1130562000, ]$adm_cd2 <- 1130562500
# 번1동
cafe_dong[cafe_dong$adm_cd2 == 1130559000, ]$adm_cd2 <- 1130559500
seoul_pop[seoul_pop$adm_cd2 == 1130559000, ]$adm_cd2 <- 1130559500
# 번2동
cafe_dong[cafe_dong$adm_cd2 == 1130560000, ]$adm_cd2 <- 1130560300
seoul_pop[seoul_pop$adm_cd2 == 1130560000, ]$adm_cd2 <- 1130560300


# 행정 구분 지도 load
seoul <-
   readOGR(
     dsn = '../data/seoul3',
     layer = 'seoul3',
     encoding = 'utf-8')



# merge
mean_pop <- seoul_pop %>% group_by(adm_cd2) %>% 
  summarise(mean_pop = mean(mean_pop))
cafe_dong <- merge(cafe_dong, mean_pop, by = 'adm_cd2')
seoul_map <- merge(seoul, cafe_dong, by = 'adm_cd2')



## 집계구 기준
seoul_detail <- read.csv("../data/seoul_detail_pop.csv")
seoul_detail_mean <- seoul_detail %>% 
  group_by(TOT_REG_CD) %>% 
  summarise(mean_pop = mean(mean_pop))

seoul <-
  readOGR(
    dsn = '../data/seoul_detail',
    layer = 'seoul_detail',
    encoding = 'utf-8')

# GRS80 좌표계를 WGS84 좌표계로 변환.
seoul <- spTransform(x = seoul, CRSobj = CRS('+proj=longlat +datum=WGS84'))

seoul@data$TOT_REG_CD <- as.character(seoul@data$TOT_REG_CD)
seoul_detail_mean$TOT_REG_CD <- as.character(seoul_detail_mean$TOT_REG_CD)
detail_map <- merge(seoul, seoul_detail_mean, by = 'TOT_REG_CD')


detail_map@data$color <- 
  ifelse(detail_map$mean_pop > boxplot(detail_map$mean_pop)$stats[4], "red",
         ifelse(detail_map$mean_pop < boxplot(detail_map$mean_pop)$stats[2], "green", "yellow"))



function(input, output, session) {

  # 초기 맵 설정
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles('CartoDB.Positron') %>% 
      #addTiles() %>%
      setView(lng = 127.06, lat = 37.566, zoom = 12)
  })

  # checkbox 선택 데이터
  checkboxData <- function(sexBy, ageBy) {
    if (sexBy == "남자") {
      checkedSex <- 'm'
    } 
    else if (sexBy == "여자") {
      checkedSex <- 'f'
    }
    else {
      checkedSex <- c("m", "f")
    }
    
    checkedAge <- substr(ageBy, 1, 2)
    selectedData <- do.call(paste0, expand.grid(checkedSex, checkedAge))
    
    return(selectedData)
  }
  
  # 그래프 그리기
  drawGraph <- function(adm_code2, selectedData) {
    
    if(is.null(adm_code2)) {
      pop_group_hour <- seoul_pop %>%
        group_by(hour) %>% 
        dplyr::select(selectedData) %>% 
        summarise_all(mean)
      
      graphData <- data.frame(hour = c(0:23),
                              mean_pop = round(rowMeans(pop_group_hour[, selectedData])))
    }
    
    else {
      pop_group_hour <- seoul_pop %>% 
        filter(adm_cd2 == adm_code2) %>% 
        group_by(hour) %>% 
        dplyr::select(selectedData) %>% 
        summarise_all(mean)
      
      graphData <- data.frame(hour = c(0:23),
                              mean_pop = round(rowMeans(pop_group_hour[, selectedData])))
    }
    
    output$graph1 <- renderPlot({
      if (nrow(graphData) == 0)
        return(NULL)
      
      ggplot(data = graphData, aes(x = hour, y = mean_pop)) + 
        geom_line(color = 'red') +
        geom_point() +
        labs(title = paste(seoul_map@data[seoul_map@data$adm_cd2 == adm_code2,]$행정동명, '시간대별 평균 생활인구'),
             subtitle = list(selectedData),
             x = '시간대', 
             y = "평균 생활인구") +
        theme(
          plot.title = element_text(hjust = 0.5,
                                    lineheight = 0.9,
                                    size = 17,
                                    face = 'bold',
                                    color = 'blue',),
          axis.title.x = element_text(size = 15,
                                      face = 'bold'),
          axis.title.y = element_text(size = 15,
                                      face = 'bold'),
          plot.background = element_rect(fill = "lightyellow", 
                                         color='yellow', 
                                         size=2)
        )
      })
  }

  # 행정구분지도
  drawmainMap <- function(adm_code2, selectedData) {
    
    # 생활 인구 대비 컬러
    selectedPop <- ungroup(seoul_pop)
    selectedPop <- selectedPop %>%
      mutate(mean_pop = rowMeans(select(selectedPop, selectedData))) %>%
      group_by(adm_cd2) %>% summarise(mean_pop = mean(mean_pop))

    seoul_map@data$mean_pop[match(selectedPop$adm_cd2, seoul_map@data$adm_cd2)] <- selectedPop$mean_pop
    
    
    seoul_map@data$rate <- seoul_map@data$count / seoul_map@data$mean_pop
    seoul_map@data$color <- 
      ifelse(seoul_map@data$rate > boxplot(seoul_map@data$rate)$stats[4], "red",
             ifelse(seoul_map@data$rate < boxplot(seoul_map@data$rate)$stats[2], "green", "yellow"))
    
    
    leafletProxy("map", data = seoul_map) %>%
      clearShapes() %>%
      addPolygons(
        color='#444444', 
        weight=1, 
        fillOpacity = 0.4, 
        fillColor=seoul_map@data$color,
        label = paste(seoul_map@data$행정동명, seoul_map@data$count),
        layerId=~adm_cd2,
        highlightOptions = highlightOptions(color = "black", weight = 2,
                                            bringToFront = TRUE))
  }
  
  # sub map
  drawSubmap <- function(adm_code2) {
    # 위도, 경도 center 
    selectedLayer <- seoul_cafe[seoul_cafe$행정동코드 == adm_code2, ]
    center_lon = mean(selectedLayer$경도)
    center_lat = mean(selectedLayer$위도)
    
    seoul_detail <- detail_map[detail_map$ADM_CD %in% c(as.character(seoul_map[seoul_map$adm_cd2 == adm_code2,]$adm_cd)), ]

    output$submap <- renderLeaflet({
      leaflet() %>%
        addTiles() %>%
        setView(lng = center_lon, lat = center_lat, zoom = 15) %>% 
        addCircleMarkers(lng=selectedLayer$경도, 
                         lat=selectedLayer$위도, 
                         label = selectedLayer$상호명,
                         radius = 2,
                         popup = paste0("업소명 : ", selectedLayer$상호명,
                                        "<br/>",
                                        "지점명 : ", selectedLayer$지점명,
                                        "<br/>",
                                        "주소 : ", selectedLayer$도로명주소,
                                        "<br/>",
                                        "상권업종중분류명 : ", selectedLayer$상권업종중분류명)
                         #clusterOptions = markerClusterOptions()
                         ) %>%
        addPolygons(data = seoul_detail, 
                    color=seoul_detail$color, 
                    weight=1, 
                    fillOpacity = 0.4,
                    label = paste("Test :", seoul_detail@data$TOT_REG_CD, "평균 인구 :", round(seoul_detail@data$mean_pop, 2)),
                    highlightOptions = highlightOptions(color = "black",
                                                        weight = 5,
                                                        bringToFront = F)) 
    })
    }

  
  # 정보 변경 시
  observe({
    sexBy <- input$check_sex
    ageBy <- input$check_age
    selectedData <- checkboxData(sexBy, ageBy)
    
    isolate({
      if(is.null(event)) {
        drawGraph(NULL, selectedData)
      }
      if(length(selectedData) >= 1) {
        drawGraph(event$id, selectedData)
        drawmainMap(event$id, selectedData)
      }
    })
  })

  
  # 지도 팝업
  showPopup <- function(adm_cd2, lat, lng) {
    selectedarea <- seoul_map@data[seoul_map@data$adm_cd2 == adm_cd2,]
    content <- as.character(tagList(
      tags$h4("행정동명 :", selectedarea$행정동명),
      tags$h4("행정동코드 :", selectedarea$adm_cd2),
      tags$h4("행정동코드1 :", selectedarea$adm_cd),
      sprintf("평균생활인구: %s 명", round(selectedarea$mean_pop)), tags$br(),
      sprintf("커피점수: %d", selectedarea$count)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = adm_cd2)
  }

  # 지도 클릭 시
  observe({
    leafletProxy("map") %>% clearPopups()
    event <<- input$map_shape_click
    sexBy <- input$check_sex
    ageBy <- input$check_age
    selectedData <- checkboxData(sexBy, ageBy)
    
    if (is.null(event))
      return()
    isolate({
      showPopup(event$id, event$lat, event$lng)
      drawGraph(event$id, selectedData)
      drawSubmap(event$id)
    })
  })


  # ## Data Explorer ###########################################
  # 
  # observe({
  #   cities <- if (is.null(input$states)) character(0) else {
  #     filter(cleantable, State %in% input$states) %>%
  #       `$`('City') %>%
  #       unique() %>%
  #       sort()
  #   }
  #   stillSelected <- isolate(input$cities[input$cities %in% cities])
  #   updateSelectizeInput(session, "cities", choices = cities,
  #     selected = stillSelected, server = TRUE)
  # })
  # 
  # observe({
  #   zipcodes <- if (is.null(input$states)) character(0) else {
  #     cleantable %>%
  #       filter(State %in% input$states,
  #         is.null(input$cities) | City %in% input$cities) %>%
  #       `$`('Zipcode') %>%
  #       unique() %>%
  #       sort()
  #   }
  #   stillSelected <- isolate(input$zipcodes[input$zipcodes %in% zipcodes])
  #   updateSelectizeInput(session, "zipcodes", choices = zipcodes,
  #     selected = stillSelected, server = TRUE)
  # })
  # 
  # observe({
  #   if (is.null(input$goto))
  #     return()
  #   isolate({
  #     map <- leafletProxy("map")
  #     map %>% clearPopups()
  #     dist <- 0.5
  #     zip <- input$goto$zip
  #     lat <- input$goto$lat
  #     lng <- input$goto$lng
  #     showZipcodePopup(zip, lat, lng)
  #     map %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
  #   })
  # })
}
