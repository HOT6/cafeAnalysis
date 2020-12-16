library(leaflet)


navbarPage("map", id="nav",
  tabPanel("map Test",
    div(class="outer",

      tags$head(
        # Include our custom CSS
        includeCSS("styles.css")
      ),

      # If not using custom CSS, set height of leafletOutput to a number instead of percent
      leafletOutput("map", width="100%", height="100%"),

      # Shiny versions prior to 0.11 should use class = "modal" instead.
      absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
        draggable = FALSE, top = 60, left = "auto", right = 20, bottom = "auto",
        width = '30%', height = "100%",

        h3("Check"),
        radioButtons("check_sex",
                     label = "성별",
                     choices = c("남자", "여자", "전체"),
                     width = '100%',
                     inline = T,
                     selected = "남자"),
        
        checkboxGroupInput("check_age",
                           "나이대", c("10대", "20대", "30대", "40대", "50대 이상"),
                           selected = "10대", 
                           width = "100%",
                           inline = T),
        

        plotOutput("graph1", width = '100%', height = '30%'),

        leafletOutput("submap", height = '40%')
      ),
      
      tags$div(id="cite",
        'Data compiled for ', tags$em('test'), ' by suchang.park'
      )
    )
  ),
  tabPanel(textOutput("cnty")),

  conditionalPanel("false", icon("crosshair"))
)
