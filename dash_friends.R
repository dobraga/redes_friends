library(shiny)
library(shinydashboard)
source("analise_redes.R")

header = dashboardHeader(title = "Friends: Redes Sociais",
                         titleWidth = 240)

sidebar = dashboardSidebar(
  sliderInput("season", label = "Temporada(s) em análise", min = 1, 
              max = 10, value = c(1,10)),
  
  sidebarMenu(
    menuItem("Exploratory", tabName = "exploratory", icon = icon("bar-chart")),
    menuItem("Models", tabName = "models", icon = icon("cogs"))
  ),
  
  wellPanel(
    tags$h4(style="color:black","Informações"),
    tags$div(style="color:black",
      tags$h6("Os dados utilizados foram extraídos da página: "),
      tags$a(style="color:blue",href="https://fangj.github.io/friends/", "https://fangj.github.io/friends/")
    )
  )
)

body = dashboardBody(tabItems(
  # First tab content
  tabItem(tabName = "exploratory",
          fluidRow(
              valueBoxOutput("myscene"),
              valueBoxOutput("qtdper")
              )
          ),
  
  # Second tab content
  tabItem(tabName = "models"
  )
)
)

##########
# SERVER #
##########

server <- function(input, output) { 
  
  dataset = reactive({
    scenes.adj %>% 
      filter(season >= input$season[1] , season <= input$season[2])
      })
  
  output$myscene = renderValueBox({
    valueBox(
      value = nrow(dataset()),
      subtitle = "Total de cenas",
      icon = icon("film")
    )
  })
  
  output$qtdper = renderValueBox({
    valueBox(
      value = sum(apply(dataset(),2,max) != 0),
      subtitle = "Total de personagens",
      icon = icon("users")
    )
  })
  
}

shinyApp(dashboardPage(header,
                       sidebar,
                       body), server)