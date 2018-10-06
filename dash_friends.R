library(shiny)
library(shinydashboard)
source("analise_redes.R")

header = dashboardHeader(title = "Friends: Redes Sociais",
                         titleWidth = 240)

sidebar = dashboardSidebar(
  sliderInput("season", label = "Temporada(s) em análise", min = 1, 
              max = 10, value = c(1,10)),
  
  sidebarMenu(
    menuItem("Exploratory", 
             tabName = "exploratory", 
             icon = icon("bar-chart"),
                menuSubItem("Open exploratory",
                            tabName = "exploratory_2",
                            icon = NULL),
             
                menuSubItem(icon = NULL,
                             infoBoxOutput("myscene_per")),
             
                menuSubItem(icon=NULL,
                            selectInput("select_per", label = "Selecione o personagem", 
                                        choices = c("Todos",colnames(scenes.adj[,-c(1,2)])), 
                                        selected = 1))
                ),
    
    menuItem("Models", 
             tabName = "models", 
             icon = icon("cogs"))
  ),
  
  wellPanel(
    tags$h4(style="color:black","Informações"),
    tags$div(style="color:black",
      tags$h6("Os dados utilizados foram extraídos da página: "),
      tags$a(style="color:blue",href="https://fangj.github.io/friends/", "https://fangj.github.io/friends/"),
      tags$h6("Alguns episódios estão com uma formatação diferente que não possui a marcação de cena e existem tambem episódios duplos, estes problemas explicam os saltos no histograma.")
    )
  )
)

body = dashboardBody(tabItems(
  # First tab content
  tabItem(tabName = "exploratory_2",
          fluidRow(
              infoBoxOutput("qtdses"),
              infoBoxOutput("myscene"),
              infoBoxOutput("qtdper")
              ),
          
          fluidRow(box(title = "Distribuição de cenas por episódio e temporadas",
                       plotOutput("histplot"),width = 6,height = 500),
                   box(title = "Network",forceNetworkOutput("network"),width = 6,height = 500))#,
          
          #fluidRow()

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
  
  dataset_per = reactive({
    if(input$select_per != "Todos"){
      scenes.adj %>% 
        filter(scenes.adj[,colnames(scenes.adj) %in% input$select_per,drop=F]==T) %>% 
        filter(season >= input$season[1] , season <= input$season[2])
    }
  })
  
  output$qtdses = renderInfoBox({
    infoBox(
      "Quantidade de temporadas",
      value = length(unique(dataset()$season)),
      icon = icon("bookmark")
    )
  })
  
  output$myscene = renderInfoBox({
    infoBox(
      "Total de cenas",
      value = nrow(dataset()),
      icon = icon("film")
    )
  })
  
  output$myscene_per = renderInfoBox({
    if(input$select_per == "Todos"){
      infoBox(
        "Total de cenas",
        value = nrow(dataset()),
        icon = icon("film")
      )
    }else{
      infoBox(
        "Total de cenas do personagem",
        value = nrow(dataset_per()),
        icon = icon("film")
      )
    }
  })
  
  output$qtdper = renderInfoBox({
    infoBox(
      "Total de personagens",
      value = sum(apply(dataset()[,-c(1,2)],2,max) != 0),
      icon = icon("users")
    )
  })
  
  output$histplot = renderPlot({
    if(input$select_per == "Todos"){
      ggplot() + 
      #geom_bar(data = scenes.adj,aes(x=episode),fill = "grey85") + 
      geom_bar(data = dataset(),aes(x=episode)) +
      labs(x="Episódio", y="Quantidade de cenas") +
      facet_wrap(~season,scales = "free")
    }else{
      ggplot() + 
      #geom_bar(data = scenes.adj,aes(x=episode),fill = "grey85") + 
      geom_bar(data = dataset(),aes(x=episode)) +
      geom_bar(data = dataset_per(),aes(x=episode),fill='red',alpha=.7) +
      labs(x="Episódio", y="Quantidade de cenas") +
      facet_wrap(~season,scales = "free")
    }
  })
  
  output$network = renderForceNetwork({
    view_net(cria.socio.matriz(dataset()))
  })
  
}

shinyApp(dashboardPage(header,
                       sidebar,
                       body), server)