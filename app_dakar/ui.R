# ============================================================
# UI.R — Interface utilisateur
# ============================================================

library(shiny)
library(bslib)

# Palette cohérente avec le rapport
couleurs_principales <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#6A994E")

ui <- fluidPage(
  theme = bs_theme(bootswatch = "flatly"),
  
  titlePanel("📊 Analyse interactive des loyers à Dakar"),
  
  sidebarLayout(
    sidebarPanel(
      h4("🔍 Filtres"),
      selectInput("type", "Type de logement :", choices = NULL),  # rempli côté serveur
      selectInput("standing", "Standing :", choices = NULL),      # rempli côté serveur
      sliderInput("loyerRange", "Plage de loyers (FCFA) :", 
                  min = 0, max = 1000, value = c(0, 1000), step = 50)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Vue d'ensemble",
                 plotly::plotlyOutput("histLoyer"),
                 br(),
                 DT::dataTableOutput("tableApercu")),
        
        tabPanel("Relations bivariées",
                 selectInput("varX", "Variable explicative :", 
                             choices = c("SurfHabitable", "NbChamBur", "NbSDB", "PrixM2Habitable")),
                 plotly::plotlyOutput("scatterPlot")),
        
        tabPanel("Modélisation",
                 plotOutput("predPlot"),
                 verbatimTextOutput("modelePerf")),
        
        tabPanel("À propos",
                 h4("Projet : Analyse des facteurs influençant le loyer à Dakar"),
                 p("Application développée dans le cadre du projet R"),
                 p("Cette application permet d’explorer les relations entre les caractéristiques des logements et le loyer."),
                 br(),
                 tags$ul(
                   tags$li("Exploration des loyers par filtres"),
                   tags$li("Visualisation des relations entre variables"),
                   tags$li("Modélisation et prédiction du loyer"),
                 ),
                 br(),
                 p("© 2025 - Université de Montpellier"))
      )
    )
  )
)



