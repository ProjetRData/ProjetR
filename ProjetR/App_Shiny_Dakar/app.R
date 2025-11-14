# Application Shiny - Analyse des Loyers à Dakar

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(tidyr)

# ============================================================================
# CHARGEMENT DES DONNÉES
# ============================================================================

dakar <- read.csv("dakar.csv", stringsAsFactors = FALSE)

dakar_clean <- dakar |>
  drop_na(Loyer) |>
  mutate(
    PrixM2Terrain = ifelse(SurfTerrain > 0, Loyer / SurfTerrain, NA),
    PrixM2Habitable = ifelse(SurfHabitable > 0, Loyer / SurfHabitable, NA),
    RatioHabitable = ifelse(SurfTerrain > 0, SurfHabitable / SurfTerrain, NA),
    TotalSDB_WC = NbSDB + NbWC,
    Standing = factor(Standing, levels = c("Non", "Oui")),
    Etat = factor(Etat, levels = c("Vetuste", "Mediocre", "Bon", "Neuf")),
    StandingQuartier = factor(
      StandingQuartier, 
      levels = c("popu", "moy", "bourge"),
      labels = c("Populaire", "Moyen", "Bourgeois")
    ),
    Type = factor(Type),
    Piscine = factor(Piscine, levels = c("Non", "Oui")),
    Garage = factor(Garage, levels = c("Non", "Oui", "Park", "Priv1v", "Priv2v")),
    BordMer = factor(BordMer, levels = c("Non", "Oui", "inf2km")),
    Segment = case_when(
      Loyer >= 600 ~ "Haut standing",
      Loyer >= 200 ~ "Moyen standing",
      TRUE ~ "Économique"
    )
  )

couleurs <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#6A994E")

# ============================================================================
# INTERFACE UTILISATEUR
# ============================================================================

ui <- dashboardPage(
  skin = "blue",
  dashboardHeader(title = "Loyers Dakar - Analyse", titleWidth = 280),
  
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "tabs",
      menuItem("📊 Tableau de Bord", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("🔍 Exploration", tabName = "exploration", icon = icon("chart-line")),
      menuItem("📋 Données", tabName = "donnees", icon = icon("table")),
      menuItem("ℹ️ À propos", tabName = "apropos", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f4f6f9; }
        .box { border-top: 3px solid #2E86AB; }
        .btn-primary { background-color: #2E86AB; border-color: #2E86AB; }
        .btn-primary:hover { background-color: #1f5f7a; }
      "))
    ),
    
    tabItems(
      # ========== TABLEAU DE BORD ==========
      tabItem(
        tabName = "dashboard",
        h2("Tableau de Bord - Vue d'Ensemble", style = "color: #2E86AB; font-weight: bold;"),
        
        fluidRow(
          valueBoxOutput("box_nb_logements", width = 3),
          valueBoxOutput("box_loyer_moyen", width = 3),
          valueBoxOutput("box_loyer_median", width = 3),
          valueBoxOutput("box_precision", width = 3)
        ),
        
        fluidRow(
          box(title = "Distribution des Loyers", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("plot_dist_loyer", height = "300px")),
          box(title = "Segmentation du Marché", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("plot_segments", height = "300px"))
        ),
        
        fluidRow(
          box(title = "Loyers par Type de Logement", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("plot_type", height = "300px")),
          box(title = "Impact du Standing", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("plot_standing", height = "300px"))
        )
      ),
      
      # ========== EXPLORATION ==========
      tabItem(
        tabName = "exploration",
        h2("Exploration des Données", style = "color: #2E86AB; font-weight: bold;"),
        
        fluidRow(
          box(title = "Filtres", status = "primary", solidHeader = TRUE, width = 3,
              selectInput("filter_type", "Type de logement:",
                          choices = c("Tous" = "all", levels(dakar_clean$Type)), selected = "all"),
              selectInput("filter_standing", "Standing:",
                          choices = c("Tous" = "all", "Oui", "Non"), selected = "all"),
              selectInput("filter_quartier", "Standing Quartier:",
                          choices = c("Tous" = "all", "Populaire", "Moyen", "Bourgeois"), selected = "all"),
              sliderInput("filter_loyer", "Plage de loyer (FCFA):",
                          min = 0, max = max(dakar_clean$Loyer),
                          value = c(0, max(dakar_clean$Loyer)), step = 50)
          ),
          
          box(title = "Statistiques Filtrées", status = "info", solidHeader = TRUE, width = 9,
              fluidRow(
                valueBoxOutput("filtered_count", width = 3),
                valueBoxOutput("filtered_mean", width = 3),
                valueBoxOutput("filtered_median", width = 3),
                valueBoxOutput("filtered_sd", width = 3)
              ),
              plotlyOutput("plot_filtered_dist", height = "300px")
          )
        ),
        
        fluidRow(
          box(title = "Relation Loyer vs Surface", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("plot_scatter", height = "350px")),
          box(title = "Prix au m² Habitable", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("plot_prixm2", height = "350px"))
        )
      ),
      
      # ========== DONNÉES ==========
      tabItem(
        tabName = "donnees",
        h2("Données Brutes", style = "color: #2E86AB; font-weight: bold;"),
        fluidRow(
          box(title = "Table des Logements", status = "primary", solidHeader = TRUE, width = 12,
              DTOutput("table_donnees"))
        )
      ),
      
      # ========== À PROPOS ==========
      tabItem(
        tabName = "apropos",
        h2("À propos de l'Application", style = "color: #2E86AB; font-weight: bold;"),
        fluidRow(
          box(title = "Description", status = "primary", solidHeader = TRUE, width = 12,
              h4("🎯 Objectif"),
              p("Cette application interactive permet d'explorer et d'analyser le marché immobilier locatif à Dakar."),
              
              h4("📊 Fonctionnalités"),
              tags$ul(
                tags$li(strong("Tableau de Bord :"), " Vue d'ensemble avec statistiques clés et visualisations"),
                tags$li(strong("Exploration :"), " Analyse approfondie avec filtres personnalisables"),
                tags$li(strong("Données :"), " Accès à la base de données complète des logements")
              ),
              
              h4("📈 Statistiques"),
              p(strong(format(nrow(dakar_clean), big.mark = " ")), " logements analysés à Dakar"),
              p("Loyer moyen : ", strong(format(round(mean(dakar_clean$Loyer), 0), big.mark = " "), "FCFA")),
              p("Loyer médian : ", strong(format(round(median(dakar_clean$Loyer), 0), big.mark = " "), "FCFA")),
              
              hr(),
              
              p(em("Application développée avec Shiny - R"), style = "text-align: center; color: #666;")
          )
        )
      )
    )
  )
)

# ============================================================================
# SERVEUR
# ============================================================================

server <- function(input, output, session) {
  
  # ========== VALUE BOXES ==========
  output$box_nb_logements <- renderValueBox({
    valueBox(
      format(nrow(dakar_clean), big.mark = " "), 
      "Logements", 
      icon = icon("home"), 
      color = "blue"
    )
  })
  
  output$box_loyer_moyen <- renderValueBox({
    valueBox(
      paste(format(round(mean(dakar_clean$Loyer), 0), big.mark = " "), "FCFA"), 
      "Loyer Moyen", 
      icon = icon("coins"), 
      color = "green"
    )
  })
  
  output$box_loyer_median <- renderValueBox({
    valueBox(
      paste(format(round(median(dakar_clean$Loyer), 0), big.mark = " "), "FCFA"), 
      "Loyer Médian", 
      icon = icon("chart-line"), 
      color = "orange"
    )
  })
  
  output$box_precision <- renderValueBox({
    valueBox(
      paste0(format(nrow(dakar_clean), big.mark = " ")), 
      "Observations", 
      icon = icon("database"), 
      color = "purple"
    )
  })
  
  # ========== GRAPHIQUES DASHBOARD ==========
  output$plot_dist_loyer <- renderPlotly({
    p <- ggplot(dakar_clean, aes(x = Loyer)) +
      geom_histogram(fill = couleurs[1], color = "white", bins = 30, alpha = 0.8) +
      labs(x = "Loyer (FCFA)", y = "Fréquence") + 
      theme_minimal()
    ggplotly(p) |> layout(showlegend = FALSE)
  })
  
  output$plot_segments <- renderPlotly({
    segments_count <- dakar_clean |>
      count(Segment) |>
      mutate(Pourcentage = n / sum(n) * 100)
    
    plot_ly(segments_count, 
            labels = ~Segment, 
            values = ~n, 
            type = 'pie',
            marker = list(colors = couleurs), 
            textinfo = 'label+percent')
  })
  
  output$plot_type <- renderPlotly({
    p <- ggplot(dakar_clean, aes(x = Type, y = Loyer, fill = Type)) +
      geom_boxplot(alpha = 0.8) + 
      scale_fill_manual(values = couleurs) +
      labs(x = "", y = "Loyer (FCFA)") + 
      theme_minimal() + 
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  output$plot_standing <- renderPlotly({
    p <- ggplot(dakar_clean, aes(x = StandingQuartier, y = Loyer, fill = StandingQuartier)) +
      geom_boxplot(alpha = 0.8) + 
      scale_fill_manual(values = couleurs[1:3]) +
      labs(x = "Standing du Quartier", y = "Loyer (FCFA)") + 
      theme_minimal() + 
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  # ========== EXPLORATION ==========
  donnees_filtrees <- reactive({
    data <- dakar_clean
    
    if (input$filter_type != "all") {
      data <- data |> filter(Type == input$filter_type)
    }
    
    if (input$filter_standing != "all") {
      data <- data |> filter(Standing == input$filter_standing)
    }
    
    if (input$filter_quartier != "all") {
      data <- data |> filter(StandingQuartier == input$filter_quartier)
    }
    
    data <- data |> 
      filter(Loyer >= input$filter_loyer[1] & Loyer <= input$filter_loyer[2])
    
    return(data)
  })
  
  output$filtered_count <- renderValueBox({
    valueBox(nrow(donnees_filtrees()), "Logements", icon = icon("filter"), color = "light-blue")
  })
  
  output$filtered_mean <- renderValueBox({
    valueBox(
      format(round(mean(donnees_filtrees()$Loyer), 0), big.mark = " "), 
      "Moyenne", 
      icon = icon("calculator"), 
      color = "green"
    )
  })
  
  output$filtered_median <- renderValueBox({
    valueBox(
      format(round(median(donnees_filtrees()$Loyer), 0), big.mark = " "), 
      "Médiane", 
      icon = icon("chart-line"), 
      color = "yellow"
    )
  })
  
  output$filtered_sd <- renderValueBox({
    valueBox(
      format(round(sd(donnees_filtrees()$Loyer), 0), big.mark = " "), 
      "Écart-type", 
      icon = icon("signal"), 
      color = "red"
    )
  })
  
  output$plot_filtered_dist <- renderPlotly({
    p <- ggplot(donnees_filtrees(), aes(x = Loyer)) +
      geom_histogram(fill = couleurs[2], color = "white", bins = 25, alpha = 0.8) +
      labs(x = "Loyer (FCFA)", y = "Fréquence") + 
      theme_minimal()
    ggplotly(p)
  })
  
  output$plot_scatter <- renderPlotly({
    p <- ggplot(donnees_filtrees(), aes(x = SurfHabitable, y = Loyer, color = Type)) +
      geom_point(alpha = 0.6, size = 2) + 
      geom_smooth(method = "lm", se = FALSE, color = "red") +
      scale_color_manual(values = couleurs) +
      labs(x = "Surface Habitable (m²)", y = "Loyer (FCFA)") + 
      theme_minimal()
    ggplotly(p)
  })
  
  output$plot_prixm2 <- renderPlotly({
    data_clean <- donnees_filtrees() |> 
      filter(!is.na(PrixM2Habitable), PrixM2Habitable < 20)
    
    p <- ggplot(data_clean, aes(x = Type, y = PrixM2Habitable, fill = Type)) +
      geom_boxplot(alpha = 0.8) + 
      scale_fill_manual(values = couleurs) +
      labs(x = "", y = "Prix au m² (FCFA)") + 
      theme_minimal() + 
      theme(legend.position = "none")
    ggplotly(p)
  })
  
  # ========== DONNÉES ==========
  output$table_donnees <- renderDT({
    datatable(
      dakar_clean, 
      options = list(
        pageLength = 25, 
        scrollX = TRUE, 
        scrollY = "500px",
        dom = 'Bfrtip'
      ),
      rownames = FALSE,
      filter = 'top'
    )
  })
}

# ============================================================================
# LANCER L'APPLICATION
# ============================================================================

shinyApp(ui = ui, server = server)