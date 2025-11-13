# Application Shiny - Analyse des Loyers à Dakar (VERSION CORRIGÉE)
# install.packages(c("shiny", "shinydashboard", "dplyr", "ggplot2", "plotly", "DT", "tidyr"))

library(shiny)
library(shinydashboard)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(scales)
library(tidyr)

# ============================================================================
# CHARGEMENT ET PRÉPARATION DES DONNÉES
# ============================================================================

dakar <- read.csv("dakar.csv", stringsAsFactors = FALSE)

dakar_clean <- dakar %>%
  drop_na(Loyer) %>%
  mutate(
    PrixM2Terrain = ifelse(SurfTerrain > 0, Loyer / SurfTerrain, NA),
    PrixM2Habitable = ifelse(SurfHabitable > 0, Loyer / SurfHabitable, NA),
    RatioHabitable = ifelse(SurfTerrain > 0, SurfHabitable / SurfTerrain, NA),
    TotalSDB_WC = NbSDB + NbWC,
    Standing = factor(Standing, levels = c("Non", "Oui")),
    Etat = factor(Etat, levels = c("Vetuste", "Mediocre", "Bon", "Neuf")),
    StandingQuartier = factor(StandingQuartier, 
                              levels = c("popu", "moy", "bourge"),
                              labels = c("Populaire", "Moyen", "Bourgeois")),
    Type = factor(Type),
    Piscine = factor(Piscine, levels = c("Non", "Oui")),
    Garage = factor(Garage, levels = c("Non", "Oui", "Park", "Priv1v", "Priv2v")),
    BordMer = factor(BordMer, levels = c("Non", "Oui", "inf2km")),
    QuartierAffaires = if("QuartierAffaires" %in% names(dakar)) {
      factor(QuartierAffaires, levels = c("Non", "Oui"))
    } else {
      factor(rep("Non", nrow(dakar)), levels = c("Non", "Oui"))
    },
    Segment = case_when(
      Loyer >= 600 ~ "Haut standing",
      Loyer >= 200 ~ "Moyen standing",
      TRUE ~ "Économique"
    )
  )

# Vérifier variables pour le modèle
vars_a_inclure <- c("SurfHabitable", "NbChamBur", "NbSDB")
vars_facteurs <- c("Type", "Standing", "Etat", "StandingQuartier", "Piscine", "Garage", "BordMer")

for(var in vars_facteurs) {
  niveaux_valides <- unique(dakar_clean[[var]][!is.na(dakar_clean[[var]])])
  if(length(niveaux_valides) > 1) {
    vars_a_inclure <- c(vars_a_inclure, var)
  }
}

formule_modele <- as.formula(paste("Loyer ~", paste(vars_a_inclure, collapse = " + ")))
dakar_modele <- dakar_clean[complete.cases(dakar_clean[, c("Loyer", vars_a_inclure)]), ]
modele_complet <- lm(formule_modele, data = dakar_modele)
vars_modele <- vars_a_inclure

couleurs <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#6A994E")

# ============================================================================
# FONCTION D'ESTIMATION DU LOYER (FORMULE CORRIGÉE - BASE 0.45)
# ============================================================================

estimer_loyer <- function(surface, type, standing_quartier, etat, standing_interieur, 
                          piscine, garage, bordmer, quartier_affaires) {
  
  # PRIX DE BASE CORRIGÉ : 0.45 FCFA/m²
  prix_base_m2 <- 0.45
  loyer_base <- surface * prix_base_m2
  
  # Facteur Type
  facteur_type <- switch(type,
                         "Appart" = 1.0,
                         "Villa" = 2.2,
                         1.0)
  
  # Facteur Standing Quartier
  facteur_quartier <- switch(standing_quartier,
                             "Populaire" = 0.7,
                             "Moyen" = 1.0,
                             "Bourgeois" = 1.5,
                             1.0)
  
  # Facteur État
  facteur_etat <- switch(etat,
                         "Vetuste" = 0.7,
                         "Mediocre" = 0.85,
                         "Bon" = 1.0,
                         "Neuf" = 1.25,
                         1.0)
  
  # Facteur Standing Intérieur
  facteur_standing <- ifelse(standing_interieur == "Oui", 1.25, 1.0)
  
  # Facteur Piscine
  facteur_piscine <- ifelse(piscine == "Oui", 1.25, 1.0)
  
  # Facteur Garage
  facteur_garage <- ifelse(garage == "Oui", 1.1, 1.0)
  
  # Facteur Bord de mer (inf2km)
  facteur_bordmer <- ifelse(bordmer %in% c("Oui", "inf2km"), 1.2, 1.0)
  
  # Facteur Quartier d'Affaires
  facteur_affaires <- ifelse(quartier_affaires == "Oui", 2.0, 1.0)
  
  # Calcul final
  loyer_estime <- loyer_base * facteur_type * facteur_quartier * facteur_etat * 
    facteur_standing * facteur_piscine * facteur_garage * 
    facteur_bordmer * facteur_affaires
  
  return(list(
    loyer = loyer_estime,
    prix_base_m2 = prix_base_m2,
    facteurs = list(
      type = facteur_type,
      quartier = facteur_quartier,
      etat = facteur_etat,
      standing = facteur_standing,
      piscine = facteur_piscine,
      garage = facteur_garage,
      bordmer = facteur_bordmer,
      affaires = facteur_affaires
    )
  ))
}

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
      menuItem("🏠 Estimateur", tabName = "estimateur", icon = icon("calculator")),
      menuItem("📈 Modélisation", tabName = "modelisation", icon = icon("chart-area")),
      menuItem("📋 Données", tabName = "donnees", icon = icon("table")),
      menuItem("ℹ️ À propos", tabName = "apropos", icon = icon("info-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper { background-color: #f4f6f9; }
        .box { border-top: 3px solid #2E86AB; }
        .info-box { min-height: 90px; }
        .small-box { border-radius: 5px; }
        .btn-primary { background-color: #2E86AB; border-color: #2E86AB; }
        .btn-primary:hover { background-color: #1f5f7a; }
      "))
    ),
    
    tabItems(
      # TABLEAU DE BORD
      tabItem(
        tabName = "dashboard",
        h2("Tableau de Bord - Vue d'Ensemble", style = "color: #2E86AB; font-weight: bold;"),
        
        fluidRow(
          valueBoxOutput("box_nb_logements", width = 3),
          valueBoxOutput("box_loyer_moyen", width = 3),
          valueBoxOutput("box_loyer_median", width = 3),
          valueBoxOutput("box_r2", width = 3)
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
      
      # EXPLORATION
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
      
      # ESTIMATEUR (VERSION CORRIGÉE)
      tabItem(
        tabName = "estimateur",
        h2("Estimateur de Loyer", style = "color: #2E86AB; font-weight: bold;"),
        
        fluidRow(
          box(title = "Caractéristiques du Logement", status = "primary", solidHeader = TRUE, width = 4,
              numericInput("est_surf", "Surface habitable (m²):", value = 294, min = 20, max = 1000),
              selectInput("est_type", "Type:", choices = levels(dakar_clean$Type), selected = "Villa"),
              selectInput("est_standing", "Standing:", choices = c("Non", "Oui"), selected = "Oui"),
              selectInput("est_etat", "État:", choices = c("Vetuste", "Mediocre", "Bon", "Neuf"), selected = "Neuf"),
              selectInput("est_quartier", "Standing Quartier:", 
                          choices = c("Populaire", "Moyen", "Bourgeois"), selected = "Bourgeois"),
              selectInput("est_piscine", "Piscine:", choices = c("Non", "Oui"), selected = "Non"),
              selectInput("est_garage", "Garage:", choices = c("Non", "Oui"), selected = "Oui"),
              selectInput("est_bordmer", "Bord de mer:", choices = c("Non", "Oui"), selected = "Oui"),
              selectInput("est_affaires", "Quartier d'Affaires:", choices = c("Non", "Oui"), selected = "Non"),
              hr(),
              p(style = "font-size: 0.9em; color: #6c757d;", 
                icon("info-circle"), " Prix de base : 0.45 FCFA/m² (selon analyse corrigée)"),
              actionButton("btn_estimer", "Estimer le Loyer", 
                           class = "btn-primary btn-block", icon = icon("calculator"))
          ),
          
          box(title = "Estimation", status = "success", solidHeader = TRUE, width = 8,
              uiOutput("estimation_result"),
              hr(),
              h4("Logements Similaires", style = "color: #2E86AB;"),
              DTOutput("table_similaires")
          )
        )
      ),
      
      # MODÉLISATION
      tabItem(
        tabName = "modelisation",
        h2("Analyse du Modèle", style = "color: #2E86AB; font-weight: bold;"),
        
        fluidRow(
          box(title = "Performance du Modèle", status = "primary", solidHeader = TRUE, width = 6,
              DTOutput("table_performance")),
          box(title = "Importance des Variables", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("plot_importance", height = "300px"))
        ),
        
        fluidRow(
          box(title = "Prédictions vs Observations", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("plot_pred_obs", height = "350px")),
          box(title = "Distribution des Résidus", status = "primary", solidHeader = TRUE, width = 6,
              plotlyOutput("plot_residus", height = "350px"))
        ),
        
        fluidRow(
          box(title = "Coefficients du Modèle", status = "info", solidHeader = TRUE, width = 12,
              DTOutput("table_coefficients"))
        )
      ),
      
      # DONNÉES
      tabItem(
        tabName = "donnees",
        h2("Données Brutes", style = "color: #2E86AB; font-weight: bold;"),
        fluidRow(
          box(title = "Table des Logements", status = "primary", solidHeader = TRUE, width = 12,
              DTOutput("table_donnees"))
        )
      ),
      
      # À PROPOS
      tabItem(
        tabName = "apropos",
        h2("À propos de l'Application", style = "color: #2E86AB; font-weight: bold;"),
        fluidRow(
          box(title = "Description", status = "primary", solidHeader = TRUE, width = 12,
              h4("Objectif"),
              p("Cette application interactive permet d'explorer et d'analyser le marché immobilier locatif à Dakar."),
              h4("Méthodologie de l'Estimateur (VERSION CORRIGÉE)"),
              p(strong("Formule :"), "Loyer = (Surface × 0.45) × F_type × F_quartier × F_etat × F_standing × F_piscine × F_garage × F_bordmer × F_affaires"),
              tags$ul(
                tags$li(strong("Prix de base : 0.45 FCFA/m²")),
                tags$li("Facteur Type : Appart (×1.0), Villa (×2.2)"),
                tags$li("Facteur Quartier : Populaire (×0.7), Moyen (×1.0), Bourgeois (×1.5)"),
                tags$li("Facteur État : Vetuste (×0.7), Mediocre (×0.85), Bon (×1.0), Neuf (×1.25)"),
                tags$li("Facteur Standing intérieur : Oui (×1.25), Non (×1.0)"),
                tags$li("Facteur Piscine : Oui (×1.25)"),
                tags$li("Facteur Garage : Oui (×1.1)"),
                tags$li("Facteur Bord de mer : Oui ou inf2km (×1.2)"),
                tags$li("Facteur Quartier d'Affaires : Oui (×2.0)")
              ),
              h4("Exemple de Calcul"),
              p("Villa neuve, 294 m², Standing Oui, Quartier Bourgeois, Garage Oui, Bord de mer Oui"),
              p(strong("= 294 × 0.45 × 2.2 × 1.5 × 1.25 × 1.25 × 1.1 × 1.2 = ≈ 850 000 FCFA")),
              hr(),
              p(em("Application développée avec Shiny - R"), style = "text-align: center;")
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
  
  # VALUE BOXES
  output$box_nb_logements <- renderValueBox({
    valueBox(format(nrow(dakar_clean), big.mark = " "), "Logements", icon = icon("home"), color = "blue")
  })
  
  output$box_loyer_moyen <- renderValueBox({
    valueBox(paste(format(round(mean(dakar_clean$Loyer), 0), big.mark = " "), "FCFA"), 
             "Loyer Moyen", icon = icon("coins"), color = "green")
  })
  
  output$box_loyer_median <- renderValueBox({
    valueBox(paste(format(round(median(dakar_clean$Loyer), 0), big.mark = " "), "FCFA"), 
             "Loyer Médian", icon = icon("chart-line"), color = "orange")
  })
  
  output$box_r2 <- renderValueBox({
    valueBox(paste0(round(summary(modele_complet)$adj.r.squared * 100, 1), "%"), 
             "R² du Modèle", icon = icon("chart-area"), color = "purple")
  })
  
  # GRAPHIQUES DASHBOARD
  output$plot_dist_loyer <- renderPlotly({
    p <- ggplot(dakar_clean, aes(x = Loyer)) +
      geom_histogram(fill = couleurs[1], color = "white", bins = 30, alpha = 0.8) +
      labs(x = "Loyer (FCFA)", y = "Fréquence") + theme_minimal()
    ggplotly(p) %>% layout(showlegend = FALSE)
  })
  
  output$plot_segments <- renderPlotly({
    segments_count <- dakar_clean %>% count(Segment) %>% mutate(Pourcentage = n / sum(n) * 100)
    plot_ly(segments_count, labels = ~Segment, values = ~n, type = 'pie',
            marker = list(colors = couleurs), textinfo = 'label+percent')
  })
  
  output$plot_type <- renderPlotly({
    p <- ggplot(dakar_clean, aes(x = Type, y = Loyer, fill = Type)) +
      geom_boxplot(alpha = 0.8) + scale_fill_manual(values = couleurs) +
      labs(x = "", y = "Loyer (FCFA)") + theme_minimal() + theme(legend.position = "none")
    ggplotly(p)
  })
  
  output$plot_standing <- renderPlotly({
    p <- ggplot(dakar_clean, aes(x = StandingQuartier, y = Loyer, fill = StandingQuartier)) +
      geom_boxplot(alpha = 0.8) + scale_fill_manual(values = couleurs[1:3]) +
      labs(x = "Standing du Quartier", y = "Loyer (FCFA)") + theme_minimal() + theme(legend.position = "none")
    ggplotly(p)
  })
  
  # EXPLORATION
  donnees_filtrees <- reactive({
    data <- dakar_clean
    if (input$filter_type != "all") data <- data %>% filter(Type == input$filter_type)
    if (input$filter_standing != "all") data <- data %>% filter(Standing == input$filter_standing)
    if (input$filter_quartier != "all") data <- data %>% filter(StandingQuartier == input$filter_quartier)
    data <- data %>% filter(Loyer >= input$filter_loyer[1] & Loyer <= input$filter_loyer[2])
    return(data)
  })
  
  output$filtered_count <- renderValueBox({
    valueBox(nrow(donnees_filtrees()), "Logements", icon = icon("filter"), color = "light-blue")
  })
  
  output$filtered_mean <- renderValueBox({
    valueBox(format(round(mean(donnees_filtrees()$Loyer), 0), big.mark = " "), 
             "Moyenne", icon = icon("calculator"), color = "green")
  })
  
  output$filtered_median <- renderValueBox({
    valueBox(format(round(median(donnees_filtrees()$Loyer), 0), big.mark = " "), 
             "Médiane", icon = icon("chart-line"), color = "yellow")
  })
  
  output$filtered_sd <- renderValueBox({
    valueBox(format(round(sd(donnees_filtrees()$Loyer), 0), big.mark = " "), 
             "Écart-type", icon = icon("signal"), color = "red")
  })
  
  output$plot_filtered_dist <- renderPlotly({
    p <- ggplot(donnees_filtrees(), aes(x = Loyer)) +
      geom_histogram(fill = couleurs[2], color = "white", bins = 25, alpha = 0.8) +
      labs(x = "Loyer (FCFA)", y = "Fréquence") + theme_minimal()
    ggplotly(p)
  })
  
  output$plot_scatter <- renderPlotly({
    p <- ggplot(donnees_filtrees(), aes(x = SurfHabitable, y = Loyer, color = Type)) +
      geom_point(alpha = 0.6, size = 2) + geom_smooth(method = "lm", se = FALSE, color = "red") +
      scale_color_manual(values = couleurs) +
      labs(x = "Surface Habitable (m²)", y = "Loyer (FCFA)") + theme_minimal()
    ggplotly(p)
  })
  
  output$plot_prixm2 <- renderPlotly({
    data_clean <- donnees_filtrees() %>% filter(!is.na(PrixM2Habitable), PrixM2Habitable < 20)
    p <- ggplot(data_clean, aes(x = Type, y = PrixM2Habitable, fill = Type)) +
      geom_boxplot(alpha = 0.8) + scale_fill_manual(values = couleurs) +
      labs(x = "", y = "Prix au m² (FCFA)") + theme_minimal() + theme(legend.position = "none")
    ggplotly(p)
  })
  
  # ESTIMATEUR
  estimation_data <- reactiveVal(NULL)
  
  observeEvent(input$btn_estimer, {
    tryCatch({
      surf_input <- max(as.numeric(input$est_surf), 20)
      
      # CALCUL AVEC FORMULE CORRIGÉE (0.45)
      estimation <- estimer_loyer(
        surface = surf_input,
        type = input$est_type,
        standing_quartier = input$est_quartier,
        etat = input$est_etat,
        standing_interieur = input$est_standing,
        piscine = input$est_piscine,
        garage = input$est_garage,
        bordmer = input$est_bordmer,
        quartier_affaires = input$est_affaires
      )
      
      # FILTRER LES LOGEMENTS SIMILAIRES
      logements_filtres <- dakar_clean %>%
        filter(Type == input$est_type,
               Standing == input$est_standing,
               Etat == input$est_etat,
               StandingQuartier == input$est_quartier,
               Piscine == input$est_piscine,
               BordMer %in% if(input$est_bordmer == "Oui") c("Oui", "inf2km") else c("Non"),
               QuartierAffaires == input$est_affaires)
      
      if(input$est_garage == "Oui") {
        logements_filtres <- logements_filtres %>% filter(Garage != "Non")
      } else {
        logements_filtres <- logements_filtres %>% filter(Garage == "Non")
      }
      
      nb_resultats <- nrow(logements_filtres)
      
      if(nb_resultats == 0) {
        estimation_data(list(
          aucun_resultat = TRUE,
          loyer_formule = estimation$loyer,
          prix_base_m2 = estimation$prix_base_m2,
          facteurs = estimation$facteurs,
          inputs = list(surf = surf_input, type = input$est_type, standing = input$est_standing,
                        etat = input$est_etat, quartier = input$est_quartier, 
                        piscine = input$est_piscine, garage = input$est_garage,
                        bordmer = input$est_bordmer, affaires = input$est_affaires)
        ))
      } else {
        loyer_moyen_filtre <- mean(logements_filtres$Loyer, na.rm = TRUE)
        estimation_data(list(
          aucun_resultat = FALSE,
          loyer_formule = estimation$loyer,
          prix_base_m2 = estimation$prix_base_m2,
          facteurs = estimation$facteurs,
          loyer_moyen_filtre = loyer_moyen_filtre,
          nb_resultats = nb_resultats,
          logements_filtres = logements_filtres,
          inputs = list(surf = surf_input, type = input$est_type, standing = input$est_standing,
                        etat = input$est_etat, quartier = input$est_quartier,
                        piscine = input$est_piscine, garage = input$est_garage,
                        bordmer = input$est_bordmer, affaires = input$est_affaires)
        ))
      }
    }, error = function(e) {
      showNotification(paste("Erreur:", e$message), type = "error", duration = 5)
    })
  })
  
  output$estimation_result <- renderUI({
    req(estimation_data())
    data <- estimation_data()
    inputs <- data$inputs
    
    loyer_formule_fmt <- format(round(data$loyer_formule, 0), big.mark = " ")
    
    if(data$aucun_resultat) {
      tagList(
        div(style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); padding: 30px; border-radius: 10px; color: white; text-align: center; margin-bottom: 20px;",
            h2(style = "margin: 0;", "Loyer Estimé (Formule)"),
            h1(style = "margin: 10px 0; font-size: 3em; font-weight: bold;", 
               paste(loyer_formule_fmt, "FCFA")),
            p(style = "margin: 0; font-size: 1.1em;", 
              paste("Surface:", inputs$surf, "m² × ", data$prix_base_m2, " FCFA/m²")),
            p(style = "margin-top: 10px; font-size: 0.9em; opacity: 0.9;",
              icon("calculator"), " Basée sur coefficients d'analyse")
        ),
        div(style = "background: #fff3cd; padding: 20px; border-radius: 5px; border-left: 4px solid #ffc107;",
            h3(style = "margin: 0; color: #856404;", icon("exclamation-triangle"), " Aucun logement similaire trouvé"),
            p(style = "margin-top: 10px; color: #856404;", 
              "L'estimation est basée uniquement sur la formule multiplicative.")
        ),
        div(style = "background: #f8f9fa; padding: 15px; border-radius: 5px; border-left: 4px solid #2E86AB; margin-top: 15px;",
            h4("Détails du Calcul", style = "color: #2E86AB;"),
            p("Prix de base: ", inputs$surf, " m² × ", data$prix_base_m2, " FCFA/m² = ", 
              format(round(inputs$surf * data$prix_base_m2, 0), big.mark = " "), " FCFA"),
            p("Facteurs appliqués:"),
            tags$ul(
              tags$li("Type ", inputs$type, ": ×", data$facteurs$type),
              tags$li("Quartier ", inputs$quartier, ": ×", data$facteurs$quartier),
              tags$li("État ", inputs$etat, ": ×", data$facteurs$etat),
              tags$li("Standing intérieur: ×", data$facteurs$standing),
              tags$li("Piscine: ×", data$facteurs$piscine),
              tags$li("Garage: ×", data$facteurs$garage),
              tags$li("Bord de mer: ×", data$facteurs$bordmer),
              tags$li("Quartier d'Affaires: ×", data$facteurs$affaires)
            )
        )
      )
    } else {
      loyer_moyen_fmt <- format(round(data$loyer_moyen_filtre, 0), big.mark = " ")
      ecart <- data$loyer_formule - data$loyer_moyen_filtre
      ecart_pct <- (ecart / data$loyer_moyen_filtre) * 100
      
      tagList(
        div(style = "background: #d4edda; padding: 15px; border-radius: 5px; border-left: 4px solid #28a745; margin-bottom: 20px;",
            p(style = "margin: 0; color: #155724;", 
              icon("check-circle"), " ", strong(data$nb_resultats), 
              " logement(s) trouvé(s) correspondant exactement à vos critères.")
        ),
        
        fluidRow(
          column(6,
                 div(style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); padding: 25px; border-radius: 10px; color: white; text-align: center;",
                     h3(style = "margin: 0; font-size: 1.3em;", "Estimation par Formule"),
                     h2(style = "margin: 10px 0; font-size: 2.5em; font-weight: bold;", 
                        paste(loyer_formule_fmt, "FCFA")),
                     p(style = "margin: 0; font-size: 0.95em; opacity: 0.9;",
                       icon("calculator"), " Base: ", data$prix_base_m2, " FCFA/m²")
                 )
          ),
          column(6,
                 div(style = "background: linear-gradient(135deg, #11998e 0%, #38ef7d 100%); padding: 25px; border-radius: 10px; color: white; text-align: center;",
                     h3(style = "margin: 0; font-size: 1.3em;", "Moyenne Logements Réels"),
                     h2(style = "margin: 10px 0; font-size: 2.5em; font-weight: bold;", 
                        paste(loyer_moyen_fmt, "FCFA")),
                     p(style = "margin: 0; font-size: 0.95em; opacity: 0.9;",
                       icon("database"), " Basée sur ", data$nb_resultats, " logement(s)")
                 )
          )
        ),
        
        br(),
        
        div(style = paste0("background: ", 
                           ifelse(abs(ecart_pct) < 10, "#d1ecf1", 
                                  ifelse(abs(ecart_pct) < 25, "#fff3cd", "#f8d7da")),
                           "; padding: 15px; border-radius: 5px; border-left: 4px solid ",
                           ifelse(abs(ecart_pct) < 10, "#0c5460", 
                                  ifelse(abs(ecart_pct) < 25, "#856404", "#721c24")), ";"),
            h4(style = paste0("color: ", 
                              ifelse(abs(ecart_pct) < 10, "#0c5460", 
                                     ifelse(abs(ecart_pct) < 25, "#856404", "#721c24")),
                              "; margin-top: 0;"), 
               icon("balance-scale"), " Comparaison"),
            p(style = paste0("margin: 5px 0; color: ", 
                             ifelse(abs(ecart_pct) < 10, "#0c5460", 
                                    ifelse(abs(ecart_pct) < 25, "#856404", "#721c24"))),
              strong("Écart:"), " ", 
              ifelse(ecart > 0, "+", ""), format(round(ecart, 0), big.mark = " "), " FCFA",
              " (", ifelse(ecart > 0, "+", ""), round(ecart_pct, 1), "%)"),
            p(style = paste0("margin: 5px 0 0 0; font-size: 0.9em; color: ", 
                             ifelse(abs(ecart_pct) < 10, "#0c5460", 
                                    ifelse(abs(ecart_pct) < 25, "#856404", "#721c24"))),
              ifelse(abs(ecart_pct) < 10, 
                     "✓ Excellente cohérence entre la formule et les données réelles",
                     ifelse(abs(ecart_pct) < 25,
                            "⚠ Écart modéré - La formule est globalement cohérente",
                            "⚠ Écart important - Variations liées aux caractéristiques spécifiques")))
        ),
        
        br(),
        
        div(style = "background: #f8f9fa; padding: 15px; border-radius: 5px; border-left: 4px solid #2E86AB;",
            h4("Détails du Calcul (Formule)", style = "color: #2E86AB; margin-top: 0;"),
            p("Base: ", inputs$surf, " m² × ", data$prix_base_m2, " = ", 
              format(round(inputs$surf * data$prix_base_m2, 0), big.mark = " "), " FCFA"),
            fluidRow(
              column(6,
                     tags$ul(
                       tags$li("Type: ×", data$facteurs$type),
                       tags$li("Quartier: ×", data$facteurs$quartier),
                       tags$li("État: ×", data$facteurs$etat),
                       tags$li("Standing: ×", data$facteurs$standing)
                     )
              ),
              column(6,
                     tags$ul(
                       tags$li("Piscine: ×", data$facteurs$piscine),
                       tags$li("Garage: ×", data$facteurs$garage),
                       tags$li("Bord mer: ×", data$facteurs$bordmer),
                       tags$li("Affaires: ×", data$facteurs$affaires)
                     )
              )
            )
        )
      )
    }
  })
  
  output$table_similaires <- renderDT({
    req(estimation_data())
    data <- estimation_data()
    
    if(data$aucun_resultat) {
      return(datatable(
        data.frame(Message = "Aucun logement ne correspond à vos critères exactes."),
        options = list(dom = 't'), rownames = FALSE
      ))
    }
    
    logements_display <- data$logements_filtres %>%
      select(Type, SurfHabitable, NbChamBur, NbSDB, Standing, Etat, 
             StandingQuartier, Piscine, Garage, BordMer, Loyer) %>%
      mutate(
        Loyer = format(Loyer, big.mark = " "),
        SurfHabitable = round(SurfHabitable, 0)
      ) %>%
      rename("Surface" = SurfHabitable, "Chambres" = NbChamBur, "SDB" = NbSDB,
             "État" = Etat, "Quartier" = StandingQuartier, "Loyer (FCFA)" = Loyer)
    
    datatable(logements_display, 
              options = list(pageLength = 10, dom = 'tp', scrollX = TRUE),
              rownames = FALSE)
  })
  
  # MODÉLISATION
  output$table_performance <- renderDT({
    rmse <- sqrt(mean(residuals(modele_complet)^2))
    performance <- data.frame(
      Métrique = c("R²", "R² ajusté", "RMSE", "AIC", "BIC", "N observations"),
      Valeur = c(
        round(summary(modele_complet)$r.squared, 4),
        round(summary(modele_complet)$adj.r.squared, 4),
        paste(format(round(rmse, 2), big.mark = " "), "FCFA"),
        format(round(AIC(modele_complet), 2), big.mark = " "),
        format(round(BIC(modele_complet), 2), big.mark = " "),
        format(nrow(dakar_modele), big.mark = " ")
      )
    )
    datatable(performance, options = list(dom = 't'), rownames = FALSE)
  })
  
  output$plot_importance <- renderPlotly({
    coefs <- summary(modele_complet)$coefficients[-1, ]
    importance <- data.frame(
      Variable = rownames(coefs),
      Coefficient = abs(coefs[, "Estimate"]),
      Pvalue = coefs[, "Pr(>|t|)"]
    ) %>%
      filter(Pvalue < 0.05) %>%
      arrange(desc(Coefficient)) %>%
      head(10)
    
    plot_ly(importance, x = ~Coefficient, y = ~reorder(Variable, Coefficient),
            type = 'bar', orientation = 'h',
            marker = list(color = couleurs[1])) %>%
      layout(xaxis = list(title = "Impact (Coefficient absolu)"),
             yaxis = list(title = ""),
             margin = list(l = 150))
  })
  
  output$plot_pred_obs <- renderPlotly({
    predictions <- predict(modele_complet)
    data_plot <- data.frame(Observe = dakar_modele$Loyer, Predit = predictions)
    
    # Calculer R²
    r2 <- round(summary(modele_complet)$r.squared, 3)
    
    p <- ggplot(data_plot, aes(x = Observe, y = Predit)) +
      geom_point(alpha = 0.5, color = couleurs[1], size = 2) +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
      labs(x = "Loyer Observé (FCFA)", y = "Loyer Prédit (FCFA)",
           title = paste0("R² = ", r2)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, color = couleurs[1], face = "bold"))
    
    ggplotly(p)
  })
  
  output$plot_residus <- renderPlotly({
    residus <- residuals(modele_complet)
    
    p <- ggplot(data.frame(Residus = residus), aes(x = Residus)) +
      geom_histogram(fill = couleurs[2], color = "white", bins = 30, alpha = 0.8) +
      geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
      labs(x = "Résidus (FCFA)", y = "Fréquence",
           title = paste0("Moyenne = ", round(mean(residus), 2), " | SD = ", round(sd(residus), 2))) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = 0.5, size = 10))
    
    ggplotly(p)
  })
  
  output$table_coefficients <- renderDT({
    coefs <- as.data.frame(summary(modele_complet)$coefficients)
    coefs$Variable <- rownames(coefs)
    coefs <- coefs[, c("Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
    colnames(coefs) <- c("Variable", "Coefficient", "Erreur Std", "Valeur t", "P-value")
    
    coefs <- coefs %>%
      mutate(
        Coefficient = round(Coefficient, 3),
        `Erreur Std` = round(`Erreur Std`, 3),
        `Valeur t` = round(`Valeur t`, 2),
        `P-value` = format.pval(`P-value`, digits = 3, eps = 0.001),
        Signif = case_when(
          as.numeric(gsub("<", "", `P-value`)) < 0.001 ~ "***",
          as.numeric(gsub("<", "", `P-value`)) < 0.01 ~ "**",
          as.numeric(gsub("<", "", `P-value`)) < 0.05 ~ "*",
          as.numeric(gsub("<", "", `P-value`)) < 0.1 ~ ".",
          TRUE ~ ""
        )
      )
    
    datatable(coefs, 
              options = list(pageLength = 20, scrollY = "400px", scrollX = TRUE),
              rownames = FALSE) %>%
      formatStyle('Signif',
                  backgroundColor = styleEqual(c("***", "**", "*"), 
                                               c('#d4edda', '#fff3cd', '#f8d7da')))
  })
  
  # DONNÉES
  output$table_donnees <- renderDT({
    datatable(dakar_clean, 
              options = list(pageLength = 25, scrollX = TRUE, scrollY = "500px",
                             dom = 'Bfrtip'),
              rownames = FALSE,
              filter = 'top')
  })
}

# ============================================================================
# LANCER L'APPLICATION
# ============================================================================

shinyApp(ui = ui, server = server)