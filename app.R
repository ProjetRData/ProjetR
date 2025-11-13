# Application Shiny - Analyse des Loyers à Dakar
# Installation des packages nécessaires (si besoin)
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

# Charger les données (assurez-vous que dakar.csv est dans le même dossier)
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
    Garage = factor(Garage, levels = c("Non", "Oui")),
    BordMer = factor(BordMer, levels = c("Non", "Oui")),
    Segment = case_when(
      Loyer >= 600 ~ "Haut standing",
      Loyer >= 200 ~ "Moyen standing",
      TRUE ~ "Économique"
    )
  )

# Vérifier quelles variables ont au moins 2 niveaux
vars_a_inclure <- c("SurfHabitable", "NbChamBur", "NbSDB")
vars_facteurs <- c("Type", "Standing", "Etat", "StandingQuartier", "Piscine", "Garage", "BordMer")

# Ajouter uniquement les facteurs avec au moins 2 niveaux (sans NA)
for(var in vars_facteurs) {
  niveaux_valides <- unique(dakar_clean[[var]][!is.na(dakar_clean[[var]])])
  if(length(niveaux_valides) > 1) {
    vars_a_inclure <- c(vars_a_inclure, var)
  } else {
    cat("Variable", var, "exclue (un seul niveau ou trop de NA)\n")
  }
}

# Construire la formule du modèle
formule_modele <- as.formula(paste("Loyer ~", paste(vars_a_inclure, collapse = " + ")))

# Retirer les lignes avec des NA dans les variables du modèle
dakar_modele <- dakar_clean[complete.cases(dakar_clean[, c("Loyer", vars_a_inclure)]), ]

# Construire le modèle
modele_complet <- lm(formule_modele, data = dakar_modele)

# Stocker les variables incluses dans le modèle
vars_modele <- vars_a_inclure

# Palette de couleurs
couleurs <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#6A994E")

# ============================================================================
# INTERFACE UTILISATEUR
# ============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  # En-tête
  dashboardHeader(
    title = "Loyers Dakar - Analyse",
    titleWidth = 280
  ),
  
  # Sidebar
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
  
  # Corps principal
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
      # ========== ONGLET 1: TABLEAU DE BORD ==========
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
          box(
            title = "Distribution des Loyers",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_dist_loyer", height = "300px")
          ),
          box(
            title = "Segmentation du Marché",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_segments", height = "300px")
          )
        ),
        
        fluidRow(
          box(
            title = "Loyers par Type de Logement",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_type", height = "300px")
          ),
          box(
            title = "Impact du Standing",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_standing", height = "300px")
          )
        )
      ),
      
      # ========== ONGLET 2: EXPLORATION ==========
      tabItem(
        tabName = "exploration",
        h2("Exploration des Données", style = "color: #2E86AB; font-weight: bold;"),
        
        fluidRow(
          box(
            title = "Filtres",
            status = "primary",
            solidHeader = TRUE,
            width = 3,
            
            selectInput("filter_type", "Type de logement:",
                        choices = c("Tous" = "all", levels(dakar_clean$Type)),
                        selected = "all"),
            
            selectInput("filter_standing", "Standing:",
                        choices = c("Tous" = "all", "Oui", "Non"),
                        selected = "all"),
            
            selectInput("filter_quartier", "Standing Quartier:",
                        choices = c("Tous" = "all", "Populaire", "Moyen", "Bourgeois"),
                        selected = "all"),
            
            sliderInput("filter_loyer", "Plage de loyer (FCFA):",
                        min = 0, max = max(dakar_clean$Loyer),
                        value = c(0, max(dakar_clean$Loyer)),
                        step = 50)
          ),
          
          box(
            title = "Statistiques Filtrées",
            status = "info",
            solidHeader = TRUE,
            width = 9,
            
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
          box(
            title = "Relation Loyer vs Surface",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_scatter", height = "350px")
          ),
          box(
            title = "Prix au m² Habitable",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_prixm2", height = "350px")
          )
        )
      ),
      
      # ========== ONGLET 3: ESTIMATEUR ==========
      tabItem(
        tabName = "estimateur",
        h2("Estimateur de Loyer", style = "color: #2E86AB; font-weight: bold;"),
        
        fluidRow(
          box(
            title = "Caractéristiques du Logement",
            status = "primary",
            solidHeader = TRUE,
            width = 4,
            
            numericInput("est_surf", "Surface habitable (m²):", value = 100, min = 20, max = 1000),
            numericInput("est_chambres", "Nombre de chambres:", value = 3, min = 1, max = 10),
            numericInput("est_sdb", "Nombre de salles de bain:", value = 2, min = 1, max = 5),
            selectInput("est_type", "Type:", choices = levels(dakar_clean$Type)),
            selectInput("est_standing", "Standing:", choices = c("Non", "Oui")),
            selectInput("est_etat", "État:", choices = c("Vetuste", "Mediocre", "Bon", "Neuf")),
            selectInput("est_quartier", "Standing Quartier:", 
                        choices = c("Populaire", "Moyen", "Bourgeois")),
            selectInput("est_piscine", "Piscine:", choices = c("Non", "Oui")),
            selectInput("est_garage", "Garage:", choices = c("Non", "Oui")),
            selectInput("est_bordmer", "Bord de mer:", choices = c("Non", "Oui")),
            
            actionButton("btn_estimer", "Estimer le Loyer", 
                         class = "btn-primary btn-block", icon = icon("calculator"))
          ),
          
          box(
            title = "Estimation",
            status = "success",
            solidHeader = TRUE,
            width = 8,
            
            uiOutput("estimation_result"),
            
            hr(),
            
            h4("Logements Similaires", style = "color: #2E86AB;"),
            DTOutput("table_similaires")
          )
        )
      ),
      
      # ========== ONGLET 4: MODÉLISATION ==========
      tabItem(
        tabName = "modelisation",
        h2("Analyse du Modèle", style = "color: #2E86AB; font-weight: bold;"),
        
        fluidRow(
          box(
            title = "Performance du Modèle",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            
            DTOutput("table_performance")
          ),
          box(
            title = "Importance des Variables",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            
            plotlyOutput("plot_importance", height = "300px")
          )
        ),
        
        fluidRow(
          box(
            title = "Prédictions vs Observations",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_pred_obs", height = "350px")
          ),
          box(
            title = "Distribution des Résidus",
            status = "primary",
            solidHeader = TRUE,
            width = 6,
            plotlyOutput("plot_residus", height = "350px")
          )
        ),
        
        fluidRow(
          box(
            title = "Coefficients du Modèle",
            status = "info",
            solidHeader = TRUE,
            width = 12,
            DTOutput("table_coefficients")
          )
        )
      ),
      
      # ========== ONGLET 5: DONNÉES ==========
      tabItem(
        tabName = "donnees",
        h2("Données Brutes", style = "color: #2E86AB; font-weight: bold;"),
        
        fluidRow(
          box(
            title = "Table des Logements",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            DTOutput("table_donnees")
          )
        )
      ),
      
      # ========== ONGLET 6: À PROPOS ==========
      tabItem(
        tabName = "apropos",
        h2("À propos de l'Application", style = "color: #2E86AB; font-weight: bold;"),
        
        fluidRow(
          box(
            title = "Description",
            status = "primary",
            solidHeader = TRUE,
            width = 12,
            
            h4("Objectif"),
            p("Cette application interactive permet d'explorer et d'analyser le marché immobilier locatif à Dakar."),
            
            h4("Fonctionnalités"),
            tags$ul(
              tags$li("📊 Tableau de bord avec indicateurs clés"),
              tags$li("🔍 Exploration interactive avec filtres"),
              tags$li("🏠 Estimateur de loyer basé sur un modèle de régression"),
              tags$li("📈 Analyse détaillée du modèle prédictif"),
              tags$li("📋 Consultation des données brutes")
            ),
            
            h4("Méthodologie"),
            p("Modèle de régression linéaire multiple (R² ajusté = ", 
              round(summary(modele_complet)$adj.r.squared, 3), 
              ") intégrant :"),
            tags$ul(
              tags$li("Caractéristiques physiques (surface, chambres, SDB)"),
              tags$li("Type et standing du logement"),
              tags$li("État général"),
              tags$li("Standing du quartier"),
              tags$li("Équipements (piscine, garage, proximité mer)")
            ),
            
            h4("Données"),
            p(strong(format(nrow(dakar_clean), big.mark = " ")), " logements analysés à Dakar"),
            p(strong(format(nrow(dakar_modele), big.mark = " ")), " logements utilisés dans le modèle (données complètes)"),
            
            hr(),
            
            p(em("Application développée avec Shiny - R"), style = "text-align: center;"),
            p(em(format(Sys.Date(), '%d %B %Y')), style = "text-align: center; color: gray;")
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
  
  # ========== VALUE BOXES - TABLEAU DE BORD ==========
  
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
  
  output$box_r2 <- renderValueBox({
    valueBox(
      paste0(round(summary(modele_complet)$adj.r.squared * 100, 1), "%"),
      "R² du Modèle",
      icon = icon("chart-area"),
      color = "purple"
    )
  })
  
  # ========== GRAPHIQUES TABLEAU DE BORD ==========
  
  output$plot_dist_loyer <- renderPlotly({
    p <- ggplot(dakar_clean, aes(x = Loyer)) +
      geom_histogram(fill = couleurs[1], color = "white", bins = 30, alpha = 0.8) +
      labs(x = "Loyer (FCFA)", y = "Fréquence") +
      theme_minimal()
    
    ggplotly(p) %>% layout(showlegend = FALSE)
  })
  
  output$plot_segments <- renderPlotly({
    segments_count <- dakar_clean %>%
      count(Segment) %>%
      mutate(Pourcentage = n / sum(n) * 100)
    
    plot_ly(segments_count, labels = ~Segment, values = ~n, type = 'pie',
            marker = list(colors = couleurs),
            textinfo = 'label+percent',
            hovertemplate = paste('<b>%{label}</b><br>',
                                  'Nombre: %{value}<br>',
                                  'Pourcentage: %{percent}',
                                  '<extra></extra>'))
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
  
  # ========== EXPLORATION - DONNÉES FILTRÉES ==========
  
  donnees_filtrees <- reactive({
    data <- dakar_clean
    
    if (input$filter_type != "all") {
      data <- data %>% filter(Type == input$filter_type)
    }
    
    if (input$filter_standing != "all") {
      data <- data %>% filter(Standing == input$filter_standing)
    }
    
    if (input$filter_quartier != "all") {
      data <- data %>% filter(StandingQuartier == input$filter_quartier)
    }
    
    data <- data %>% filter(Loyer >= input$filter_loyer[1] & Loyer <= input$filter_loyer[2])
    
    return(data)
  })
  
  output$filtered_count <- renderValueBox({
    valueBox(
      nrow(donnees_filtrees()),
      "Logements",
      icon = icon("filter"),
      color = "light-blue"
    )
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
    data_clean <- donnees_filtrees() %>%
      filter(!is.na(PrixM2Habitable), PrixM2Habitable < 20)
    
    p <- ggplot(data_clean, aes(x = Type, y = PrixM2Habitable, fill = Type)) +
      geom_boxplot(alpha = 0.8) +
      scale_fill_manual(values = couleurs) +
      labs(x = "", y = "Prix au m² (FCFA)") +
      theme_minimal() +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # ========== ESTIMATEUR ==========
  # Utiliser reactiveVal pour stocker l'estimation uniquement quand le bouton est cliqué
  estimation_data <- reactiveVal(NULL)
  
  observeEvent(input$btn_estimer, {
    # Utiliser tryCatch pour capturer les erreurs
    tryCatch({
      # Mapper les niveaux correctement
      standing_quartier_map <- c("Populaire" = "popu", "Moyen" = "moy", "Bourgeois" = "bourge")
      
      # Créer le data frame de base
      nouveau_logement <- data.frame(
        SurfHabitable = as.numeric(input$est_surf),
        NbChamBur = as.numeric(input$est_chambres),
        NbSDB = as.numeric(input$est_sdb)
      )
      
      # Ajouter uniquement les variables présentes dans le modèle
      if("Type" %in% vars_modele) {
        nouveau_logement$Type <- factor(input$est_type, levels = levels(dakar_clean$Type))
      }
      if("Standing" %in% vars_modele) {
        nouveau_logement$Standing <- factor(input$est_standing, levels = c("Non", "Oui"))
      }
      if("Etat" %in% vars_modele) {
        nouveau_logement$Etat <- factor(input$est_etat, levels = c("Vetuste", "Mediocre", "Bon", "Neuf"))
      }
      if("StandingQuartier" %in% vars_modele) {
        nouveau_logement$StandingQuartier <- factor(
          standing_quartier_map[input$est_quartier],
          levels = c("popu", "moy", "bourge"),
          labels = c("Populaire", "Moyen", "Bourgeois")
        )
      }
      if("Piscine" %in% vars_modele) {
        nouveau_logement$Piscine <- factor(input$est_piscine, levels = c("Non", "Oui"))
      }
      if("Garage" %in% vars_modele) {
        nouveau_logement$Garage <- factor(input$est_garage, levels = c("Non", "Oui"))
      }
      if("BordMer" %in% vars_modele) {
        nouveau_logement$BordMer <- factor(input$est_bordmer, levels = c("Non", "Oui"))
      }
      
      prediction <- predict(modele_complet, nouveau_logement, interval = "prediction", level = 0.95)
      
      # Stocker les résultats dans le reactiveVal
      estimation_data(list(
        prediction = prediction,
        inputs = list(
          surf = input$est_surf,
          type = input$est_type,
          standing = input$est_standing,
          etat = input$est_etat,
          quartier = input$est_quartier,
          piscine = input$est_piscine,
          garage = input$est_garage,
          bordmer = input$est_bordmer
        )
      ))
    }, error = function(e) {
      # Afficher l'erreur dans la console
      cat("Erreur dans l'estimation:", e$message, "\n")
      showNotification(
        paste("Erreur lors de l'estimation:", e$message),
        type = "error",
        duration = 5
      )
    })
  })
  
  # Afficher l'estimation uniquement si estimation_data() existe
  output$estimation_result <- renderUI({
    req(estimation_data())
    
    data <- estimation_data()
    prediction <- data$prediction
    inputs <- data$inputs
    
    loyer_estime <- round(prediction[1, "fit"], 0)
    lower <- round(prediction[1, "lwr"], 0)
    upper <- round(prediction[1, "upr"], 0)
    
    equipements <- c()
    if(inputs$piscine == "Oui") equipements <- c(equipements, "Piscine")
    if(inputs$garage == "Oui") equipements <- c(equipements, "Garage")
    if(inputs$bordmer == "Oui") equipements <- c(equipements, "Bord de mer")
    equip_text <- if(length(equipements) > 0) paste(equipements, collapse = ", ") else "Aucun"
    
    tagList(
      div(
        style = "background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                 padding: 30px; border-radius: 10px; color: white; text-align: center;",
        h2(style = "margin: 0;", "Loyer Estimé"),
        h1(style = "margin: 10px 0; font-size: 3em; font-weight: bold;", 
           paste(format(loyer_estime, big.mark = " "), "FCFA")),
        p(style = "margin: 0; font-size: 1.1em;", 
          paste("Intervalle de confiance 95%:", 
                format(lower, big.mark = " "), "-", 
                format(upper, big.mark = " "), "FCFA"))
      ),
      br(),
      div(
        style = "background: #f8f9fa; padding: 15px; border-radius: 5px; border-left: 4px solid #2E86AB;",
        h4("Détails de l'Estimation", style = "color: #2E86AB; margin-top: 0;"),
        p(strong("Surface:"), inputs$surf, "m²"),
        p(strong("Type:"), inputs$type),
        p(strong("Standing:"), inputs$standing, "| État:", inputs$etat),
        p(strong("Quartier:"), inputs$quartier),
        p(strong("Équipements:"), equip_text)
      )
    )
  })
  
  # Afficher les logements similaires uniquement si estimation_data() existe
  output$table_similaires <- renderDT({
    req(estimation_data())
    
    data <- estimation_data()
    inputs <- data$inputs
    
    similaires <- dakar_clean %>%
      filter(
        Type == inputs$type,
        abs(SurfHabitable - inputs$surf) <= 50,
        Standing == inputs$standing
      ) %>%
      arrange(abs(SurfHabitable - inputs$surf)) %>%
      head(5) %>%
      select(Type, SurfHabitable, NbChamBur, NbSDB, Etat, 
             StandingQuartier, Loyer) %>%
      mutate(Loyer = format(Loyer, big.mark = " "))
    
    datatable(similaires, 
              options = list(pageLength = 5, dom = 't'),
              rownames = FALSE)
  })
  
  # ========== MODÉLISATION ==========
  
  output$table_performance <- renderDT({
    performance <- data.frame(
      Métrique = c("R²", "R² ajusté", "RMSE", "AIC", "BIC"),
      Valeur = c(
        round(summary(modele_complet)$r.squared, 4),
        round(summary(modele_complet)$adj.r.squared, 4),
        round(sqrt(mean(residuals(modele_complet)^2)), 2),
        round(AIC(modele_complet), 2),
        round(BIC(modele_complet), 2)
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
      layout(xaxis = list(title = "Coefficient (valeur absolue)"),
             yaxis = list(title = ""))
  })
  
  output$plot_pred_obs <- renderPlotly({
    predictions <- predict(modele_complet)
    data_plot <- data.frame(Observe = dakar_modele$Loyer, Predit = predictions)
    
    p <- ggplot(data_plot, aes(x = Observe, y = Predit)) +
      geom_point(alpha = 0.5, color = couleurs[1]) +
      geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
      labs(x = "Loyer Observé (FCFA)", y = "Loyer Prédit (FCFA)") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$plot_residus <- renderPlotly({
    residus <- residuals(modele_complet)
    
    p <- ggplot(data.frame(Residus = residus), aes(x = Residus)) +
      geom_histogram(fill = couleurs[2], color = "white", bins = 30, alpha = 0.8) +
      geom_vline(xintercept = 0, color = "red", linetype = "dashed", linewidth = 1) +
      labs(x = "Résidus", y = "Fréquence") +
      theme_minimal()
    
    ggplotly(p)
  })
  
  output$table_coefficients <- renderDT({
    coefs <- as.data.frame(summary(modele_complet)$coefficients)
    coefs$Variable <- rownames(coefs)
    coefs <- coefs[, c("Variable", "Estimate", "Std. Error", "t value", "Pr(>|t|)")]
    colnames(coefs) <- c("Variable", "Coefficient", "Erreur Std", "t", "P-value")
    
    coefs <- coefs %>%
      mutate(
        Coefficient = round(Coefficient, 2),
        `Erreur Std` = round(`Erreur Std`, 2),
        t = round(t, 2),
        `P-value` = format.pval(`P-value`, digits = 3),
        Signif = case_when(
          as.numeric(`P-value`) < 0.001 ~ "***",
          as.numeric(`P-value`) < 0.01 ~ "**",
          as.numeric(`P-value`) < 0.05 ~ "*",
          TRUE ~ ""
        )
      )
    
    datatable(coefs, options = list(pageLength = 15), rownames = FALSE)
  })
  
  # ========== DONNÉES BRUTES ==========
  
  output$table_donnees <- renderDT({
    datatable(dakar_clean, 
              options = list(
                pageLength = 25,
                scrollX = TRUE,
                dom = 'Bfrtip',
                buttons = c('copy', 'csv', 'excel')
              ),
              rownames = FALSE,
              filter = 'top')
  })
}

# ============================================================================
# LANCER L'APPLICATION
# ============================================================================

shinyApp(ui = ui, server = server)