# ============================================================
# SERVER.R — Logique du serveur
# ============================================================

library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(DT)

# --- Chargement et préparation des données ---
dakar <- read.csv("dakar.csv", stringsAsFactors = TRUE)

dakar_clean <- dakar %>%
  tidyr::drop_na(Loyer) %>%
  mutate(
    PrixM2Habitable = ifelse(SurfHabitable > 0, Loyer / SurfHabitable, NA),
    Standing = factor(Standing, levels = c("Non", "Oui")),
    Etat = factor(Etat, levels = c("Vetuste", "Mediocre", "Bon", "Neuf")),
    StandingQuartier = factor(StandingQuartier,
                              levels = c("popu", "moy", "bourge"),
                              labels = c("Populaire", "Moyen", "Bourgeois"))
  )

couleurs_principales <- c("#2E86AB", "#A23B72", "#F18F01", "#C73E1D", "#6A994E")

theme_custom <- theme_minimal() +
  theme(
    plot.title = element_text(size = 14, face = "bold", hjust = 0.5),
    axis.title = element_text(size = 11, face = "bold"),
    legend.position = "bottom"
  )
theme_set(theme_custom)

# ============================================================
# Serveur
# ============================================================
server <- function(input, output, session) {
  
  # --- Initialisation des menus déroulants dynamiques ---
  updateSelectInput(session, "type", 
                    choices = c("Tous", levels(dakar_clean$Type)))
  updateSelectInput(session, "standing", 
                    choices = c("Tous", levels(dakar_clean$Standing)))
  updateSliderInput(session, "loyerRange",
                    min = min(dakar_clean$Loyer),
                    max = max(dakar_clean$Loyer),
                    value = c(min(dakar_clean$Loyer), max(dakar_clean$Loyer)))
  
  # --- Données filtrées selon les entrées utilisateur ---
  data_filtered <- reactive({
    d <- dakar_clean
    if (input$type != "Tous") d <- d %>% filter(Type == input$type)
    if (input$standing != "Tous") d <- d %>% filter(Standing == input$standing)
    d <- d %>% filter(Loyer >= input$loyerRange[1], Loyer <= input$loyerRange[2])
    return(d)
  })
  
  # --- Histogramme interactif du loyer ---
  output$histLoyer <- renderPlotly({
    p <- ggplot(data_filtered(), aes(x = Loyer)) +
      geom_histogram(fill = couleurs_principales[1], color = "white", bins = 30, alpha = 0.8) +
      labs(title = "Distribution des loyers", x = "Loyer (FCFA)", y = "Fréquence")
    ggplotly(p)
  })
  
  # --- Tableau interactif ---
  output$tableApercu <- renderDataTable({
    datatable(data_filtered(), options = list(pageLength = 10))
  })
  
  # --- Relation bivariée (nuage de points) ---
  output$scatterPlot <- renderPlotly({
    var <- input$varX
    p <- ggplot(data_filtered(), aes_string(x = var, y = "Loyer")) +
      geom_point(alpha = 0.6, color = couleurs_principales[2]) +
      geom_smooth(method = "lm", color = couleurs_principales[3]) +
      labs(title = paste("Loyer vs", var), x = var, y = "Loyer (FCFA)")
    ggplotly(p)
  })
  
  # --- Modèle linéaire (le même que dans le Rmd) ---
  modele3 <- lm(Loyer ~ SurfHabitable + NbChamBur + NbSDB + Type +
                  Standing + Etat + StandingQuartier + Piscine + Garage + BordMer,
                data = dakar_clean)
  
  # --- Graphique de prédiction ---
  output$predPlot <- renderPlot({
    dakar_clean$Prediction <- predict(modele3, dakar_clean)
    ggplot(dakar_clean, aes(x = Loyer, y = Prediction)) +
      geom_point(alpha = 0.6, color = couleurs_principales[1]) +
      geom_abline(intercept = 0, slope = 1, color = "#E63946", linetype = "dashed") +
      labs(title = "Prédictions vs Observations", x = "Loyer observé", y = "Loyer prédit")
  })
  
  # --- Résumé du modèle ---
  output$modelePerf <- renderPrint({
    summary(modele3)
  })
}

