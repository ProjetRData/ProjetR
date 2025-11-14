Application Analyse Loyers Dakar

Application Shiny pour analyser les loyers à Dakar.
Installation

    Installer R et RStudio

    Installer les packages requis :

r

install.packages(c("shiny", "shinydashboard", "dplyr", "ggplot2", "plotly", "DT", "scales", "tidyr"))

Utilisation

    Placer les fichiers app.R et dakar.csv dans le même dossier

    Ouvrir app.R dans RStudio

    Cliquer sur "Run App"

Fonctionnalités

    Tableau de bord : Statistiques générales et graphiques

    Exploration : Filtres et analyses détaillées

    Données : Tableau interactif des logements

Structure

L'application comprend :

    41 logements analysés

    Données sur les loyers, surfaces, quartiers

    Visualisations interactives

    Filtres personnalisables

Fichiers nécessaires

    app.R : Code de l'application

    dakar.csv : Base de données des loyers
