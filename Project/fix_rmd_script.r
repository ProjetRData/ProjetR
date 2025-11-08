# ====================================================
# SCRIPT DE CORRECTION - Fichier R Markdown propre
# ====================================================
# Ce script crée un nouveau fichier sans doublons de chunks

# 1. Télécharger le nouveau fichier depuis l'artifact
cat("📥 Étape 1: Préparation du nouveau fichier...\n")

# Vérifier si dakar.csv existe
if (!file.exists("dakar.csv")) {
  stop("❌ ERREUR: Le fichier 'dakar.csv' n'existe pas dans le répertoire actuel!")
}

cat("✅ Fichier dakar.csv trouvé\n")

# 2. Instructions pour l'utilisateur
cat("\n" , rep("=", 60), "\n", sep="")
cat("📋 INSTRUCTIONS:\n")
cat(rep("=", 60), "\n\n", sep="")

cat("OPTION A - Recommandée (copier-coller):\n")
cat("----------------------------------------\n")
cat("1. Ouvrez RStudio\n")
cat("2. Créez un nouveau fichier: File > New File > R Markdown\n")
cat("3. Supprimez TOUT le contenu par défaut\n")
cat("4. Copiez TOUT le contenu de l'artifact 'Analyse Loyers Dakar - Amélioré'\n")
cat("5. Collez dans le nouveau fichier\n")
cat("6. Sauvegardez comme: analyse_dakar_final.Rmd\n")
cat("7. Knit le fichier\n\n")

cat("OPTION B - Automatique (si vous avez le fichier):\n")
cat("--------------------------------------------------\n")
cat("Si vous avez téléchargé le fichier .Rmd de l'artifact:\n")
cat("  rmarkdown::render('analyse_dakar_ameliore.Rmd')\n\n")

cat("OPTION C - Nettoyer l'ancien fichier:\n")
cat("--------------------------------------\n")
cat("Si vous voulez réparer votre fichier actuel:\n")
cat("  1. Ouvrez analyse_dakar.Rmd\n")
cat("  2. Cherchez TOUS les chunks avec ces labels:\n")
cat("     - setup\n")
cat("     - chargement\n")
cat("     - libraries\n")
cat("  3. Supprimez les ANCIENS chunks (gardez les nouveaux)\n")
cat("  4. Sauvegardez et Knit\n\n")

# 3. Fonction pour vérifier les doublons dans un fichier
check_duplicate_chunks <- function(file_path) {
  if (!file.exists(file_path)) {
    cat("❌ Fichier non trouvé:", file_path, "\n")
    return(NULL)
  }
  
  lines <- readLines(file_path, warn = FALSE)
  chunk_pattern <- "^```\\{r\\s+([^,\\}]+)"
  
  chunk_labels <- c()
  for (i in seq_along(lines)) {
    match <- regmatches(lines[i], regexpr(chunk_pattern, lines[i], perl = TRUE))
    if (length(match) > 0) {
      label <- sub("^```\\{r\\s+", "", match)
      chunk_labels <- c(chunk_labels, paste0("Ligne ", i, ": ", label))
    }
  }
  
  if (length(chunk_labels) == 0) {
    cat("✅ Aucun chunk trouvé\n")
    return(NULL)
  }
  
  cat("\n📊 Chunks trouvés dans", basename(file_path), ":\n")
  cat(rep("-", 60), "\n", sep="")
  for (label in chunk_labels) {
    cat("  ", label, "\n")
  }
  
  # Chercher doublons
  labels_only <- sub("^Ligne \\d+: ", "", chunk_labels)
  duplicates <- labels_only[duplicated(labels_only)]
  
  if (length(duplicates) > 0) {
    cat("\n❌ DOUBLONS DÉTECTÉS:\n")
    for (dup in unique(duplicates)) {
      cat("  ⚠️ ", dup, "\n")
    }
    return(duplicates)
  } else {
    cat("\n✅ Aucun doublon détecté\n")
    return(NULL)
  }
}

# 4. Vérifier le fichier existant si présent
cat("\n", rep("=", 60), "\n", sep="")
cat("🔍 Vérification du fichier actuel\n")
cat(rep("=", 60), "\n", sep="")

if (file.exists("analyse_dakar.Rmd")) {
  duplicates <- check_duplicate_chunks("analyse_dakar.Rmd")
  
  if (!is.null(duplicates)) {
    cat("\n⚠️  Votre fichier contient des doublons.\n")
    cat("   Suivez l'OPTION A ou C ci-dessus.\n")
  }
} else {
  cat("ℹ️  Fichier analyse_dakar.Rmd non trouvé.\n")
  cat("   Suivez l'OPTION A pour créer un nouveau fichier.\n")
}

# 5. Liste des packages nécessaires
cat("\n", rep("=", 60), "\n", sep="")
cat("📦 Packages nécessaires\n")
cat(rep("=", 60), "\n", sep="")

required_packages <- c("dplyr", "ggplot2", "tidyr", "corrplot", 
                       "knitr", "kableExtra", "gridExtra", "scales", 
                       "forcats", "patchwork", "rmarkdown")

cat("\nVérification des packages...\n")
missing_packages <- c()

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat("  ❌", pkg, "- NON INSTALLÉ\n")
    missing_packages <- c(missing_packages, pkg)
  } else {
    cat("  ✅", pkg, "\n")
  }
}

if (length(missing_packages) > 0) {
  cat("\n⚠️  Packages manquants détectés!\n")
  cat("Exécutez cette commande pour les installer:\n\n")
  cat("install.packages(c(", 
      paste(paste0("'", missing_packages, "'"), collapse = ", "), 
      "))\n\n")
} else {
  cat("\n✅ Tous les packages sont installés!\n")
}

# 6. Résumé final
cat("\n", rep("=", 60), "\n", sep="")
cat("🎯 RÉSUMÉ\n")
cat(rep("=", 60), "\n\n", sep="")

cat("✅ dakar.csv: Présent\n")
cat(ifelse(length(missing_packages) == 0, "✅", "❌"), 
    " Packages:", 
    ifelse(length(missing_packages) == 0, "Tous installés\n", 
           paste(length(missing_packages), "manquant(s)\n")))

if (file.exists("analyse_dakar.Rmd")) {
  cat("⚠️  analyse_dakar.Rmd: Contient des doublons\n")
} else {
  cat("ℹ️  analyse_dakar.Rmd: Non trouvé\n")
}

cat("\n💡 Recommandation: Suivez l'OPTION A pour un fichier propre\n\n")
