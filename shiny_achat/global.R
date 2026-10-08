# =============================================================================
# global.R
# Controle Achat Magasin
# Version : 1.0
# Source unique : feuille MOUVEMENT
# =============================================================================

# =============================================================================
# 1. PACKAGES
# =============================================================================

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(readxl)
library(dplyr)
library(stringr)
library(stringdist)
library(openxlsx)
library(lubridate)


# =============================================================================
# 2. CONFIGURATION
# =============================================================================

options(shiny.maxRequestSize = 50 * 1024^2)

# Date de l'inventaire initial
CUTOFF_DATE <- as.Date("2025-12-31")


# =============================================================================
# 3. COLONNES OBLIGATOIRES
# =============================================================================

REQUIRED_COLS <- c(
  "DATE",
  "OPERATION",
  "N° BON",
  "FOURNISSEUR & CLIENT",
  "ARTICLE",
  "PRODUIT",
  "REF PRODUIT",
  "ENTRIE",
  "PU ENTRIE",
  "SORTEE"
)


# =============================================================================
# 4. NORMALISATION DU TEXTE
# =============================================================================

normalize_name <- function(x) {
  
  x <- as.character(x)
  x[is.na(x)] <- ""
  
  x <- str_squish(x)
  
  x
}


# =============================================================================
# 5. CLÉ DE RECHERCHE
# =============================================================================

normalize_key <- function(x) {
  
  x <- normalize_name(x)
  
  # minuscules
  x <- str_to_lower(x)
  
  # espaces multiples
  x <- str_squish(x)
  
  x
}


# =============================================================================
# 6. CONVERSION DES DATES
# =============================================================================

safe_to_date <- function(x) {
  
  # Déjà Date
  if (inherits(x, "Date")) {
    return(x)
  }
  
  # POSIX
  if (inherits(x, "POSIXct")) {
    return(as.Date(x))
  }
  
  # Caractère
  if (is.character(x)) {
    
    x <- str_squish(x)
    
    # Essai Excel serial
    x_num <- suppressWarnings(as.numeric(x))
    
    if (
      any(!is.na(x_num)) &&
      all(
        is.na(x_num) |
        (x_num > 20000 & x_num < 60000)
      )
    ) {
      
      return(
        as.Date(
          x_num,
          origin = "1899-12-30"
        )
      )
    }
    
    # Différents formats possibles
    result <- suppressWarnings(
      as.Date(
        x,
        format = "%Y-%m-%d"
      )
    )
    
    if (sum(!is.na(result)) > 0) {
      return(result)
    }
    
    result <- suppressWarnings(
      as.Date(
        x,
        format = "%d/%m/%Y"
      )
    )
    
    if (sum(!is.na(result)) > 0) {
      return(result)
    }
    
    result <- suppressWarnings(
      as.Date(
        x,
        format = "%d-%m-%Y"
      )
    )
    
    if (sum(!is.na(result)) > 0) {
      return(result)
    }
    
    result <- suppressWarnings(
      as.Date(
        x,
        format = "%d.%m.%Y"
      )
    )
    
    return(result)
  }
  
  
  # Numérique = Excel serial
  if (is.numeric(x)) {
    
    return(
      as.Date(
        x,
        origin = "1899-12-30"
      )
    )
  }
  
  
  # Dernier recours
  suppressWarnings(
    lubridate::as_date(x)
  )
}


# =============================================================================
# 7. CONVERSION DES NOMBRES
# =============================================================================

safe_to_numeric <- function(x) {
  
  x <- as.character(x)
  
  x <- str_squish(x)
  
  x[x == ""] <- NA_character_
  
  # Supprimer espaces
  x <- str_replace_all(x, "\\s+", "")
  
  # Si nombre avec virgule décimale
  x <- str_replace_all(x, ",", ".")
  
  suppressWarnings(
    as.numeric(x)
  )
}


# =============================================================================
# 8. NETTOYAGE DES NOMS DE PRODUITS
# =============================================================================

clean_product_reference <- function(x) {
  
  x <- normalize_name(x)
  
  # Remplace les espaces insécables
  x <- str_replace_all(x, "\u00A0", " ")
  
  # Normalise les espaces
  x <- str_squish(x)
  
  x
}


# =============================================================================
# 9. NORMALISATION DE L'OPÉRATION
# =============================================================================

clean_operation <- function(x) {
  
  x <- normalize_name(x)
  
  x <- str_to_upper(x)
  
  x <- str_squish(x)
  
  x
}


# =============================================================================
# 10. FONCTION POUR ÉVITER LES NA DANS LES SOMMES
# =============================================================================

safe_sum <- function(x) {
  
  x <- suppressWarnings(
    as.numeric(x)
  )
  
  sum(
    x,
    na.rm = TRUE
  )
}


# =============================================================================
# 11. STRUCTURE DE RÉFÉRENCE DU STOCK
# =============================================================================
#
# Le fonctionnement sera :
#
# MOUVEMENT
#     ↓
# STOCK INITIAL
#     +
# TOTAL ENTRÉE
#     -
# TOTAL SORTIE
#     ↓
# STOCK ACTUEL
#
# Le fichier Excel STOCK_ACTUEL n'est donc PAS nécessaire.
#
# =============================================================================