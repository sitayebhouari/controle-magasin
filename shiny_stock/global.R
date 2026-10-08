# =============================================================================
# global.R  -  Controle Stock Magasin Central (fichier MOUVMENT)
# Source unique : feuille "mouvment" (quantites signees : entree +, sortie -)
# =============================================================================

library(shiny)
library(shinydashboard)
library(DT)
library(readxl)
library(dplyr)
library(stringr)
library(stringdist)
library(openxlsx)
library(lubridate)

options(shiny.maxRequestSize = 100 * 1024^2)

# Le fichier contient un titre puis l'en-tete : on la detecte automatiquement
REQUIRED_COLS <- c("N", "DATE", "CATEGORIE", "ARTICLE", "REFERENCE",
                   "OPERATION", "TIERS", "QUANTITE", "PRIX_UNIT")

# Operations considerees comme des ACHATS (pour le prix de reference)
OPS_ACHAT <- c("ACHAT_LOCAL", "ACHAT_IMPORTAION")

`%||%` <- function(a, b) if (is.null(a)) b else a

normalize_name <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- str_replace_all(x, "\u00A0", " ")
  str_squish(x)
}

normalize_key <- function(x) str_to_lower(normalize_name(x))

safe_to_numeric <- function(x) {
  if (is.numeric(x)) return(x)
  x <- str_replace_all(str_squish(as.character(x)), "\\s+", "")
  x[x == ""] <- NA_character_
  suppressWarnings(as.numeric(str_replace_all(x, ",", ".")))
}

safe_to_date <- function(x) {
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXct")) return(as.Date(x))
  n <- suppressWarnings(as.numeric(as.character(x)))
  if (any(!is.na(n))) return(as.Date(n, origin = "1899-12-30"))
  suppressWarnings(as.Date(parse_date_time(as.character(x),
                   orders = c("Ymd", "dmY", "d-m-Y"))))
}

# Detecte la ligne d'en-tete (celle qui contient REFERENCE et OPERATION)
find_header_row <- function(path, sheet) {
  raw <- read_excel(path, sheet = sheet, col_names = FALSE,
                    n_max = 40, col_types = "text")
  for (i in seq_len(nrow(raw))) {
    v <- str_to_upper(str_squish(as.character(unlist(raw[i, ]))))
    if (all(c("REFERENCE", "OPERATION") %in% v)) return(i)
  }
  NA_integer_
}

fmt_num <- function(x, nsmall = 0) {
  ifelse(is.na(x), "",
         format(round(as.numeric(x), 3), big.mark = " ",
                decimal.mark = ".", nsmall = nsmall, trim = TRUE))
}
