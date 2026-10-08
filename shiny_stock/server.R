# =============================================================================
# server.R  -  Controle Stock Magasin Central
# =============================================================================

server <- function(input, output, session) {

  raw_data   <- reactiveVal(NULL)
  stock_data <- reactiveVal(NULL)

  # ---------------------------------------------------------------------------
  # 1. CHARGEMENT
  # ---------------------------------------------------------------------------
  observeEvent(input$file, {
    req(input$file)

    tryCatch({
      path   <- input$file$datapath
      sheets <- excel_sheets(path)
      sheet  <- if ("mouvment" %in% tolower(sheets)) sheets[tolower(sheets) == "mouvment"][1] else sheets[1]

      hdr <- find_header_row(path, sheet)
      if (is.na(hdr)) stop("Ligne d'en-tete introuvable (colonnes REFERENCE / OPERATION).")

      df <- read_excel(path, sheet = sheet, skip = hdr - 1, col_types = "text")
      names(df) <- str_squish(names(df))
      df <- df[, names(df) != "" & !is.na(names(df)) & !str_detect(names(df), "^\\.\\.\\.")]

      missing_cols <- setdiff(REQUIRED_COLS, names(df))
      if (length(missing_cols) > 0)
        stop(paste("Colonnes manquantes :", paste(missing_cols, collapse = ", ")))

      if (!"UNITE" %in% names(df)) df$UNITE <- ""

      df <- df %>%
        mutate(
          N         = safe_to_numeric(N),
          DATE      = safe_to_date(DATE),
          CATEGORIE = normalize_name(CATEGORIE),
          ARTICLE   = normalize_name(ARTICLE),
          REFERENCE = normalize_name(REFERENCE),
          OPERATION = str_to_upper(normalize_name(OPERATION)),
          TIERS     = normalize_name(TIERS),
          UNITE     = normalize_name(UNITE),
          QUANTITE  = safe_to_numeric(QUANTITE),
          PRIX_UNIT = safe_to_numeric(PRIX_UNIT),
          REF_KEY   = normalize_key(REFERENCE)
        ) %>%
        filter(REF_KEY != "", !is.na(QUANTITE)) %>%
        arrange(DATE, N)

      # -----------------------------------------------------------------------
      # STOCK = somme des quantites signees (independant de l'ordre de saisie)
      # -----------------------------------------------------------------------
      stock <- df %>%
        group_by(REF_KEY) %>%
        summarise(
          REFERENCE = first(REFERENCE),
          ARTICLE   = last(ARTICLE[ARTICLE != ""], default = ""),
          CATEGORIE = last(CATEGORIE[CATEGORIE != ""], default = ""),
          UNITE     = last(UNITE[!UNITE %in% c("", "0")], default = ""),
          STOCK_INITIAL = sum(QUANTITE[OPERATION == "STOCK_INITIAL"]),
          TOTAL_ACHAT   = sum(QUANTITE[OPERATION %in% OPS_ACHAT]),
          TRANS_IN      = sum(QUANTITE[OPERATION == "TRANS_IN"]),
          TRANS_OUT     = -sum(QUANTITE[OPERATION == "TRANS_OUT"]),
          SORTIE        = -sum(QUANTITE[OPERATION == "SORTIE"]),
          STOCK_ACTUEL  = round(sum(QUANTITE), 3),
          NB_MVT        = n(),
          DERNIER_MVT   = max(DATE, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(
          ETAT = case_when(
            STOCK_ACTUEL < 0  ~ "STOCK NEGATIF",
            STOCK_ACTUEL == 0 ~ "RUPTURE",
            TRUE              ~ "STOCK DISPONIBLE"
          )
        )

      # -----------------------------------------------------------------------
      # PRIX DE REFERENCE : dernier achat > stock initial > dernier prix > 0
      # -----------------------------------------------------------------------
      last_price <- function(d, src) {
        d %>%
          filter(!is.na(PRIX_UNIT), PRIX_UNIT > 0) %>%
          arrange(REF_KEY, desc(DATE), desc(N)) %>%
          group_by(REF_KEY) %>% slice(1) %>% ungroup() %>%
          transmute(REF_KEY, PRIX = PRIX_UNIT, SRC = src,
                    FOURN = TIERS, DPRIX = DATE)
      }

      p1 <- last_price(df %>% filter(OPERATION %in% OPS_ACHAT), "DERNIER ACHAT")
      p2 <- last_price(df %>% filter(OPERATION == "STOCK_INITIAL"), "STOCK INITIAL")
      p3 <- last_price(df, "DERNIER MOUVEMENT")

      prices <- bind_rows(p1, p2, p3) %>%
        group_by(REF_KEY) %>% slice(1) %>% ungroup() %>%
        transmute(REF_KEY,
                  PRIX_REFERENCE = PRIX,
                  SOURCE_PRIX = SRC,
                  DERNIER_FOURNISSEUR = ifelse(SRC == "DERNIER ACHAT", FOURN, ""),
                  DATE_PRIX = DPRIX)

      stock <- stock %>% left_join(prices, by = "REF_KEY")

      raw_data(df)
      stock_data(stock)

      showNotification(
        paste("Fichier charge :", nrow(df), "mouvements,", nrow(stock), "references."),
        type = "message", duration = 6)

    }, error = function(e) {
      showNotification(paste("Erreur :", e$message), type = "error", duration = 12)
    })
  })

  # ---------------------------------------------------------------------------
  # 2. ETAT DU FICHIER
  # ---------------------------------------------------------------------------
  output$health <- renderPrint({
    req(raw_data(), stock_data())
    df <- raw_data(); st <- stock_data()
    cat("============================================\n")
    cat("       CONTROLE STOCK MAGASIN CENTRAL\n")
    cat("============================================\n\n")
    cat("Mouvements              :", nrow(df), "\n")
    cat("References uniques      :", nrow(st), "\n")
    cat("Periode                 :", format(min(df$DATE, na.rm = TRUE), "%d/%m/%Y"),
        "->", format(max(df$DATE, na.rm = TRUE), "%d/%m/%Y"), "\n")
    cat("Stock disponible        :", sum(st$ETAT == "STOCK DISPONIBLE"), "\n")
    cat("Ruptures (stock = 0)    :", sum(st$ETAT == "RUPTURE"), "\n")
    cat("Stocks negatifs (erreur):", sum(st$ETAT == "STOCK NEGATIF"), "\n\n")
    cat("Operations :\n")
    print(table(df$OPERATION))
  })

  # ---------------------------------------------------------------------------
  # 3. RECHERCHE PRODUIT
  # ---------------------------------------------------------------------------
  observe({
    req(stock_data())
    updateSelectizeInput(session, "prod",
                         choices = sort(unique(stock_data()$REFERENCE)),
                         server = TRUE)
  })

  # ---------------------------------------------------------------------------
  # 3b. RECHERCHE RAPIDE DANS "DEMANDES D'ACHAT"
  # ---------------------------------------------------------------------------
  observe({
    req(stock_data())
    ch <- unique(stock_data()$REFERENCE)
    ch <- sort(ch[!is.na(ch) & ch != ""])
    updateSelectizeInput(session, "req_prod", choices = ch, server = FALSE)
  })

  selected_product <- reactive({
    req(stock_data(), input$prod)
    stock_data() %>% filter(REF_KEY == normalize_key(input$prod))
  })

  output$badge <- renderUI({
    p <- selected_product(); req(nrow(p) > 0)
    if (p$STOCK_ACTUEL < 0) span("STOCK NEGATIF", class = "badge-red")
    else if (p$STOCK_ACTUEL == 0) span("RUPTURE", class = "badge-grey")
    else span("STOCK DISPONIBLE", class = "badge-green")
  })

  vbox <- function(field, subtitle, ic, col) {
    renderValueBox({
      p <- selected_product(); req(nrow(p) > 0)
      valueBox(fmt_num(p[[field]]), subtitle, icon = icon(ic), color = col)
    })
  }
  output$v_init <- vbox("STOCK_INITIAL", "Stock initial",       "warehouse", "light-blue")
  output$v_in   <- vbox("TOTAL_ACHAT",   "Total achats",        "cart-plus", "green")
  output$v_out  <- vbox("SORTIE",        "Sorties",             "arrow-up",  "orange")
  output$v_act  <- vbox("STOCK_ACTUEL",  "Stock actuel",        "boxes",     "purple")

  output$prod_info <- renderPrint({
    p <- selected_product(); req(nrow(p) > 0)
    cat("Categorie    :", p$CATEGORIE, "\n")
    cat("Article      :", p$ARTICLE, "\n")
    cat("Unite        :", ifelse(p$UNITE == "", "-", p$UNITE), "\n")
    cat("Transferts IN  :", fmt_num(p$TRANS_IN), "\n")
    cat("Transferts OUT :", fmt_num(p$TRANS_OUT), "\n")
    if (is.na(p$PRIX_REFERENCE)) {
      cat("Prix reference : non disponible\n")
    } else {
      cat("Prix reference :", fmt_num(p$PRIX_REFERENCE, 2), "DA (", p$SOURCE_PRIX, ")\n")
      if (p$DERNIER_FOURNISSEUR != "") cat("Fournisseur    :", p$DERNIER_FOURNISSEUR, "\n")
      cat("Date du prix   :", format(p$DATE_PRIX, "%d/%m/%Y"), "\n")
    }
  })

  output$last_mvt <- renderDT({
    req(raw_data(), input$prod)
    d <- raw_data() %>%
      filter(REF_KEY == normalize_key(input$prod)) %>%
      arrange(desc(DATE), desc(N)) %>%
      select(any_of(c("DATE", "OPERATION", "TIERS", "QUANTITE", "PRIX_UNIT", "N°BON"))) %>%
      head(50)
    datatable(d, rownames = FALSE,
              options = list(pageLength = 10, scrollX = TRUE, order = list()))
  })

  # ---------------------------------------------------------------------------
  # 4. STOCK ACTUEL
  # ---------------------------------------------------------------------------
  observe({
    req(stock_data())
    updateSelectInput(session, "f_cat",
                      choices = c("Toutes", sort(unique(stock_data()$CATEGORIE))))
  })

  stock_view <- reactive({
    req(stock_data())
    d <- stock_data()
    if (!is.null(input$f_cat) && input$f_cat != "Toutes") d <- d %>% filter(CATEGORIE == input$f_cat)
    if (!is.null(input$f_etat) && input$f_etat != "Tous") d <- d %>% filter(ETAT == input$f_etat)
    d
  })

  output$stock_table <- renderDT({
    d <- stock_view() %>%
      transmute(REFERENCE, CATEGORIE, ARTICLE, UNITE,
                STOCK_INITIAL, TOTAL_ACHAT, TRANS_IN, TRANS_OUT, SORTIE,
                STOCK_ACTUEL, ETAT, PRIX_REFERENCE, SOURCE_PRIX,
                DERNIER_MVT)
    datatable(d, rownames = FALSE, filter = "top",
              options = list(pageLength = 25, scrollX = TRUE)) %>%
      formatStyle("ETAT",
        backgroundColor = styleEqual(c("STOCK DISPONIBLE", "RUPTURE", "STOCK NEGATIF"),
                                     c("#d4edda", "#e2e3e5", "#f8d7da")),
        fontWeight = "bold") %>%
      formatRound(c("STOCK_INITIAL", "TOTAL_ACHAT", "TRANS_IN", "TRANS_OUT",
                    "SORTIE", "STOCK_ACTUEL", "PRIX_REFERENCE"), 2)
  })

  output$xlsx_stock <- downloadHandler(
    filename = function() paste0("STOCK_ACTUEL_", Sys.Date(), ".xlsx"),
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, "STOCK_ACTUEL")
      writeData(wb, "STOCK_ACTUEL", stock_view())
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  # ---------------------------------------------------------------------------
  # 5. DEMANDE D'ACHAT
  # ---------------------------------------------------------------------------
  request_data <- eventReactive(input$check, {
    req(raw_data(), stock_data())

    produits <- normalize_name(input$req_prod %||% character(0))
    quantites <- normalize_name(str_split(input$txt_qte %||% "", "\n")[[1]])

    n <- max(length(produits), length(quantites))
    length(produits) <- n; length(quantites) <- n
    produits[is.na(produits)] <- ""; quantites[is.na(quantites)] <- ""

    res <- tibble(PRODUIT_DEMANDE = produits,
                  QTE_DEMANDEE = safe_to_numeric(quantites)) %>%
      filter(PRODUIT_DEMANDE != "" | !is.na(QTE_DEMANDEE))

    validate(need(nrow(res) > 0, "Saisissez au moins un produit."))

    st <- stock_data()

    res <- res %>%
      mutate(REF_KEY = normalize_key(PRODUIT_DEMANDE)) %>%
      left_join(st %>% select(REF_KEY, REFERENCE, UNITE, STOCK_ACTUEL,
                              PRIX_REFERENCE, SOURCE_PRIX, DERNIER_FOURNISSEUR),
                by = "REF_KEY") %>%
      mutate(
        QTE_ACHAT = case_when(
          is.na(QTE_DEMANDEE) | is.na(STOCK_ACTUEL) ~ NA_real_,
          STOCK_ACTUEL >= QTE_DEMANDEE ~ 0,
          TRUE ~ QTE_DEMANDEE - pmax(STOCK_ACTUEL, 0)
        ),
        DECISION = case_when(
          PRODUIT_DEMANDE == "" ~ "PRODUIT MANQUANT",
          is.na(QTE_DEMANDEE) ~ "QTE MANQUANTE",
          is.na(STOCK_ACTUEL) ~ "NON TROUVE",
          STOCK_ACTUEL <= 0 & QTE_DEMANDEE > 0 ~ "ACHAT NECESSAIRE",
          STOCK_ACTUEL >= QTE_DEMANDEE ~ "STOCK SUFFISANT",
          TRUE ~ "STOCK PARTIEL"
        ),
        COUT_ESTIME = ifelse(!is.na(QTE_ACHAT) & !is.na(PRIX_REFERENCE),
                             QTE_ACHAT * PRIX_REFERENCE, NA_real_)
      )

    # suggestion pour les produits non trouves
    all_refs <- st$REFERENCE
    all_keys <- normalize_key(all_refs)
    res$SUGGESTION <- vapply(seq_len(nrow(res)), function(i) {
      if (res$DECISION[i] != "NON TROUVE") return("")
      dist <- stringdist(res$REF_KEY[i], all_keys, method = "jw")
      b <- which.min(dist)
      if (length(b) && dist[b] <= 0.20) all_refs[b] else ""
    }, character(1))

    res %>% select(PRODUIT_DEMANDE, REFERENCE, UNITE, QTE_DEMANDEE, STOCK_ACTUEL,
                   QTE_ACHAT, PRIX_REFERENCE, COUT_ESTIME, DECISION,
                   SOURCE_PRIX, DERNIER_FOURNISSEUR, SUGGESTION)
  })

  kpi <- function(expr_fun, subtitle, ic, col) {
    renderValueBox({
      d <- request_data()
      valueBox(expr_fun(d), subtitle, icon = icon(ic), color = col)
    })
  }
  output$vb_buy  <- kpi(function(d) sum(d$QTE_ACHAT > 0, na.rm = TRUE),
                        "Produits a acheter", "shopping-cart", "green")
  output$vb_not  <- kpi(function(d) sum(d$DECISION == "STOCK SUFFISANT", na.rm = TRUE),
                        "Stock suffisant", "check", "blue")
  output$vb_rup  <- kpi(function(d) sum(d$DECISION == "ACHAT NECESSAIRE", na.rm = TRUE),
                        "Rupture / achat", "exclamation-triangle", "red")
  output$vb_part <- kpi(function(d) sum(d$DECISION == "STOCK PARTIEL", na.rm = TRUE),
                        "Stock partiel", "adjust", "yellow")

  output$total_cost <- renderText({
    d <- request_data()
    paste("Cout total estime des achats :", fmt_num(sum(d$COUT_ESTIME, na.rm = TRUE), 2), "DA")
  })

  output$tbl <- renderDT({
    d <- request_data()
    disp <- d %>% mutate(
      QTE_DEMANDEE = fmt_num(QTE_DEMANDEE),
      STOCK_ACTUEL = fmt_num(STOCK_ACTUEL),
      QTE_ACHAT = fmt_num(QTE_ACHAT),
      PRIX_REFERENCE = ifelse(is.na(PRIX_REFERENCE), "", paste(fmt_num(PRIX_REFERENCE, 2), "DA")),
      COUT_ESTIME = ifelse(is.na(COUT_ESTIME), "", paste(fmt_num(COUT_ESTIME, 2), "DA"))
    )
    names(disp) <- c("PRODUIT DEMANDE", "REFERENCE TROUVEE", "UNITE", "QTE DEMANDEE",
                     "STOCK ACTUEL", "QTE A ACHETER", "PRIX REFERENCE", "COUT ESTIME",
                     "DECISION", "SOURCE DU PRIX", "DERNIER FOURNISSEUR", "SUGGESTION")

    datatable(disp, rownames = FALSE, filter = "top",
              extensions = c("Buttons"),
              options = list(pageLength = 25, scrollX = TRUE, dom = "Bfrtip",
                             buttons = c("copy", "csv", "excel")),
              class = "cell-border stripe hover") %>%
      formatStyle("DECISION",
        backgroundColor = styleEqual(
          c("STOCK SUFFISANT", "STOCK PARTIEL", "ACHAT NECESSAIRE", "NON TROUVE", "QTE MANQUANTE", "PRODUIT MANQUANT"),
          c("#d4edda", "#fff3cd", "#f8d7da", "#e2e3e5", "#e2e3e5", "#e2e3e5")),
        fontWeight = "bold") %>%
      formatStyle("QTE A ACHETER", fontWeight = "bold")
  })

  # ---------------------------------------------------------------------------
  # 6. EXPORTS
  # ---------------------------------------------------------------------------
  output$xlsx <- downloadHandler(
    filename = function() paste0("CONTROLE_ACHAT_", Sys.Date(), ".xlsx"),
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, "CONTROLE_ACHAT"); writeData(wb, "CONTROLE_ACHAT", request_data())
      addWorksheet(wb, "STOCK_ACTUEL");   writeData(wb, "STOCK_ACTUEL", stock_data())
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )

  output$xlsx_buy <- downloadHandler(
    filename = function() paste0("A_ACHETER_", Sys.Date(), ".xlsx"),
    content = function(file) {
      wb <- createWorkbook()
      addWorksheet(wb, "A_ACHETER")
      writeData(wb, "A_ACHETER", request_data() %>% filter(QTE_ACHAT > 0))
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}
