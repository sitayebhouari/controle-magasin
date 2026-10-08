# =============================================================================
# server.R
# Controle Achat Magasin
# Source unique : MOUVEMENT
# =============================================================================

server <- function(input, output, session) {
  
  # ===========================================================================
  # 1. STOCKAGE DES DONNÉES
  # ===========================================================================
  
  raw_data <- reactiveVal(NULL)
  stock_data <- reactiveVal(NULL)
  price_data <- reactiveVal(NULL)
  
  
  # ===========================================================================
  # 2. CHARGEMENT DU FICHIER EXCEL
  # ===========================================================================
  
  observeEvent(input$file, {
    
    req(input$file)
    
    tryCatch({
      
      # -----------------------------------------------------------------------
      # Lecture de la feuille MOUVEMENT
      # -----------------------------------------------------------------------
      
      df <- read_excel(
        input$file$datapath,
        sheet = "MOUVEMENT",
        col_types = "text"
      )
      
      
      # -----------------------------------------------------------------------
      # Vérification des colonnes
      # -----------------------------------------------------------------------
      
      missing_cols <- setdiff(
        REQUIRED_COLS,
        names(df)
      )
      
      if (length(missing_cols) > 0) {
        
        showNotification(
          paste(
            "Colonnes manquantes :",
            paste(missing_cols, collapse = ", ")
          ),
          type = "error",
          duration = 10
        )
        
        return(NULL)
      }
      
      
      # -----------------------------------------------------------------------
      # Nettoyage des données
      # -----------------------------------------------------------------------
      
      df <- df %>%
        mutate(
          
          DATE = safe_to_date(DATE),
          
          OPERATION = clean_operation(OPERATION),
          
          `FOURNISSEUR & CLIENT` =
            normalize_name(`FOURNISSEUR & CLIENT`),
          
          ARTICLE =
            normalize_name(ARTICLE),
          
          PRODUIT =
            normalize_name(PRODUIT),
          
          `REF PRODUIT` =
            clean_product_reference(`REF PRODUIT`),
          
          UNIT =
            if ("UNIT" %in% names(.))
              normalize_name(UNIT)
          else
            "",
          
          ENTRIE =
            safe_to_numeric(ENTRIE),
          
          `PU ENTRIE` =
            safe_to_numeric(`PU ENTRIE`),
          
          SORTEE =
            safe_to_numeric(SORTEE)
        )
      
      
      # -----------------------------------------------------------------------
      # Clé de recherche produit
      # -----------------------------------------------------------------------
      
      df <- df %>%
        mutate(
          REF_KEY = normalize_key(`REF PRODUIT`)
        )
      
      
      # -----------------------------------------------------------------------
      # Supprimer les lignes sans référence produit
      # -----------------------------------------------------------------------
      
      df_stock <- df %>%
        filter(
          !is.na(REF_KEY),
          REF_KEY != ""
        )
      
      
      # =========================================================================
      # 3. CALCUL DU STOCK
      # =========================================================================
      
      stock <- df_stock %>%
        group_by(REF_KEY) %>%
        summarise(
          
          REF_PRODUIT =
            first(
              `REF PRODUIT`[
                !is.na(`REF PRODUIT`) &
                  `REF PRODUIT` != ""
              ],
              default = ""
            ),
          
          PRODUIT =
            first(
              PRODUIT[
                !is.na(PRODUIT) &
                  PRODUIT != ""
              ],
              default = ""
            ),
          
          ARTICLE =
            first(
              ARTICLE[
                !is.na(ARTICLE) &
                  ARTICLE != ""
              ],
              default = ""
            ),
          
          UNIT =
            first(
              UNIT[
                !is.na(UNIT) &
                  UNIT != ""
              ],
              default = ""
            ),
          
          STOCK_INITIAL =
            sum(
              ENTRIE[
                DATE == CUTOFF_DATE &
                  OPERATION == "INVENTAIRE"
              ],
              na.rm = TRUE
            ),
          
          TOTAL_ENTREE =
            sum(
              ENTRIE[
                DATE > CUTOFF_DATE
              ],
              na.rm = TRUE
            ),
          
          TOTAL_SORTIE =
            sum(
              SORTEE[
                DATE > CUTOFF_DATE
              ],
              na.rm = TRUE
            ),
          
          .groups = "drop"
        ) %>%
        mutate(
          
          STOCK_ACTUEL =
            STOCK_INITIAL +
            TOTAL_ENTREE -
            TOTAL_SORTIE,
          
          ETAT =
            case_when(
              
              STOCK_ACTUEL < 0 ~
                "🔴 STOCK NÉGATIF",
              
              STOCK_ACTUEL == 0 ~
                "⚪ RUPTURE",
              
              TRUE ~
                "🟢 STOCK DISPONIBLE"
            )
        )
      
      
      # =========================================================================
      # 4. CALCUL DU PRIX DE RÉFÉRENCE
      # =========================================================================
      
      # -----------------------------------------------------------------------
      # Priorité :
      #
      # 1. Dernière opération ENTREE avec prix valide
      # 2. Sinon dernier INVENTAIRE avec prix valide
      # -----------------------------------------------------------------------
      
      purchases <- df_stock %>%
        filter(
          OPERATION == "ENTREE",
          !is.na(`PU ENTRIE`),
          `PU ENTRIE` > 0
        ) %>%
        arrange(
          REF_KEY,
          desc(DATE)
        ) %>%
        group_by(REF_KEY) %>%
        slice(1) %>%
        ungroup() %>%
        transmute(
          
          REF_KEY,
          
          PRIX_REFERENCE =
            `PU ENTRIE`,
          
          SOURCE_PRIX =
            "PRIX DERNIER ACHAT",
          
          DERNIER_FOURNISSEUR =
            `FOURNISSEUR & CLIENT`,
          
          DATE_PRIX =
            DATE
        )
      
      
      # -----------------------------------------------------------------------
      # Prix provenant de l'inventaire
      # -----------------------------------------------------------------------
      
      inventory_price <- df_stock %>%
        filter(
          OPERATION == "INVENTAIRE",
          !is.na(`PU ENTRIE`),
          `PU ENTRIE` > 0
        ) %>%
        arrange(
          REF_KEY,
          desc(DATE)
        ) %>%
        group_by(REF_KEY) %>%
        slice(1) %>%
        ungroup() %>%
        transmute(
          
          REF_KEY,
          
          PRIX_REFERENCE =
            `PU ENTRIE`,
          
          SOURCE_PRIX =
            "PRIX INVENTAIRE",
          
          DERNIER_FOURNISSEUR =
            "AUCUN ACHAT ENREGISTRÉ",
          
          DATE_PRIX =
            DATE
        )
      
      
      # -----------------------------------------------------------------------
      # Compléter les prix manquants par le prix inventaire
      # -----------------------------------------------------------------------
      
      prices <- stock %>%
        select(REF_KEY) %>%
        left_join(
          purchases,
          by = "REF_KEY"
        ) %>%
        left_join(
          inventory_price,
          by = "REF_KEY",
          suffix = c("", "_INV")
        ) %>%
        mutate(
          
          PRIX_REFERENCE =
            ifelse(
              is.na(PRIX_REFERENCE),
              PRIX_REFERENCE_INV,
              PRIX_REFERENCE
            ),
          
          SOURCE_PRIX =
            ifelse(
              is.na(SOURCE_PRIX),
              SOURCE_PRIX_INV,
              SOURCE_PRIX
            ),
          
          DERNIER_FOURNISSEUR =
            ifelse(
              is.na(DERNIER_FOURNISSEUR),
              DERNIER_FOURNISSEUR_INV,
              DERNIER_FOURNISSEUR
            ),
          
          DATE_PRIX =
            ifelse(
              is.na(DATE_PRIX),
              DATE_PRIX_INV,
              DATE_PRIX
            )
        ) %>%
        select(
          REF_KEY,
          PRIX_REFERENCE,
          SOURCE_PRIX,
          DERNIER_FOURNISSEUR,
          DATE_PRIX
        )
      
      
      # =========================================================================
      # 5. AJOUT DU PRIX AU STOCK
      # =========================================================================
      
      stock <- stock %>%
        left_join(
          prices,
          by = "REF_KEY"
        )
      
      
      # =========================================================================
      # 6. SAUVEGARDE EN MÉMOIRE
      # =========================================================================
      
      raw_data(df)
      stock_data(stock)
      price_data(prices)
      
      
      # =========================================================================
      # 7. MESSAGE DE CONFIRMATION
      # =========================================================================
      
      showNotification(
        paste(
          "Fichier chargé avec succès.",
          nrow(df),
          "lignes MOUVEMENT."
        ),
        type = "message",
        duration = 5
      )
      
    },
    
    error = function(e) {
      
      showNotification(
        paste(
          "Erreur lors du chargement :",
          e$message
        ),
        type = "error",
        duration = 10
      )
    })
  })
  
  
  # ===========================================================================
  # 8. ÉTAT DU FICHIER
  # ===========================================================================
  
  output$health <- renderPrint({
    
    req(raw_data())
    
    df <- raw_data()
    stock <- stock_data()
    
    cat("============================================\n")
    cat("       CONTRÔLE ACHAT MAGASIN\n")
    cat("============================================\n\n")
    
    cat("Fichier chargé : OUI\n")
    
    cat(
      "Nombre de lignes MOUVEMENT :",
      nrow(df),
      "\n"
    )
    
    cat(
      "Nombre de produits uniques :",
      nrow(stock),
      "\n"
    )
    
    cat(
      "Date inventaire :",
      format(CUTOFF_DATE, "%d/%m/%Y"),
      "\n"
    )
    
    cat(
      "Date minimale :",
      format(
        min(df$DATE, na.rm = TRUE),
        "%d/%m/%Y"
      ),
      "\n"
    )
    
    cat(
      "Date maximale :",
      format(
        max(df$DATE, na.rm = TRUE),
        "%d/%m/%Y"
      ),
      "\n"
    )
    
    cat("\n============================================\n")
  })
  
  
  # ===========================================================================
  # 9. RECHERCHE PRODUIT
  # ===========================================================================
  
  # Recherche rapide dans "Demandes d'achat" (selection multiple)
  observe({
    
    req(stock_data())
    
    ch <- unique(stock_data()$REF_PRODUIT)
    ch <- sort(ch[!is.na(ch) & ch != ""])
    
    updateSelectizeInput(
      session,
      "req_prod",
      choices = ch,
      server = FALSE
    )
  })
  
  
  observe({
    
    req(stock_data())
    
    stock <- stock_data()
    
    choices <- stock$REF_PRODUIT
    
    choices <- choices[
      !is.na(choices) &
        choices != ""
    ]
    
    updateSelectizeInput(
      session,
      "prod",
      choices = choices,
      server = TRUE
    )
  })
  
  
  # ===========================================================================
  # 10. PRODUIT SÉLECTIONNÉ
  # ===========================================================================
  
  selected_product <- reactive({
    
    req(
      stock_data(),
      input$prod
    )
    
    key <- normalize_key(input$prod)
    
    stock_data() %>%
      filter(
        REF_KEY == key
      )
  })
  
  
  # ===========================================================================
  # 11. BADGE STOCK
  # ===========================================================================
  
  output$badge <- renderUI({
    
    p <- selected_product()
    
    req(nrow(p) > 0)
    
    if (p$STOCK_ACTUEL < 0) {
      
      span(
        "STOCK NÉGATIF",
        class = "badge-red"
      )
      
    } else if (p$STOCK_ACTUEL == 0) {
      
      span(
        "RUPTURE",
        class = "badge-grey"
      )
      
    } else {
      
      span(
        "STOCK DISPONIBLE",
        class = "badge-green"
      )
    }
  })
  
  
  # ===========================================================================
  # 12. VALUE BOXES RECHERCHE
  # ===========================================================================
  
  output$v_init <- renderValueBox({
    
    p <- selected_product()
    
    req(nrow(p) > 0)
    
    valueBox(
      value = format(
        p$STOCK_INITIAL,
        big.mark = " ",
        decimal.mark = "."
      ),
      subtitle = "Stock initial",
      icon = icon("warehouse")
    )
  })
  
  
  output$v_in <- renderValueBox({
    
    p <- selected_product()
    
    req(nrow(p) > 0)
    
    valueBox(
      value = format(
        p$TOTAL_ENTREE,
        big.mark = " ",
        decimal.mark = "."
      ),
      subtitle = "Total entrée",
      icon = icon("arrow-down")
    )
  })
  
  
  output$v_out <- renderValueBox({
    
    p <- selected_product()
    
    req(nrow(p) > 0)
    
    valueBox(
      value = format(
        p$TOTAL_SORTIE,
        big.mark = " ",
        decimal.mark = "."
      ),
      subtitle = "Total sortie",
      icon = icon("arrow-up")
    )
  })
  
  
  output$v_act <- renderValueBox({
    
    p <- selected_product()
    
    req(nrow(p) > 0)
    
    valueBox(
      value = format(
        p$STOCK_ACTUEL,
        big.mark = " ",
        decimal.mark = "."
      ),
      subtitle = "Stock actuel",
      icon = icon("boxes")
    )
  })
  
  # ===========================================================================
  # TABLEAU STOCK ACTUEL
  # ===========================================================================
  
  output$stock_table <- renderDT({
    
    req(stock_data())
    
    d <- stock_data() %>%
      select(
        REF_PRODUIT,
        PRODUIT,
        ARTICLE,
        UNIT,
        STOCK_INITIAL,
        TOTAL_ENTREE,
        TOTAL_SORTIE,
        STOCK_ACTUEL,
        ETAT,
        PRIX_REFERENCE,
        SOURCE_PRIX,
        DERNIER_FOURNISSEUR
      )
    
    datatable(
      d,
      rownames = FALSE,
      filter = "top",
      options = list(
        pageLength = 25,
        scrollX = TRUE,
        scrollY = "600px",
        scroller = TRUE
      )
    )
  })
  
  # ===========================================================================
  # 13. DERNIERS MOUVEMENTS
  # ===========================================================================
  
  output$last_mvt <- renderDT({
    
    req(
      raw_data(),
      input$prod
    )
    
    key <- normalize_key(input$prod)
    
    df <- raw_data() %>%
      filter(
        REF_KEY == key
      ) %>%
      arrange(
        desc(DATE)
      ) %>%
      select(
        DATE,
        OPERATION,
        `N° BON`,
        `FOURNISSEUR & CLIENT`,
        ARTICLE,
        PRODUIT,
        `REF PRODUIT`,
        ENTRIE,
        `PU ENTRIE`,
        SORTEE
      ) %>%
      head(20)
    
    datatable(
      df,
      options = list(
        pageLength = 10,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
  
  
  # ===========================================================================
  # 14. DERNIER ACHAT
  # ===========================================================================
  
  output$last_buy <- renderPrint({
    
    req(
      raw_data(),
      input$prod
    )
    
    key <- normalize_key(input$prod)
    
    df <- raw_data() %>%
      filter(
        REF_KEY == key,
        OPERATION == "ENTREE",
        !is.na(`PU ENTRIE`),
        `PU ENTRIE` > 0
      ) %>%
      arrange(
        desc(DATE)
      )
    
    
    # -------------------------------------------------------------------------
    # S'il n'existe aucun achat
    # -------------------------------------------------------------------------
    
    if (nrow(df) == 0) {
      
      inv <- raw_data() %>%
        filter(
          REF_KEY == key,
          OPERATION == "INVENTAIRE",
          !is.na(`PU ENTRIE`),
          `PU ENTRIE` > 0
        ) %>%
        arrange(
          desc(DATE)
        )
      
      if (nrow(inv) == 0) {
        
        cat("Aucun prix disponible.")
        
      } else {
        
        cat(
          "Prix inventaire :",
          round(inv$`PU ENTRIE`[1], 2),
          "\n"
        )
        
        cat(
          "Date :",
          format(
            inv$DATE[1],
            "%d/%m/%Y"
          ),
          "\n"
        )
      }
      
    } else {
      
      cat(
        "Date dernier achat :",
        format(
          df$DATE[1],
          "%d/%m/%Y"
        ),
        "\n"
      )
      
      cat(
        "Fournisseur :",
        df$`FOURNISSEUR & CLIENT`[1],
        "\n"
      )
      
      cat(
        "Prix unitaire :",
        round(
          df$`PU ENTRIE`[1],
          2
        ),
        "\n"
      )
      
      cat(
        "Quantité entrée :",
        df$ENTRIE[1],
        "\n"
      )
      
      cat(
        "N° Bon :",
        df$`N° BON`[1],
        "\n"
      )
    }
  })
  
  
  # ===========================================================================
  # 15. LECTURE DE LA DEMANDE D'ACHAT
  # ===========================================================================
  
  request_data <- reactive({
    
    req(
      input$check,
      raw_data(),
      stock_data()
    )
    
    produits <- input$req_prod %||% character(0)
    
    quantites <- str_split(
      input$txt_qte %||% "",
      "\n"
    )[[1]]
    
    
    produits <- normalize_name(produits)
    
    quantites <- normalize_name(quantites)
    
    
    # -------------------------------------------------------------------------
    # Supprimer les lignes complètement vides
    # -------------------------------------------------------------------------
    
    max_n <- max(
      length(produits),
      length(quantites)
    )
    
    length(produits) <- max_n
    length(quantites) <- max_n
    
    produits[is.na(produits)] <- ""
    quantites[is.na(quantites)] <- ""
    
    
    # -------------------------------------------------------------------------
    # Conversion des quantités
    # -------------------------------------------------------------------------
    
    qte <- safe_to_numeric(
      quantites
    )
    
    
    # -------------------------------------------------------------------------
    # Tableau de résultat initial
    # -------------------------------------------------------------------------
    
    result <- tibble(
      
      PRODUIT_DEMANDE = produits,
      
      QTE_DEMANDEE = qte
    ) %>%
      filter(
        PRODUIT_DEMANDE != "" |
          !is.na(QTE_DEMANDEE)
      )
    
    
    if (nrow(result) == 0) {
      
      return(
        tibble(
          PRODUIT_DEMANDE = character(),
          QTE_DEMANDEE = numeric(),
          REF_PRODUIT = character(),
          STOCK_ACTUEL = numeric(),
          QTE_ACHAT = numeric(),
          PRIX_REFERENCE = numeric(),
          COUT_ESTIME = numeric(),
          DECISION = character(),
          SOURCE_PRIX = character(),
          DERNIER_FOURNISSEUR = character()
        )
      )
    }
    
    
    # =========================================================================
    # 16. RECHERCHE EXACTE DU PRODUIT
    # =========================================================================
    
    result <- result %>%
      mutate(
        REF_KEY = normalize_key(
          PRODUIT_DEMANDE
        )
      )
    
    
    # -------------------------------------------------------------------------
    # Jointure avec le stock
    # -------------------------------------------------------------------------
    
    result <- result %>%
      left_join(
        stock_data() %>%
          select(
            REF_KEY,
            REF_PRODUIT,
            PRODUIT,
            STOCK_ACTUEL,
            PRIX_REFERENCE,
            SOURCE_PRIX,
            DERNIER_FOURNISSEUR
          ),
        by = "REF_KEY"
      )
    
    
    # =========================================================================
    # 17. CALCUL DE LA QUANTITÉ À ACHETER
    # =========================================================================
    
    result <- result %>%
      mutate(
        
        QTE_ACHAT =
          case_when(
            
            is.na(QTE_DEMANDEE) ~
              NA_real_,
            
            is.na(STOCK_ACTUEL) ~
              NA_real_,
            
            STOCK_ACTUEL >= QTE_DEMANDEE ~
              0,
            
            STOCK_ACTUEL < QTE_DEMANDEE ~
              QTE_DEMANDEE - pmax(
                STOCK_ACTUEL,
                0
              )
          )
      )
    
    
    # =========================================================================
    # 18. DÉCISION
    # =========================================================================
    
    result <- result %>%
      mutate(
        
        DECISION =
          case_when(
            
            PRODUIT_DEMANDE == "" ~
              "PRODUIT MANQUANT",
            
            is.na(QTE_DEMANDEE) ~
              "QTE MANQUANTE",
            
            is.na(STOCK_ACTUEL) ~
              "NON TROUVE",
            
            STOCK_ACTUEL <= 0 &
              QTE_DEMANDEE > 0 ~
              "ACHAT NECESSAIRE",
            
            STOCK_ACTUEL >= QTE_DEMANDEE ~
              "STOCK SUFFISANT",
            
            STOCK_ACTUEL < QTE_DEMANDEE ~
              "STOCK PARTIEL",
            
            TRUE ~
              ""
          )
      )
    
    
    # =========================================================================
    # 19. COÛT ESTIMÉ
    # =========================================================================
    
    result <- result %>%
      mutate(
        
        COUT_ESTIME =
          ifelse(
            !is.na(QTE_ACHAT) &
              !is.na(PRIX_REFERENCE),
            
            QTE_ACHAT *
              PRIX_REFERENCE,
            
            NA_real_
          )
      )
    
    
    # =========================================================================
    # 20. SUGGESTION POUR LES PRODUITS NON TROUVÉS
    # =========================================================================
    
    all_refs <- stock_data()$REF_PRODUIT
    
    result <- result %>%
      rowwise() %>%
      mutate(
        
        SUGGESTION =
          if (
            DECISION == "NON TROUVE" &&
            PRODUIT_DEMANDE != ""
          ) {
            
            distances <- stringdist(
              normalize_key(PRODUIT_DEMANDE),
              normalize_key(all_refs),
              method = "jw"
            )
            
            if (length(distances) > 0) {
              
              best <- which.min(
                distances
              )
              
              if (
                distances[best] <= 0.20
              ) {
                all_refs[best]
              } else {
                ""
              }
              
            } else {
              ""
            }
            
          } else {
            ""
          }
      ) %>%
      ungroup()
    
    
    # =========================================================================
    # 21. COLONNES FINALES
    # =========================================================================
    
    result %>%
      select(
        PRODUIT_DEMANDE,
        REF_PRODUIT,
        QTE_DEMANDEE,
        STOCK_ACTUEL,
        QTE_ACHAT,
        PRIX_REFERENCE,
        COUT_ESTIME,
        DECISION,
        SOURCE_PRIX,
        DERNIER_FOURNISSEUR,
        SUGGESTION
      )
  })
  
  
  # ===========================================================================
  # 22. KPI - À ACHETER
  # ===========================================================================
  
  output$vb_buy <- renderValueBox({
    
    req(request_data())
    
    d <- request_data()
    
    n <- sum(
      d$QTE_ACHAT > 0,
      na.rm = TRUE
    )
    
    valueBox(
      value = n,
      subtitle = "Produits à acheter",
      icon = icon("shopping-cart"),
      color = "green"
    )
  })
  
  
  # ===========================================================================
  # 23. KPI - STOCK SUFFISANT
  # ===========================================================================
  
  output$vb_not <- renderValueBox({
    
    req(request_data())
    
    d <- request_data()
    
    n <- sum(
      d$DECISION == "STOCK SUFFISANT",
      na.rm = TRUE
    )
    
    valueBox(
      value = n,
      subtitle = "Stock suffisant",
      icon = icon("check"),
      color = "blue"
    )
  })
  
  
  # ===========================================================================
  # 24. KPI - RUPTURE
  # ===========================================================================
  
  output$vb_rup <- renderValueBox({
    
    req(request_data())
    
    d <- request_data()
    
    n <- sum(
      d$DECISION == "ACHAT NECESSAIRE",
      na.rm = TRUE
    )
    
    valueBox(
      value = n,
      subtitle = "Rupture / achat",
      icon = icon("exclamation-triangle"),
      color = "red"
    )
  })
  
  
  # ===========================================================================
  # 25. KPI - STOCK PARTIEL
  # ===========================================================================
  
  output$vb_part <- renderValueBox({
    
    req(request_data())
    
    d <- request_data()
    
    n <- sum(
      d$DECISION == "STOCK PARTIEL",
      na.rm = TRUE
    )
    
    valueBox(
      value = n,
      subtitle = "Stock partiel",
      icon = icon("adjust"),
      color = "yellow"
    )
  })
  
  
  
  # ===========================================================================
  # 26. TABLEAU DES RÉSULTATS
  # ===========================================================================
  
  output$tbl <- renderDT({
    
    req(request_data())
    
    d <- request_data()
    
    
    # -------------------------------------------------------------------------
    # ترتيب الأعمدة بالشكل المطلوب
    # -------------------------------------------------------------------------
    
    d_display <- d %>%
      select(
        PRODUIT_DEMANDE,
        REF_PRODUIT,
        QTE_DEMANDEE,
        STOCK_ACTUEL,
        QTE_ACHAT,
        PRIX_REFERENCE,
        COUT_ESTIME,
        DECISION,
        SOURCE_PRIX,
        DERNIER_FOURNISSEUR,
        SUGGESTION
      )
    
    
    # -------------------------------------------------------------------------
    # تغيير أسماء الأعمدة للعرض فقط
    # -------------------------------------------------------------------------
    
    names(d_display) <- c(
      "PRODUIT DEMANDÉ",
      "RÉF PRODUIT",
      "QTÉ DEMANDÉE",
      "STOCK ACTUEL",
      "QTÉ À ACHETER",
      "PRIX RÉFÉRENCE",
      "COÛT ESTIMÉ",
      "DÉCISION",
      "SOURCE DU PRIX",
      "DERNIER FOURNISSEUR",
      "SUGGESTION"
    )
    
    
    # -------------------------------------------------------------------------
    # تنسيق الأرقام
    # -------------------------------------------------------------------------
    
    d_display$`STOCK ACTUEL` <- ifelse(
      is.na(d_display$`STOCK ACTUEL`),
      "",
      format(
        as.numeric(d_display$`STOCK ACTUEL`),
        big.mark = " ",
        decimal.mark = ".",
        nsmall = 0
      )
    )
    
    
    d_display$`QTÉ DEMANDÉE` <- ifelse(
      is.na(d_display$`QTÉ DEMANDÉE`),
      "",
      format(
        as.numeric(d_display$`QTÉ DEMANDÉE`),
        big.mark = " ",
        decimal.mark = ".",
        nsmall = 0
      )
    )
    
    
    d_display$`QTÉ À ACHETER` <- ifelse(
      is.na(d_display$`QTÉ À ACHETER`),
      "",
      format(
        as.numeric(d_display$`QTÉ À ACHETER`),
        big.mark = " ",
        decimal.mark = ".",
        nsmall = 0
      )
    )
    
    
    # -------------------------------------------------------------------------
    # Prix en DA
    # -------------------------------------------------------------------------
    
    d_display$`PRIX RÉFÉRENCE` <- ifelse(
      is.na(d_display$`PRIX RÉFÉRENCE`),
      "",
      paste0(
        format(
          as.numeric(d_display$`PRIX RÉFÉRENCE`),
          big.mark = " ",
          decimal.mark = ".",
          nsmall = 2
        ),
        " DA"
      )
    )
    
    
    # -------------------------------------------------------------------------
    # Coût estimé en DA
    # -------------------------------------------------------------------------
    
    d_display$`COÛT ESTIMÉ` <- ifelse(
      is.na(d_display$`COÛT ESTIMÉ`),
      "",
      paste0(
        format(
          as.numeric(d_display$`COÛT ESTIMÉ`),
          big.mark = " ",
          decimal.mark = ".",
          nsmall = 2
        ),
        " DA"
      )
    )
    
    
    # -------------------------------------------------------------------------
    # Tableau interactif
    # -------------------------------------------------------------------------
    
    dt <- datatable(
      
      d_display,
      
      rownames = FALSE,
      
      filter = "top",
      
      extensions = c(
        "Buttons",
        "FixedHeader"
      ),
      
      options = list(
        
        pageLength = 25,
        
        scrollX = TRUE,
        
        scrollY = "600px",
        
        fixedHeader = TRUE,
        
        autoWidth = TRUE,
        
        dom = "Bfrtip",
        
        buttons = c(
          "copy",
          "csv",
          "excel"
        )
      ),
      
      class = "cell-border stripe hover"
    )
    
    
    # -------------------------------------------------------------------------
    # Couleurs des décisions
    # -------------------------------------------------------------------------
    
    dt <- dt %>%
      
      formatStyle(
        "DÉCISION",
        
        target = "cell",
        
        backgroundColor = styleEqual(
          
          c(
            "STOCK SUFFISANT",
            "STOCK PARTIEL",
            "ACHAT NECESSAIRE",
            "RUPTURE",
            "NON TROUVE",
            "QTE MANQUANTE"
          ),
          
          c(
            "#d4edda",
            "#fff3cd",
            "#f8d7da",
            "#f8d7da",
            "#e2e3e5",
            "#e2e3e5"
          )
        ),
        
        color = styleEqual(
          
          c(
            "STOCK SUFFISANT",
            "STOCK PARTIEL",
            "ACHAT NECESSAIRE",
            "RUPTURE",
            "NON TROUVE",
            "QTE MANQUANTE"
          ),
          
          c(
            "#155724",
            "#856404",
            "#721c24",
            "#721c24",
            "#383d41",
            "#383d41"
          )
        ),
        
        fontWeight = "bold"
      )
    
    
    # -------------------------------------------------------------------------
    # إبراز كمية الشراء
    # -------------------------------------------------------------------------
    
    dt <- dt %>%
      
      formatStyle(
        "QTÉ À ACHETER",
        
        fontWeight = "bold"
      )
    
    
    dt
  })
  
  # ===========================================================================
  # 27. EXPORT EXCEL COMPLET
  # ===========================================================================
  
  output$xlsx <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "CONTROLE_ACHAT_",
        Sys.Date(),
        ".xlsx"
      )
    },
    
    content = function(file) {
      
      wb <- createWorkbook()
      
      # -----------------------------------------------------------------------
      # Feuille résultat
      # -----------------------------------------------------------------------
      
      addWorksheet(
        wb,
        "CONTROLE_ACHAT"
      )
      
      writeData(
        wb,
        "CONTROLE_ACHAT",
        request_data()
      )
      
      
      # -----------------------------------------------------------------------
      # Feuille stock
      # -----------------------------------------------------------------------
      
      addWorksheet(
        wb,
        "STOCK_ACTUEL"
      )
      
      writeData(
        wb,
        "STOCK_ACTUEL",
        stock_data()
      )
      
      
      # -----------------------------------------------------------------------
      # Feuille mouvements
      # -----------------------------------------------------------------------
      
      addWorksheet(
        wb,
        "MOUVEMENT"
      )
      
      writeData(
        wb,
        "MOUVEMENT",
        raw_data()
      )
      
      
      saveWorkbook(
        wb,
        file,
        overwrite = TRUE
      )
    }
  )
  
  
  # ===========================================================================
  # 28. EXPORT EXCEL - UNIQUEMENT À ACHETER
  # ===========================================================================
  
  output$xlsx_buy <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "A_ACHETER_",
        Sys.Date(),
        ".xlsx"
      )
    },
    
    content = function(file) {
      
      d <- request_data() %>%
        filter(
          QTE_ACHAT > 0
        )
      
      
      wb <- createWorkbook()
      
      addWorksheet(
        wb,
        "A_ACHETER"
      )
      
      writeData(
        wb,
        "A_ACHETER",
        d
      )
      
      saveWorkbook(
        wb,
        file,
        overwrite = TRUE
      )
    }
  )
  
  
  # ===========================================================================
  # 29. EXPORT PDF
  # ===========================================================================
  
  output$pdf <- downloadHandler(
    
    filename = function() {
      
      paste0(
        "CONTROLE_ACHAT_",
        Sys.Date(),
        ".pdf"
      )
    },
    
    content = function(file) {
      
      d <- request_data()
      
      tmp <- tempfile(
        fileext = ".html"
      )
      
      html <- paste0(
        "<html>",
        "<head>",
        "<meta charset='UTF-8'>",
        "<style>",
        "body{font-family:Arial;}",
        "table{border-collapse:collapse;width:100%;}",
        "th,td{border:1px solid #999;padding:6px;}",
        "th{background:#eeeeee;}",
        "</style>",
        "</head>",
        "<body>",
        
        "<h1>CONTROLE ACHAT MAGASIN</h1>",
        
        "<p>Date : ",
        Sys.Date(),
        "</p>",
        
        knitr::kable(
          d,
          format = "html"
        ),
        
        "</body>",
        "</html>"
      )
      
      writeLines(
        html,
        tmp,
        useBytes = TRUE
      )
      
      if (
        requireNamespace(
          "pagedown",
          quietly = TRUE
        )
      ) {
        
        pagedown::chrome_print(
          input = tmp,
          output = file
        )
        
      } else {
        
        showNotification(
          "Le package 'pagedown' est nécessaire pour exporter le PDF.",
          type = "error",
          duration = 10
        )
      }
    }
  )
}