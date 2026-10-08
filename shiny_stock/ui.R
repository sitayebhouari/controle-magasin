# =============================================================================
# ui.R  -  Controle Stock Magasin Central
# =============================================================================

ui <- dashboardPage(

  dashboardHeader(title = "Controle Stock Magasin"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Chargement",        tabName = "upload",  icon = icon("upload")),
      menuItem("Recherche produit", tabName = "search",  icon = icon("search")),
      menuItem("Stock actuel",      tabName = "stock",   icon = icon("boxes")),
      menuItem("Demandes d'achat",  tabName = "request", icon = icon("clipboard-list"))
    )
  ),

  dashboardBody(

    tags$head(tags$link(rel = "stylesheet", href = "style.css")),

    tabItems(

      # ---- 1. CHARGEMENT ----------------------------------------------------
      tabItem(tabName = "upload",
        box(width = 12, title = "Charger le fichier Excel",
            status = "primary", solidHeader = TRUE,
            p("Feuille attendue : ", strong("mouvment"),
              " (le titre au-dessus de l'en-tete est ignore automatiquement)."),
            p("Stock = somme des quantites signees (entrees +, sorties -)."),
            fileInput("file", "Choisir le fichier .xlsx", accept = c(".xlsx", ".xls")),
            hr(),
            h4("Etat du fichier"),
            verbatimTextOutput("health"))
      ),

      # ---- 2. RECHERCHE -----------------------------------------------------
      tabItem(tabName = "search",
        box(width = 12, title = "Recherche produit",
            status = "primary", solidHeader = TRUE,
            selectizeInput("prod", "Reference produit", choices = NULL,
                           multiple = FALSE, width = "100%",
                           options = list(placeholder = "Tapez ou selectionnez une reference...")),
            uiOutput("badge"), br(),
            fluidRow(
              valueBoxOutput("v_init", width = 3),
              valueBoxOutput("v_in",   width = 3),
              valueBoxOutput("v_out",  width = 3),
              valueBoxOutput("v_act",  width = 3)
            ),
            hr(), h4("Fiche produit"),
            verbatimTextOutput("prod_info"),
            hr(), h4("Derniers mouvements"),
            DTOutput("last_mvt"))
      ),

      # ---- 3. STOCK ACTUEL --------------------------------------------------
      tabItem(tabName = "stock",
        box(width = 12, title = "Stock actuel",
            status = "success", solidHeader = TRUE,
            fluidRow(
              column(4, selectInput("f_cat", "Categorie", choices = "Toutes", width = "100%")),
              column(4, selectInput("f_etat", "Etat",
                                    choices = c("Tous", "STOCK DISPONIBLE", "RUPTURE", "STOCK NEGATIF"),
                                    width = "100%")),
              column(4, br(), downloadButton("xlsx_stock", "Exporter le stock", icon = icon("file-excel")))
            ),
            DTOutput("stock_table"))
      ),

      # ---- 4. DEMANDES D'ACHAT ---------------------------------------------
      tabItem(tabName = "request",
        box(width = 12, title = "Demandes d'achat",
            status = "primary", solidHeader = TRUE,
            fluidRow(
              column(8, div(class = "cell-produit",
                selectizeInput("req_prod",
                  "Rechercher un produit (tapez quelques lettres)",
                  choices = NULL, multiple = TRUE, width = "100%",
                  options = list(
                    placeholder = "Tapez des lettres ou des mots, puis choisissez...",
                    maxOptions = 100,
                    plugins = list("remove_button"))))),
              column(4, div(class = "cell-qte",
                textAreaInput("txt_qte", "QTE DEMANDEE", rows = 10, width = "100%",
                  placeholder = paste("Une quantite par ligne,", "dans le meme ordre que les produits choisis", "",
                                      "Exemple :", "50000", "1000", "200", sep = "\n"))))
            ),
            br(),
            actionButton("check", "Verifier la demande", class = "btn-primary", icon = icon("check")),
            downloadButton("xlsx", "Exporter Excel", icon = icon("file-excel")),
            downloadButton("xlsx_buy", "Exporter a acheter", icon = icon("shopping-cart"))
        ),

        fluidRow(
          valueBoxOutput("vb_buy",  width = 3),
          valueBoxOutput("vb_not",  width = 3),
          valueBoxOutput("vb_rup",  width = 3),
          valueBoxOutput("vb_part", width = 3)
        ),

        box(width = 12, title = "Resultat du controle",
            status = "success", solidHeader = TRUE,
            h4(textOutput("total_cost")),
            DTOutput("tbl"))
      )
    )
  )
)
