# =============================================================================
# ui.R
# Controle Achat Magasin
# =============================================================================

ui <- dashboardPage(
  
  # ===========================================================================
  # HEADER
  # ===========================================================================
  
  dashboardHeader(
    title = "Controle Achat Magasin"
  ),
  
  
  # ===========================================================================
  # SIDEBAR
  # ===========================================================================
  
  dashboardSidebar(
    
    sidebarMenu(
      
      menuItem(
        "Chargement",
        tabName = "upload",
        icon = icon("upload")
      ),
      
      menuItem(
        "Recherche produit",
        tabName = "search",
        icon = icon("search")
      ),
      
      menuItem(
        "Stock actuel",
        tabName = "stock",
        icon = icon("boxes")
      ),
      
      menuItem(
        "Demandes d'achat",
        tabName = "request",
        icon = icon("clipboard-list")
      )
    )
  ),
  
  
  # ===========================================================================
  # BODY
  # ===========================================================================
  
  dashboardBody(
    
    tags$head(
      
      tags$link(
        rel = "stylesheet",
        href = "style.css"
      )
    ),
    
    
    tabItems(
      
      # =======================================================================
      # 1. CHARGEMENT
      # =======================================================================
      
      tabItem(
        tabName = "upload",
        
        box(
          width = 12,
          title = "Charger le fichier Excel",
          status = "primary",
          solidHeader = TRUE,
          
          p(
            "Le fichier doit contenir une feuille nommée ",
            strong("MOUVEMENT"),
            "."
          ),
          
          p(
            "Le stock actuel et le contrôle achat sont calculés automatiquement."
          ),
          
          fileInput(
            "file",
            "Choisir le fichier .xlsx",
            accept = c(
              ".xlsx",
              ".xls"
            )
          ),
          
          hr(),
          
          h4("État du fichier"),
          
          verbatimTextOutput(
            "health"
          )
        )
      ),
      
      
      # =======================================================================
      # 2. RECHERCHE PRODUIT
      # =======================================================================
      
      tabItem(
        tabName = "search",
        
        box(
          width = 12,
          title = "Recherche produit",
          status = "primary",
          solidHeader = TRUE,
          
          selectizeInput(
            "prod",
            "Produit / Référence",
            choices = NULL,
            multiple = FALSE,
            options = list(
              placeholder = "Tapez ou sélectionnez un produit..."
            )
          ),
          
          br(),
          
          uiOutput(
            "badge"
          ),
          
          br(),
          
          fluidRow(
            
            valueBoxOutput(
              "v_init",
              width = 3
            ),
            
            valueBoxOutput(
              "v_in",
              width = 3
            ),
            
            valueBoxOutput(
              "v_out",
              width = 3
            ),
            
            valueBoxOutput(
              "v_act",
              width = 3
            )
          ),
          
          
          hr(),
          
          h4("Derniers mouvements"),
          
          DTOutput(
            "last_mvt"
          ),
          
          hr(),
          
          h4("Dernier achat / prix"),
          
          verbatimTextOutput(
            "last_buy"
          )
        )
      ),
      
      
      # =======================================================================
      # 3. STOCK ACTUEL
      # =======================================================================
      
      tabItem(
        tabName = "stock",
        
        box(
          width = 12,
          title = "Stock actuel",
          status = "success",
          solidHeader = TRUE,
          
          p(
            "Cette table est calculée automatiquement à partir de la feuille ",
            strong("MOUVEMENT"),
            "."
          ),
          
          p(
            "Formule : Stock initial + Entrées - Sorties"
          ),
          
          DTOutput(
            "stock_table"
          )
        )
      ),
      
      
      # =======================================================================
      # 4. DEMANDES D'ACHAT
      # =======================================================================
      
      tabItem(
        tabName = "request",
        
        box(
          width = 12,
          title = "Demandes d'achat",
          status = "primary",
          solidHeader = TRUE,
          
          
          fluidRow(
            
            # -----------------------------------------------------------------
            # PRODUITS
            # -----------------------------------------------------------------
            
            column(
              width = 8,
              
              div(
                class = "cell-produit",
                
                selectizeInput(
                  "req_prod",
                  "Rechercher un produit (tapez quelques lettres)",
                  choices = NULL,
                  multiple = TRUE,
                  width = "100%",
                  options = list(
                    placeholder = "Tapez des lettres ou des mots, puis choisissez...",
                    maxOptions = 100,
                    plugins = list("remove_button")
                  )
                )
              )
            ),
            
            
            # -----------------------------------------------------------------
            # QUANTITÉS
            # -----------------------------------------------------------------
            
            column(
              width = 4,
              
              div(
                class = "cell-qte",
                
                textAreaInput(
                  "txt_qte",
                  
                  "QTÉ DEMANDÉE",
                  
                  rows = 10,
                  
                  width = "100%",
                  
                  placeholder =
                    paste(
                      "Une quantité par ligne,",
                      "dans le même ordre que les produits choisis",
                      "",
                      "Exemple :",
                      "10000",
                      "5000",
                      "100",
                      sep = "\n"
                    )
                )
              )
            )
          ),
          
          
          br(),
          
          
          # -------------------------------------------------------------------
          # BOUTONS
          # -------------------------------------------------------------------
          
          actionButton(
            "check",
            "Vérifier la demande",
            class = "btn-primary",
            icon = icon("check")
          ),
          
          downloadButton(
            "xlsx",
            "Exporter Excel",
            icon = icon("file-excel")
          ),
          
          downloadButton(
            "xlsx_buy",
            "Exporter à acheter",
            icon = icon("shopping-cart")
          ),
          
          downloadButton(
            "pdf",
            "Exporter PDF",
            icon = icon("file-pdf")
          )
        ),
        
        
        # =====================================================================
        # KPI
        # =====================================================================
        
        fluidRow(
          
          valueBoxOutput(
            "vb_buy",
            width = 3
          ),
          
          valueBoxOutput(
            "vb_not",
            width = 3
          ),
          
          valueBoxOutput(
            "vb_rup",
            width = 3
          ),
          
          valueBoxOutput(
            "vb_part",
            width = 3
          )
        ),
        
        
        # =====================================================================
        # TABLEAU RÉSULTAT
        # =====================================================================
        
        box(
          width = 12,
          title = "Résultat du contrôle",
          status = "success",
          solidHeader = TRUE,
          
          DTOutput(
            "tbl"
          )
        )
      )
    )
  )
)