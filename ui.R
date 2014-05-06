library(shiny)

# Define UI for miles per gallon application
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Prix en Belgique"),
  
  sidebarPanel(
    
    checkboxInput(inputId = "supra",
                  label = "Catégories générales",
                  value = TRUE),
    
    checkboxInput(inputId = "category.choice",
                label = "Tous les produits",
                value = TRUE),
    
    uiOutput("subcategories"),
    
    selectInput(inputId = "lags.choice",
                label = "Type de variation des prix",
                choices = c("Annuelle", "Mensuelle"), # "Indices"),
                selected = "Annuelle"),
    
    selectInput(inputId = "window.start.year",
                label = "A partir de",
                choices = c(2006:2014),
                selected = 2013),
    
    selectInput(inputId = "window.start.month",
                label = "",
                choices = c(1:12),
                selected = 2),
    
    selectInput(inputId = "window.end.year",
                label = "Jusqu'à",
                choices = c(2006:2014),
                selected = 2014),
    
    selectInput(inputId = "window.end.month",
                label = "",
                choices = c(1:12),
                selected = 2),
    
    checkboxInput(inputId = "weighted",
                  label = strong("Données pondérées"),
                  value = TRUE),
    
    checkboxInput(inputId = "ipc",
                  label = strong("Indice des prix à la consommation"),
                  value = TRUE),
    
    checkboxInput(inputId = "sante",
                  label = strong("Indice santé"),
                  value = FALSE),
    
    checkboxInput(inputId = "pivot",
                  label = strong("Dépassements de l'indice pivot"),
                  value = FALSE),
    
    p("Les quatre groupe ci-dessus sont des groupes de référence:"),
    
    checkboxInput(inputId = "alimentaires",
                  label = strong("Produits alimentaires"),
                  value = FALSE),
    checkboxInput(inputId = "nonalimentaires",
                  label = strong("Produits non-alimentaires"),
                  value = FALSE),
    checkboxInput(inputId = "services",
                  label = strong("Services"),
                  value = FALSE),
    checkboxInput(inputId = "loyers",
                  label = strong("Loyers"),
                  value = FALSE),
    br(),
    
    p("Le SPF Economie publie mensuellement la valeur de l'indice des prix
      à la consommation. Cet indice est une somme des prix d'un panier de biens,
      pondérés par leur importance au consommateur."),
    
    p("L'inflation est habituellement mesurée par un glissement
       mensuel ou annuel de l'indice des prix. Autrement dit,
       la variation d'un mois à l'autre ou bien le niveau des prix actuel
       par rapport à celui d'il y a un an."),
    
    p("L'indice santé est une autre mesure de l'inflation qui exclut les produits
      dits nuisibles à la santé comme l'alcool, le tabac ou encore les carburants. Le niveau
      de l'indice santé est utilisé pour des fins d'indexation."),
        
    p("Les données utilisées proviennent du ", a("SPF Economie.", href="http://economie.fgov.be/fr/statistiques/chiffres/economie/prix_consommation/")),
    
    p("Pour signaler des erreurs ou des suggestions:", br(), a(href="mailto:rytis.bagdziunas@uclouvain.be", "rytis.bagdziunas@uclouvain.be"), br(), "ou via", a(href="https://github.com/rbagd/inflation", "github."))
    
    ),
  
  mainPanel( 
     tabsetPanel(
       tabPanel("Graphique", plotOutput("testPlot")), 
       tabPanel("Tableau des poids", tableOutput("view")),
       tabPanel("Comprendre les graphes",
		p("L'axe horizontale représente le temps et la largeur d'une barre est normalement un mois. Les données sont mensuelles
		  et disponibles seulement à partir de janvier 2006. Le choix d'une variation annuelle ou mensuelle réduit par conséquent
		  l'échantillon disponible."),
		
		p("L'interprétation de l'axe verticale varie en fonction des options choisies et peut n'avoir même aucun sens
		  dans certains cas."),
		  
		p("Les graphes les plus informatifs représentent l'évolution mensuelle/annuelle des données pondérées auquel cas l'axe
		  verticale peut être comprise comme 'Pourcentage'. Plus particulièrement et pour un mois donné, la hauteur nette de la
		  barre, c'est-à-dire la hauteur de tous éléments positifs moins la hauteur de tous éléments négatifs, approxime la
		  variation en pourcentage ce mois-là des prix de l'ensemble des produits. Chaque sous-élément coloré représente donc
		  la contribution de la sous-catégorie à la variation totale. Par exemple, une hauteur de 3% signifierait que l'inflation
		  ce mois-là aurait été de 3%, dont 1% aurait été contribué par les produits alimentaires."),

		p("Si les données ne sont pas pondérées, alors la hauteur de chaque barre colorée représente la variation des prix du produit
		  respectif. Dans ce cas-là, la hauteur (nette) de la barre totale n'a pas de signification particulière."))
                 )
            )
))

