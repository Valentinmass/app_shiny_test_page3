# =============================================================================
# SCRIPT DE TEST - PAGE 1 (Liste) → PAGE 2 (Détails)
# =============================================================================

library(shiny)
library(httr)
library(jsonlite)
library(shinycssloaders)
library(DT)

# Charger la clé API
readRenviron(".Renviron")
api_key <- Sys.getenv("TMDB_API_KEY")

# =============================================================================
# FONCTIONS API TMDB
# =============================================================================

get_tmdb_id <- function(imdb_id, api_key) {
  url <- paste0(
    "https://api.themoviedb.org/3/find/",
    imdb_id,
    "?api_key=", api_key,
    "&external_source=imdb_id"
  )
  res <- fromJSON(content(GET(url), "text"))
  if (length(res$movie_results) > 0) {
    res$movie_results$id[1]
  } else {
    NA
  }
}

get_movie_details <- function(tmdb_id, api_key) {
  url <- paste0(
    "https://api.themoviedb.org/3/movie/",
    tmdb_id,
    "?api_key=", api_key,
    "&language=fr-FR"
  )
  fromJSON(content(GET(url), "text"))
}

get_providers <- function(tmdb_id, api_key, country = "FR") {
  url <- paste0(
    "https://api.themoviedb.org/3/movie/",
    tmdb_id,
    "/watch/providers?api_key=", api_key
  )
  res <- fromJSON(content(GET(url), "text"))
  
  if (!is.null(res$results[[country]]$flatrate)) {
    paste(res$results[[country]]$flatrate$provider_name, collapse = ", ")
  } else {
    "Non disponible en streaming"
  }
}

# =============================================================================
# DONNÉES DE TEST
# =============================================================================

# Créer une liste de films de test
films_test <- data.frame(
  tconst = c("tt0133093", "tt0111161", "tt0068646", "tt0468569", "tt0120737", 
             "tt0167260", "tt0110912", "tt0109830", "tt1375666", "tt0816692"),
  title = c("The Matrix", "The Shawshank Redemption", "The Godfather", 
            "The Dark Knight", "The Lord of the Rings: The Fellowship of the Ring",
            "The Lord of the Rings: The Return of the King", "Pulp Fiction",
            "Forrest Gump", "Inception", "Interstellar"),
  year = c(1999, 1994, 1972, 2008, 2001, 2003, 1994, 1994, 2010, 2014),
  runtime = c(136, 142, 175, 152, 178, 201, 154, 142, 148, 169),
  genres = c("Action, Sci-Fi", "Drama", "Crime, Drama", "Action, Crime, Drama",
             "Adventure, Drama, Fantasy", "Adventure, Drama, Fantasy", 
             "Crime, Drama", "Drama, Romance", "Action, Sci-Fi, Thriller",
             "Adventure, Drama, Sci-Fi"),
  rating = c(8.7, 9.3, 9.2, 9.0, 8.9, 9.0, 8.9, 8.8, 8.8, 8.7),
  stringsAsFactors = FALSE
)

# =============================================================================
# INTERFACE UTILISATEUR
# =============================================================================

ui <- fluidPage(
  
  tags$head(
    tags$style(HTML("
      body {
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        min-height: 100vh;
      }
      
      .main-container {
        background: white;
        border-radius: 15px;
        padding: 30px;
        margin: 20px auto;
        max-width: 1400px;
        box-shadow: 0 10px 30px rgba(0,0,0,0.3);
      }
      
      .page-title {
        text-align: center;
        color: #667eea;
        font-size: 48px;
        margin-bottom: 30px;
        text-shadow: 2px 2px 4px rgba(0,0,0,0.1);
      }
      
      /* Style pour les titres cliquables */
      .dataTables_wrapper a {
        color: #667eea;
        font-weight: bold;
        text-decoration: none;
        cursor: pointer;
        transition: all 0.3s ease;
      }
      
      .dataTables_wrapper a:hover {
        color: #764ba2;
        text-decoration: underline;
        transform: translateX(3px);
      }
      
      /* Overlay pour la page 2 */
      .movie-details-overlay {
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        background: rgba(0,0,0,0.85);
        z-index: 9998;
        display: flex;
        align-items: center;
        justify-content: center;
        animation: fadeIn 0.3s ease-out;
      }
      
      @keyframes fadeIn {
        from { opacity: 0; }
        to { opacity: 1; }
      }
      
      .movie-details-container {
        width: 90%;
        max-width: 1200px;
        max-height: 90vh;
        overflow-y: auto;
        background: white;
        border-radius: 15px;
        box-shadow: 0 20px 60px rgba(0,0,0,0.5);
        animation: slideIn 0.3s ease-out;
      }
      
      @keyframes slideIn {
        from {
          opacity: 0;
          transform: translateY(-50px);
        }
        to {
          opacity: 1;
          transform: translateY(0);
        }
      }
      
      .movie-details-header {
        padding: 20px 30px;
        border-bottom: 2px solid #667eea;
        display: flex;
        justify-content: space-between;
        align-items: center;
        background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
        border-radius: 15px 15px 0 0;
      }
      
      .movie-details-content {
        padding: 30px;
      }
      
      .close-btn {
        background: rgba(255,255,255,0.2);
        border: 2px solid white;
        color: white;
        padding: 10px 25px;
        border-radius: 25px;
        cursor: pointer;
        font-weight: bold;
        transition: all 0.3s;
        font-size: 16px;
      }
      
      .close-btn:hover {
        background: white;
        color: #667eea;
        transform: scale(1.05);
      }
    "))
  ),
  
  # ============================================
  # PAGE 1 : LISTE DES FILMS
  # ============================================
  
  div(class = "main-container",
      h1(class = "page-title", "🎬 Films Populaires"),
      p(style = "text-align: center; font-size: 18px; color: #666; margin-bottom: 30px;",
        "Cliquez sur un titre pour voir ses détails complets"),
      
      DTOutput("films_table")
  ),
  
  # ============================================
  # PAGE 2 : DÉTAILS DU FILM (overlay)
  # ============================================
  
  conditionalPanel(
    condition = "output.show_details",
    
    div(class = "movie-details-overlay",
        onclick = "if (event.target === this) { Shiny.setInputValue('close_details', Math.random(), {priority: 'event'}); }",
        
        div(class = "movie-details-container",
            # Header
            div(class = "movie-details-header",
                h3(style = "margin: 0; color: white;", 
                   icon("film"), " Détails du film"),
                tags$button(
                  class = "close-btn",
                  onclick = "Shiny.setInputValue('close_details', Math.random(), {priority: 'event'});",
                  "✕ Fermer"
                )
            ),
            
            # Contenu
            div(class = "movie-details-content",
                uiOutput("movie_details_ui") %>% withSpinner(color = "#667eea")
            )
        )
    )
  )
)

# =============================================================================
# LOGIQUE SERVEUR
# =============================================================================

server <- function(input, output, session) {
  
  # Variables réactives
  selected_movie <- reactiveVal(NULL)
  show_details_page <- reactiveVal(FALSE)
  
  # ============================================
  # AFFICHER LE TABLEAU DES FILMS
  # ============================================
  
  output$films_table <- renderDT({
    
    # Créer les titres cliquables
    films_avec_liens <- films_test
    films_avec_liens$row_id <- 1:nrow(films_test)
    films_avec_liens$Titre_cliquable <- sprintf(
      '<a href="#" onclick="Shiny.setInputValue(\'view_details\', %d, {priority: \'event\'}); return false;">%s</a>',
      films_avec_liens$row_id,
      films_avec_liens$title
    )
    
    # Préparer les données à afficher
    display_data <- films_avec_liens[, c("Titre_cliquable", "year", "runtime", "genres", "rating")]
    colnames(display_data) <- c("Titre", "Année", "Durée (min)", "Genres", "Note IMDB")
    
    datatable(
      display_data,
      escape = FALSE,  # Important pour afficher le HTML
      rownames = FALSE,
      options = list(
        pageLength = 10,
        lengthMenu = c(5, 10, 20),
        dom = 'lftip',
        language = list(
          search = "Rechercher :",
          lengthMenu = "Afficher _MENU_ films",
          info = "Films _START_ à _END_ sur _TOTAL_",
          paginate = list(previous = "Précédent", `next` = "Suivant"),
          zeroRecords = "Aucun film trouvé"
        )
      )
    ) %>%
      formatStyle(
        'Note IMDB',
        background = styleColorBar(range(films_test$rating), '#90EE90'),
        backgroundSize = '100% 90%',
        backgroundRepeat = 'no-repeat',
        backgroundPosition = 'center'
      )
  })
  
  # ============================================
  # GESTION DU CLIC SUR UN TITRE
  # ============================================
  
  observeEvent(input$view_details, {
    req(input$view_details)
    
    # Récupérer le film correspondant
    row_index <- input$view_details
    film <- films_test[row_index, ]
    
    # Stocker le film sélectionné
    selected_movie(film)
    
    # Afficher la page 2
    show_details_page(TRUE)
  })
  
  # ============================================
  # FERMER LA PAGE 2
  # ============================================
  
  observeEvent(input$close_details, {
    show_details_page(FALSE)
    selected_movie(NULL)
  })
  
  # Indicateur pour afficher/masquer la page 2
  output$show_details <- reactive({
    show_details_page()
  })
  outputOptions(output, "show_details", suspendWhenHidden = FALSE)
  
  # ============================================
  # AFFICHER LES DÉTAILS DU FILM
  # ============================================
  
  output$movie_details_ui <- renderUI({
    req(selected_movie())
    
    film <- selected_movie()
    imdb_id <- film$tconst
    
    # Afficher un message pendant le chargement
    withProgress(message = 'Chargement des détails...', value = 0, {
      
      incProgress(0.3, detail = "Connexion à TMDB...")
      
      # Récupérer l'ID TMDB
      tmdb_id <- tryCatch({
        get_tmdb_id(imdb_id, api_key)
      }, error = function(e) {
        NA
      })
      
      if (is.na(tmdb_id)) {
        return(
          div(style = "background: #f5f5f5; border-radius: 10px; padding: 30px; text-align: center;",
              h3(icon("exclamation-triangle"), " Informations limitées", 
                 style = "color: #f39c12;"),
              p("Ce film n'a pas été trouvé dans la base TMDB."),
              hr(),
              div(style = "text-align: left; background: white; padding: 20px; border-radius: 8px;",
                  h4(style = "color: #667eea;", film$title),
                  p(strong("Année : "), film$year),
                  p(strong("Durée : "), film$runtime, " min"),
                  p(strong("Genres : "), film$genres),
                  p(strong("Note IMDb : "), film$rating, "/10")
              )
          )
        )
      }
      
      incProgress(0.5, detail = "Récupération des détails...")
      
      # Récupérer les détails TMDB
      details <- tryCatch({
        get_movie_details(tmdb_id, api_key)
      }, error = function(e) {
        NULL
      })
      
      incProgress(0.7, detail = "Récupération des plateformes...")
      
      # Récupérer les plateformes
      providers <- tryCatch({
        get_providers(tmdb_id, api_key, "FR")
      }, error = function(e) {
        "Non disponible"
      })
      
      incProgress(1, detail = "Terminé !")
      
      if (is.null(details)) {
        return(
          div(style = "background: #f5f5f5; border-radius: 10px; padding: 30px; text-align: center;",
              h3(icon("times-circle"), " Erreur", style = "color: #e74c3c;"),
              p("Impossible de récupérer les détails du film depuis TMDB.")
          )
        )
      }
      
      # URL de l'affiche
      poster_url <- if (!is.null(details$poster_path)) {
        paste0("https://image.tmdb.org/t/p/w500", details$poster_path)
      } else {
        "https://via.placeholder.com/300x450?text=Pas+d%27affiche"
      }
      
      # Genres
      genres_tags <- if (!is.null(details$genres) && nrow(details$genres) > 0) {
        lapply(details$genres$name, function(g) {
          span(style = "display: inline-block; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                        color: white; padding: 6px 14px; border-radius: 20px; margin: 5px;
                        font-size: 0.9em; box-shadow: 0 2px 5px rgba(0,0,0,0.2);", g)
        })
      } else {
        span(style = "display: inline-block; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%);
                      color: white; padding: 6px 14px; border-radius: 20px; margin: 5px;
                      font-size: 0.9em;", film$genres)
      }
      
      # Note
      rating <- if (!is.null(details$vote_average)) {
        round(details$vote_average, 1)
      } else {
        film$rating
      }
      
      # Construction de la fiche film
      div(
        # Header avec affiche et infos
        div(style = "display: flex; gap: 30px; margin-bottom: 30px; flex-wrap: wrap;",
            # Affiche
            div(style = "flex-shrink: 0;",
                img(src = poster_url, 
                    style = "width: 300px; max-width: 100%; border-radius: 12px; 
                         box-shadow: 0 8px 20px rgba(0,0,0,0.3);")
            ),
            # Informations
            div(style = "flex-grow: 1; min-width: 300px;",
                h2(style = "margin-top: 0; color: #667eea; font-size: 2em;", 
                   details$title),
                
                # Métadonnées
                div(style = "font-size: 1.1em; color: #666; margin-bottom: 20px;",
                    span(style = "display: inline-block; background: linear-gradient(135deg, #667eea 0%, #764ba2 100%); 
                           color: white; padding: 8px 16px; border-radius: 25px; 
                           margin-right: 10px; font-weight: bold;
                           box-shadow: 0 3px 10px rgba(102,126,234,0.4);",
                         icon("star"), " ", rating, "/10"),
                    span(style = "margin-right: 10px;", icon("calendar"), " ", film$year),
                    span(icon("clock"), " ", film$runtime, " min")
                ),
                
                # Genres
                div(style = "margin-top: 20px;",
                    h4(style = "color: #667eea; margin-bottom: 10px;", 
                       icon("tags"), " Genres"),
                    div(genres_tags)
                ),
                
                # Plateformes
                div(style = "margin-top: 20px; background: linear-gradient(135deg, rgba(102,126,234,0.1) 0%, rgba(118,75,162,0.1) 100%);
                         padding: 15px; border-radius: 10px; border-left: 4px solid #667eea;",
                    h4(style = "color: #667eea; margin-top: 0;", 
                       icon("tv"), " Disponibilité"),
                    p(style = "margin: 0; font-size: 1.05em;", providers)
                )
            )
        ),
        
        # Synopsis
        if (!is.null(details$overview) && details$overview != "") {
          div(style = "margin-top: 30px;",
              h3(style = "color: #667eea; border-bottom: 3px solid #667eea; 
                        padding-bottom: 10px; display: flex; align-items: center;",
                 icon("book-open"), 
                 span(style = "margin-left: 10px;", "Synopsis")),
              p(style = "font-size: 1.1em; line-height: 1.8; text-align: justify; 
                       color: #333; margin-top: 15px;",
                details$overview)
          )
        },
        
        # Tagline
        if (!is.null(details$tagline) && details$tagline != "") {
          div(style = "margin-top: 20px; padding: 15px; background: #f9f9f9; 
                       border-left: 4px solid #764ba2; border-radius: 5px;",
              p(style = "margin: 0; font-style: italic; color: #666; font-size: 1.05em;",
                icon("quote-left"), " ", details$tagline, " ", icon("quote-right"))
          )
        },
        
        # Budget & Revenue (si disponibles)
        if (!is.null(details$budget) && details$budget > 0) {
          div(style = "margin-top: 20px; display: flex; gap: 20px;",
              div(style = "flex: 1; background: #e8f5e9; padding: 15px; border-radius: 8px;",
                  strong("Budget : "), 
                  format(details$budget, big.mark = " "), " $"
              ),
              if (!is.null(details$revenue) && details$revenue > 0) {
                div(style = "flex: 1; background: #e3f2fd; padding: 15px; border-radius: 8px;",
                    strong("Revenus : "), 
                    format(details$revenue, big.mark = " "), " $"
                )
              }
          )
        }
      )
    })
  })
}

# =============================================================================
# LANCEMENT DE L'APPLICATION
# =============================================================================

shinyApp(ui = ui, server = server)