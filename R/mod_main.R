#' Main module
#'
#' @description This module calls everything.

#' @param id The Module namespace
#' @rdname mod_main
#' @importFrom shiny NS tagList
mod_main_ui <- function(id){
  ns <- NS(id)
  tagList(
    tags$div(
      class = "container",
      tags$div(
        class = "row",
        mod_header_ui(ns("header_1"))
      ),
      tags$div(
        class = "row",
        mod_top_ui(ns("top_1"))
      ),
      tags$div(
        class = "row",
        mod_mid_ui(ns("mid_1"))
      ),
      tags$div(
        class = "row",
        mod_bot_ui(ns("bot_1"))
      )
    )
  )
}

#' @rdname mod_main
#' @importFrom shiny moduleServer
#' @importFrom rtweet auth_as
mod_main_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    lexicons <- process_lexicons()

    ta <- TweetAnalysis$new(stop_words = stop_words,
                            lexicons = lexicons)
    init("analyze-tweets")


    # Try to authenticate with rds. If fails, launch ask user for token.
    tryCatch({
      auth_as("my-twitter-app")
    }, error = function(e) {
      mod_creds_modal_server("creds_modal_1")
    })


    mod_header_server("header_1")
    mod_top_server("top_1", ta)
    mod_mid_server("mid_1", ta)
    mod_bot_server("bot_1", ta)

  })
}

## To be copied in the UI
# mod_mod_main_ui("mod_main_1")

## To be copied in the server
# mod_mod_main_server("mod_main_1")
