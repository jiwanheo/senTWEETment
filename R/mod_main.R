#' main UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#' @export
#'
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

#' main Server Functions
#'
#' @noRd
#' @export
mod_main_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    mod_header_server("header_1")
    mod_top_server("top_1")
    mod_mid_server("mid_1")
    mod_bot_server("bot_1")
  })
}

## To be copied in the UI
# mod_mod_main_ui("mod_main_1")

## To be copied in the server
# mod_mod_main_server("mod_main_1")
