#' Header module
#'
#' A shiny Module that contains the header of the app,
#' namely the title and the image.

#' @rdname mod_header
#' @param id The Module namespace
#' @importFrom shiny NS tagList imageOutput
mod_header_ui <- function(id){
  ns <- NS(id)

  tagList(
    tags$div(
      class = "text-center h1", # This is probably bad. I just want to have bottom margin without clogging up the code.
      tags$h1("senTWEETment"),
      imageOutput(ns("img"), height = "auto")
    )
  )
}

#' @rdname mod_header
#' @importFrom shiny moduleServer renderImage
mod_header_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$img <- renderImage({
      list(src = "inst/app/www/hex.png", height = "200px")
    }, deleteFile = FALSE)
  })
}

## To be copied in the UI
# mod_header_ui("header_1")

## To be copied in the server
# mod_header_server("header_1")
