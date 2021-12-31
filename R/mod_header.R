#' header UI Function
#' @title   mod_header_ui and mod_header_name_server
#' @description A shiny Module, that contains the header of the app,
#' namely the title and the image.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @export
#'
#' @importFrom shiny NS tagList imageOutput
mod_header_ui <- function(id){
  ns <- NS(id)

  tagList(
    tags$div(
      class = "text-center h1", # This is probably bad. I just want to have bottom margin without clogging up the code.
      h1("senTWEETment"),
      imageOutput(ns("img"), height = "auto")
    )
  )
}

#' header Server Functions
#'
#' @importFrom shiny renderImage
mod_header_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    output$img <- renderImage({
      list(src = "inst/app/www/hex.png", width = "200px", height = "200px")
    }, deleteFile = FALSE)
  })
}

## To be copied in the UI
# mod_header_ui("header_1")

## To be copied in the server
# mod_header_server("header_1")
