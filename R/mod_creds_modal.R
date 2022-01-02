#' creds_modal UI Function
#'
#' @description A shiny Module containing the modalBox that asks users for
#' Twitter Dev API credentials.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList modalDialog HTML textInput actionButton modalButton
mod_creds_modal_ui <- function(id){
  ns <- NS(id)
  tagList(
    modalDialog(
      easyClose = TRUE,

      tagList(
        h2("Please provide Twitter API keys!"),
        HTML("You don't have Twitter API keys saved in your environment! <br>
             Enter the creds, and save to environment, so you won't have to see this message again! <br>
             Or you can continue to supply these creds every time this app opens"),
        textInput(
          ns("api_key"),
          "Enter API Key:"
        ),
        textInput(
          ns("api_key_secret"),
          "Enter API Key Secret:",
        ),
        textInput(
          ns("access_token"),
          "Enter Access Token:"
        ),
        textInput(
          ns("access_token_secret"),
          "Enter Access Token Secret:"
        )
      ),

      footer = tagList(
        actionButton(
          ns("save_creds"),
          "Save the creds to R environment"
        ),
        actionButton(
          ns("just_use_creds"),
          "Use the creds, but don't save them"
        ),
        modalButton("Cancel")

      )
    )
  )
}

#' creds_modal Server Functions
#'
#' @noRd
mod_creds_modal_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_creds_modal_ui("creds_modal_1")

## To be copied in the server
# mod_creds_modal_server("creds_modal_1")
