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
#' @importFrom shinyjs disabled
mod_creds_modal_ui <- function(id){
  ns <- NS(id)
  tagList(
    modalDialog(
      tagList(
        h2("Please provide Twitter API keys!"),
        HTML("Enter the creds and save to environment, so you won't have to see this message again! <br>
             Or you can continue to supply these creds every time this app opens. <br>
             **If any of the below is disabled, it means it already exists in your .Renviron!"),

        # API Key inputs are initially disabled.
        # We will receive from mod_main, which inputs we need, and
        # enable them in the server.
        disabled(
          textInput(
            ns("api_key"),
            "API Key:",
            placeholder = "API_KEY"
          )
        ),
        disabled(
          textInput(
            ns("api_key_secret"),
            "API Key Secret:",
            placeholder = "API_KEY_SECRET"
          )
        ),
        disabled(
          textInput(
            ns("access_token"),
            "Access Token:",
            placeholder = "ACCESS_TOKEN"
          )
        ),
        disabled(
          textInput(
            ns("access_token_secret"),
            "Access Token Secret:",
            placeholder = "ACCESS_TOKEN_SECRET"
          )
        ),
        actionButton(
          ns("ahi"),
          "Hi"
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
        )
      )
    )
  )
}

#' creds_modal Server Functions
#'
#' @noRd
#' @importFrom shiny observeEvent
#' @importFrom shinyjs enable
mod_creds_modal_server <- function(id, to_enable){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # For each API key we need to ask the user, enable the UI input.
    lapply(to_enable, function(x) {enable(id = x)})


    # I think I'm having trouble because server doesn't talk to UI
    # after it has been rendered. (using observEvent to enable works.)
    # I think the potential soltion is to watch/trigger.

    observeEvent(input$ahi, {
      enable("access_token_secret")
    })



  })
}

## To be copied in the UI
# mod_creds_modal_ui("creds_modal_1")

## To be copied in the server
# mod_creds_modal_server("creds_modal_1")
