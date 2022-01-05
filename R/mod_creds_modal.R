#' creds_modal Module
#'
#' @description A shiny Module containing the modalBox that asks users for
#' Twitter Dev API credentials.

#' @param id The Module namespace
#' @rdname mod_creds_modal
#' @importFrom shiny NS tagList modalDialog HTML textInput actionButton modalButton
#' @importFrom shinyjs disabled
mod_creds_modal_ui <- function(id){
  ns <- NS(id)
  tagList(
    modalDialog(
      tagList(
        tags$h2("This app requires a Twitter API connection!"),

        HTML('Click <a href="https://cran.r-project.org/web/packages/rtweet/vignettes/auth.html">here</a> for a how-to. <br>
             You don\'t <em>have</em> to save these to your R environment, but it is recommended. <br>
             **If any of the below is disabled, it means it already exists in your .Renviron!'),

        # API Key inputs are initially disabled.
        # We will receive from mod_main, which inputs we need, and
        # enable them in the server.
        disabled(
          textInput(
            ns("app"),
            "App Name:",
            placeholder = "APP_NAME"
          )
        ),
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
        )
      ),

      footer = tagList(
        actionButton(
          ns("save_creds"),
          "Save my API keys to R environment"
        ),
        actionButton(
          ns("just_use_creds"),
          "Use the API keys, but don't save them"
        )
      )
    )
  )
}

#' @rdname mod_creds_modal
#' @param to_enable a character vector of missing API credentials (input id of modal).
#' @importFrom shiny observeEvent removeModal
#' @importFrom shinyjs enable
#' @importFrom shinyalert shinyalert
mod_creds_modal_server <- function(id, to_enable){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    # Observe the "missing_cred" trigger, that is fired off in `mod_main()`
    # If triggered, update the UI (enable some user inputs)
    observeEvent( watch("missing_creds"), {
      lapply(to_enable, function(x) {enable(id = x)})
    })

    # When user hits "save_cred", save the supplied inputs to user-specific
    # .Renviron.
    observeEvent( input$save_creds, {

      renv_loc <- file.path(Sys.getenv("R_USER"), ".Renviron")

      lapply(to_enable, function(x) {
        input_expr <- bquote(input[[.(x)]])
        renv_entry <- paste0('TWITTER_', toupper(x), ' = ', '"', eval(input_expr), '"')
        cat(renv_entry, file = renv_loc, append = TRUE)
      })

      removeModal()
      connect_to_api(creds_list = get_creds())
      shinyalert("Connected!", "Start analyzing tweets!", type = "success")
    })

    # When user hits "save_cred", just pass it along get_creds(), and connect_to_api()
    observeEvent( input$just_use_creds, {

      # Helper to pass in NULL to get_creds, if input is disabled
      input_or_null <- function(key){
        input_expr <- eval(bquote(input[[.(key)]]))

        if(input_expr == "") NULL else input_expr
      }

      # Need to pass in the disabled inputs as NULL
      creds <- get_creds(
        # app = ifelse(input$app == "", NULL, input$app),
        app                 = input_or_null("app"),
        api_key             = input_or_null("api_key"),
        api_key_secret      = input_or_null("api_key_secret"),
        access_token        = input_or_null("access_token"),
        access_token_secret = input_or_null("access_token_secret")
      )

      removeModal()
      connect_to_api(creds_list = creds)
      shinyalert("Connected!", "Start analyzing tweets!", type = "success")
    })

  })
}

## To be copied in the UI
# mod_creds_modal_ui("creds_modal_1")

## To be copied in the server
# mod_creds_modal_server("creds_modal_1")
