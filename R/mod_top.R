#' Top module
#'
#' A shiny Module that contains the first step of the app,
#' namely the various ways to pull in tweets. Users must specifty 1 of 4 ways
#' to filter tweets, and hit "Pull Tweets!" button, which will pull in tweets
#' and display as a table. Once table is loaded, it is saved as the R6 object's
#' `data`field.
#'
#' @param id The Module namespace
#' @rdname mod_top
#' @importFrom shiny NS fluidRow tagList textInput actionButton tableOutput numericInput radioButtons tagAppendAttributes
#' @importFrom shinydashboard box
#' @importFrom DT DTOutput
#' @importFrom magrittr %>%
mod_top_ui <- function(id){
  ns <- NS(id)

  tagList(
    box(
      width = 12,
      title = "Step 1: Pull in Tweets",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = TRUE,

      # col 12 / (6 + 6) / 12 setup
      fluidRow(
        class = "box-content box-content-left",
        col_12(
          tags$p("Use 1 of hashtag, location or user to look up tweets, with the exception of hashtag+location working with each other.")
        ),
        col_6(
          col_12(
            textInput(
              ns("q"),
              "Hashtag or text:",
              value = ""
            )
          ),
          col_12(
            textInput(
              ns("location"),
              "Location:",
              value = ""
            )
          ),
          col_12(
            textInput(
              ns("user"),
              "User:",
              value = ""
            )
          )
        ),
        col_6(
          col_12(
            numericInput(
              ns("n_tweets"),
              "Number of tweets: (not guaranteed)",
              value = 10
            )
          ),
          col_12(
            radioButtons(
              ns("include_rts"),
              "Include Retweets?",
              choices = c("Yes", "No"),
              selected = "No"
            )
          ),
          col_12(
            radioButtons(
              ns("pull_by"),
              "Pull By",
              choices = c("Recent", "Popular", "Mixed"),
              selected = "Recent"
            )
          )
        ),
        col_12(
          actionButton(
            ns("pull_tweets"),
            "Pull Tweets!"
          )
        ) %>% tagAppendAttributes(class = "text-center")
      ),

      fluidRow(
        class = "box-content",
        col_12(
          DTOutput(ns("table"))
        )
      )
    )
  )
}


#' @rdname mod_top
#' @param ta TweetAnalysis object, to hold analysis process in R6.
#' @importFrom shiny moduleServer renderTable observeEvent updateTextInput req
#' @importFrom DT renderDT datatable
#' @importFrom shinyalert shinyalert
#' @importFrom shinyjs disable enable
#' @importFrom waiter waiter_show spin_fading_circles waiter_hide
mod_top_server <- function(id, ta){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$pull_tweets, {

      waiter_show(
        html = tagList(
          spin_fading_circles(),
          "Loading ..."
        ),
        color = "rgba(0, 0, 0, 0.5)"
      )

      tweets <- tryCatch(
        pull_tweets(q = input$q,
                    user = input$user,
                    location = input$location,
                    n = input$n_tweets,
                    type = input$pull_by,
                    include_rts = input$include_rts
        ),
        error = function(e) {
          shinyalert(
            title = e$message,
            type = "error",
            inputId = "shinyalert_error1"
          )
        }
      )

      tweets <- pretty_tweets(tweets)
      ta$data <- tweets

      output$table <- renderDT({
        req(is.data.frame(ta$data))
        datatable(ta$data,
                  class = "hover row-border",
                  escape = FALSE,
                  options = list(scrollX = TRUE,
                                 pageLength = 5))
      })
      waiter_hide()
    })

    observeEvent(input$user, {
      if(input$user != "") {
        disable("include_rts")
        disable("pull_by")
      } else {
        enable("include_rts")
        enable("pull_by")
      }
    })

  })
}

## To be copied in the UI
# mod_top_ui("top_1")

## To be copied in the server
# mod_top_server("top_1")
