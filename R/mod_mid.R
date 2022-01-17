#' Mid module
#'
#' A shiny Module that contains the second step of the app, namely the 3
#' customizable aspects of the sentiment analysis, broken up into their own
#' module. After customization, users can initiate the analysis, which will be
#' saved in ta R6 object. Finally, it triggers the "analyze-tweets" trigger.
#'
#' Child Modules
#' \itemize{
#'   \item{mod_mid_lexicons:} {Choose sentiment lexicons}
#'   \item{mod_mid_negation_words:} {Carries out bigram negation adjustment}
#'   \item{mod_mid_stop_words:} {Add/remove stop words}
#' }
#' @param id The Module namespace
#' @rdname mod_mid
#' @importFrom shiny NS HTML tagList
#' @importFrom shinydashboard box
mod_mid_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(width = 12,
        title = "Step 2: Customize Analysis",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,

        col_12(
          tags$p(HTML("This lexicon-based sentiment analysis follows methods suggested in <a href='https://www.tidytextmining.com/index.html'>Text Mining with R</a>."))
        ),

        mod_mid_lexicons_ui(ns("mid_lexicons_1")),
        mod_mid_negation_words_ui(ns("mid_negation_words_1")),
        mod_mid_stop_words_ui(ns("mid_stop_words_1")),

        col_12(
          class = "text-center",
          actionButton(ns("analyze"),"Analyze!")
        )
    )
  )
}

#' @rdname mod_mid
#' @param ta TweetAnalysis object, to hold analysis process in R6.
#' @importFrom shiny moduleServer observeEvent
#' @importFrom shinyalert shinyalert
#' @importFrom waiter waiter_show spin_fading_circles waiter_hide

mod_mid_server <- function(id, ta){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    mod_mid_lexicons_server("mid_lexicons_1", ta)
    mod_mid_negation_words_server("mid_negation_words_1", ta)
    mod_mid_stop_words_server("mid_stop_words_1", ta)

    observeEvent(input$analyze, {
      # Do the analysis, with the R6 method, and assign it to a field in R6
      tryCatch({
        waiter_show(
          html = tagList(
            spin_fading_circles(),
            "Loading ..."
          ),
          color = "rgba(0, 0, 0, 0.5)"
        )

        ta$analysis_result <- ta$analyze()
        trigger("analyze-tweets")

        waiter_hide()

        shinyalert(
          title = "Analysis Complete!",
          type = "success",
          inputId = "r6_analyze_success"
        )
      },
      error = function(e) {
        shinyalert(
          title = e$message,
          type = "error",
          inputId = "r6_analyze_error"
        )
      })
    })
  })
}

## To be copied in the UI
# mod_mid_ui("mid_1")

## To be copied in the server
# mod_mid_server("mid_1")
