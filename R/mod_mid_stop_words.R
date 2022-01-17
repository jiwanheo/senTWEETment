#' mid_stop_words module
#'
#' Called from mod_mid, this module is responsible for adding/removing/seeing
#' the list of stop words used for the analysis. Everytime this list is edited,
#' it directly edits the `stop_words` field in R6 right away, without trigger.
#'
#' @rdname mod_mid_stop_words
#' @param id The Module namespace
#' @importFrom shiny moduleServer NS HTML tagList actionButton
#' @importFrom shinydashboard box
mod_mid_stop_words_ui <- function(id){
  ns <- NS(id)

  box(
    width = 6,
    title = "Custom stop words (Optional)",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    collapsed = TRUE,

    tagList(
      tags$p(HTML("Words like <strong>'the', 'of', 'to'</strong> don't hold much meaning, and are excluded.<br>")),
      tags$br(),

      textInput(
        ns("stop_word"),
        "Add/Remove Word",
        value = ""
      ),

      actionButton(
        ns("add_stop_word"),
        "Add word"
      ),
      actionButton(
        ns("remove_stop_word"),
        "Remove word"
      ),
      actionButton(
        ns("see_stop_words"),
        "See list"
      )
    )
  )
}

#' @rdname mod_mid_stop_words
#' @param ta TweetAnalysis object, to hold analysis process in R6.
#' @importFrom shiny moduleServer observeEvent tagList
#' @importFrom shinyalert shinyalert
#' @importFrom DT datatable renderDT
#' @importFrom tibble tibble
#'
mod_mid_stop_words_server <- function(id, ta){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$add_stop_word, {
      if(input$stop_word != "") {

        tryCatch_alert(
          ta$add_stop_word(input$stop_word),
          input_id = "stop_word",
          type = "add"
        )
      }
    })

    observeEvent(input$remove_stop_word, {
      if(input$stop_word != "") {

        tryCatch_alert(
          ta$remove_stop_word(input$stop_word),
          input_id = "stop_word",
          type = "remove"
        )
      }
    })

    observeEvent(input$see_stop_words, {
      shinyalert(
        title = "List of stop words",
        inputId = "shinyalert_see_stop_word1",
        closeOnClickOutside = TRUE,
        html = TRUE,
        text = tagList(
          renderDT(
            datatable(tibble(word = ta$stop_words$word),
                      class = "hover row-border",
                      options = list(pageLength = 5,
                                     scrollX = TRUE)
            )
          )
        )
      )
    })

  })
}

## To be copied in the UI
# mod_mid_stop_words_ui("mid_stop_words_1")

## To be copied in the server
# mod_mid_stop_words_server("mid_stop_words_1")
