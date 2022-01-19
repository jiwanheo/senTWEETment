#' mid_negation_words module
#'
#' Called from mod_mid, this module is responsible for adding/removing/seeing
#' the list of negation words used for the analysis. Everytime this list is
#' edited, it directly edits the `negation_words` field in R6 right away,
#' without trigger. Further details on the process can be found in
#' `bigram_adjustment()`
#'
#' @rdname mod_mid_negation_words
#' @param id The Module namespace
#' @importFrom shiny moduleServer NS HTML tagList radioButtons textInput actionButton
#' @seealso [bigram_adjustment()]
mod_mid_negation_words_ui <- function(id){
  ns <- NS(id)

  box(
    width = 6,
    title = "Negation adjustment",
    status = "primary",
    solidHeader = TRUE,
    collapsible = TRUE,
    collapsed = TRUE,

    tagList(
      tags$p(HTML("Any word that comes right after a negation word gets its sentiment reversed. <br>('I am <strong>not</strong> happy', 'Weather is <strong>never</strong> bad here')")),
      tags$br(),

      radioButtons(
        ns("adjust_negation"),
        "Adjust?",
        choices = c("Yes", "No"),
        selected = "Yes",
        inline = TRUE
      ),

      textInput(
        ns("negation_word"),
        "Add/Remove Word",
        value = ""
      ),

      actionButton(
        ns("add_negation_word"),
        "Add word"
      ),
      actionButton(
        ns("remove_negation_word"),
        "Remove word"
      ),
      actionButton(
        ns("see_negation_words"),
        "See list"
      )
    )
  )
}

#' @rdname mod_mid_negation_words
#' @param ta TweetAnalysis object, to hold analysis process in R6.
#' @importFrom shiny moduleServer observeEvent tagList
#' @importFrom shinyalert shinyalert
#' @importFrom DT datatable renderDT
#' @importFrom tibble tibble
mod_mid_negation_words_server <- function(id, ta){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    observeEvent(input$add_negation_word, {
      if(input$negation_word != "") {

        tryCatch_alert(
          ta$add_negation_word(input$negation_word),
          input_id = "negation_word",
          type = "add"
        )

      }
    })

    observeEvent(input$remove_negation_word, {
      if(input$negation_word != "") {

        tryCatch_alert(
          ta$remove_negation_word(input$negation_word),
          input_id = "negation_word",
          type = "remove"
        )

      }
    })

    observeEvent(input$see_negation_words, {
      shinyalert(
        title = "List of negation words",
        inputId = "shinyalert_see_negation_word1",
        closeOnClickOutside = TRUE,
        html = TRUE,
        text = tagList(
          renderDT(
            datatable(tibble(word = ta$negation_words),
                      class = "hover row-border",
                      options = list(pageLength = 5,
                                     searching = FALSE,
                                     lengthChange = FALSE)
            )
          )
        )
      )
    })

    # Disable adjustment options, if "No" for adjust.
    observeEvent(input$adjust_negation, {
      if(input$adjust_negation == "No") {
        disable("negation_word")
        disable("add_negation_word")
        disable("remove_negation_word")
        disable("see_negation_words")
      } else {
        enable("negation_word")
        enable("add_negation_word")
        enable("remove_negation_word")
        enable("see_negation_words")
      }
    })

  })
}

## To be copied in the UI
# mod_mid_negation_words_ui("mid_negation_words_1")

## To be copied in the server
# mod_mid_negation_words_server("mid_negation_words_1")
