#' Mid module
#'
#' A shiny Module that contains the second step of the app,
#' namely the 2 customizable aspects of the sentiment analysis. Firstly,
#' Negation adjustment carries out the bi-gram adjustment to the original
#' uni-gram analysis. Users can provide their own negation words. Secondly,
#' users can specify their own filler words - words that are irrelevant to the
#' sentiment of a sentence ('the', 'of'). For both these options, users can
#' check if a word they'd like to submit already exists in the analysis by
#' clicking "see list" button.
#'

#' @param id The Module namespace
#' @rdname mod_mid
#' @importFrom shiny NS tagList HTML textInput actionButton radioButtons
#' @importFrom shinydashboard box
mod_mid_ui <- function(id){
  ns <- NS(id)
  tagList(
    box(
      width = 12,
      title = "Step 2: Customize Analysis",
      status = "primary",
      solidHeader = TRUE,
      collapsible = TRUE,
      collapsed = FALSE,

      col_12(
        tags$p(HTML("The analysis in this app follows the one-token-per-row method, suggested in <a href='https://www.tidytextmining.com/index.html'>Text Mining with R</a>.
                    A tweet is broken up into tokens, a meaningful unit of texts (1 or 2 words for us). Then, each token is matched with a sentiment score, by looking it up on
                    a word-score dictionary. We'll use three (AFINN, bing, nrc). At the end, all scores are added, to determine the overall sentiment of a tweet."))
      ),

      box(
        width = 6,
        title = "Negation adjustment",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,

        tags$p(HTML("With no adjustment, the sentence 'I am <strong>not</strong> happy' will return positive sentiment. With adjustment, every 2 consecutive words are analyzed. If negation words come first, the sentiment flips.")),
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
          "Word",
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
      ),

      box(
        width = 6,
        title = "Add custom filler words (Optional)",
        status = "primary",
        solidHeader = TRUE,
        collapsible = TRUE,
        collapsed = TRUE,

        tags$p(HTML("Words like <strong>'the', 'of', 'to'</strong> don't hold much sentiment, and are excluded. Check if your favourite filler word is excluded, and add them.")),
        tags$br(),

        textInput(
          ns("filler_word"),
          "Word",
          value = ""
        ),

        actionButton(
          ns("add_filler_word"),
          "Add word"
        ),
        actionButton(
          ns("remove_filler_word"),
          "Remove word"
        ),
        actionButton(
          ns("see_filler_words"),
          "See list"
        )
      ),

      col_12(
        class = "text-center",
        actionButton(ns("analyze"),"Analyze!")
      )
    )
  )
}

#' @rdname mod_mid
#' @param ta TweetAnalysis object, to hold analysis process in R6.
#' @importFrom shiny moduleServer reactiveVal updateTextInput tagList
#' @importFrom tidytext get_sentiments
#' @importFrom dplyr transmute filter
#' @importFrom shinyalert shinyalert
#' @importFrom DT renderDT datatable
#' @importFrom shinyjs disable enable
mod_mid_server <- function(id, ta){
  moduleServer( id, function(input, output, session){
    ns <- session$ns




    # produce_analysis_df(tweets, lexicons)


    # Negation Words----------------------------------------------------------------

    observeEvent(input$add_negation_word, {
      if(input$negation_word != "") {

        tryCatch({
          ta$add_negation_word(input$negation_word)
          shinyalert(
            title = "Negation word added!",
            type = "success",
            inputId = "shinyalert_add_negation_word1"
          )
        },
        warning = function(w) {
          shinyalert(
            title = w$message,
            type = "error",
            inputId = "shinyalert_add_negation_word1"
          )
        },
        finally = {
          updateTextInput(inputId = "negation_word",
                          value = "")
        })
      }
    })

    observeEvent(input$remove_negation_word, {
      if(input$negation_word != "") {

        tryCatch({
          ta$remove_negation_word(input$negation_word)
          shinyalert(
            title = "Negation word removed!",
            type = "success",
            inputId = "shinyalert_remove_negation_word1"
          )
        },
        warning = function(w) {
          shinyalert(
            title = w$message,
            type = "error",
            inputId = "shinyalert_remove_negation_word1"
          )
        },
        finally = {
          updateTextInput(inputId = "negation_word",
                          value = "")
        })
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

    # Filler Words--------------------------------------------------------------------

    observeEvent(input$add_filler_word, {

      if(input$filler_word != "") {

        tryCatch({
          ta$add_filler_word(input$filler_word)
          shinyalert(
            title = "Filler word added!",
            type = "success",
            inputId = "shinyalert_add_filler_word1"
          )
        },
        warning = function(w) {
          shinyalert(
            title = w$message,
            type = "error",
            inputId = "shinyalert_add_filler_word1"
          )
        },
        finally = {
          updateTextInput(inputId = "filler_word",
                          value = "")
        })
      }
    })

    observeEvent(input$remove_filler_word, {
      if(input$filler_word != "") {

        tryCatch({
          ta$remove_filler_word(input$filler_word)
          shinyalert(
            title = "Filler word removed!",
            type = "success",
            inputId = "shinyalert_remove_filler_word1"
          )
        },
        warning = function(w) {
          shinyalert(
            title = w$message,
            type = "error",
            inputId = "shinyalert_remove_filler_word1"
          )
        },
        finally = {
          updateTextInput(inputId = "filler_word",
                          value = "")
        })
      }
    })

    observeEvent(input$see_filler_words, {
      shinyalert(
        title = "List of filler words",
        inputId = "shinyalert_see_filler_word1",
        closeOnClickOutside = TRUE,
        html = TRUE,
        text = tagList(
          renderDT(
            datatable(tibble(word = ta$filler_words$word),
                      class = "hover row-border",
                      options = list(pageLength = 5,
                                     scrollX = TRUE)
            )
          )
        )
      )
    })

    # Analyze-------------------------------------------------------------------

    observeEvent(input$analyze, {
      # Do the analysis, with the R6 method, and assign it to a field in R6
      ta$analysis_result <- ta$analyze()

      # Should probably trigger something here, so the next step can respond.

    })



  })
}

## To be copied in the UI
# mod_mid_ui("mid_1")

## To be copied in the server
# mod_mid_server("mid_1")
