#' mid_lexicons module
#'
#' Called from mod_mid, this module is responsible for retrieving the Lexicons
#' to be used for sentiment analysis. Everytime the lexicon is changed,
#' it directly edits the `lexicons` field in R6 right away, without trigger.
#'
#' Currently it supports 3 lexicons + user-uploaded csv file
#' \itemize{
#'   \item{AFINN}
#'   \item{Bing}
#'   \item{nrc}
#'   \item{User-uploaded csv:} {a dataframe with column names "word" & "value"}
#' }
#'
#' @rdname mod_mid_lexicons
#' @param id The Module namespace
#'
#' @importFrom shiny moduleServer NS HTML tagList radioButtons fileInput
#' @importFrom DT DTOutput renderDT datatable
#' @importFrom shinyjs disabled enable disable
mod_mid_lexicons_ui <- function(id){
  ns <- NS(id)

  tagList(
    col_12(
      class = "lexicon-section",

      col_12(
        col_6(
          tags$h3("Choose Lexicon (Unigram)"),
          radioButtons(
            ns("lexicon"),
            "",
            choices = c("AFINN", "Bing", "NRC", "Upload my own"),
            selected = "AFINN"
          ),

          disabled(
            fileInput(ns("file_upload"),
                      "CSV with 'word' and 'value' as column names",
                      multiple = FALSE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv"))
          )
        ),
        col_6(
          tags$h3("Preview"),
          DTOutput(ns("lexicon_preview"))
        )
      )
    )
  )
}

#' @rdname mod_mid_lexicons
#' @param ta TweetAnalysis object, to hold analysis process in R6.
#' @importFrom shiny moduleServer reactiveVal observeEvent updateRadioButtons
#' @importFrom DT datatable renderDT
#' @importFrom shinyjs enable disable
#' @importFrom shinyalert shinyalert
#' @importFrom readr read_csv
mod_mid_lexicons_server <- function(id, ta){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

    init("preview")

    lexicon_list <- process_lexicons()

    observeEvent(input$lexicon, {
      if(input$lexicon == "Upload my own") {
        enable("file_upload")
      }
      else {
        disable("file_upload")
        lexicons <- lexicon_list[[tolower(input$lexicon)]]
        ta$lexicons <- lexicons
        trigger("preview")
      }
    })

    observeEvent(input$file_upload, {
      tryCatch({
        req(!is.null(input$file_upload))
        lexicons <- read_csv(input$file_upload$datapath)

        # If custom files have wrong names, reset.
        if(names(lexicons) != c("word", "value")) {
          updateRadioButtons(inputId = "lexicon", selected = "AFINN")
          stop("Column names must be 'word' & 'value'")
        }

        ta$lexicons <- lexicons
        trigger("preview")
      },
      error = function(e) {
        shinyalert(
          e$message,
          type = "error",
          inputId = "shinyalert_failed_1"
        )
      })
    })

    output$lexicon_preview <- renderDT({

      watch("preview")
      datatable(ta$lexicons,
                class = "hover row-border",
                options = list(pageLength = 5,
                               scrollX = TRUE)
      )
    })
  })
}

## To be copied in the UI
# mod_mid_lexicons_ui("mid_lexicons_1")

## To be copied in the server
# mod_mid_lexicons_server("mid_lexicons_1")
