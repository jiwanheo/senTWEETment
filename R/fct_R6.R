#' R6 class of tweet analyzing process
#'
#' R6 object to encapsulate the reading/cleaning/export of tweets
#'
#' @importFrom R6 R6Class
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom rlang .data
#' @importFrom shinyalert shinyalert
#'
#' @section Public fields:
#' * `data`: Tweets in tibble.
TweetAnalysis <- R6Class(
  "TweetAnalysis",
  public = list(

    #' @field data Pulled tweets. This is subject to change, every time a set
    #' of tweets are pulled.
    data = NULL,


    #' @field lexicons A list of tibbles containing lexicons. It is NULL by
    #' default, but is given the list at initialization
    lexicons = NULL,

    # Negation words------------------------------------------------------------

    #' @field adjust_negation If "yes", do adjust bi-gram adjustment.
    adjust_negation = "yes",

    #' @field negation_words  Negation words used for bi-gram adjustments.
    negation_words = c("not", "no", "never", "without",
                       "didn't", "didnt", "don't", "dont",
                       "doesn't", "doesnt", "isn't", "isnt",
                       "wasn't", "wasnt", "shouldn't", "shouldnt",
                       "couldn't", "couldnt", "won't", "wont",
                       "can't", "cant"),

    # Alot of the ones I added are already a filler word. (Let's think
    # about if I want to remove them from the filler words or not.)

    #' @description
    #' Add word to negation_words (lower case). Throw a warning if the word
    #' already exists.
    #' @param negation_word Negation word
    add_negation_word = function(negation_word) {

      negation_word <- tolower(negation_word)

      if(!negation_word %in% self$negation_words) {
        self$negation_words <- c(self$negation_words, negation_word)
      }
      else {
        warning("Already a negation word!")
      }

      invisible(self)
    },

    #' @description
    #' Remove word from negation_words (lower case). Throw a warning if the
    #' word doesn't exists.
    #' @param negation_word Negation word
    remove_negation_word = function(negation_word) {

      negation_word <- tolower(negation_word)

      if(negation_word %in% self$negation_words) {
        self$negation_words <- self$negation_words[self$negation_words != negation_word]
      }
      else {
        warning("Not a registered negation word!")
      }

      invisible(self)
    },

    # Filler words--------------------------------------------------------------

    #' @field filler_words  Stop words that are irrelevant from analysis.
    #' Dataset from tidytext.
    filler_words = NULL,

    #' @description
    #' Add word from filler_words (lower case). Throw a warning if the word
    #' already exists.
    #' @param filler_word Filler word
    add_filler_word = function(filler_word) {

      filler_word <- tolower(filler_word)

      if(!filler_word %in% self$filler_words$word) {
        self$filler_words <- rbind(self$filler_words,
                                   tibble(word = c(word),
                                          lexicon = c("custom")))
      }
      else {
        warning("Already a filler word!")
      }

      invisible(self)
    },

    #' @description
    #' Remove word from filler_words (lower case). Throw a warning if the word
    #' doesn't exists.
    #' @param filler_word Filler word
    remove_filler_word = function(filler_word) {

      filler_word <- tolower(filler_word)

      if(filler_word %in% self$filler_words$word) {
        self$filler_words <- self$filler_words %>%
          filter(.data$word != filler_word)
      }
      else {
        warning("Not a registered filler word!")
      }

      invisible(self)
    },

    # Conduct analysis----------------------------------------------------------

    #' @description
    #' Conduct sentiment analysis on self$data, using self$lexicons,
    #' self$filler_words and self$negation_words.
    analyze = function() {
      if(is.null(self$data)) {

        shinyalert(
          title = "No tweets pulled yet!",
          type = "error",
          inputId = "r6_analyze_error"
        )

        invisible(self)
      }
      else {
        analysis_outputs <- tokenize_tweets(
          tweets = self$data,
          lexicons = self$lexicons,
          filler_words = self$filler_words,
          negation_words = self$negation_words,
          adjust_negation = self$adjust_negation
        )

        return(analysis_outputs)

        # or I can take the list of outputs, and plug them into a R6 field!
      }
    },

    #' @field analysis_result A list of two tibbles and a ggplot plot, that
    #' contain the result of text analysis, performed by self$analyze()
    analysis_result = NULL,

    # Initialize ---------------------------------------------------------------

    #' @description
    #' Pass in the stop_words internal data.
    #'
    #' @param filler_words stop_words internal data that is available in
    #' R/sysdata.rda. I don't know why I need to initialize this, and
    #' simply doing filler_words = stop_words don't work?
    #'
    #' @param lexicons A list of tibbles containing lexicons.
    #'
    initialize = function(filler_words, lexicons) {
      stopifnot(is.data.frame(filler_words))
      stopifnot(is.list(lexicons) & all(unlist(lapply(lexicons, is.data.frame))))

      self$filler_words <- filler_words
      self$lexicons <- lexicons
    }
  )
)


