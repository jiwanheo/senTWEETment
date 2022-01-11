#' R6 class of tweet analyzing process
#'
#' R6 object to encapsulate the reading/cleaning/export of tweets
#'
#' @importFrom R6 R6Class
#' @importFrom tibble tibble
#' @importFrom magrittr %>%
#' @importFrom dplyr filter
#' @importFrom rlang .data
#'
#' @section Public fields:
#' * `data`:
#' * `data_print`: print the data
TweetAnalysis <- R6Class(
  "TweetAnalysis",
  public = list(

    #' @field data Pulled tweets. This is subject to change, every time a set
    #' of tweets are pulled.
    data = NULL,

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

    #' @description
    #' Pass in the stop_words internal data.
    #'
    #' @param stop_words stop_words internal data that is available in
    #' R/sysdata.rda. I don't know why I need to initialize this, and
    #' simply doing filler_words = stop_words don't work?
    #'
    initialize = function(stop_words) {
      stopifnot(is.data.frame(stop_words))

      self$filler_words <- stop_words
    }
  )
)


