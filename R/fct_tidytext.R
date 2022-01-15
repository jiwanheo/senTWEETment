# Lexicon related --------------------------------------------------------------
#' Download lexicons
#'
#' Download Lexicons (AFINN, bing, nrc) to the user cache. This
#' function asks the user to explicitly agree the conditions for the data.
#'
#' @param retrieve If TRUE, return a list of 3 lexicons. If FALSE, invisibly
#' return the list. (used for downloading the lexicons initially)
#' @export
#' @importFrom textdata lexicon_afinn lexicon_bing lexicon_nrc

get_lexicons <- function(retrieve = FALSE) {

  afinn <- lexicon_afinn()
  bing <- lexicon_bing()
  nrc <- lexicon_nrc()

  lexicons <- list(
    afinn = afinn,
    bing = bing,
    nrc = nrc
  )

  if(retrieve) {
    lexicons
  }
  else {
    invisible(lexicons)
  }
}

#' Process lexicons
#'
#' Get the lexicons, and process them to word-value format.
#'
#' @importFrom magrittr %>%
#' @importFrom dplyr transmute filter
#' @importFrom rlang .data

process_lexicons <- function() {
  lexicons <- get_lexicons(retrieve = TRUE)

  lexicons$bing <- lexicons$bing %>%
    transmute(.data$word,
              value = ifelse(.data$sentiment == "positive", 1, -1))

  lexicons$nrc <- lexicons$nrc %>%
    filter(.data$sentiment %in% c("negative", "positive")) %>%
    transmute(.data$word,
              value = ifelse(.data$sentiment == "positive", 1, -1))

  lexicons
}

# Analysis related--------------------------------------------------------------

#' Conduct Analysis
#'
#' A function to conduct the text sentiment analysis. It first strips the text
#' column of the HTML tags. Then, it tokenizes the texts with by 1 word, and
#' weed out stop words. This tokenized tibble is `inner_join`'d with the
#' lexicons to get the final score. If `adjust_negation` is "yes", then the
#' bigram negation adjustment will be performed.
#'
#' @param tweets Tweets to analyze. A tibble.
#' @param lexicons Lexicons to use, A named list of tibbles.
#' @param stop_words stop words not relevant to analysis, from
#' TweetAnalysis R6 class. A tibble.
#' @param negation_words Negation words to use from TweetAnalysis R6 class.
#' A tibble.
#' @param adjust_negation Boolean indicating whether or not to use bigram
#' adjustment.
#' @importFrom dplyr row_number mutate select filter inner_join group_by summarize
#' @importFrom magrittr %>%
#' @importFrom tidytext unnest_tokens
#' @importFrom rlang .data
tokenize_tweets <- function(tweets,
                            lexicons,
                            stop_words,
                            negation_words,
                            adjust_negation) {

  # `row_num` is used as a join column between original tweets and tokens.
  tweets <- tweets %>%
    mutate(row_num = row_number())

  # Strip the html tags
  text_stripped <- tweets %>%
    select(-c(.data$Picture, .data$User, .data$Date)) %>%
    mutate(Text = gsub('^<a.*">', "", .data$Text),
           Text = gsub("</a>", "", .data$Text))

  tokenized <- text_stripped %>%
    unnest_tokens(.data$word, .data$Text, token = "tweets")

  # token must not be a stop word
  tokenized_stopped <- tokenized  %>%
    filter_stop_words(stop_words)

  # INNER join with lexicons
  sentimented <- tokenized_stopped %>%
    inner_join(lexicons, by = "word") # I have to find a way to do this with all the lexicons

  # Bi-gram adjustment ----

  if(adjust_negation == "yes") {
    negated_words <- bigram_adjustment(lexicons, text_stripped, negation_words, stop_words)

    # Tally up the unigram sentiments with bigram sentiments.
    sentimented <- sentimented %>%
      rbind(negated_words) %>%
      group_by(.data$row_num, .data$word) %>%
      summarize(value = sum(.data$value), .groups = "drop") %>%
      filter(.data$value != 0)
  }

  # To pass to next function ----

  dfs <- list(
    tweets = tweets,
    sentimented = sentimented
  )

  produce_analysis_output(dfs)

}

#' Bigram negation adjustment
#'
#' Makes adjustments to assign a negative score to phrases like "I am not happy",
#' that would have gotten a positive score, had it not been adjusted (looking
#' at single word at a time).
#'
#' Traditionally, a single word tokenization results in a single row of
#' "word to sentiment value" per word. This function tokenizes the texts with 2
#' words. Any token that has as the first word, a negative word per
#' `negation_words`, instead gets two rows. One with the full 2-word token, and
#' another row with the original word. The sentiment value of both rows is the
#' sentiment value of the original word multiplied by -1. Then both these rows
#' are appended to the 1-word-tokenized tibble, and are summed at the word/tweet
#' level, canceling out the original word's sentiment, and adding the bigram
#' sentiment. Since we are specifically looking only for the negative words,
#' stop words will exclude negation words.
#' @param lexicons Lexicons to use, A named list of tibbles.
#' @param text_stripped Texts of tweets, processed in `produce_analysis_df`
#' @param negation_words Negation words to use from TweetAnalysis R6 class.
#' A character vector.
#' @param stop_words Stop words to use from TweetAnalysis R6 class. A tibble.
#'
#' @importFrom tidytext unnest_tokens
#' @importFrom tidyr separate
#' @importFrom tibble tibble
#' @importFrom dplyr filter inner_join mutate
#' @importFrom purrr imap_dfr pmap_dfr
#' @importFrom rlang .data

bigram_adjustment <- function(lexicons, text_stripped, negation_words, stop_words) {

  stop_words <- stop_words %>%
    filter(!.data$word %in% negation_words)

  # unigrams, stripped of stop words
  unigrams <- text_stripped %>%
    unnest_tokens(.data$word, .data$Text) %>%
    filter_stop_words(stop_words)

  # Make bigrams out of the above unigrams.

  bigrams <- imap_dfr(unigrams$word, function(x, y) {
    curr_rownum <- unigrams[[y, "row_num"]]

    if(y == nrow(unigrams)) {
      # Reached the end of unigrams df
      NULL
    }
    else if(curr_rownum == unigrams[[y+1, "row_num"]]) {
      tibble::tibble(
        row_num = curr_rownum,
        word = paste(unigrams[[y, "word"]], unigrams[[y+1, "word"]])
      )
    }
    else {
      # Last unigram of current sentence. Stop.
      NULL
    }

  }) %>% separate(.data$word, c("word1", "word2"), sep = " ")

  negated_words <- bigrams %>%
    filter(.data$word1 %in% negation_words) %>%
    inner_join(lexicons, by = c("word2" = "word")) %>%
    mutate(word1 = paste(.data$word1, .data$word2)) %>%
    pmap_dfr(function(row_num, word1, word2, value){
      data.frame(row_num = row_num,
                 word = c(word1, word2),
                 value = c(-value, -value))
    })

  negated_words
}

#' Produce the analysis outputs
#'
#' Makes adjustments to assign a negative score to phrases like "I am not happy",
#' that would have gotten a positive score, had it not been adjusted (looking
#' at single word at a time).
#' @param dfs A list of tibbles, passed from `tokenize_tweets`. This list
#' contains the original tweets, as well as tokenized sentiments.
#' @importFrom dplyr group_by summarize left_join mutate arrange slice_max
#' @importFrom ggplot2 ggplot aes geom_col
#' @importFrom forcats fct_reorder

produce_analysis_output <- function(dfs) {

  sentiment_by_tweet <- dfs$sentimented %>%
    group_by(.data$row_num) %>%
    summarize(Sentiment = sum(.data$value), .groups = "drop")

  all_tweets_scored <- dfs$tweets %>%
    left_join(sentiment_by_tweet, by = "row_num") %>%
    mutate(Sentiment = ifelse(is.na(.data$Sentiment), 0, .data$Sentiment)) %>%
    select(-.data$row_num)

  overall_score <- all_tweets_scored %>%
    summarize(Sentiment = sum(.data$Sentiment)) %>%
    as.numeric()

  word_plot <- dfs$sentimented %>%
    group_by(.data$word) %>%
    summarize(Sentiment = sum(.data$value)) %>%
    slice_max(abs(.data$Sentiment), n = 20) %>%
    mutate(word = fct_reorder(.data$word, .data$Sentiment)) %>%
    mutate(is_positive = .data$Sentiment > 0) %>%
    ggplot(aes(.data$Sentiment, .data$word, fill = .data$is_positive)) +
    geom_col()

  list(
    sentiment_by_tweet = sentiment_by_tweet,
    all_tweets_scored = all_tweets_scored,
    overall_score = overall_score,
    word_plot = word_plot
  )
}

#' Filter stop words from unigrams

#' @param tokenized_unigrams A unigram dataframe.
#' @param stop_words A stop_words dataframe.
#' @importFrom dplyr filter
#' @importFrom rlang .data

filter_stop_words <- function(tokenized_unigrams, stop_words) {

  tokenized_unigrams  %>%
    filter(!grepl("^\\@|^http", .data$word),
           !.data$word %in% stop_words$word,
           !.data$word %in% gsub("'", "", stop_words$word),
           grepl("[a-z]", .data$word))
}
