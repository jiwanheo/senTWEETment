test_that("R6 works", {
  # Creation without stop_words fails
  expect_error(TweetAnalysis$new())

  ta <-  TweetAnalysis$new(stop_words = stop_words)

  # Check if fields displays correct behaviours after initialization

  expect_null(ta$data)
  expect_null(ta$lexicons)
  expect_equal(ta$adjust_negation, "yes")
  expect_true(is.data.frame(ta$stop_words))
  expect_true(is.character(ta$negation_words))
  expect_null(ta$analysis_result)

  # Word adding/removing works

  ta$add_negation_word("random_word")
  expect_true("random_word" %in% ta$negation_words)

  ta$remove_negation_word("random_word")
  expect_true(!"random_word" %in% ta$negation_words)

  ta$add_stop_word("random_word")
  expect_true("random_word" %in% ta$stop_words$word)

  ta$remove_stop_word("random_word")
  expect_true(!"random_word" %in% ta$stop_words$word)

  # Lexicon adding works
  ta$lexicons <- data.frame(word = "bad", value = -3)
  expect_true(!is.null(ta$lexicons))

  # analyze fails without tweets
  expect_error(ta$analyze())

  # analyze works
  tweets <- data.frame(
    Picture = '<a href=\"https://twitter.com/jiwanheo\" target=_blank><img class=\"profile-table-img\" src=https://pbs.twimg.com/profile_images/1387996217959985156/1N4AcdFH_normal.jpg></img></a>',
    User = '@jiwanheo',
    Date = '2022-01-14 02:09:26 UTC',
    ID = '<a href=\"https://twitter.com/jiwanheo/status/1481810657817374720\">1481810657817374720</a>',
    Text = 'Testing not that bad sentiment analysis. @dontshowup https://thisshouldntshow #thisshouldshow'
  )
  ta$data <- tweets
  expect_true(!is.null(ta$data))

  analysis_output <- ta$analyze()

  expect_true(identical(
    names(analysis_output),
    c("sentiment_by_tweet",
      "all_words_scored",
      "all_tweets_scored",
      "overall_scores",
      "word_plot")
    ))
})
