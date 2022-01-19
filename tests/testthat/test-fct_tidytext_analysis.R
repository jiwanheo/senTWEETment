test_that("conduct_analysis works", {

  # Setup
  tweets <- data.frame(
    Picture = '<a href=\"https://twitter.com/jiwanheo\" target=_blank><img class=\"profile-table-img\" src=https://pbs.twimg.com/profile_images/1387996217959985156/1N4AcdFH_normal.jpg></img></a>',
    User = '@jiwanheo',
    Date = '2022-01-14 02:09:26 UTC',
    ID = '<a href=\"https://twitter.com/jiwanheo/status/1481810657817374720\">1481810657817374720</a>',
    Text = 'Testing not that bad sentiment analysis. @dontshowup https://thisshouldntshow #thisshouldshow'
  )
  lexicons <- data.frame(word = "bad", value = -3)
  stop_words <- stop_words
  negation_words <- c("no", "not")

  dfs <- conduct_analysis(tweets,
                          lexicons,
                          stop_words,
                          negation_words,
                          adjust_negation = "no")

  # return type is correct
  expect_true(is.list(dfs))
  expect_true(is.data.frame(dfs$tweets))
  expect_true(is.data.frame(dfs$sentimented))

  # The first df of conduct_analysis returns the same length row.
  expect_true(nrow(dfs$tweets) == nrow(tweets))

  # Regular Sentiment analysis per AFINN worked.
  expect_equal(dfs$sentimented$word[[1]], "bad")
  expect_equal(dfs$sentimented$value[[1]], -3)

  # Negation adjustment worked.
  dfs <- conduct_analysis(tweets,
                          lexicons,
                          stop_words,
                          negation_words,
                          adjust_negation = "yes")

  expect_equal(dfs$sentimented$word[[1]], "not bad")
  expect_equal(dfs$sentimented$value[[1]], 3)

  # produce_analysis_output works

  outputs <- produce_analysis_output(dfs)

  expect_true(nrow(dfs$tweets) == nrow(outputs$sentiment_by_tweet))
  expect_true(nrow(dfs$tweets) == nrow(outputs$all_tweets_scored))
  expect_true(nrow(dfs$tweets) <= nrow(outputs$all_words_scored))

  expect_equal(outputs$overall_scores$sentiment_n, 1)
  expect_equal(outputs$overall_scores$sentiment_sum, 3)
  expect_equal(outputs$overall_scores$sentiment_avg, 3)

  expect_s3_class(outputs$word_plot, "ggplot")
})
