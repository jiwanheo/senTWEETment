test_that("produce_analysis_output works", {

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

  suppressMessages(
    dfs <- conduct_analysis(tweets,
                            lexicons,
                            stop_words,
                            negation_words,
                            adjust_negation = "no")
  )

  outputs <- produce_analysis_output(dfs)

  expect_true(all(identical(names(outputs), c("sentiment_by_tweet",
                                              "all_words_scored",
                                              "all_tweets_scored",
                                              "overall_scores",
                                              "word_plot"))))

  # The sentiment dfs should have same # rows as the original tweets
  expect_true(nrow(dfs$tweets) == nrow(outputs$sentiment_by_tweet))
  expect_true(nrow(dfs$tweets) == nrow(outputs$all_tweets_scored))

  # Word sentiment should have at least the # rows as the original tweets.
  expect_true(nrow(dfs$tweets) <= nrow(outputs$all_words_scored))

  expect_equal(outputs$overall_scores$sentiment_n, 1)
  expect_equal(outputs$overall_scores$sentiment_sum, -3)
  expect_equal(outputs$overall_scores$sentiment_avg, -3)

  expect_s3_class(outputs$word_plot, "ggplot")

  # Output values except ggplot
  expect_snapshot(outputs$sentiment_by_tweet)
  expect_snapshot(outputs$all_words_scored)
  expect_snapshot(outputs$all_tweets_scored)
  expect_snapshot(outputs$overall_scores)
})
