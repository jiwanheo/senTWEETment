test_that("bigram_adjustment works", {

  tweets <- data.frame(
    Picture = '<a href=\"https://twitter.com/jiwanheo\" target=_blank><img class=\"profile-table-img\" src=https://pbs.twimg.com/profile_images/1387996217959985156/1N4AcdFH_normal.jpg></img></a>',
    User = '@jiwanheo',
    Date = '2022-01-14 02:09:26 UTC',
    ID = '<a href=\"https://twitter.com/jiwanheo/status/1481810657817374720\">1481810657817374720</a>',
    Text = 'Testing not that bad sentiment analysis. @dontshowup https://thisshouldntshow #thisshouldshow'
  )

  lexicons <- data.frame(word = "bad", value = -3)
  stop_words <- data.frame(word = "that", lexicon = "custom")
  negation_words <- c("no", "not")

  bigrams <- bigram_adjustment(lexicons, tweets, negation_words, stop_words)

  expect_equal(nrow(bigrams), 2)
  expect_true(all(c("not bad", "bad") %in% bigrams$word))
  expect_equal(bigrams[bigrams$word == "not bad", "value"], 3)
  expect_equal(bigrams[bigrams$word == "bad", "value"], 3)
})
