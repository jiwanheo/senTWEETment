test_that("filter_stop_words works", {
  df <- data.frame(
    ID = "1",
    word = c("@hello", "https://fakeurl.com", "aint", "ain't", "123", "abc")
  )

  stop_words <- data.frame(
    word = "ain't",
    lexicon = "custom"
  )

  df_ <- filter_stop_words(df, stop_words)

  # All the other words should have been filtered out.
  expect_equal(nrow(df_), 1)
  expect_equal(df_[1, "word"], "abc")

})
