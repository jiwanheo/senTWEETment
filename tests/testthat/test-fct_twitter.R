test_that("connect_to_api fails, when no value given", {
  expect_error(connect_to_api(bearer_token = NULL))
  expect_error(connect_to_api(bearer_token = ""))
})

test_that("pull_tweets regular+location query returns tweets data", {
  connect_to_api("my-twitter-app")

  df <- pull_tweets(q = "twitter", n = 20, location = "Toronto")
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) > 10)

  # df has location
  expect_true("location" %in% names(attr(df, "users")))
})

test_that("pull_tweets user query returns tweets data", {
  connect_to_api("my-twitter-app")

  df <- pull_tweets(user = "jiwanheo", n = 10)
  expect_s3_class(df, "data.frame")
  expect_true(nrow(df) == 10)

  # pretty_tweets works
  df <- pretty_tweets(df)
  expect_true(nrow(df) == 10)
  expect_equal(names(df), c("Picture", "User", "Date", "Text"))

})

