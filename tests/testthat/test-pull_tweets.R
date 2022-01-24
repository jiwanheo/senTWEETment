test_that("throw error correctly, no parameter", {
  expect_snapshot_error(pull_tweets(q = "", user = "", location = ""))
})

test_that("throws error corectly, both `q` and `user`", {
  expect_snapshot_error(pull_tweets(q = "#abc", user = "abc"))
})

test_that("throws error corectly, both `user` and `location`", {
  expect_snapshot_error(pull_tweets(user = "abc", location = "Toronto"))
})

# Commenting out for now, GH Actions doesn't like it (can't find token,
# but don't want to hard code it)

# test_that("pull_tweets regular+location query returns tweets data", {
#
#   suppressMessages(
#     connect_to_api("my-twitter-app")
#   )
#
#   df <- pull_tweets(q = "twitter", n = 20, location = "Toronto")
#   expect_s3_class(df, "data.frame")
#   expect_true(nrow(df) > 10)
#
#   # df has location
#   expect_true("location" %in% names(attr(df, "users")))
# })
#
# test_that("pull_tweets user query returns tweets data", {
#
#   suppressMessages(
#     connect_to_api("my-twitter-app")
#   )
#
#   df <- pull_tweets(user = "jiwanheo", n = 10)
#   expect_s3_class(df, "data.frame")
#   expect_true(nrow(df) == 10)
#
#   # pretty_tweets works
#   df <- pretty_tweets(df)
#   expect_true(nrow(df) == 10)
#   expect_equal(names(df), c("Picture", "User", "Date", "ID", "Text"))
#
# })

