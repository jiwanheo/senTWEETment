test_that("connect_to_api fails, when no value given", {
  expect_error(connect_to_api(bearer_token = NULL))
  expect_error(connect_to_api(bearer_token = ""))
})

test_that("can save and reload auth", {
  withr::local_options("rtweet:::config_dir" = tempfile())

  auth1 <- rtweet::rtweet_app("abc")
  suppressMessages({
    rtweet::auth_save(auth1, "test")
    path <- file.path(getOption("rtweet:::config_dir", rappdirs::user_config_dir("rtweet", "R")), "test.rds")
    auth2 <- readRDS(path)
  })
  expect_equal(auth1, auth2)
})
