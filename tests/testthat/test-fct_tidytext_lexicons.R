test_that("get_lexicons works", {
  lexicons <- get_lexicons()

  expect_true(is.list(lexicons))
  expect_equal(names(lexicons), c("afinn", "bing", "nrc"))

  # All lexicons have non empty rows.
  expect_true(all(as.logical(lapply(lexicons, function(x){nrow(x) != 0}))))
})

test_that("process_lexicons works", {
  lexicons <- process_lexicons()

  expect_true(is.list(lexicons))
  expect_equal(names(lexicons), c("afinn", "bing", "nrc"))

  # All lexicons have correct column names
  expect_true(all(as.logical(lapply(lexicons, function(x){identical(names(x), c("word", "value"))}))))
  # All lexicons have "value" column as dbl
  expect_true(all(as.logical(lapply(lexicons, function(x){is.double(x$value)}))))
})
