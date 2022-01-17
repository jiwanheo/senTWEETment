test_that("multiplication works", {
  coords <- lookup_coords_nominatim("Toronto")

  # coords has correct class
  expect_true(identical(attr(coords, "class"), c("coords", "list")))

  # coords has correct names
  expect_true(identical(names(coords), c("place", "box", "point")))

  # place is correct
  expect_true(coords$place == "Toronto, Golden Horseshoe, Ontario, Canada")

  # box is correct (rounded at 5 digits)
  expect_true(identical(
    round(coords$box, 5),
    c(sw.lng = -79.49282,
      sw.lat = 43.61038,
      ne.lng = -79.27851,
      ne.lat = 43.73602)
  ))

  # point is correct (rounded at 5 digits)
  expect_true(identical(
    round(coords$point, 5),
    c(lat = 43.65348,
      lng = -79.38393)
  ))
})
