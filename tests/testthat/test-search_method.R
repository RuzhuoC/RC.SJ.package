test_that("search_method filters based on method keywords", {
  # Using fake/mock data
  fake_df <- data.frame(
    id = c(1, 2, 3),
    methods = c("This experiment used...", "Simulation performed...", NA),
    abstract = c("Study involved experiment", NA, NA),  # <--- Add this
    stringsAsFactors = FALSE
  )

  result <- search_method(fake_df, c("experiment"))
  expect_true(all(grepl("experiment", tolower(result$methods))))
})

test_that("search_method returns no rows if no match", {
  fake_df <- data.frame(
    id = c(1, 2, 3),
    methods = c("This experiment used...", "Simulation performed...", NA),
    abstract = c("Study involved experiment", NA, NA),  # <--- Add this
    stringsAsFactors = FALSE
  )

  result <- search_method(fake_df, c("genome"))
  expect_equal(nrow(result), 0)
})
