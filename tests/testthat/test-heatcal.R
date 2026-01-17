test_that("heatcal validates input arguments", {
  # NULL/empty dates

  expect_error(heatcal(NULL, 1:5), "dates cannot be NULL or empty")
  expect_error(heatcal(character(0), numeric(0)), "dates cannot be NULL or empty")

  # NULL/empty values
  expect_error(heatcal(Sys.Date(), NULL), "values cannot be NULL or empty")

  # Mismatched lengths
  expect_error(
    heatcal(Sys.Date() + 0:2, 1:5),
    "dates and values must have the same length"
  )

  # Invalid ncolors
  expect_error(heatcal(Sys.Date(), 1, ncolors = -1), "ncolors must be a positive integer")
  expect_error(heatcal(Sys.Date(), 1, ncolors = 1.5), "ncolors must be a positive integer")
  expect_error(heatcal(Sys.Date(), 1, ncolors = NA), "ncolors must be a positive integer")

  # Non-numeric values
  expect_error(heatcal(Sys.Date(), "a"), "values must be numeric")

  # Invalid color scheme
  expect_error(heatcal(Sys.Date(), 1, color = "invalid"), "color must be one of")

  # Invalid date format
  expect_error(
    heatcal(Sys.Date(), 1, date.form = 123),
    "date.form must be a single character string"
  )

  # Unparseable dates
  expect_error(
    heatcal("not-a-date", 1),
    "Some dates could not be parsed"
  )
})

test_that("heatcal accepts different date formats", {
  skip_on_cran()

  # Date objects
  dates <- as.Date("2024-01-01") + 0:9
  values <- 1:10
  expect_silent(result <- heatcal(dates, values))
  expect_s3_class(result, "data.frame")

  # Character dates with default format
  dates_char <- as.character(dates)
  expect_silent(heatcal(dates_char, values))

  # Character dates with custom format
  dates_custom <- format(dates, "%d/%m/%Y")
  expect_silent(heatcal(dates_custom, values, date.form = "%d/%m/%Y"))

  # POSIXct
  dates_posix <- as.POSIXct(dates)
  expect_silent(heatcal(dates_posix, values))
})

test_that("heatcal returns correct data structure", {
  skip_on_cran()

  dates <- as.Date("2024-01-01") + 0:30
  values <- rnorm(31)

  result <- heatcal(dates, values)

  expect_true(is.data.frame(result))
  expect_true(all(c("date.seq", "value", "dotw", "woty", "yr", "month") %in% names(result)))
  expect_s3_class(result$date.seq, "Date")
  expect_true(is.numeric(result$value))
  expect_true(is.numeric(result$dotw))
  expect_true(is.numeric(result$woty))
  expect_true(is.factor(result$yr))
  expect_true(is.numeric(result$month))
})

test_that("heatcal warns on duplicate dates", {
  skip_on_cran()

  dates <- rep(as.Date("2024-01-01"), 3)
  values <- 1:3

  expect_warning(heatcal(dates, values), "Duplicate dates found")
})

test_that("heatcal accepts all color schemes", {
  skip_on_cran()

  dates <- as.Date("2024-01-01") + 0:9
  values <- 1:10

  for (scheme in c("r2g", "g2r", "r2b", "w2b")) {
    expect_silent(heatcal(dates, values, color = scheme))
  }

  # Case insensitive
  expect_silent(heatcal(dates, values, color = "R2G"))
})

test_that("heatcal handles week.start options", {
  skip_on_cran()

  dates <- as.Date("2024-01-01") + 0:9
  values <- 1:10

  result_mon <- heatcal(dates, values, week.start = "Monday")
  result_sun <- heatcal(dates, values, week.start = "Sunday")

  # Day of week values should differ based on week start
  expect_true(is.data.frame(result_mon))
  expect_true(is.data.frame(result_sun))
})
