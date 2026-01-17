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

# --- Tests for get_value_color helper function ---

test_that("get_value_color maps values to colors correctly", {
  col.vec <- c("red", "orange", "yellow", "green", "blue")
  val.range <- c(0, 100)

  # Test boundary values
  expect_equal(heatcal:::get_value_color(0, val.range, col.vec), "red")
  expect_equal(heatcal:::get_value_color(100, val.range, col.vec), "blue")


  # Test middle value
  expect_equal(heatcal:::get_value_color(50, val.range, col.vec), "yellow")

  # Test values that map to intermediate colors
  expect_equal(heatcal:::get_value_color(25, val.range, col.vec), "orange")
  expect_equal(heatcal:::get_value_color(75, val.range, col.vec), "green")
})

test_that("get_value_color handles NA values", {
  col.vec <- c("red", "yellow", "green")
  val.range <- c(0, 10)

  result <- heatcal:::get_value_color(c(0, NA, 10), val.range, col.vec)
  expect_equal(result[1], "red")
  expect_true(is.na(result[2]))
  expect_equal(result[3], "green")
})

test_that("get_value_color clamps out-of-range values", {
  col.vec <- c("red", "yellow", "green")
  val.range <- c(0, 10)

  # Values below range should clamp to first color
 expect_equal(heatcal:::get_value_color(-5, val.range, col.vec), "red")

  # Values above range should clamp to last color
  expect_equal(heatcal:::get_value_color(15, val.range, col.vec), "green")
})

test_that("get_value_color handles vector input", {
  col.vec <- c("red", "yellow", "green")
  val.range <- c(0, 10)

  values <- c(0, 5, 10)
  result <- heatcal:::get_value_color(values, val.range, col.vec)

  expect_length(result, 3)
  expect_equal(result, c("red", "yellow", "green"))
})

# --- Tests for draw_year_boundary helper function ---

test_that("draw_year_boundary executes without error", {
  skip_on_cran()

  # Create sample year data similar to what heatcal produces
  dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day")
  yr.dat <- data.frame(
    date.seq = dates,
    value = runif(length(dates)),
    dotw = (as.numeric(format(dates, "%w")) + 6) %% 7,
    woty = as.numeric(format(dates, "%W")) + 1,
    yr = factor(format(dates, "%Y")),
    month = as.numeric(format(dates, "%m"))
  )

  # Use temp file for graphics output (better for coverage tracing)
  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp)
  on.exit(dev.off(), add = TRUE)

  plot(NULL, xlim = c(0, 54), ylim = c(-0.5, 6.5))

  expect_silent(heatcal:::draw_year_boundary(yr.dat))
})

test_that("draw_year_boundary handles year starting mid-week", {
  skip_on_cran()

  # 2025 starts on Wednesday (dotw = 2 for Monday-start week)
  dates <- seq(as.Date("2025-01-01"), as.Date("2025-12-31"), by = "day")
  yr.dat <- data.frame(
    date.seq = dates,
    value = runif(length(dates)),
    dotw = (as.numeric(format(dates, "%w")) + 6) %% 7,
    woty = as.numeric(format(dates, "%W")) + 1,
    yr = factor(format(dates, "%Y")),
    month = as.numeric(format(dates, "%m"))
  )

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp)
  on.exit(dev.off(), add = TRUE)

  plot(NULL, xlim = c(0, 54), ylim = c(-0.5, 6.5))

  # Year starts on Wednesday (dotw > 0), so special boundary handling
  expect_true(yr.dat$dotw[1] > 0)
  expect_silent(heatcal:::draw_year_boundary(yr.dat))
})

test_that("draw_year_boundary handles year ending mid-week", {
  skip_on_cran()

  # 2024 ends on Tuesday (dotw = 1 for Monday-start week)
  dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day")
  yr.dat <- data.frame(
    date.seq = dates,
    value = runif(length(dates)),
    dotw = (as.numeric(format(dates, "%w")) + 6) %% 7,
    woty = as.numeric(format(dates, "%W")) + 1,
    yr = factor(format(dates, "%Y")),
    month = as.numeric(format(dates, "%m"))
  )

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp)
  on.exit(dev.off(), add = TRUE)

  plot(NULL, xlim = c(0, 54), ylim = c(-0.5, 6.5))

  # Year ends on Tuesday (dotw < 6), so special boundary handling
  expect_true(yr.dat$dotw[nrow(yr.dat)] < 6)
  expect_silent(heatcal:::draw_year_boundary(yr.dat))
})

test_that("draw_year_boundary handles year starting on Monday (dotw=0)", {
  skip_on_cran()

  # 2024 starts on Monday - test the else branch (y.start == 0)
  # We need a year that starts on Monday with Monday-start weeks
  # 2024 actually starts on Monday
  dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day")
  yr.dat <- data.frame(
    date.seq = dates,
    value = runif(length(dates)),
    dotw = (as.numeric(format(dates, "%w")) + 6) %% 7,
    woty = as.numeric(format(dates, "%W")) + 1,
    yr = factor(format(dates, "%Y")),
    month = as.numeric(format(dates, "%m"))
  )

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp)
  on.exit(dev.off(), add = TRUE)

  plot(NULL, xlim = c(0, 54), ylim = c(-0.5, 6.5))

  # 2024 starts on Monday (dotw = 0)
  expect_equal(yr.dat$dotw[1], 0)
  expect_silent(heatcal:::draw_year_boundary(yr.dat))
})

test_that("draw_year_boundary handles year ending on Sunday (dotw=6)", {
  skip_on_cran()

  # Need a year that ends on Sunday with Monday-start weeks
  # 2023 ends on Sunday
  dates <- seq(as.Date("2023-01-01"), as.Date("2023-12-31"), by = "day")
  yr.dat <- data.frame(
    date.seq = dates,
    value = runif(length(dates)),
    dotw = (as.numeric(format(dates, "%w")) + 6) %% 7,
    woty = as.numeric(format(dates, "%W")) + 1,
    yr = factor(format(dates, "%Y")),
    month = as.numeric(format(dates, "%m"))
  )

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp)
  on.exit(dev.off(), add = TRUE)

  plot(NULL, xlim = c(0, 54), ylim = c(-0.5, 6.5))

  # 2023 ends on Sunday (dotw = 6)
  expect_equal(yr.dat$dotw[nrow(yr.dat)], 6)
  expect_silent(heatcal:::draw_year_boundary(yr.dat))
})

# --- Tests for main heatcal plotting logic ---

test_that("heatcal renders full calendar without error", {
  skip_on_cran()

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp)
  on.exit(dev.off(), add = TRUE)

  dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day")
  values <- sin(seq(0, 4 * pi, length.out = length(dates))) * 10

  result <- heatcal(dates, values)

  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 366)  # 2024 is a leap year
})

test_that("heatcal handles multi-year data", {
  skip_on_cran()

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp)
  on.exit(dev.off(), add = TRUE)

  dates <- seq(as.Date("2022-06-01"), as.Date("2024-06-30"), by = "day")
  values <- rnorm(length(dates))

  result <- heatcal(dates, values)

  expect_s3_class(result, "data.frame")
  # Should cover 3 full years
  expect_equal(length(unique(result$yr)), 3)
})

test_that("heatcal handles identical values", {
  skip_on_cran()

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp)
  on.exit(dev.off(), add = TRUE)

  dates <- as.Date("2024-01-01") + 0:30
  values <- rep(5, 31)  # All identical values

  # Should not error - expands range by +/- 0.5
  expect_silent(result <- heatcal(dates, values))
  expect_s3_class(result, "data.frame")
})

test_that("heatcal errors on all NA dates", {
  expect_error(
    heatcal(as.Date(c(NA, NA, NA)), 1:3),
    "No valid dates provided"
  )
})

test_that("heatcal handles sparse data across year", {
  skip_on_cran()

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp)
  on.exit(dev.off(), add = TRUE)

  # Only a few dates scattered across the year
  dates <- as.Date(c("2024-01-15", "2024-06-01", "2024-12-25"))
  values <- c(10, 20, 30)

  result <- heatcal(dates, values)

  expect_s3_class(result, "data.frame")
  # Full year should be generated
  expect_equal(nrow(result), 366)
  # Only 3 values should be non-NA
  expect_equal(sum(!is.na(result$value)), 3)
})

test_that("heatcal correctly calculates week of year for Sunday start", {
  skip_on_cran()

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp)
  on.exit(dev.off(), add = TRUE)

  dates <- seq(as.Date("2024-01-01"), as.Date("2024-01-14"), by = "day")
  values <- 1:14

  result <- heatcal(dates, values, week.start = "Sunday")

  # Verify Sunday is day 0
  sunday_idx <- which(format(result$date.seq, "%A") == "Sunday")[1]
  expect_equal(result$dotw[sunday_idx], 0)
})

test_that("heatcal correctly calculates week of year for Monday start", {
  skip_on_cran()

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp)
  on.exit(dev.off(), add = TRUE)

  dates <- seq(as.Date("2024-01-01"), as.Date("2024-01-14"), by = "day")
  values <- 1:14

  result <- heatcal(dates, values, week.start = "Monday")

  # Verify Monday is day 0
  monday_idx <- which(format(result$date.seq, "%A") == "Monday")[1]
  expect_equal(result$dotw[monday_idx], 0)
})

test_that("heatcal handles different ncolors values", {
  skip_on_cran()

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp)
  on.exit(dev.off(), add = TRUE)

  dates <- as.Date("2024-01-01") + 0:30
  values <- 1:31

  # Small number of colors
  expect_silent(heatcal(dates, values, ncolors = 5))

  # Large number of colors
  expect_silent(heatcal(dates, values, ncolors = 256))
})

test_that("heatcal renders with negative values", {
  skip_on_cran()

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp)
  on.exit(dev.off(), add = TRUE)

  dates <- as.Date("2024-01-01") + 0:30
  values <- seq(-50, 50, length.out = 31)

  result <- heatcal(dates, values)
  expect_s3_class(result, "data.frame")
})

test_that("heatcal handles months with no data in middle of year", {
  skip_on_cran()

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp)
  on.exit(dev.off(), add = TRUE)

  # Data only in January and December
  dates <- c(
    as.Date("2024-01-01") + 0:14,
    as.Date("2024-12-15") + 0:15
  )
  values <- c(1:15, 16:31)

  result <- heatcal(dates, values)
  expect_s3_class(result, "data.frame")
  expect_equal(nrow(result), 366)
})

test_that("heatcal month boundaries are drawn correctly", {
  skip_on_cran()

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp)
  on.exit(dev.off(), add = TRUE)

  # Full year to test all month boundaries
  dates <- seq(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day")
  values <- rep(1:12, each = 31)[1:length(dates)]

  expect_silent(result <- heatcal(dates, values))
  expect_equal(length(unique(result$month)), 12)
})

test_that("heatcal handles month ending on last day of week", {
  skip_on_cran()

  tmp <- tempfile(fileext = ".png")
  on.exit(unlink(tmp), add = TRUE)
  grDevices::png(tmp)
  on.exit(dev.off(), add = TRUE)

  # Test to cover the branch where last.day$dotw == LAST_DAY_IDX (line 344)
  # March 2024 ends on Sunday (dotw = 6 for Monday-start)
  dates <- seq(as.Date("2024-03-01"), as.Date("2024-03-31"), by = "day")
  values <- 1:31

  result <- heatcal(dates, values, week.start = "Monday")
  expect_s3_class(result, "data.frame")
})

# --- Tests for heatcal_aspect function ---

test_that("heatcal_aspect validates input arguments", {
  # Non-positive integers

  expect_error(heatcal_aspect(0), "nyears must be a positive integer")
  expect_error(heatcal_aspect(-1), "nyears must be a positive integer")
  expect_error(heatcal_aspect(-5), "nyears must be a positive integer")

  # Non-integer values
  expect_error(heatcal_aspect(1.5), "nyears must be a positive integer")
  expect_error(heatcal_aspect(2.7), "nyears must be a positive integer")

  # NA values
  expect_error(heatcal_aspect(NA), "nyears must be a positive integer")
  expect_error(heatcal_aspect(NA_integer_), "nyears must be a positive integer")
  expect_error(heatcal_aspect(NA_real_), "nyears must be a positive integer")

  # Non-numeric types
  expect_error(heatcal_aspect("1"), "nyears must be a positive integer")
  expect_error(heatcal_aspect("a"), "nyears must be a positive integer")
  expect_error(heatcal_aspect(TRUE), "nyears must be a positive integer")
  expect_error(heatcal_aspect(NULL), "nyears must be a positive integer")

  # Multiple values

  expect_error(heatcal_aspect(c(1, 2)), "nyears must be a positive integer")
  expect_error(heatcal_aspect(1:3), "nyears must be a positive integer")
})

test_that("heatcal_aspect returns correct numeric value", {
  result <- heatcal_aspect(1)
  expect_true(is.numeric(result))
  expect_length(result, 1)
  expect_true(result > 0)
})

test_that("heatcal_aspect calculates correct aspect ratios", {
  # Formula: 7 * (nyears + 0.8) / 53

  # 1 year: 7 * 1.8 / 53 = 12.6 / 53
  expect_equal(heatcal_aspect(1), 7 * 1.8 / 53)

  # 2 years: 7 * 2.8 / 53 = 19.6 / 53
  expect_equal(heatcal_aspect(2), 7 * 2.8 / 53)

  # 3 years: 7 * 3.8 / 53 = 26.6 / 53
  expect_equal(heatcal_aspect(3), 7 * 3.8 / 53)

  # 5 years: 7 * 5.8 / 53 = 40.6 / 53
  expect_equal(heatcal_aspect(5), 7 * 5.8 / 53)

  # 10 years: 7 * 10.8 / 53 = 75.6 / 53
  expect_equal(heatcal_aspect(10), 7 * 10.8 / 53)
})

test_that("heatcal_aspect increases with more years", {
  # Aspect ratio should increase as number of years increases
  expect_lt(heatcal_aspect(1), heatcal_aspect(2))
  expect_lt(heatcal_aspect(2), heatcal_aspect(3))
  expect_lt(heatcal_aspect(3), heatcal_aspect(5))
  expect_lt(heatcal_aspect(5), heatcal_aspect(10))
})

test_that("heatcal_aspect handles integer-like numeric values", {
  # Should accept numeric values that are whole numbers
  expect_equal(heatcal_aspect(1.0), heatcal_aspect(1))
  expect_equal(heatcal_aspect(3.0), heatcal_aspect(3))
})

test_that("heatcal_aspect produces reasonable aspect ratios", {
  # 1 year should be quite wide (aspect < 0.5)
  expect_lt(heatcal_aspect(1), 0.5)

  # Many years should be taller (aspect can exceed 1)
  expect_gt(heatcal_aspect(10), 1)

  # All ratios should be positive
  for (n in 1:20) {
    expect_gt(heatcal_aspect(n), 0)
  }
})
