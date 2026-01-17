##############################################################################
#                        Calendar Heatmap                                    #
#                    Base R graphics version by                              #
#                        Lars Bernhardsson                                   #
#           Derived from the original lattice version by                     #
#                         Paul Bleicher                                      #
##############################################################################

## heatcal: An R function to display time-series data as a calendar heatmap
## Copyright 2009 Humedica. All rights reserved.

## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.

## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.

## You can find a copy of the GNU General Public License, Version 2 at:
## http://www.gnu.org/licenses/gpl-2.0.html

# Package-level constant: color palette definitions
# Each style defines 5 anchor colors that are interpolated to create the gradient
COLOR_STYLES <- list(
  r2b = c("#0571B0", "#92C5DE", "#F7F7F7", "#F4A582", "#CA0020"),
  r2g = c("#D61818", "#FFAE63", "#FFFFBD", "#B5E384", "#1E9B1E"),
  w2b = c("#F1EEF6", "#BDC9E1", "#74A9CF", "#2B8CBE", "#045A8D"),
  g2r = c("#1E9B1E", "#B5E384", "#FFFFBD", "#FFAE63", "#D61818")
)

# Layout constants for plot margins and sizing
YEAR_PANEL_MARGINS <- c(1, 7, 2, 2)
LEGEND_MARGINS <- c(2, 7, 3, 2)
LEGEND_HEIGHT_RATIO <- 0.8

# Grid structure constants
DAYS_PER_WEEK <- 7
FIRST_DAY_IDX <- 0
LAST_DAY_IDX <- 6
MONTHS_PER_YEAR <- 12

# Drawing constants
CELL_OFFSET <- 0.5
BOUNDARY_LWD <- 1.5
CELL_BORDER_LWD <- 0.5
LEGEND_BORDER_LWD <- 1

# Text sizing constants
YEAR_LABEL_CEX <- 0.9
AXIS_LABEL_CEX <- 0.7
LEGEND_LABEL_CEX <- 0.8
YEAR_LABEL_LINE <- 3
AXIS_LINE_OFFSET <- -1
LEGEND_AXIS_LINE <- -0.5

# Value range expansion for identical values
IDENTICAL_VALUE_EXPAND <- 0.5

# Helper function to map numeric values to colors
#' @noRd
get_value_color <- function(v, val.range, col.vec) {
  ncolors <- length(col.vec)
  idx <- round((v - val.range[1]) / diff(val.range) * (ncolors - 1)) + 1
  idx <- pmax(1, pmin(ncolors, idx))
  cols <- col.vec[idx]
  cols[is.na(v)] <- NA
  cols
}

# Helper function to draw year boundary (outer border)
#' @noRd
draw_year_boundary <- function(yr.dat) {
  y.start <- yr.dat$dotw[1]
  y.end <- yr.dat$dotw[nrow(yr.dat)]
  x.start <- yr.dat$woty[1] - CELL_OFFSET
  x.end <- yr.dat$woty[nrow(yr.dat)] + CELL_OFFSET

  top.edge <- LAST_DAY_IDX + CELL_OFFSET
  bottom.edge <- FIRST_DAY_IDX - CELL_OFFSET

  start.offset <- if (y.start > FIRST_DAY_IDX) 1 else 0
  end.offset <- if (y.end < LAST_DAY_IDX) 1 else 0

  if (y.start > FIRST_DAY_IDX) {
    graphics::segments(x.start + 1, y.start - CELL_OFFSET, x.start + 1, bottom.edge, col = "black", lwd = BOUNDARY_LWD)
    graphics::segments(x.start, y.start - CELL_OFFSET, x.start + 1, y.start - CELL_OFFSET, col = "black", lwd = BOUNDARY_LWD)
    graphics::segments(x.start, y.start - CELL_OFFSET, x.start, top.edge, col = "black", lwd = BOUNDARY_LWD)
  } else {
    graphics::segments(x.start, bottom.edge, x.start, top.edge, col = "black", lwd = BOUNDARY_LWD)
  }

  graphics::segments(x.start, top.edge, x.end - end.offset, top.edge, col = "black", lwd = BOUNDARY_LWD)
  graphics::segments(x.start + start.offset, bottom.edge, x.end, bottom.edge, col = "black", lwd = BOUNDARY_LWD)

  if (y.end < LAST_DAY_IDX) {
    graphics::segments(x.end, y.end + CELL_OFFSET, x.end, bottom.edge, col = "black", lwd = BOUNDARY_LWD)
    graphics::segments(x.end - 1, y.end + CELL_OFFSET, x.end, y.end + CELL_OFFSET, col = "black", lwd = BOUNDARY_LWD)
    graphics::segments(x.end - 1, y.end + CELL_OFFSET, x.end - 1, top.edge, col = "black", lwd = BOUNDARY_LWD)
  } else {
    graphics::segments(x.end, bottom.edge, x.end, top.edge, col = "black", lwd = BOUNDARY_LWD)
  }
}

#' Create a Calendar Heatmap
#'
#' Displays time-series data as a calendar heatmap, where each day is represented
#' as a cell colored according to its value. Years are stacked vertically with
#' months labeled across the top and days of the week on the left side.
#'
#' @param dates A vector of dates. Can be character strings, factors, Date objects,
#'   or POSIXt objects. Character/factor dates are parsed using `date.form`.
#' @param values A numeric vector of values corresponding to each date. Must be
#'   the same length as `dates`.
#' @param ncolors Integer. The number of colors in the gradient palette. Higher
#'   values give smoother color transitions. Default is 99.
#' @param color Character string specifying the color scheme (case-insensitive).
#'   Options are:
#'   \itemize{
#'     \item \code{"r2g"}: Red to green (default) - good for showing negative to positive
#'     \item \code{"g2r"}: Green to red - reversed, good for "lower is better" metrics
#'     \item \code{"r2b"}: Blue to red - diverging, colorblind-friendlier
#'     \item \code{"w2b"}: White to blue - sequential, good for counts/frequencies
#'   }
#' @param date.form Character string specifying the date format for parsing
#'   character/factor dates. Uses [strptime()] format codes.
#'   Default is `"%Y-%m-%d"` (e.g., "2024-01-15").
#' @param week.start Character string specifying which day starts the week.
#'   Either "Monday" (default, ISO standard) or "Sunday" (US standard).
#'
#' @return Invisibly returns a data frame containing the processed calendar data
#'   with columns:
#'   \itemize{
#'     \item \code{date.seq}: Date sequence covering all years in the input
#'     \item \code{value}: The mapped values (NA for dates not in input)
#'     \item \code{dotw}: Day of the week (0-6, where 0 is the first day per week.start)
#'     \item \code{woty}: Week of the year (1-53)
#'     \item \code{yr}: Year as a factor
#'     \item \code{month}: Month number (1-12)
#'   }
#'
#' @details
#' The function creates a multi-panel plot with one row per year and a color
#' legend at the bottom. Each cell represents one day, colored according to its
#' value using the specified color gradient. Days without data are left uncolored.
#'
#' Month boundaries are drawn with thicker black lines to help distinguish
#' months visually. The color scale is linear, mapping the minimum value to
#' one end of the palette and the maximum to the other.
#'
#' If duplicate dates are provided, a warning is issued and only the last value
#' for each date is used. If all values are identical, the color range is
#' expanded by +/-0.5 to ensure proper color mapping.
#'
#' Graphics parameters are saved and restored on exit, so the function should
#' not affect subsequent plots.
#'
#' @examples
#' # Basic usage with random data
#' set.seed(42)
#' dates <- seq(as.Date("2023-01-01"), as.Date("2024-06-30"), by = "day")
#' values <- cumsum(rnorm(length(dates)))
#' heatcal(dates, values)
#'
#' # Using different color schemes
#' heatcal(dates, values, color = "w2b") # White to blue
#' heatcal(dates, values, color = "r2b") # Blue to red (diverging)
#'
#' # Sunday-start weeks (US style)
#' heatcal(dates, values, week.start = "Sunday")
#'
#' # Parsing dates from character strings
#' date_strings <- c("01/15/2024", "01/16/2024", "01/17/2024")
#' vals <- c(10, 20, 15)
#' heatcal(date_strings, vals, date.form = "%m/%d/%Y")
#'
#' @seealso [strptime()] for date format codes,
#'   [grDevices::colorRampPalette()] for the color interpolation used internally
#'
#' @author Paul Bleicher (original), Lars Bernhardsson (base R port)
#'
#' @importFrom grDevices colorRampPalette
#' @importFrom graphics axis layout mtext par plot rect segments
#' @export
heatcal <- function(dates,
                    values,
                    ncolors = 99,
                    color = "r2g",
                    date.form = "%Y-%m-%d",
                    week.start = c("Monday", "Sunday")) {
  # --- Input Validation ---
  if (is.null(dates) || length(dates) == 0) {
    stop("dates cannot be NULL or empty")
  }
  if (is.null(values) || length(values) == 0) {
    stop("values cannot be NULL or empty")
  }
  if (length(dates) != length(values)) {
    stop("dates and values must have the same length")
  }
  if (!is.numeric(ncolors) || length(ncolors) != 1 || is.na(ncolors) ||
    ncolors < 1 || ncolors != floor(ncolors)) {
    stop("ncolors must be a positive integer")
  }
  ncolors <- as.integer(ncolors)
  if (!is.numeric(values)) {
    stop("values must be numeric")
  }
  color <- tolower(color)
  if (!color %in% names(COLOR_STYLES)) {
    stop("color must be one of: ", paste(names(COLOR_STYLES), collapse = ", "))
  }
  if (!is.character(date.form) || length(date.form) != 1 || is.na(date.form)) {
    stop("date.form must be a single character string")
  }

  week.start <- match.arg(week.start)

  # --- Date Parsing ---
  if (is.character(dates) || is.factor(dates)) {
    original_na <- is.na(dates)
    dates <- strptime(dates, date.form)
    if (any(is.na(dates) & !original_na)) {
      stop("Some dates could not be parsed with format: ", date.form)
    }
  }

  if (all(is.na(dates))) {
    stop("No valid dates provided")
  }

  # --- Build Calendar Data Frame ---
  min.date <- as.Date(paste0(format(min(dates, na.rm = TRUE), "%Y"), "-01-01"))
  max.date <- as.Date(paste0(format(max(dates, na.rm = TRUE), "%Y"), "-12-31"))

  caldat <- data.frame(date.seq = seq(min.date, max.date, by = "days"), value = NA)
  dates <- as.Date(dates)

  if (anyDuplicated(dates)) {
    warning("Duplicate dates found in input; only the last value for each date will be used")
  }

  valid_idx <- !is.na(dates) & !duplicated(dates, fromLast = TRUE)
  match_idx <- match(dates[valid_idx], caldat$date.seq)
  matched <- !is.na(match_idx)
  caldat$value[match_idx[matched]] <- values[valid_idx][matched]

  # --- Calculate Day of Week and Week of Year ---
  if (week.start == "Monday") {
    caldat$dotw <- (as.numeric(format(caldat$date.seq, "%w")) + 6) %% 7
    caldat$woty <- as.numeric(format(caldat$date.seq, "%W")) + 1
    day.labels <- c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun")
  } else {
    caldat$dotw <- as.numeric(format(caldat$date.seq, "%w"))
    caldat$woty <- as.numeric(format(caldat$date.seq, "%U")) + 1
    day.labels <- c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat")
  }
  caldat$yr <- as.factor(format(caldat$date.seq, "%Y"))
  caldat$month <- as.numeric(format(caldat$date.seq, "%m"))

  # --- Color Palette Setup ---
  calendar.pal <- grDevices::colorRampPalette(COLOR_STYLES[[color]], space = "Lab")
  col.vec <- calendar.pal(ncolors)

  # --- Layout Configuration ---
  yrs <- unique(caldat$yr)
  nyr <- length(yrs)
  max.woty <- max(caldat$woty) + 1

  old.par <- graphics::par(no.readonly = TRUE)
  on.exit(graphics::par(old.par))

  graphics::layout(matrix(c(1:nyr, nyr + 1), ncol = 1),
    heights = c(rep(1, nyr), LEGEND_HEIGHT_RATIO)
  )

  # --- Value-to-Color Mapping ---
  val.range <- range(caldat$value, na.rm = TRUE)
  if (diff(val.range) == 0) {
    val.range <- val.range + c(-IDENTICAL_VALUE_EXPAND, IDENTICAL_VALUE_EXPAND)
  }

  # --- Render Each Year ---
  for (yr.idx in seq_along(yrs)) {
    yr <- yrs[yr.idx]
    yr.dat <- caldat[caldat$yr == yr, ]

    graphics::par(mar = YEAR_PANEL_MARGINS)
    graphics::plot(NULL,
      xlim = c(CELL_OFFSET, max.woty),
      ylim = c(LAST_DAY_IDX + CELL_OFFSET, FIRST_DAY_IDX - CELL_OFFSET),
      xlab = "", ylab = "", axes = FALSE, asp = NA
    )

    graphics::mtext(as.character(yr),
      side = 2, line = YEAR_LABEL_LINE,
      las = 1, cex = YEAR_LABEL_CEX
    )

    graphics::axis(2,
      at = FIRST_DAY_IDX:LAST_DAY_IDX, labels = day.labels,
      las = 1, tick = FALSE, cex.axis = AXIS_LABEL_CEX,
      line = AXIS_LINE_OFFSET
    )

    month.centers <- sapply(seq_len(MONTHS_PER_YEAR), function(m) {
      month.days <- which(yr.dat$month == m)
      if (length(month.days) > 0) {
        mean(c(yr.dat$woty[month.days[1]], yr.dat$woty[month.days[length(month.days)]]))
      } else {
        NA
      }
    })
    graphics::axis(3,
      at = month.centers, labels = month.abb, tick = FALSE,
      cex.axis = AXIS_LABEL_CEX, line = AXIS_LINE_OFFSET
    )

    # --- Draw Calendar Cells ---
    cell.cols <- get_value_color(yr.dat$value, val.range, col.vec)
    graphics::rect(yr.dat$woty - CELL_OFFSET, yr.dat$dotw - CELL_OFFSET,
      yr.dat$woty + CELL_OFFSET, yr.dat$dotw + CELL_OFFSET,
      col = cell.cols, border = "grey", lwd = CELL_BORDER_LWD
    )

    # --- Draw Month Boundaries ---
    top.edge <- LAST_DAY_IDX + CELL_OFFSET
    bottom.edge <- FIRST_DAY_IDX - CELL_OFFSET

    for (m in seq_len(MONTHS_PER_YEAR)) {
      month.days <- yr.dat[yr.dat$month == m, ]
      if (nrow(month.days) == 0) next

      last.day <- month.days[nrow(month.days), ]
      x.last <- last.day$woty + CELL_OFFSET
      y.last <- last.day$dotw + CELL_OFFSET

      graphics::segments(x.last, bottom.edge, x.last, y.last,
        col = "black", lwd = BOUNDARY_LWD
      )

      if (last.day$dotw < LAST_DAY_IDX) {
        graphics::segments(x.last, y.last, x.last - 1, y.last,
          col = "black", lwd = BOUNDARY_LWD
        )
        graphics::segments(x.last - 1, y.last, x.last - 1, top.edge,
          col = "black", lwd = BOUNDARY_LWD
        )
      }
    }

    draw_year_boundary(yr.dat)
  }

  # --- Draw Color Legend ---
  graphics::par(mar = LEGEND_MARGINS)
  graphics::plot(NULL,
    xlim = c(0, ncolors), ylim = c(0, 1),
    xlab = "", ylab = "", axes = FALSE
  )

  graphics::rect(0:(ncolors - 1), 0, 1:ncolors, 1, col = col.vec, border = NA)
  graphics::rect(0, 0, ncolors, 1, border = "black", lwd = LEGEND_BORDER_LWD)

  graphics::axis(1,
    at = c(0, ncolors / 2, ncolors),
    labels = round(c(val.range[1], mean(val.range), val.range[2]), 2),
    tick = FALSE, cex.axis = LEGEND_LABEL_CEX, line = LEGEND_AXIS_LINE
  )

  invisible(caldat)
}
