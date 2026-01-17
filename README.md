# heatcal

Create calendar heatmap visualizations for time-series data in R using base graphics.

Each day is displayed as a colored cell, with colors representing values. Years are stacked vertically with months labeled across the top and days of the week on the left side.

## Installation

```r
# Install from GitHub
devtools::install_github("lab1702/heatcal")
```

## Usage

```r
library(heatcal)

# Generate sample data
set.seed(42)
dates <- seq(as.Date("2023-01-01"), as.Date("2024-06-30"), by = "day")
values <- cumsum(rnorm(length(dates)))

# Create a calendar heatmap
heatcal(dates, values)
```

## Color Schemes

Four color schemes are available:

| Scheme | Description | Use Case |
|--------|-------------|----------|
| `r2g` | Red to green (default) | Negative to positive values |
| `g2r` | Green to red | "Lower is better" metrics |
| `r2b` | Blue to red (diverging) | Colorblind-friendlier option |
| `w2b` | White to blue (sequential) | Counts and frequencies |

```r
heatcal(dates, values, color = "w2b")  # White to blue
heatcal(dates, values, color = "r2b")  # Blue to red
```

## Week Start

Choose between Monday (ISO standard) or Sunday (US standard) as the first day of the week:

```r
heatcal(dates, values, week.start = "Monday")  # Default
heatcal(dates, values, week.start = "Sunday")  # US style
```

## Date Parsing

The function accepts Date objects, POSIXt objects, or character strings. For character dates, specify the format:

```r
date_strings <- c("01/15/2024", "01/16/2024", "01/17/2024")
vals <- c(10, 20, 15)
heatcal(date_strings, vals, date.form = "%m/%d/%Y")
```

## Authors

- Paul Bleicher (original lattice version)
- Lars Bernhardsson (base R port)

## License

GPL-2
