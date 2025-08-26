
# Data Quality – Four Tables (Numeric, Categorical, Date, Logical)
# Contributed by: Gelan Ye
#
# 0) Libraries & Data
library(dplyr)
library(purrr)
library(tibble)
library(knitr)
library(kableExtra)
library(scales)

train_all <- readRDS("data/train_all.rds")

# render a table consistently
render_table <- function(df, caption, digits = 2) {
  if (nrow(df) == 0) {
    # Show a small placeholder table if no variables of this type exist
    df <- tibble(Note = "No variables of this type found in the dataset.")
  }
  kable(df, format = "html", caption = caption, digits = digits) |>
    kable_styling(full_width = FALSE)
}

# 1) Numeric variables
summarize_numeric <- function(x) {
  tibble(
    `Data Type`      = typeof(x),
    `Unique Values`  = n_distinct(x),
    `Missing Values` = sum(is.na(x)),
    `Missing %`      = percent(mean(is.na(x)), accuracy = 0.1),
    `Mean`           = mean(x, na.rm = TRUE),
    `Min`            = min(x,  na.rm = TRUE),
    Q1               = quantile(x, 0.25, na.rm = TRUE),
    Median           = median(x, na.rm = TRUE),
    Q3               = quantile(x, 0.75, na.rm = TRUE),
    `Max`            = max(x,  na.rm = TRUE)
  )
}

num_report <-
  train_all |>
  select(where(is.numeric)) |>
  imap_dfr(~ summarize_numeric(.x), .id = "Variable")

render_table(num_report, "Numerical Data Quality Report")

# 2) Categorical variables (character or factor)
summarize_categorical <- function(x) {
  ux    <- x[!is.na(x)]
  freqs <- table(ux)
  mode_val  <- names(freqs)[which.max(freqs)]
  mode_freq <- as.integer(max(freqs))
  tibble(
    `Data Type`      = class(x)[1],
    `Unique Values`  = n_distinct(x),
    `Missing Values` = sum(is.na(x)),
    `Missing %`      = percent(mean(is.na(x)), accuracy = 0.1),
    Mode             = mode_val,
    `Mode freq.`     = mode_freq,
    `Mode %`         = percent(mode_freq / length(ux), accuracy = 0.1)
  )
}

cat_report <-
  train_all |>
  select(where(~ is.character(.) || is.factor(.))) |>
  imap_dfr(~ summarize_categorical(.x), .id = "Variable")

render_table(cat_report, "Categorical Data Quality Report")

# 3) Date/time variables (Date, POSIXct, POSIXlt)
summarize_date <- function(x) {
  tibble(
    `Data Type`      = class(x)[1],
    `Missing Values` = sum(is.na(x)),
    `Missing %`      = percent(mean(is.na(x)), accuracy = 0.1),
    `Min Date`       = suppressWarnings(min(x, na.rm = TRUE)),
    `Max Date`       = suppressWarnings(max(x, na.rm = TRUE))
  )
}

date_report <-
  train_all |>
  select(where(~ inherits(., c("Date", "POSIXct", "POSIXlt")))) |>
  imap_dfr(~ summarize_date(.x), .id = "Variable")

render_table(date_report, "Date/Datetime Variables Data Quality Report", digits = 3)

# 4) Logical variables
summarize_logical <- function(x) {
  tibble(
    `Data Type`      = class(x)[1],
    `Missing Values` = sum(is.na(x)),
    `Missing %`      = percent(mean(is.na(x)), accuracy = 0.1),
    `TRUE %`         = percent(mean(x == TRUE,  na.rm = TRUE), accuracy = 0.1),
    `FALSE %`        = percent(mean(x == FALSE, na.rm = TRUE), accuracy = 0.1)
  )
}

log_report <-
  train_all |>
  select(where(is.logical)) |>
  imap_dfr(~ summarize_logical(.x), .id = "Variable")

render_table(log_report, "Logical Variables Data Quality Report")

